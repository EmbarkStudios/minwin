use rayon::prelude::*;
use serde::{Deserialize, Serialize};
use std::path::Path;

fn update_winmd(md_dir: &Path) {
    let md = cargo_metadata::MetadataCommand::new()
        .manifest_path("Cargo.toml")
        .exec()
        .expect("failed to retrieve metadata");

    let wm = md
        .packages
        .iter()
        .find(|pkg| pkg.name == "windows-metadata")
        .expect("failed to find windows-metadata");

    // Read the VCS info for windows-metadata from ~/.cargo/registry/src/github.com-1ecc6299db9ec823/windows-metadata-<version>/.cargo_vcs_info.json
    let vcs_info_path = wm
        .manifest_path
        .parent()
        .unwrap()
        .join(".cargo_vcs_info.json");

    #[derive(Deserialize)]
    struct Git {
        sha1: String,
    }

    #[derive(Deserialize)]
    struct VcsInfo {
        git: Git,
        path_in_vcs: String,
    }

    let vcs_info =
        std::fs::read_to_string(vcs_info_path).expect("failed to read .cargo_vcs_info.json");
    let vcs_info: VcsInfo =
        serde_json::from_str(&vcs_info).expect("failed to deserialize .cargo_vcs_info.json");

    let winmd_files = ["Windows.Win32.winmd", "Windows.winmd", "Windows.Wdk.winmd"];

    let url_root = format!(
        "https://github.com/microsoft/windows-rs/raw/{}/{}/default/",
        vcs_info.git.sha1, vcs_info.path_in_vcs
    );

    let client = reqwest::blocking::Client::new();

    winmd_files.into_par_iter().for_each(|winmd| {
        println!("Downloading {winmd}");
        let buffer = client
            .get(format!("{url_root}{winmd}"))
            .send()
            .expect("failed to send request")
            .error_for_status()
            .expect("failed to retrieve winmd file")
            .bytes()
            .expect("failed to read response");

        println!("Compressing {winmd}");
        let compressed =
            zstd::encode_all(std::io::Cursor::new(buffer), 5).expect("failed to compress");

        println!("Writing {winmd}");
        std::fs::write(md_dir.join(winmd).with_extension("winmd.zstd"), compressed)
            .expect("failed to write winmd file");
    });
}

fn update_layout(md_dir: &Path) {
    #[derive(Serialize, PartialEq, Eq, Debug, Copy, Clone)]
    #[serde(tag = "l", content = "s")]
    enum Layout {
        Packed(u8),
        Align(u8),
    }

    #[derive(Deserialize, Debug)]
    struct OwnedDecl {
        name: String,
    }

    #[derive(Deserialize, Debug, Clone)]
    struct Type {
        #[serde(rename = "qualType")]
        qt: Option<String>,
    }

    #[derive(Deserialize, Debug)]
    #[serde(untagged)]
    enum GoddammitClang {
        Number(u8),
        Dumb(String),
    }

    #[derive(Deserialize, Debug)]
    pub enum Item {
        RecordDecl {
            name: Option<String>,
            #[serde(default, rename = "completeDefinition")]
            complete_definition: bool,
        },
        MaxFieldAlignmentAttr {
            // This field won't be present with an unpatched clang!
            alignment: u8,
        },
        TypedefDecl {
            name: String,
            #[serde(rename = "type")]
            kind: Option<Type>,
        },
        TypedefType,
        PointerType,
        ElaboratedType {
            #[serde(rename = "ownedTagDecl")]
            owned_tag_decl: Option<OwnedDecl>,
        },
        AlignedAttr,
        ConstantExpr {
            value: GoddammitClang,
        },
        /// We don't care about other items
        Other,
    }

    // We need to use a patched clang, as in vanilla clang the MaxFieldAlignmentAttr
    // that is emitted on records that are between push(pack, <packing>)...don't
    // actually specify what that packing is, making it useless
    const CLANG: &str = "../temp-clones/build-clang/bin/clang";
    // Just use xwin so we aren't locked to doing this on a windows machine
    const XWIN_SDK_INC: &str = "../xwin/.xwin-cache/splat/sdk/include";

    let mut collected = Vec::new();

    ["i686", "x86_64", "aarch64"]
        .into_par_iter()
        .map(|arch| {
            let mut cmd = std::process::Command::new(CLANG);
            cmd.arg(format!("--target={arch}-pc-windows-msvc"));
            cmd.args([
                "-fsyntax-only",
                "-Xclang",
                "-ast-dump=json",
                "-fms-compatibility",
            ]);
            cmd.arg("-I");
            cmd.arg(format!("{XWIN_SDK_INC}/um"));
            cmd.arg("-I");
            cmd.arg(format!("{XWIN_SDK_INC}/shared"));
            cmd.arg(format!("{XWIN_SDK_INC}/um/windows.h"));

            cmd.stdout(std::process::Stdio::piped());
            cmd.stderr(std::process::Stdio::piped());

            let captured = cmd.output().expect("failed to run clang to gather AST");

            // Ignore errors, there's a ton of bullshit in windows.h and friends
            // we just don't need to care about
            // if !captured.status.success() {
            //     panic!(
            //         "clang failed to gather AST {:?}\n{}",
            //         captured.status,
            //         String::from_utf8(captured.stderr).unwrap_or(String::new())
            //     );
            // }

            type Node = clang_ast::Node<Item>;

            let root_node: Node =
                serde_json::from_slice(&captured.stdout).expect("failed to parse AST");

            let mut layouts = std::collections::BTreeMap::new();

            for node in root_node.inner {
                match node.kind {
                    Item::RecordDecl {
                        name,
                        complete_definition,
                    } if complete_definition && name.is_some() => {
                        let Some(layout) = node.inner.into_iter().find_map(|inn| {
                            match inn.kind {
                                Item::AlignedAttr => inn.inner.into_iter().find_map(|cinn| {
                                    let Item::ConstantExpr { value } = cinn.kind else { return None; };
                                    match value {
                                        GoddammitClang::Number(value) => Some(Layout::Align(value)),
                                        GoddammitClang::Dumb(value) => Some(Layout::Align(value.parse().expect("failed to parse alignment"))),
                                    }
                                }),
                                Item::MaxFieldAlignmentAttr { alignment } => {
                                    Some(Layout::Packed(alignment))
                                }
                                _ => None,
                            }
                        }) else { continue; };

                        layouts.insert(name.unwrap(), layout);
                    }
                    Item::TypedefDecl { name, kind } => {
                        #[inline]
                        fn find_public_type(nodes: Vec<Node>) -> Option<String> {
                            if let Some(n) = nodes.iter().find_map(|n| {
                                if let Item::ElaboratedType { owned_tag_decl: Some(otd) } = &n.kind {
                                    Some(otd.name.clone())
                                } else {
                                    None
                                }
                            }) {
                                return Some(n);
                            }

                            for node in nodes.into_iter().filter(|n| !matches!(n.kind, Item::PointerType)) {
                                if let Some(n) = find_public_type(node.inner) {
                                    return Some(n);
                                }
                            }

                            None
                        }

                        let def = if let Some(qt) = kind.clone().and_then(|k| k.qt.filter(|qt| !qt.contains(' '))) {
                            qt
                        } else if let Some(def) = find_public_type(node.inner) {
                            def
                        } else {
                            continue;
                        };

                        let Some(layout) = layouts.remove(&def) else { continue; };
                        layouts.insert(name, layout);
                    }
                    _ => {}
                }
            }

            (arch, layouts)
        })
        .collect_into_vec(&mut collected);

    let mut layouts = std::collections::BTreeMap::<String, Layouts>::new();

    const X86: u8 = 1 << 0;
    const X86_64: u8 = 1 << 1;
    const AARCH64: u8 = 1 << 2;

    // A vast majority of the packing/alignment are not platform specific
    fn all_arches(a: &u8) -> bool {
        *a == 7
    }

    #[derive(Serialize)]
    struct ArchLayout {
        #[serde(skip_serializing_if = "all_arches")]
        a: u8,
        l: Layout,
    }

    #[derive(Serialize)]
    struct Layouts(Vec<ArchLayout>);

    impl Layouts {
        fn new(a: u8, l: Layout) -> Self {
            Self(vec![ArchLayout { a, l }])
        }

        fn append(&mut self, a: u8, l: Layout) {
            if let Some(matched) = self.0.iter_mut().find(|al| al.l == l) {
                matched.a |= a;
            } else {
                self.0.push(ArchLayout { a, l });
            }
        }
    }

    for (arch, lm) in collected {
        let arch = match arch {
            "i686" => X86,
            "x86_64" => X86_64,
            "aarch64" => AARCH64,
            _ => unreachable!("nope, bad arch"),
        };

        for (k, v) in lm {
            layouts
                .entry(k)
                .and_modify(|e| e.append(arch, v))
                .or_insert_with(|| Layouts::new(arch, v));
        }
    }

    let serialized = serde_json::to_vec(&layouts).expect("failed to serialize layouts");
    let compressed =
        zstd::encode_all(std::io::Cursor::new(serialized), 5).expect("failed to compress layouts");
    std::fs::write("md/layouts.json.zstd", compressed).expect("failed to write layouts.json.zstd");
}

fn main() {
    let md_dir = Path::new("md");

    if !md_dir.exists() {
        std::fs::create_dir(&md_dir).expect("failed to create 'md'");
    }

    if std::env::args().count() > 1 {
        update_layout(&md_dir);
    } else {
        update_winmd(&md_dir);
    }
}

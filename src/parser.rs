use camino::Utf8PathBuf as PathBuf;
use cargo_metadata::Package;
use std::ops::Range;

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum BindItemKind {
    Constant,
    Struct,
    Union,
    Function,
}

struct BindItemInner {
    kind: BindItemKind,
    name_range: Range<usize>,
    item_range: Range<usize>,
}

pub struct BindItem<'pi> {
    pub kind: BindItemKind,
    pub name: &'pi str,
    pub full_item: &'pi str,
}

pub struct BindingFile {
    pub path: PathBuf,

    bind_items: Vec<BindItemInner>,
}

impl BindingFile {
    pub fn iter_binds(&self) -> impl Iterator<Item = BindItem<'_>> {
        self.bind_items.iter().map(|bii| BindItem {
            kind: bii.kind,
            name: &self.bind_block[bii.name_range.clone()],
            full_item: &self.bind_block[bii.item_range.clone()],
        })
    }
}

pub struct Parser {
    files: Vec<PathBuf>,
}

impl Parser {
    #[inline]
    pub fn add_file(&mut self, p: impl Into<PathBuf>) {
        self.files.push(p.into());
    }

    #[inline]
    pub fn add_crate(&mut self, krate: &Package) {
        //krate.manifest_path.parent().unwrap()
    }

    pub fn parse(&self) -> Vec<BindingFile> {
        let mut pis = Vec::new();
        for file in &self.files {
            let buf = match std::fs::read_to_string(file) {
                Ok(buf) => buf,
                Err(e) => {
                    log::error!("failed to read {file}: {e}");
                    continue;
                }
            };

            let parsed = match syn::parse_file(&buf) {
                Ok(p) => p,
                Err(e) => {
                    log::error!("failed to parse {file}: {e}");
                    continue;
                }
            };

            // Find any modules in the file that have the #[minwin] attribute
            let mw_mods: Vec<_> = parsed
                .items
                .iter()
                .filter_map(|item| {
                    if let syn::Item::Mod(modi) = item {
                        if modi.attrs.iter().any(|attr| attr.path == "minwin") {
                            return Some(modi);
                        }
                    }

                    None
                })
                .collect();

            if mw_mods.is_empty() {
                log::debug!("skipping {file}, no minwin modules detected");
                continue;
            }

            if mw_mods.len() > 1 {
                let len = mw_mods.len();

                log::error!("{file} contains {len} minwin modules, [{}]");
                continue;
            }

            let mw_mod = mw_mods.pop().unwrap();
        }

        pis
    }
}

use camino::Utf8PathBuf as PathBuf;
use cargo_metadata::Package;
use syn::{Ident, Item, ItemMod};
use ustr::Ustr;

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum BindItemKind {
    Enum,
    Constant,
    Struct,
    Union,
    Function,
    FunctionPtr,
    Typedef,
}

#[derive(Debug)]
pub struct BindItem<'pi> {
    pub module: &'pi ItemMod,
    pub kind: BindItemKind,
    pub inner: &'pi Item,
    pub ident: &'pi Ident,
    pub namespace: Option<Ustr>,
}

impl<'pi> BindItem<'pi> {
    #[inline]
    pub fn iter_enum(&self) -> Option<impl Iterator<Item = &'pi Ident>> {
        if let Item::Enum(enm) = self.inner {
            Some(enm.variants.iter().map(|v| &v.ident))
        } else {
            None
        }
    }
}

pub struct BindingFile {
    pub path: PathBuf,
    file: syn::File,
    mods: Vec<(usize, Option<proc_macro2::TokenStream>)>,
}

impl BindingFile {
    pub fn iter_bind_modules(&self) -> impl Iterator<Item = &ItemMod> {
        self.mods.iter().filter_map(|(i, _)| {
            if let syn::Item::Mod(modi) = &self.file.items[*i] {
                Some(modi)
            } else {
                None
            }
        })
    }

    pub fn replace_module(
        &mut self,
        index: usize,
        ts: proc_macro2::TokenStream,
    ) -> anyhow::Result<()> {
        anyhow::ensure!(index < self.mods.len(), "index is out of range");
        self.mods[index].1 = Some(ts);
        Ok(())
    }

    pub fn iter_module(modi: &ItemMod) -> Option<impl Iterator<Item = BindItem<'_>>> {
        Some(modi.content.as_ref()?.1.iter().filter_map(|item| {
            let ns = |attrs: &[syn::Attribute]| -> Option<Ustr> {
                attrs.iter().find_map(|attr| {
                    let syn::Meta::NameValue(nv) = &attr.meta else { return None; };
                    if !nv.path.is_ident("ns") {
                        None
                    } else {
                        if let syn::Expr::Lit(syn::ExprLit {
                            lit: syn::Lit::Str(lit),
                            ..
                        }) = &nv.value
                        {
                            Some(Ustr::from(&lit.value()))
                        } else {
                            None
                        }
                    }
                })
            };

            let (kind, ident, namespace) = match item {
                Item::Enum(enm) => (BindItemKind::Enum, &enm.ident, ns(&enm.attrs)),
                Item::Const(cnst) => (BindItemKind::Constant, &cnst.ident, ns(&cnst.attrs)),
                Item::Fn(func) => (BindItemKind::Function, &func.sig.ident, ns(&func.attrs)),
                Item::Struct(stru) => (BindItemKind::Struct, &stru.ident, ns(&stru.attrs)),
                Item::Union(un) => (BindItemKind::Union, &un.ident, ns(&un.attrs)),
                Item::Type(ty) => {
                    if matches!(*ty.ty, syn::Type::BareFn(_)) {
                        (BindItemKind::FunctionPtr, &ty.ident, ns(&ty.attrs))
                    } else {
                        (BindItemKind::Typedef, &ty.ident, ns(&ty.attrs))
                    }
                }
                _ => return None,
            };

            Some(BindItem {
                module: modi,
                kind,
                inner: item,
                ident,
                namespace,
            })
        }))
    }

    pub fn iter_binds(&self) -> impl Iterator<Item = BindItem<'_>> {
        self.mods
            .iter()
            .filter_map(|(i, _)| {
                if let syn::Item::Mod(modi) = &self.file.items[*i] {
                    Self::iter_module(modi)
                } else {
                    None
                }
            })
            .flatten()
    }

    pub fn gather_hints(&self, hints: &mut crate::resolver::Hints) {
        for (ns, hint) in self.iter_binds().filter_map(|bi| {
            let ns = bi.namespace?;

            if bi.kind != BindItemKind::Enum {
                None
            } else {
                let name = bi.ident.to_string();

                if name.starts_with('_') {
                    Some((ns, crate::resolver::Hint::SuffixedEnum(name.into())))
                } else if name.ends_with('_') {
                    Some((ns, crate::resolver::Hint::PrefixedEnum(name.into())))
                } else {
                    None
                }
            }
        }) {
            hints.add_hint(ns, hint);
        }
    }

    pub fn generate(&self, format: bool) -> anyhow::Result<String> {
        let mut file = std::fs::read_to_string(&self.path)
            .with_context(|| format!("failed to read {}", self.path))?;

        let mut end = file.len();

        // Walk the modules in reverse order so we never have to care about bookkeeping
        for (ind, ts) in self.mods.iter().rev() {
            let Some(ts) = ts else { continue; };
            let Some(syn::Item::Mod(modi)) = self.file.items.get(*ind) else { continue; };

            let modname = modi.ident.to_string();

            // It'd be nice to get the span of the module braces...but that only
            // works on nightly...and only in regular proc macros...
            let range = {
                let s = &file[..end];
                let attr = s
                    .rfind("#[minwin]")
                    .with_context(|| format!("unable to find attribute for module '{modname}'"))?;
                end = attr;

                let inner = &s[attr..];
                let m = inner
                    .find("mod")
                    .with_context(|| format!("unable to find mod for module '{modname}'"))?;
                inner[m..]
                    .find(&modname)
                    .with_context(|| format!("unable to find mod named '{modname}'"))?;

                let obrace = inner.find('{').with_context(|| {
                    format!("unable to find opening brace for module '{modname}'")
                })?;
                let mut scope = 1;

                let ebrace = inner[obrace + 1..]
                    .char_indices()
                    .find_map(|(i, c)| {
                        if c == '{' {
                            scope += 1;
                        } else if c == '}' {
                            scope -= 1;

                            if scope == 0 {
                                return Some(i);
                            }
                        }

                        None
                    })
                    .with_context(|| {
                        format!("unable to find closing brace for module '{}'", modi.ident)
                    })?;

                attr + obrace + 1..attr + obrace + ebrace
            };

            // It'd be nice to just insert our generated token stream and replace
            // the contents of the original module...but alas
            // https://github.com/rust-lang/rust/pull/109002
            file.replace_range(range, &ts.to_string());
        }

        if !format {
            return Ok(file);
        }

        use anyhow::Context as _;
        let mut child = std::process::Command::new("rustfmt")
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .context("rustfmt is not installed")?;

        {
            use std::io::Write;
            let mut stdin = child.stdin.take().context("failed to open stdin")?;
            stdin
                .write_all(file.as_bytes())
                .context("failed to write to stdin")?;
        }

        let output = child
            .wait_with_output()
            .context("rustfmt seems to have crashed")?;

        if output.status.success() {
            Ok(String::from_utf8(output.stdout).context("output was not utf-8")?)
        } else {
            anyhow::bail!(
                "rustfmt failed ({}):\n{}",
                output.status,
                String::from_utf8_lossy(&output.stderr)
            )
        }
    }

    pub fn replace(&self, format: bool) -> anyhow::Result<()> {
        use anyhow::Context as _;
        let text = self.generate(format)?;
        std::fs::write(&self.path, text).with_context(|| format!("failed to replace {}", self.path))
    }
}

#[derive(Default)]
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
        if let Some(kp) = krate.manifest_path.parent() {
            let src = kp.join("src");
            let wd = if src.exists() {
                walkdir::WalkDir::new(&src)
            } else {
                // If a src directory doesn't exist, it's _most likely_ the user
                // just has a few rust source files in the root, but we don't
                // want to recurse
                walkdir::WalkDir::new(kp).max_depth(1)
            };

            for file in wd.follow_links(false).into_iter() {
                if let Ok(file) = file {
                    if file.file_type().is_file() {
                        if let Ok(pb) = PathBuf::from_path_buf(file.into_path()) {
                            if pb.extension() == Some("rs") {
                                self.add_file(pb);
                            }
                        }
                    }
                }
            }
        } else {
            unreachable!("crate {} didn't have a parent?", krate.name);
        }
    }

    #[inline]
    pub fn add_workspace(&mut self, cm: &cargo_metadata::Metadata) {
        for wm in cm.workspace_packages() {
            self.add_crate(wm);
        }
    }

    pub fn parse(&self) -> Vec<BindingFile> {
        let mut pis = Vec::new();

        // Note we aren't using rayon here because syn is internally using Rc<>
        for file in &self.files {
            let s = tracing::debug_span!("parse_file", file = file.as_str());
            let _s = s.enter();

            let buf = match std::fs::read_to_string(file) {
                Ok(buf) => buf,
                Err(e) => {
                    tracing::error!(error = %e, "failed to read file");
                    continue;
                }
            };

            let parsed = match syn::parse_file(&buf) {
                Ok(p) => p,
                Err(e) => {
                    tracing::error!(error = %e, "failed to parse file");
                    continue;
                }
            };

            // Find any modules in the file that have the #[minwin] attribute
            let mw_mods: Vec<_> = parsed
                .items
                .iter()
                .enumerate()
                .filter_map(|(i, item)| {
                    if let syn::Item::Mod(modi) = item {
                        if modi.attrs.iter().any(|attr| attr.path().is_ident("minwin")) {
                            return Some(i);
                        }
                    }

                    None
                })
                .collect();

            if mw_mods.is_empty() {
                tracing::debug!("skipping file, no `minwin` modules detected");
                continue;
            }

            tracing::info!("detected {} minwin modules", mw_mods.len());

            pis.push(BindingFile {
                path: file.clone(),
                file: parsed,
                mods: mw_mods.into_iter().map(|i| (i, None)).collect(),
            });
        }

        pis
    }
}

use camino::Utf8PathBuf as PathBuf;
use cargo_metadata::Package;
use syn::{Ident, Item, ItemMod};

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
    pub ident: &'pi Ident,
}

pub struct BindingFile {
    pub path: PathBuf,
    file: syn::File,
    mods: Vec<usize>,
}

impl BindingFile {
    pub fn iter_bind_modules(&self) -> impl Iterator<Item = &ItemMod> {
        self.mods.iter().filter_map(|i| {
            if let syn::Item::Mod(modi) = &self.file.items[*i] {
                Some(modi)
            } else {
                None
            }
        })
    }

    pub fn iter_module(modi: &ItemMod) -> Option<impl Iterator<Item = BindItem<'_>>> {
        Some(modi.content.as_ref()?.1.iter().filter_map(|item| {
            let (kind, ident) = match item {
                Item::Enum(enm) => (BindItemKind::Enum, &enm.ident),
                Item::Const(cnst) => (BindItemKind::Constant, &cnst.ident),
                Item::Fn(func) => (BindItemKind::Function, &func.sig.ident),
                Item::Struct(stru) => (BindItemKind::Struct, &stru.ident),
                Item::Union(un) => (BindItemKind::Union, &un.ident),
                Item::Type(ty) => {
                    if matches!(*ty.ty, syn::Type::BareFn(_)) {
                        (BindItemKind::FunctionPtr, &ty.ident)
                    } else {
                        (BindItemKind::Typedef, &ty.ident)
                    }
                }
                _ => return None,
            };

            Some(BindItem {
                module: modi,
                kind,
                ident,
            })
        }))
    }

    pub fn iter_binds(&self) -> impl Iterator<Item = BindItem<'_>> {
        self.mods
            .iter()
            .filter_map(|i| {
                if let syn::Item::Mod(modi) = &self.file.items[*i] {
                    Self::iter_module(modi)
                } else {
                    None
                }
            })
            .flatten()
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
    pub fn add_crate(&mut self, _krate: &Package) {
        //krate.manifest_path.parent().unwrap()
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
                        if modi.attrs.iter().any(|attr| attr.path.is_ident("minwin")) {
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

            pis.push(BindingFile {
                path: file.clone(),
                file: parsed,
                mods: mw_mods,
            });
        }

        pis
    }
}

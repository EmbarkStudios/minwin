use camino::Utf8PathBuf as PathBuf;
use cargo_metadata::Package;
use quote::ToTokens;
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
    mods: Vec<(usize, Option<proc_macro2::TokenStream>)>,
}

impl BindingFile {
    pub fn iter_bind_modules(&self) -> impl Iterator<Item = (usize, &ItemMod)> {
        self.mods.iter().filter_map(|(i, _)| {
            if let syn::Item::Mod(modi) = &self.file.items[*i] {
                Some((*i, modi))
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
            .filter_map(|(i, _)| {
                if let syn::Item::Mod(modi) = &self.file.items[*i] {
                    Self::iter_module(modi)
                } else {
                    None
                }
            })
            .flatten()
    }

    pub fn generate(&self, format: bool) -> anyhow::Result<String> {
        let mut file = std::fs::read_to_string(&self.path)
            .with_context(|| format!("failed to read {}", self.path))?;

        let get_span_range = |s: proc_macro2::Span| -> std::ops::Range<usize> {
            let s = s.unwrap();
            let start = dbg!(s.start());
            let end = dbg!(s.end());
            0..0

            // let s = file.as_bytes();

            // let mut line = 0;
            // let begin = loop {
            //     if line < start.line
            // };

            // begin..end
        };

        // Walk the modules in reverse order so we never have to care about bookkeeping
        for (ind, ts) in self.mods.iter().rev() {
            let Some(ts) = ts else { continue; };
            let Some(syn::Item::Mod(modi)) = self.file.items.get(*ind) else { continue; };
            let Some(span) = modi.content.as_ref().map(|c| c.0.span) else { continue; };

            // It'd be nice to just insert our generated token stream and replace
            // the contents of the original module...but alas
            // https://github.com/rust-lang/rust/pull/109002
            let range = get_span_range(span);
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
                mods: mw_mods.into_iter().map(|i| (i, None)).collect(),
            });
        }

        pis
    }
}

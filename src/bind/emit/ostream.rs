use proc_macro2::{self as pm, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::collections::BTreeMap;
use ustr::Ustr;
use windows_metadata::reader::{Field, MethodDef, Reader, Type, TypeDef};

bitflags::bitflags! {
    #[derive(Debug, Copy, Clone)]
    pub struct Attrs: u8 {
        const X86 = 1 << 0;
        const X86_64 = 1 << 1;
        const AARCH64 = 1 << 2;

        const COPY_CLONE = 1 << 3;
        const UNION = 1 << 4;
        const DEPRECATED = 1 << 5;

        const ARCH = Attrs::X86.bits() | Attrs::X86_64.bits() | Attrs::AARCH64.bits();
    }
}

impl ToTokens for Attrs {
    fn to_tokens(&self, ts: &mut TokenStream) {
        let arch = self.intersection(Attrs::ARCH);

        let count = arch.iter().count();
        let arches = arch.iter().map(|a| {
            if a.contains(Attrs::X86) {
                "x86"
            } else if a.contains(Attrs::X86_64) {
                "x86_64"
            } else if a.contains(Attrs::AARCH64) {
                "aarch64"
            } else {
                unreachable!()
            }
        });

        let cfg_parts = if count == 1 {
            quote! { #(target_arch = #arches),* }
        } else {
            quote! { any(#(target_arch = #arches),*) }
        };

        ts.extend(cfg_parts);
    }
}

pub struct OutputStream {
    root: TokenStream,
    libs: Vec<(Ustr, bool, TokenStream)>,
    arches: Vec<(Attrs, Ustr, TokenStream)>,
    enums: BTreeMap<TypeDef, TokenStream>,
    constants: BTreeMap<Field, (pm::Ident, TokenStream)>,
    types: BTreeMap<Type, (pm::Ident, Option<TokenStream>)>,
    functions: BTreeMap<Ustr, Vec<(MethodDef, TokenStream)>>,
}

impl OutputStream {
    pub fn new() -> Self {
        Self {
            root: TokenStream::new(),
            libs: Vec::new(),
            arches: Vec::new(),
            enums: BTreeMap::new(),
            constants: BTreeMap::new(),
            types: BTreeMap::new(),
            functions: BTreeMap::new(),
        }
    }

    pub fn get_arch_block(&mut self, attrs: Attrs) -> &mut TokenStream {
        let arches = attrs & Attrs::ARCH;

        if arches.is_empty() {
            &mut self.root
        } else if let Some(ab) = self
            .arches
            .iter()
            .position(|(attrs, _, _ts)| arches.bits() == attrs.bits())
        {
            &mut self.arches[ab].2
        } else {
            let mut mod_name = String::new();

            for (i, arch) in arches
                .iter()
                .map(|a| {
                    if a.contains(Attrs::X86) {
                        "x86"
                    } else if a.contains(Attrs::X86_64) {
                        "x86_64"
                    } else if a.contains(Attrs::AARCH64) {
                        "aarch64"
                    } else {
                        unreachable!()
                    }
                })
                .enumerate()
            {
                if i > 0 {
                    mod_name.push('_');
                }

                mod_name.push_str(arch);
            }

            self.arches
                .push((arches, mod_name.into(), TokenStream::new()));
            &mut self.arches.last_mut().unwrap().2
        }
    }

    // pub fn get_extern_block(&mut self, name: Ustr, is_system: bool) -> &mut TokenStream {
    //     let eb = if let Some(eb) = self
    //         .libs
    //         .iter()
    //         .position(|(lname, is, _ts)| name == lname && is_system == *is)
    //     {
    //         eb
    //     } else {
    //         self.libs.push((*name, is_system, TokenStream::new()));
    //         self.libs.len() - 1
    //     };

    //     &mut self.libs[eb].2
    // }

    /// When outputting actual enums, we wrap them in a module, so that the
    /// constants are accessible similarly to a regular Rust enum, but still
    /// keep the flexibility of not needing to worry about things like multiple
    /// constants with the same value etc
    ///
    /// ```rust
    /// pub mod enum_name {
    ///     pub type Enum = u32;
    ///     pub const VARIANT: Enum = 1;
    /// }
    /// ```
    #[inline]
    pub fn get_enum_block(&mut self, def: TypeDef, emit: &super::Emit<'_>) -> &mut TokenStream {
        self.enums.entry(def).or_insert_with(|| {
            let kind = emit.type_printer(emit.reader.type_def_underlying_type(def));

            quote! {
                pub type Enum = #kind;
            }
        })
    }

    #[inline]
    pub fn insert_type(&mut self, ty: Type, ident: pm::Ident, ts: TokenStream) {
        self.types.insert(ty, (ident, Some(ts)));
    }

    #[inline]
    pub fn insert_constant(&mut self, field: Field, ident: pm::Ident, ts: TokenStream) {
        self.constants.insert(field, (ident, ts));
    }

    #[inline]
    pub fn insert_function(&mut self, library: Ustr, func: MethodDef, ts: TokenStream) {
        self.functions.entry(library).or_default().push((func, ts));
    }

    pub fn finalize(self, use_windows_targets: bool) -> TokenStream {
        let mut root = self.root;

        for (attrs, mn, ts) in self.arches {
            let mn = format_ident!("{mn}");
            root.extend(quote! {
                #[cfg(#attrs)]
                mod #mn {
                    use super::*;

                    #ts
                }

                #[cfg(#attrs)]
                use #mn::*;
            });
        }

        for (lib, is_system, ts) in self.libs {
            let lib = lib.as_str();
            let cc = if is_system { "system" } else { "C" };

            if let Some(lib) = lib.strip_suffix(".dll") {
                root.extend(quote! {
                    #[link(name = #lib, kind = "raw-dylib")]
                    extern #cc {
                        #ts
                    }
                });
            } else {
                root.extend(quote! {
                    #[link(name = #lib, kind = "raw-dylib", modifiers = "+verbatim")]
                    extern #cc {
                        #ts
                    }
                });
            }
        }

        root
    }
}

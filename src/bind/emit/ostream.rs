use crate::bind::{
    emit::shared::{to_ident, Attrs, IdentKind, Value},
    EnumStyle,
};
use proc_macro2::{self as pm, Ident, TokenStream};
use quote::{format_ident, quote};
use std::{
    cmp,
    collections::{BTreeMap, BTreeSet},
};
use ustr::Ustr;
use windows_metadata::reader::{MethodDef, Reader, Type, TypeDef};

use super::shared::TypePrinter;

struct EnumConstant {
    name: Ident,
    value: Value,
}

impl cmp::Eq for EnumConstant {}

impl cmp::PartialEq for EnumConstant {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == cmp::Ordering::Equal
    }
}

impl cmp::Ord for EnumConstant {
    fn cmp(&self, other: &Self) -> cmp::Ordering {
        match self.value.cmp(&other.value) {
            cmp::Ordering::Equal => self.name.cmp(&other.name),
            ord => ord,
        }
    }
}

impl cmp::PartialOrd for EnumConstant {
    fn partial_cmp(&self, other: &Self) -> Option<cmp::Ordering> {
        Some(self.cmp(other))
    }
}

struct EnumBlock {
    /// The core type of the enum eg i32/u32
    ty: Type,
    /// The unique constants, note that they can be unique by name, but not value
    constants: BTreeSet<EnumConstant>,
}

impl EnumBlock {
    #[inline]
    fn insert(&mut self, ec: EnumConstant) {
        self.constants.insert(ec);
    }
}

pub struct OutputStream<'r> {
    reader: &'r Reader<'r>,
    pub(crate) root: TokenStream,
    /// Blocks of items partitioned by `target_arch`.
    ///
    /// The top level BTreeMap is overkill but the entry() API is too good
    arch_blocks: BTreeMap<u8, BTreeMap<pm::Ident, TokenStream>>,
    enums: BTreeMap<TypeDef, EnumBlock>,
    constants: BTreeMap<Ident, TokenStream>,
    types: BTreeMap<Ident, (Type, Option<TokenStream>)>,
    functions: BTreeMap<(Ustr, bool), BTreeMap<(Ident, Attrs), (MethodDef, TokenStream)>>,
}

impl<'r> OutputStream<'r> {
    pub fn new(reader: &'r Reader<'r>) -> Self {
        Self {
            reader,
            root: TokenStream::new(),
            arch_blocks: BTreeMap::new(),
            enums: BTreeMap::new(),
            constants: BTreeMap::new(),
            types: BTreeMap::new(),
            functions: BTreeMap::new(),
        }
    }

    /// Inserts a record (struct or union)
    ///
    /// Note that unlike other items, records can be arch specific, so we group
    /// them via target arch so we can avoid emitting `cfg` attributes for each
    /// unique record that needs them (and potentially on Copy/Clone impls)
    #[inline]
    pub fn insert_record(&mut self, rec: TypeDef, ident: Ident, attrs: Attrs, ts: TokenStream) {
        let arches = attrs.bits();

        if arches == 0 {
            // If the record isn't arch specific, just insert it as a regular type
            // so that they are lexicographically ordered with each other
            self.insert_type(Type::TypeDef((rec, Vec::new())), ident, ts);
        } else {
            let block = self.arch_blocks.entry(arches).or_default();
            block.insert(ident, ts);
        }
    }

    #[inline]
    pub fn insert_type(&mut self, ty: Type, ident: Ident, ts: TokenStream) {
        self.types.insert(ident, (ty, Some(ts)));
    }

    #[inline]
    pub fn insert_enum_constant(&mut self, def: TypeDef, name: Ident, value: Value) {
        let enum_block = self.enums.entry(def).or_insert_with(|| EnumBlock {
            ty: self.reader.type_def_underlying_type(def),
            constants: BTreeSet::new(),
        });

        enum_block.insert(EnumConstant { name, value });
    }

    #[inline]
    pub fn insert_constant(&mut self, ident: Ident, ts: TokenStream) {
        self.constants.insert(ident, ts);
    }

    #[inline]
    pub fn insert_function(
        &mut self,
        library: Ustr,
        is_system: bool,
        func: MethodDef,
        ident: Ident,
        attrs: Attrs,
        ts: TokenStream,
    ) {
        self.functions
            .entry((library, is_system))
            .or_default()
            .insert((ident, attrs), (func, ts));
    }

    pub fn finalize(
        self,
        linking_style: crate::bind::LinkingStyle,
        enum_style: EnumStyle,
        use_rust_casing: bool,
    ) -> TokenStream {
        let mut root = self.root;

        for ((lib, is_system), functions) in self.functions {
            if linking_style == crate::bind::LinkingStyle::WindowsTargets {
                for (_, func_ts) in functions.into_values() {
                    root.extend(func_ts);
                }
            } else {
                let lib = lib.as_str();
                let cc = if is_system { "system" } else { "C" };

                let funcs = functions.into_values().map(|(_, f)| f);

                let link = if linking_style == crate::bind::LinkingStyle::RawDylib {
                    if let Some(lib) = lib.strip_suffix(".dll") {
                        quote! { #[link(name = #lib, kind = "raw-dylib")] }
                    } else {
                        quote! { #[link(name = #lib, kind = "raw-dylib", modifiers = "+verbatim")] }
                    }
                } else {
                    let lib = lib.rfind('.').map(|ext| &lib[..ext]).unwrap_or(lib);
                    quote! { #[link(name = #lib)] }
                };

                root.extend(quote! {
                    #link
                    extern #cc {
                        #(#funcs)*
                    }
                });
            }
        }

        let reader = self.reader;

        let enum_blocks: BTreeMap<_, _> = self
            .enums
            .into_iter()
            .map(|(td, eb)| {
                let name = reader.type_def_name(td);

                let ident_kind = match enum_style {
                    EnumStyle::Bindgen => IdentKind::Type,
                    EnumStyle::Minwin => IdentKind::Enum,
                };

                let ident = to_ident(name, ident_kind, use_rust_casing, false);

                let typ = TypePrinter {
                    r: self.reader,
                    ty: eb.ty,
                    use_rust_casing,
                    use_windows_core: false, // irrelevant for enums
                };

                let mut ts = TokenStream::new();

                match enum_style {
                    EnumStyle::Bindgen => {
                        ts.extend(quote! {
                            pub type #ident = #typ;
                        });

                        for ec in eb.constants {
                            let name = ec.name;
                            let val = ec.value;
                            ts.extend(quote! {
                                pub const #name: #ident = #val;
                            });
                        }
                    }
                    EnumStyle::Minwin => {
                        let constants = eb.constants.into_iter().map(|ec| {
                            let name = ec.name;
                            let val = ec.value;
                            quote! {
                                pub const #name: Enum = #val;
                            }
                        });

                        ts.extend(quote! {
                            pub mod #ident {
                                pub type Enum = #typ;
                                #(#constants)*
                            }
                        });
                    }
                }

                (ident, ts)
            })
            .collect();

        for enum_block in enum_blocks.into_values() {
            root.extend(enum_block);
        }

        for cts in self.constants.into_values() {
            root.extend(cts);
        }

        for (_ident, (_ty, ts)) in self.types {
            root.extend(ts);
        }

        // Write out arch specific items last so that we get lexicographic ordering
        // for a vast majority of items without weird arch specific types sprinkled in
        for (attrs, recs) in self.arch_blocks {
            let arches = Attrs::from_bits(attrs).expect("this should be impossible");
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

            let mn = format_ident!("{mod_name}");

            let recs = recs.into_values();

            // Add the arch specific module
            //
            // Note we glob import the parent module so that all types that aren't
            // arch specific can be located, and then reexport all the types
            // in the module to make them available to the crate
            root.extend(quote! {
                #arches
                mod #mn {
                    use super::*;

                    #(#recs)*
                }

                #arches
                use #mn::*;
            });
        }

        root
    }
}

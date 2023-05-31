use crate::bind::{
    emit::shared::{Attrs, IdentKind, Value},
    EnumStyle,
};
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote, ToTokens};
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

pub struct EnumBlock {
    /// The core type of the enum eg i32/u32
    ty: Type,
    /// For builtin types
    alias: Option<TokenStream>,
    /// The unique constants, note that they can be unique by name, but not value
    constants: BTreeSet<EnumConstant>,
}

impl EnumBlock {
    #[inline]
    fn insert(&mut self, ec: EnumConstant) {
        self.constants.insert(ec);
    }
}

struct TypedefBlock {
    ident: Ident,
    ts: Option<TokenStream>,
}

struct InterfaceBlock {
    ident: Ident,
    vtable: Option<TokenStream>,
    imp: Option<TokenStream>,
}

enum TypeStream {
    EnumBlock(EnumBlock),
    Typedef(TypedefBlock),
}

pub struct OutputStream<'r> {
    reader: &'r Reader<'r>,
    pub(crate) root: TokenStream,
    /// Blocks of items partitioned by `target_arch`.
    ///
    /// The top level BTreeMap is overkill but the entry() API is too good
    arch_blocks: BTreeMap<u8, BTreeMap<Ident, TokenStream>>,
    types: BTreeMap<Type, TypeStream>,
    constants: BTreeMap<Ident, TokenStream>,
    functions: BTreeMap<(Ustr, bool), BTreeMap<(Ident, Attrs), (MethodDef, TokenStream)>>,
    interfaces: BTreeMap<TypeDef, InterfaceBlock>,
}

impl<'r> OutputStream<'r> {
    pub fn new(reader: &'r Reader<'r>) -> Self {
        Self {
            reader,
            root: TokenStream::new(),
            arch_blocks: BTreeMap::new(),
            constants: BTreeMap::new(),
            types: BTreeMap::new(),
            functions: BTreeMap::new(),
            interfaces: BTreeMap::new(),
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
        self.types.insert(
            ty,
            TypeStream::Typedef(TypedefBlock {
                ident,
                ts: Some(ts),
            }),
        );
    }

    #[inline]
    pub fn insert_enum(&mut self, ty: Type) -> &mut EnumBlock {
        let make_block = |ty: Type| {
            let ty = if let Type::TypeDef((def, _)) = &ty {
                self.reader.type_def_underlying_type(*def)
            } else {
                ty
            };
            EnumBlock {
                ty,
                alias: None,
                constants: BTreeSet::new(),
            }
        };

        let TypeStream::EnumBlock(eb) = self.types.entry(ty.clone()).and_modify(|ts| {
            let alias = match ts {
                TypeStream::EnumBlock(_) => return,
                TypeStream::Typedef(tdb) => {
                    tdb.ts.clone()
                }
            };
            
            let mut enum_block = make_block(ty.clone());
            enum_block.alias = alias;

            *ts = TypeStream::EnumBlock(enum_block);
            }).or_insert_with(|| {
            TypeStream::EnumBlock(make_block(ty))
        }) else {
            panic!("type is not an enum...");
        };
        eb
    }

    #[inline]
    pub fn insert_enum_constant(&mut self, ty: Type, name: Ident, value: Value) {
        let enum_block = self.insert_enum(ty);
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

    #[inline]
    pub fn insert_vtable(&mut self, td: TypeDef, ident: Ident, vtable: TokenStream) {
        self.interfaces.entry(td).or_insert_with(|| {
            InterfaceBlock { ident, vtable: None, imp: None }
        }).vtable = Some(vtable)
    }

    #[inline]
    pub fn insert_interface(&mut self, td: TypeDef, ident: Ident, imp: TokenStream) {
        self.interfaces.entry(td).or_insert_with(|| {
            InterfaceBlock { ident, vtable: None, imp: None }
        }).imp = Some(imp)
    }

    /// Checks whether the specified vtable has been inserted already
    #[inline]
    pub fn has_vtable(&mut self, td: TypeDef) -> bool {
        self.interfaces.get(&td).and_then(|iface| iface.vtable.as_ref()).is_some()
    }

    pub fn finalize(mut self, config: crate::bind::MinwinBindConfig) -> TokenStream {
        let mut root = self.root;

        for ((lib, is_system), functions) in self.functions {
            if config.linking_style == crate::bind::LinkingStyle::WindowsTargets {
                for (_, func_ts) in functions.into_values() {
                    root.extend(func_ts);
                }
            } else {
                let lib = lib.as_str();
                let cc = if is_system { "system" } else { "C" };

                let funcs = functions.into_values().map(|(_, f)| f);

                let link = if config.linking_style == crate::bind::LinkingStyle::RawDylib {
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

        for cts in self.constants.into_values() {
            root.extend(cts);
        }

        self.types.extend(self.interfaces.into_iter().map(|(td, block)| {
            let InterfaceBlock {
                ident,
                vtable,
                imp,
            } = block;
            let ts = quote! {
                #vtable
                #imp
            };
            (Type::TypeDef((td, Vec::new())), TypeStream::Typedef(TypedefBlock {
                ident,
                ts: Some(ts),
            }))
        }));

        let type_blocks: BTreeMap<_, _> = self
            .types
            .into_iter()
            .map(|(ty, ts)| match ts {
                TypeStream::Typedef(tdb) => (tdb.ident.to_string(), tdb.ts.unwrap_or_default()),
                TypeStream::EnumBlock(eb) => {
                    let Type::TypeDef((td, _)) = &ty else {
                            let typ = TypePrinter {
                                r: self.reader,
                                ty,
                                config,
                            };

                            let mut ts = TokenStream::new();
                            ts.extend(eb.alias);

                            for ec in eb.constants {
                                let name = ec.name;
                                let val = ec.value;

                                let cs = if config.use_core {
                                    quote! {
                                        pub const #name: #typ = #typ(#val);
                                    }
                                } else {
                                    quote! {
                                        pub const #name: #typ = #val as _;
                                    }
                                };

                                ts.extend(cs);
                            }

                            return (typ.into_token_stream().to_string(), ts);
                        };

                    let td = *td;
                    let name = reader.type_def_name(td);

                    // In some rare cases there are special predefined enum variants
                    // for handle types, such as `HWND_TOPMOST`, they are always
                    // emitted in the default bindgen style
                    let enum_style = if reader.type_def_is_handle(td) {
                        EnumStyle::Bindgen
                    } else {
                        config.enum_style
                    };

                    let ident_kind = match enum_style {
                        EnumStyle::Bindgen => IdentKind::Type,
                        EnumStyle::Minwin => IdentKind::Enum,
                    };

                    let ident = config.make_ident(name, ident_kind);

                    let typ = TypePrinter {
                        r: self.reader,
                        ty: eb.ty,
                        config,
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

                    (ident.to_string(), ts)
                }
            })
            .collect();

        for type_block in type_blocks.into_values() {
            root.extend(type_block);
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

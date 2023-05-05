use super::shared::Attrs;
use proc_macro2::{self as pm, TokenStream};
use quote::{format_ident, quote};
use std::collections::BTreeMap;
use ustr::Ustr;
use windows_metadata::reader::{Field, MethodDef, Type, TypeDef};

pub struct OutputStream {
    root: TokenStream,
    libs: Vec<(Ustr, bool, TokenStream)>,
    /// Blocks of records partitioned by `target_arch`.
    ///
    /// The top level BTreeMap is overkill but the entry() API is too good
    rec_blocks: BTreeMap<u8, BTreeMap<pm::Ident, TokenStream>>,
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
            rec_blocks: BTreeMap::new(),
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
    pub fn insert_record(&mut self, rec: TypeDef, ident: pm::Ident, attrs: Attrs, ts: TokenStream) {
        let arches = (attrs & Attrs::ARCH).bits();

        if arches == 0 {
            // If the record isn't arch specific, just insert it as a regular type
            // so that they are lexicographically ordered with each other
            self.insert_type(Type::TypeDef((rec, Vec::new())), ident, ts);
        } else {
            let block = self.rec_blocks.entry(arches).or_default();
            block.insert(ident, ts);
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

        // Write out arch specific items last so that we get lexicographic ordering
        // for a vast majority of items without weird arch specific types sprinkled in
        for (attrs, recs) in self.rec_blocks {
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

            // Add the arch specific module
            //
            // Note we glob import the parent module so that all types that aren't
            // arch specific can be located, and then reexport all the types
            // in the module to make them available to the crate
            root.extend(quote! {
                #[cfg(#attrs)]
                mod #mn {
                    use super::*;

                    #(recs*)
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

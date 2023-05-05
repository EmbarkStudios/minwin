mod constant;
mod function;
mod function_pointer;
mod ostream;
mod record;
mod shared;

use ostream::OutputStream;
use shared::*;

use super::{Impls, InterfaceMap};
use anyhow::Context as _;
use proc_macro2::{self as pm, TokenStream};
use quote::quote;
use std::collections::BTreeMap;
use windows_metadata::reader::{self as wmr, Field, Reader, Type, TypeDef, TypeKind};

pub struct Emit<'r> {
    /// The actual items we are emitting
    pub items: super::ItemSet,
    /// Used to query the metadata database
    pub reader: &'r Reader<'r>,
    /// List of interfaces to emit, if an interface/class is not in this list it
    /// is emitted as a void* type alias instead
    pub ifaces: InterfaceMap,
    /// We collect additional layout information not currently present in the
    /// the metadata to supplement it
    pub layouts: Option<&'static BTreeMap<String, ArchLayouts>>,
    /// If true, use the `windows-targets::link!` macro to link functions,
    /// otherwise functions are collated in extern blocks and linked with the
    /// appropriate dll
    pub link_targets: bool,
    /// If true, the `windows-core` crate is
    pub use_core: bool,
    /// If true, identifiers are fixed to remove pointless Hungarian notation
    pub fix_naming: bool,
    /// If true, the casing of all items will be changed to follow Rust casing conventions
    pub use_rust_casing: bool,
    /// If true, formats the output
    pub pretty_print: bool,
}

impl<'r> Emit<'r> {
    pub fn emit(self) -> anyhow::Result<String> {
        let mut os = OutputStream::new();
        let reader = self.reader;

        for ty in self.items.types.keys().cloned() {
            if !self.use_core && !matches!(ty, Type::TypeDef(..)) {
                let (ident, ts) = match ty {
                    Type::HRESULT => {
                        let ident = self.to_ident("HRESULT", IdentKind::Type);
                        let ts = quote! { pub type #ident = i32; };
                        (ident, ts)
                    }
                    Type::String => {
                        let ident = self.to_ident("HSTRING", IdentKind::Type);
                        let ts = quote! { pub type #ident = *mut ::core::ffi::c_void; };
                        (ident, ts)
                    }
                    Type::IUnknown => {
                        let ident = self.to_ident("IUnknown", IdentKind::Type);
                        let ts = quote! { pub type #ident = *mut ::core::ffi::c_void; };
                        (ident, ts)
                    }
                    Type::IInspectable => {
                        let ident = self.to_ident("IInspectable", IdentKind::Type);
                        let ts = quote! { pub type #ident = *mut ::core::ffi::c_void; };
                        (ident, ts)
                    }
                    Type::PSTR => {
                        let ident = self.to_ident("PSTR", IdentKind::Type);
                        let ts = quote! { pub type #ident = *mut u8; };
                        (ident, ts)
                    }
                    Type::PWSTR => {
                        let ident = self.to_ident("PSTR", IdentKind::Type);
                        let ts = quote! { pub type #ident = *mut u16; };
                        (ident, ts)
                    }
                    Type::PCSTR => {
                        let ident = self.to_ident("PCSTR", IdentKind::Type);
                        let ts = quote! { pub type #ident = *const u8; };
                        (ident, ts)
                    }
                    Type::PCWSTR => {
                        let ident = self.to_ident("PCWSTR", IdentKind::Type);
                        let ts = quote! { pub type #ident = *const u16; };
                        (ident, ts)
                    }
                    Type::BSTR => {
                        let ident = self.to_ident("BSTR", IdentKind::Type);
                        let ts = quote! { pub type #ident = *const u16; };
                        (ident, ts)
                    }
                    Type::GUID => {
                        let ident = self.to_ident("GUID", IdentKind::Record);
                        let impls = self.impls(&ident, Impls::COPY);

                        let ts = quote! {
                            #[repr(C)]
                            pub struct #ident {
                                pub data1: u32,
                                pub data2: u16,
                                pub data3: u16,
                                pub data4: [u8; 8],
                            }

                            #impls

                            impl #ident {
                                #[allow(dead_code)]
                                pub const fn from_u128(uuid: u128) -> Self {
                                    Self { data1: (uuid >> 96) as u32, data2: (uuid >> 80 & 0xffff) as u16, data3: (uuid >> 64 & 0xffff) as u16, data4: (uuid as u64).to_be_bytes() }
                                }
                            }
                        };
                        (ident, ts)
                    }
                    _ => {
                        continue;
                    }
                };

                os.insert_type(ty, ident, ts);
            } else if let Type::TypeDef((td, _)) = &ty {
                let def = *td;
                let kind = reader.type_def_kind(def);
                let name = reader.type_def_name(def);

                let (ident, ts) = match kind {
                    TypeKind::Class => {
                        let ident = self.to_ident(name, IdentKind::Record);

                        let ts = if self.use_core {
                            //self.emit_class(&mut os, reader, def)
                            unreachable!()
                        } else {
                            quote! { pub type #ident = *mut ::core::ffi::c_void; }
                        };

                        (ident, ts)
                    }
                    TypeKind::Interface => {
                        let ident = self.to_ident(name, IdentKind::Record);

                        let ts = if self.use_core {
                            //self.emit_interface(&mut os, reader, def)
                            unreachable!()
                        } else {
                            quote! { pub type #ident = *mut ::core::ffi::c_void; }
                        };

                        (ident, ts)
                    }
                    TypeKind::Enum => {
                        unreachable!()
                        // let ident = self.to_ident(name, IdentKind::Enum);
                        // self.emit_enum(&mut os, reader, def);

                        // let ts = if self.use_core {

                        // } else {
                        //     quote!{ pub type #ident = *mut ::core::ffi::c_void; }
                        // };

                        // (ident, ts)
                        // sorted.insert(gen.reader.type_def_name(def), enums::gen(gen, def));
                    }
                    TypeKind::Struct => {
                        self.emit_record(&mut os, def)?;
                        continue;
                    }
                    TypeKind::Delegate => self.emit_function_pointer(def)?,
                };

                os.insert_type(ty, ident, ts);
            }
        }

        for constant in &self.items.constants {
            self.emit_constant(&mut os, *constant);
        }

        for func in &self.items.functions {
            self.emit_func(&mut os, *func);
        }

        let ts = os.finalize(self.link_targets);

        if self.pretty_print {
            let file = syn::parse2(ts).context("unable to parse output as a valid Rust file")?;
            Ok(prettyplease::unparse(&file))
        } else {
            Ok(ts.to_string())
        }
    }

    #[inline]
    fn to_ident(&self, name: &str, kind: IdentKind) -> pm::Ident {
        shared::to_ident(name, kind, self.use_rust_casing, self.fix_naming)
    }
}

pub fn load_clang_layouts() -> &'static BTreeMap<String, ArchLayouts> {
    use std::sync::Once;

    const COMPRESSED_LAYOUTS: &[u8] = include_bytes!("../../md/layouts.json.zstd");
    static INIT: Once = Once::new();
    static mut LAYOUTS: Option<BTreeMap<String, ArchLayouts>> = None;

    unsafe {
        INIT.call_once(|| {
            let decompressed = zstd::decode_all(std::io::Cursor::new(COMPRESSED_LAYOUTS))
                .expect("failed to decompress layouts");
            LAYOUTS =
                Some(serde_json::from_slice(&decompressed).expect("failed to deserialize layouts"));
        });
        LAYOUTS.as_ref().expect("failed to deserialize layouts")
    }
}

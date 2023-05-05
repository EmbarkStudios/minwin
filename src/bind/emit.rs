mod ostream;
mod constant;
mod function_pointer;
mod function;
mod record;
mod shared;

use shared::*;
use ostream::OutputStream;

use super::InterfaceMap;
use anyhow::Context as _;
use proc_macro2::{self as pm, TokenStream};
use quote::quote;
use windows_metadata::reader::{self as wmr, Reader, Type, TypeDef, TypeKind, Field};

pub struct Emit<'r> {
    /// Used to query the metadata database
    reader: &'r Reader<'r>,
    /// List of interfaces to emit, if an interface/class is not in this list it
    /// is emitted as a void* type alias instead
    ifaces: InterfaceMap,
    /// We collect additional layout information not currently present in the
    /// the metadata to supplement it
    layouts: Option<&'static ustr::UstrMap<ArchLayouts>>,
    /// If true, use the `windows-targets::link!` macro to link functions,
    /// otherwise functions are collated in extern blocks and linked with the
    /// appropriate dll
    link_targets: bool,
    /// If true, the `windows-core` crate is
    use_core: bool,
    /// If true, identifiers are fixed to remove pointless Hungarian notation
    fix_naming: bool,
    /// If true, the casing of all items will be changed to follow Rust casing conventions
    use_rust_casing: bool,
    /// If true, formats the output
    pretty_print: bool,
}

impl<'r> Emit<'r> {
    #[inline]
    fn to_ident(&self, name: &str, kind: IdentKind) -> pm::Ident {
        shared::to_ident(name, kind, self.use_rust_casing, self.fix_naming)
    }
}

pub fn emit_items(emit: Emit<'_>, items: super::ItemSet) -> anyhow::Result<String> {
    let mut os = OutputStream::new();
    let reader = emit.reader;

    for ty in items.types {
        if !emit.use_core && !matches!(ty, Type::TypeDef(..)) {
            let (ident, ts) = match ty {
                Type::HRESULT => {
                    let ident = emit.to_ident("HRESULT", IdentKind::Type);
                    let ts = quote! { pub type #ident = i32; };
                    (ident, ts)
                }
                Type::String => {
                    let ident = emit.to_ident("HSTRING", IdentKind::Type);
                    let ts = quote! { pub type #ident = *mut ::core::ffi::c_void; };
                    (ident, ts)
                }
                Type::IUnknown => {
                    let ident = emit.to_ident("IUnknown", IdentKind::Type);
                    let ts = quote! { pub type #ident = *mut ::core::ffi::c_void; };
                    (ident, ts)
                }
                Type::IInspectable => {
                    let ident = emit.to_ident("IInspectable", IdentKind::Type);
                    let ts = quote! { pub type #ident = *mut ::core::ffi::c_void; };
                    (ident, ts)
                }
                Type::PSTR => {
                    let ident = emit.to_ident("PSTR", IdentKind::Type);
                    let ts = quote! { pub type #ident = *mut u8; };
                    (ident, ts)
                }
                Type::PWSTR => {
                    let ident = emit.to_ident("PSTR", IdentKind::Type);
                    let ts = quote! { pub type #ident = *mut u16; };
                    (ident, ts)
                }
                Type::PCSTR => {
                    let ident = emit.to_ident("PCSTR", IdentKind::Type);
                    let ts = quote! { pub type #ident = *const u8; };
                    (ident, ts)
                }
                Type::PCWSTR => {
                    let ident = emit.to_ident("PCWSTR", IdentKind::Type);
                    let ts = quote! { pub type #ident = *const u16; };
                    (ident, ts)
                }
                Type::BSTR => {
                    let ident = emit.to_ident("BSTR", IdentKind::Type);
                    let ts = quote! { pub type #ident = *const u16; };
                    (ident, ts)
                },
                Type::GUID => {
                    let ident = emit.to_ident("GUID", IdentKind::Record);
                    let copy_clone_impl = emit.copy_clone_impl(&ident, true);

                    let ts= quote! {
                        #[repr(C)]
                        pub struct #ident {
                            pub data1: u32,
                            pub data2: u16,
                            pub data3: u16,
                            pub data4: [u8; 8],
                        }
                        
                        #copy_clone_impl

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
                    let ident = emit.to_ident(name, IdentKind::Record);

                    let ts = if emit.use_core {
                        emit.emit_class(&mut os, reader, def)
                    } else {
                        quote!{ pub type #ident = *mut ::core::ffi::c_void; }
                    };

                    (ident, ts)
                }
                TypeKind::Interface => {
                    let ident = emit.to_ident(name, IdentKind::Record);

                    let ts = if emit.use_core {
                        emit.emit_interface(&mut os, reader, def)
                    } else {
                        quote!{ pub type #ident = *mut ::core::ffi::c_void; }
                    };

                    (ident, ts)
                }
                TypeKind::Enum => {
                    let ident = emit.to_ident(name, IdentKind::Enum);
                    emit.emit_enum(&mut os, reader, def)

                    let ts = if emit.use_core {
                        
                    } else {
                        quote!{ pub type #ident = *mut ::core::ffi::c_void; }
                    };

                    (ident, ts)
                    sorted.insert(gen.reader.type_def_name(def), enums::gen(gen, def));
                }
                TypeKind::Struct => {
                    emit.emit_record(def)?
                }
                TypeKind::Delegate => {
                    emit.emit_function_pointer(def)?
                }
            };

            os.insert_type(ty, ident, ts);
        }
    }

    for constant in items.constants {
        emit.emit_constant(&mut os, constant);
    }

    for func in items.functions {
        emit.emit_func(&mut os, func);
    }

    let ts = os.finalize(emit.link_targets);

    if emit.pretty_print {
        let file = syn::parse2(ts).context("unable to parse output as a valid Rust file")?;
        Ok(prettyplease::unparse(&file))
    } else {
        Ok(ts.to_string())
    }
}

pub fn load_clang_layouts() -> &'static ustr::UstrMap<ArchLayout> {
    use std::sync::Once;

    const COMPRESSED_LAYOUTS: &[u8] = include_bytes!("../../md/layouts.json.zstd");
    static INIT: Once = Once::new();
    static mut LAYOUTS: Option<ustr::UstrMap<ArchLayout>> = None;

    unsafe {
        INIT.call_once(|| {
            let decompressed = zstd::decode_all(std::io::Cursor::new(COMPRESSED_LAYOUTS)).expect("failed to decompress layouts");
            LAYOUTS = Some(serde_json::from_slice(&decompressed).expect("failed to deserialize layouts"));
        });
        LAYOUTS.as_ref().expect("failed to deserialize layouts")
    }
}
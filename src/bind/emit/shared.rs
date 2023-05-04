use proc_macro2::{self as pm, TokenStream};
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use std::fmt;
use windows_metadata::reader as wmr;

#[derive(Copy, Clone)]
pub(super) enum IdentKind<'res> {
    Const,
    Enum,
    Field,
    Function,
    FunctionPointer,
    Param,
    Record,
    Type,
    Variant(Option<&'res str>),
}

impl<'res> IdentKind<'res> {
    pub(super) fn convert(self, name: &str, fix_naming: bool) -> String {
        use heck::*;

        match self {
            Self::Const => name.to_shouty_snake_case(),
            Self::Enum | Self::Record | Self::FunctionPointer | Self::Type => {
                name.to_upper_camel_case()
            }
            Self::Field | Self::Param => {
                // Many, but not all, record fields/params use Hungarian notation
                // which is ugly and pointless, so we strip it off before case conversion
                let to_skip = if fix_naming {
                    name.find(|c: char| c.is_ascii_uppercase()).unwrap_or(0)
                } else {
                    0
                };
                name[to_skip..].to_snake_case()
            }
            Self::Function => name.to_snake_case(),
            Self::Variant(prefix) => {
                let unprefixed = prefix
                    .and_then(|prefix| name.strip_prefix(prefix))
                    .unwrap_or(name);
                unprefixed.to_upper_camel_case()
            }
        }
    }
}

#[rustfmt::skip]
pub(super) fn to_ident(name: &str, kind: IdentKind, use_rust_casing: bool, fix_naming: bool) -> pm::Ident {
    let mut is = if use_rust_casing {
        kind.convert(&name, fix_naming)
    } else {
        name.to_owned()
    };

    // keywords list based on https://doc.rust-lang.org/reference/keywords.html
    if matches!(
        is.as_str(),
        "abstract" | "as" | "become" | "box" | "break" | "const" | "continue" |
        "crate" | "do" | "else" | "enum" | "extern" | "false" | "final" | "fn" |
        "for" | "if" | "impl" | "in" | "let" | "loop" | "macro" | "match" | "mod" |
        "move" | "mut" | "override" | "priv" | "pub" | "ref" | "return" | "static" |
        "struct" | "super" | "trait" | "true" | "type" | "typeof" | "unsafe" |
        "unsized" | "use" | "virtual" | "where" | "while" | "yield" | "try" |
        "async" | "await" | "dyn" | "self" | "Self"
    ) {
        is.push('_');
    }

    quote::format_ident!("{is}")
}

pub struct Value {
    pub val: wmr::Value,
    pub is_wide_str: bool,
}

impl From<wmr::Value> for Value {
    fn from(val: wmr::Value) -> Self {
        Self {
            val,
            is_wide_str: false,
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use wmr::Value;
        match &self.val {
            Value::Bool(v) => write!(f, "{v}: bool"),
            Value::I8(v) => write!(f, "{v}: i8"),
            Value::U8(v) => write!(f, "{v}: u8"),
            Value::I16(v) => write!(f, "{v}: i16"),
            Value::U16(v) => write!(f, "{v}: u16"),
            Value::I32(v) => write!(f, "{v}: i32"),
            Value::U32(v) => write!(f, "{v}: u32"),
            Value::I64(v) => write!(f, "{v}: i64"),
            Value::U64(v) => write!(f, "{v}: u64"),
            Value::F32(v) => write!(f, "{v}: f32"),
            Value::F64(v) => write!(f, "{v}: f64"),
            Value::String(s) => f.write_str(s),
            Value::Enum(_td, v) => {
                use windows_metadata::reader::Integer;
                match v {
                    Integer::I8(v) => write!(f, "{v}: enum i8"),
                    Integer::U8(v) => write!(f, "{v}: enum u8"),
                    Integer::I16(v) => write!(f, "{v}: enum i16"),
                    Integer::U16(v) => write!(f, "{v}: enum u16"),
                    Integer::I32(v) => write!(f, "{v}: enum i32"),
                    Integer::U32(v) => write!(f, "{v}: enum u32"),
                    Integer::I64(v) => write!(f, "{v}: enum i64"),
                    Integer::U64(v) => write!(f, "{v}: enum u64"),
                }
            }
            Value::TypeDef(_td) => unreachable!("uhm...typedef"),
        }
    }
}

impl ToTokens for Value {
    fn to_tokens(&self, ts: &mut TokenStream) {
        use pm::Literal;
        use wmr::Value;

        let q = match self.val {
            Value::F32(f) => Literal::f32_unsuffixed(f),
            Value::F64(d) => Literal::f64_unsuffixed(d),
            Value::I8(i) => Literal::i8_unsuffixed(i),
            Value::U8(i) => Literal::u8_unsuffixed(i),
            Value::I16(i) => Literal::i16_unsuffixed(i),
            Value::U16(i) => Literal::u16_unsuffixed(i),
            Value::I32(i) => Literal::i32_unsuffixed(i),
            Value::U32(i) => Literal::u32_unsuffixed(i),
            Value::I64(i) => Literal::i64_unsuffixed(i),
            Value::U64(i) => Literal::u64_unsuffixed(i),
            Value::Enum(..) => {
                unreachable!();
            }
            Value::String(ref s) => {
                let s = format!("{s}\0");
                if self.is_wide_str {
                    let lits = s.encode_utf16().map(|u| Literal::u16_unsuffixed(u));

                    // We emit a doc comment so that the user knows what the string's value is
                    ts.extend(quote! {
                        #[doc = #s]
                        [#(#lits),*].as_ptr()
                    });
                } else {
                    let lit = Literal::byte_string(s.as_bytes());
                    ts.extend(quote! {
                        #lit.as_ptr()
                    });
                }

                return;
            }
            Value::TypeDef(_td) => {
                unreachable!("windows_metadata doesn't parse this...at least, it didn't")
            }
            Value::Bool(b) => {
                ts.append(pm::Ident::new(
                    if b { "true" } else { "false" },
                    pm::Span::call_site(),
                ));
                return;
            }
        };

        ts.append(q);
    }
}

use wmr::GUID;

pub struct Guid {
    pub value: GUID,
    pub use_rust_casing: bool,
}

impl ToTokens for Guid {
    fn to_tokens(&self, ts: &mut TokenStream) {
        let GUID(a, b, c, d, e, f, g, h, i, j, k) = self.value;
        let gname = if self.use_rust_casing { "Guid" } else { "GUID" };
        ts.extend(format!("{gname}::from_u128(0x{a:08x?}_{b:04x?}_{c:04x?}_{d:02x?}{e:02x?}_{f:02x?}{g:02x?}{h:02x?}{i:02x?}{j:02x?}{k:02x?})").parse::<TokenStream>().unwrap());
    }
}

pub struct TypePrinter<'r> {
    r: &'r wmr::Reader<'r>,
    ty: wmr::Type,
    use_rust_casing: bool,
}

impl<'r> ToTokens for TypePrinter<'r> {
    fn to_tokens(&self, ts: &mut TokenStream) {
        use wmr::Type;
        let ty_name = match self.ty {
            Type::U32 => "u32",
            Type::I32 => "i32",
            Type::U16 => "u16",
            Type::I16 => "i16",
            Type::U64 => "u64",
            Type::I64 => "i64",
            Type::U8 => "u8",
            Type::I8 => "i8",
            Type::F32 => "f32",
            Type::F64 => "f64",
            Type::Void => "::core::ffi::c_void",
            Type::BSTR => {
                if self.use_rust_casing {
                    "Bstr"
                } else {
                    "BSTR"
                }
            }
            Type::GUID => {
                if self.use_rust_casing {
                    "Guid"
                } else {
                    "GUID"
                }
            }
            Type::HRESULT => Ok(self.to_ident("HRESULT", IdentKind::Type)),
            Type::String => anyhow::bail!("can't resolve unless format of string is known"),
            // Sigh, used by JS :(
            Type::Bool => "bool",
        };

        ts.extend(quote::quote! { #ty_name });
    }
}

impl super::Emit {
    #[inline]
    fn type_print<'r>(&self, ty: Type, reader: &'r Reader) -> TypePrinter<'r> {}
}

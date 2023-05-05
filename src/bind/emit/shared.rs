use crate::bind::Impls;
use pm::Ident;
use proc_macro2::{self as pm, TokenStream};
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use std::fmt;
use windows_metadata::reader::{self as wmr, Type};

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

    format_ident!("{is}")
}

pub struct Value {
    pub val: wmr::Value,
    /// String constants just use the Type::String which doesn't have encoding
    /// information, which is only available via the wrapping item, eg Constant
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

pub struct Guid<'r> {
    pub value: GUID,
    pub printer: TypePrinter<'r>,
}

impl<'r> ToTokens for Guid<'r> {
    fn to_tokens(&self, ts: &mut TokenStream) {
        let GUID(a, b, c, d, e, f, g, h, i, j, k) = self.value;
        self.printer.to_tokens(ts);
        ts.extend(format!("::from_u128(0x{a:08x}_{b:04x}_{c:04x}_{d:02x}{e:02x}_{f:02x}{g:02x}{h:02x}{i:02x}{j:02x}{k:02x})").parse::<TokenStream>().unwrap());
    }
}

pub struct TypePrinter<'r> {
    r: &'r wmr::Reader<'r>,
    ty: wmr::Type,
    use_rust_casing: bool,
    use_windows_core: bool,
}

impl<'r> TypePrinter<'r> {
    #[inline]
    fn core_type(&self, name: &'static str) -> &'static str {
        const CORE_TYPES: &[(&'static str, &'static str)] = &[
            ("::windows_core::BSTR", "Bstr"),
            ("::windows_core::Guid", "Guid"),
            ("::windows_core::HRESULT", "HResult"),
            ("::windows_core::PCSTR", "PCStr"),
            ("::windows_core::PCWSTR", "PCWstr"),
            ("::windows_core::PSTR", "PStr"),
            ("::windows_core::PWSTR", "PWstr"),
        ];

        if !self.use_windows_core && !self.use_rust_casing {
            name
        } else {
            CORE_TYPES
                .iter()
                .find_map(|(n, g)| {
                    let us = &n[16..];
                    (us == name).then_some(if self.use_windows_core { n } else { g })
                })
                .expect("unknown core type")
        }
    }

    #[inline]
    fn wrap(&self, ty: Type) -> Self {
        Self {
            r: self.r,
            ty,
            use_rust_casing: self.use_rust_casing,
            use_windows_core: self.use_rust_casing,
        }
    }
}

struct Ptrs(usize, bool);

impl ToTokens for Ptrs {
    fn to_tokens(&self, ts: &mut TokenStream) {
        let tok = if self.1 { "*mut" } else { "*const" };
        ts.append_separated((0..self.0).map(|_| tok), ' ');
    }
}

impl<'r> ToTokens for TypePrinter<'r> {
    fn to_tokens(&self, ts: &mut TokenStream) {
        let ty_name = match &self.ty {
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
            Type::ISize => "isize",
            Type::USize => "usize",
            Type::Char => "u16",
            Type::PCSTR => self.core_type("PCSTR"),
            Type::PCWSTR => self.core_type("PCWSTR"),
            Type::PSTR => self.core_type("PSTR"),
            Type::PWSTR => self.core_type("PWSTR"),
            Type::BSTR => self.core_type("BSTR"),
            Type::String => panic!("can't resolve unless format of string is known"),
            Type::HRESULT => self.core_type("HRESULT"),
            Type::GUID => self.core_type("GUID"),
            Type::IUnknown => {
                if self.use_windows_core {
                    "::windows_core::IUnknown"
                } else {
                    "IUnknown"
                }
            }
            Type::IInspectable => {
                if self.use_windows_core {
                    "::windows_core::IInspectable"
                } else {
                    "IInspectable"
                }
            }
            Type::Win32Array((ty, len)) => {
                let element = self.wrap(*ty.clone());
                let len = pm::Literal::usize_unsuffixed(*len);

                ts.extend(quote! { [#element; #len] });
                return;
            }
            Type::TypeDef((def, _generics)) => {
                let name = self.r.type_def_name(*def);

                if !self.use_rust_casing {
                    name
                } else {
                    use wmr::TypeKind;
                    let kind = match self.r.type_def_kind(*def) {
                        TypeKind::Struct | TypeKind::Class | TypeKind::Interface => {
                            IdentKind::Record
                        }
                        TypeKind::Delegate => IdentKind::FunctionPointer,
                        TypeKind::Enum => IdentKind::Enum,
                    };

                    let ident = to_ident(name, kind, self.use_rust_casing, false);
                    ts.append(ident);
                    return;
                }
            }
            Type::MutPtr((ty, pointers)) => {
                let ptrs = Ptrs(*pointers, true);
                let element = self.wrap(*ty.clone());

                ts.extend(quote! { #ptrs #element });
                return;
            }
            Type::ConstPtr((ty, pointers)) => {
                let ptrs = Ptrs(*pointers, false);
                let element = self.wrap(*ty.clone());

                ts.extend(quote! { #ptrs #element });
                return;
            }
            Type::GenericParam(generic) => {
                panic!(
                    "generic parameter '{}' is not supported",
                    self.r.generic_param_name(*generic)
                );
            }
            Type::WinrtArray(_) | Type::WinrtArrayRef(_) | Type::WinrtConstRef(_) => {
                panic!("WinRT type is not supported");
            }
            // Sigh, used by JS :(
            Type::Bool => "bool",
            Type::TypeName => unreachable!("this should never happen..."),
        };

        ts.append(format_ident!("{ty_name}"));
    }
}

pub(crate) struct ParamsPrinter<'r, 's> {
    sig: &'s wmr::Signature,
    r: &'r wmr::Reader<'r>,
    use_rust_casing: bool,
    use_windows_core: bool,
    fix_naming: bool,
}

impl<'r, 's> ToTokens for ParamsPrinter<'r, 's> {
    fn to_tokens(&self, ts: &mut TokenStream) {
        let piter = self.sig.params.iter().map(|param| {
            let pname = to_ident(
                self.r.param_name(param.def),
                IdentKind::Param,
                self.use_rust_casing,
                self.fix_naming,
            );
            let ty = TypePrinter {
                ty: param.ty.clone(),
                r: self.r,
                use_rust_casing: self.use_rust_casing,
                use_windows_core: self.use_windows_core,
            };

            quote! { #pname: #ty }
        });

        ts.append_separated(piter, ", ");
    }
}

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

        if arch.is_empty() {
            return;
        }

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

#[derive(Debug, Copy, Clone, serde::Deserialize)]
#[serde(tag = "l", content = "s")]
pub enum Layout {
    Packed(u8),
    Align(u8),
}

impl ToTokens for Layout {
    fn to_tokens(&self, ts: &mut TokenStream) {
        let repr = match self {
            Self::Align(align) => {
                let align = pm::Literal::u8_unsuffixed(*align);
                quote! { align(#align) }
            }
            Layout::Packed(pack) => {
                let pack = pm::Literal::u8_unsuffixed(*pack);
                quote! { packed(#pack) }
            }
        };

        ts.extend(repr);
    }
}

#[derive(Debug)]
pub enum RecordLayout {
    None,
    Agnostic(Layout),
    Arch(Vec<ArchLayout>),
}

impl ToTokens for RecordLayout {
    fn to_tokens(&self, ts: &mut TokenStream) {
        match self {
            Self::None => ts.extend(quote! {#[repr(C)]}),
            Self::Agnostic(layout) => {
                ts.extend(quote! { #[repr(C, #layout)]});
            }
            Self::Arch(layouts) => {
                for al in layouts {
                    let attrs = Attrs::from_bits(al.a).unwrap();
                    let layout = al.l;

                    ts.extend(quote! { #[cfg_attr(#attrs, repr(C, #layout))] });
                }
            }
        }
    }
}

#[derive(serde::Deserialize, Debug, Clone, Copy)]
pub struct ArchLayout {
    #[serde(default)]
    pub a: u8,
    pub l: Layout,
}

#[derive(serde::Deserialize, Debug, Clone, Default)]
pub struct ArchLayouts(Vec<ArchLayout>);

impl ArchLayouts {
    #[inline]
    pub fn get_layout(&self, attrs: Attrs) -> RecordLayout {
        let arches = attrs.intersection(Attrs::ARCH).bits();

        if arches == 0 || self.0[0].a == 0 {
            RecordLayout::Agnostic(self.0[0].l)
        } else {
            let count = self.0.iter().filter(|al| al.a & arches != 0).count();

            if count > 1 {
                RecordLayout::Arch(
                    self.0
                        .iter()
                        .filter_map(|al| (al.a & arches != 0).then_some(*al))
                        .collect(),
                )
            } else {
                if let Some(l) = self
                    .0
                    .iter()
                    .filter_map(|al| (al.a & arches != 0).then_some(al.l))
                    .next()
                {
                    RecordLayout::Agnostic(l)
                } else {
                    RecordLayout::None
                }
            }
        }
    }
}

impl From<Vec<ArchLayout>> for ArchLayouts {
    fn from(v: Vec<ArchLayout>) -> Self {
        Self(v)
    }
}

pub(crate) struct ImplsPrinter<'i>(&'i Ident, Impls);

impl<'i> ToTokens for ImplsPrinter<'i> {
    fn to_tokens(&self, ts: &mut TokenStream) {
        let ident = self.0;
        if self.1.contains(Impls::COPY) {
            ts.extend(quote! { impl ::core::marker::Copy for #ident {} });
        }

        if self.1.contains(Impls::CLONE) {
            ts.extend(quote! {
                impl ::core::clone::Clone for #ident {
                    fn clone(&self) -> Self {
                        *self
                    }
                }
            });
        }
    }
}

impl<'r> super::Emit<'r> {
    #[inline]
    pub(crate) fn type_printer(&self, ty: Type) -> TypePrinter<'r> {
        TypePrinter {
            r: self.reader,
            ty,
            use_rust_casing: self.use_rust_casing,
            use_windows_core: self.use_core,
        }
    }

    #[inline]
    pub(crate) fn guid_printer(&self, guid: GUID) -> Guid<'r> {
        Guid {
            value: guid,
            printer: TypePrinter {
                r: self.reader,
                ty: Type::GUID,
                use_rust_casing: self.use_rust_casing,
                use_windows_core: self.use_core,
            },
        }
    }

    #[inline]
    pub(crate) fn param_printer<'s>(&self, sig: &'s wmr::Signature) -> ParamsPrinter<'r, 's> {
        ParamsPrinter {
            sig,
            r: self.reader,
            use_rust_casing: self.use_rust_casing,
            use_windows_core: self.use_core,
            fix_naming: self.fix_naming,
        }
    }

    #[inline]
    pub(crate) fn attributes(&self, attributes: impl Iterator<Item = wmr::Attribute>) -> Attrs {
        let reader = self.reader;
        let mut attrs = Attrs::empty();

        for attr in attributes {
            match reader.attribute_name(attr) {
                "SupportedArchitectureAttribute" => {
                    if let Some((_, wmr::Value::Enum(_, wmr::Integer::I32(value)))) =
                        reader.attribute_args(attr).get(0)
                    {
                        if value & 1 != 0 {
                            attrs.insert(Attrs::X86);
                        }
                        if value & 2 != 0 {
                            attrs.insert(Attrs::X86_64);
                        }
                        if value & 4 != 0 {
                            attrs.insert(Attrs::AARCH64);
                        }
                    }
                }
                "DeprecatedAttribute" => {
                    attrs.insert(Attrs::DEPRECATED);
                }
                _ => {}
            }
        }

        attrs
    }

    /// Adds a Clone and optional Copy implementation for the specified type
    #[inline]
    pub(crate) fn impls<'i>(&self, ident: &'i Ident, impls: Impls) -> ImplsPrinter<'i> {
        ImplsPrinter(ident, impls)
    }

    /// Checks if a type is `Copy`
    #[inline]
    pub(crate) fn is_copy(&self, ty: &Type) -> bool {
        if let Type::TypeDef((td, _)) = ty {
            if matches!(self.reader.type_def_kind(*td), wmr::TypeKind::Struct) {
                return self
                    .items
                    .types
                    .get(ty)
                    .map(|i| i.contains(Impls::COPY))
                    .unwrap_or_default();
            }
        }

        self.reader.type_is_copyable(ty)
    }
}

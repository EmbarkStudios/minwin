use anyhow::Context as _;
use rayon::prelude::*;
use std::collections::BTreeMap;
use windows_metadata::reader::{self, File, Reader};

pub struct Tree {
    files: Vec<File>,
}

impl Tree {
    pub fn new() -> anyhow::Result<Self> {
        let mds = ["Windows", "Windows.Win32", "Windows.Win32.Interop"];

        let mut files = Vec::new();

        mds.into_par_iter()
            .map(|p| -> anyhow::Result<File> {
                let compressed = std::fs::read("md/{p}/.winmd.zstd")?;
                let decompressed = zstd::decode_all(std::io::Cursor::new(compressed))?;
                Ok(File::from_buffer(decompressed, format!("{p}.winmd"))?)
            })
            .collect_into_vec(&mut files);

        Ok(Self {
            files: files.into_iter().collect::<anyhow::Result<Vec<_>>>()?,
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Builtin {
    Void,
    Bool,
    Float,
    Double,
    Char,
    UChar,
    Short,
    UShort,
    Int,
    UInt,
    Long,
    ULong,
    USize,
    ISize,
    // Win32 "builtin" types
    Guid,
    Hresult,
    Pstr,
    Pwstr,
    Pcstr,
    Pcwstr,
    Bstr,
}

impl TryFrom<reader::Type> for Builtin {
    type Error = anyhow::Error;

    fn try_from(ty: reader::Type) -> anyhow::Result<Self> {
        use reader::Type;
        let bi = match ty {
            Type::Void => Self::Void,
            Type::Bool => Self::Bool,
            Type::I8 => Self::Char,
            Type::U8 | Type::Char => Self::UChar,
            Type::I16 => Self::Short,
            Type::U16 => Self::UShort,
            Type::I32 => Self::Int,
            Type::U32 => Self::UInt,
            Type::I64 => Self::Long,
            Type::U64 => Self::ULong,
            Type::ISize => Self::ISize,
            Type::USize => Self::USize,
            Type::F32 => Self::Float,
            Type::F64 => Self::Double,
            Type::GUID => Self::Guid,
            Type::HRESULT => Self::Hresult,
            Type::BSTR => Self::Bstr,
            Type::PSTR => Self::Pcstr,
            Type::PWSTR => Self::Pwstr,
            Type::PCSTR => Self::Pcstr,
            Type::PCWSTR => Self::Pcwstr,
            _ => anyhow::bail!("type is not a builtin"),
        };

        Ok(bi)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum QualType<'tree> {
    Pointer {
        is_const: bool,
        is_pointee_const: bool,
        pointee: Box<QualType<'tree>>,
    },
    // Reference {
    //     is_const: bool,
    //     pointee: Box<QualType<'ast>>,
    // },
    Builtin(Builtin),
    FunctionPointer {
        name: &'tree str,
    },
    Array {
        element: Box<QualType<'tree>>,
        len: u32,
    },
    Enum {
        name: &'tree str,
        //cxx_qt: &'ast str,
        //repr: Builtin,
    },
    // Flags {
    //     name: &'ast str,
    //     repr: Builtin,
    // },
    Record {
        name: &'tree str,
    },
    // TemplateTypedef {
    //     name: String,
    // },
}

impl<'tree> TryFrom<reader::Type> for QualType<'tree> {
    type Error = anyhow::Error;

    fn try_from(ty: reader::Type) -> anyhow::Result<Self> {
        use reader::Type;

        fn resolve_ptr<'tree>(
            inner: Box<Type>,
            depth: usize,
            is_const: bool,
        ) -> anyhow::Result<QualType<'tree>> {
            let pointee = if depth > 1 {
                resolve_ptr(inner, depth - 1, is_const)?
            } else {
                (*inner).try_into()?
            };

            Ok(QualType::Pointer {
                is_const: false,
                is_pointee_const: is_const,
                pointee: Box::new(pointee),
            })
        }

        let qt = match ty {
            Type::MutPtr((inner, depth)) => resolve_ptr(inner, depth, false)?,
            Type::ConstPtr((inner, depth)) => resolve_ptr(inner, depth, true)?,
            Type::Win32Array((ele, len)) => Self::Array {
                element: Box::new((*ele).try_into()?),
                len: len as u32,
            },
            Type::String
            | Type::IUnknown
            | Type::IInspectable
            | Type::TypeName
            | Type::GenericParam(_)
            | Type::WinrtArray(_)
            | Type::WinrtArrayRef(_)
            | Type::WinrtConstRef(_)
            | Type::TypeDef(_) => {
                anyhow::bail!("type is not supported");
            }
            other => Self::Builtin(other.try_into()?),
        };

        Ok(qt)
    }
}

#[derive(Debug)]
pub struct Item<'tree> {
    pub name: &'tree str,
    pub kind: QualType<'tree>,
}

#[derive(Debug, Copy, Clone)]
enum Layout {
    Packed(u32),
    Align(u32),
}

bitflags::bitflags! {
    pub struct RecAttrs: u32 {
        const X86 = 1 << 0;
        const X86_64 = 1 << 1;
        const AARCH64 = 1 << 2;

        const COPY_CLONE = 1 << 3;
        const UNION = 1 << 4;
        const DEPRECATED = 1 << 5;
    }
}

#[derive(Debug)]
pub struct Record<'tree> {
    fields: Vec<Item<'tree>>,
    layout: Option<Layout>,
    attrs: RecAttrs,
    nested: Vec<Record<'tree>>,
}

#[derive(Debug)]
pub struct Func<'tree> {
    params: Vec<Item<'tree>>,
    ret: Option<QualType<'tree>>,
    module: Option<&'tree str>,
    is_system: bool,
}

pub struct Value(pub reader::Value);

impl From<reader::Value> for Value {
    fn from(v: reader::Value) -> Self {
        Self(v)
    }
}

use std::fmt;

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use reader::Value;
        match &self.0 {
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
            Value::TypeDef(td) => unreachable!("uhm...typedef"),
        }
    }
}

#[derive(Debug)]
pub struct EnumVariant<'tree> {
    name: &'tree str,
    value: Value,
}

#[derive(Debug)]
pub struct Enum<'tree> {
    repr: Builtin,
    variants: Vec<EnumVariant<'tree>>,
}

#[derive(Debug)]
pub struct Constant<'tree> {
    value: Value,
    kind: QualType<'tree>,
    needs_conversion: bool,
    is_ansi: bool,
}

pub struct Resolver<'tree> {
    trees: Vec<&'tree reader::Tree<'tree>>,
    reader: reader::Reader<'tree>,
    constants: BTreeMap<&'tree str, Constant<'tree>>,
    functions: BTreeMap<&'tree str, Func<'tree>>,
    records: BTreeMap<&'tree str, Vec<Record<'tree>>>,
    enums: BTreeMap<&'tree str, Enum<'tree>>,
    function_pointers: BTreeMap<&'tree str, Func<'tree>>,
}

#[derive(Default)]
struct TreeItems<'tree> {
    constants: BTreeMap<&'tree str, Constant<'tree>>,
    functions: BTreeMap<&'tree str, Func<'tree>>,
    records: BTreeMap<&'tree str, Vec<Record<'tree>>>,
    enums: BTreeMap<&'tree str, Enum<'tree>>,
    function_pointers: BTreeMap<&'tree str, Func<'tree>>,
}

impl<'tree> TreeItems<'tree> {
    fn try_merge(&mut self, other: TreeItems<'tree>) -> anyhow::Result<()> {
        for (k, v) in other.constants {
            if let Some(old) = self.constants.insert(k, v) {
                anyhow::bail!("a constant named '{k}' already existed: {old:?}");
            }
        }

        for (k, v) in other.functions {
            if let Some(old) = self.functions.insert(k, v) {
                anyhow::bail!("a function named '{k}' already existed: {old:?}");
            }
        }

        for (k, v) in other.records {
            if let Some(old) = self.records.insert(k, v) {
                anyhow::bail!("a record named '{k}' already existed: {old:?}");
            }
        }

        for (k, v) in other.enums {
            if let Some(old) = self.enums.insert(k, v) {
                anyhow::bail!("an enum named '{k}' already existed: {old:?}");
            }
        }

        for (k, v) in other.function_pointers {
            if let Some(old) = self.function_pointers.insert(k, v) {
                anyhow::bail!("a function pointer named '{k}' already existed: {old:?}");
            }
        }

        Ok(())
    }
}

impl<'tree> Resolver<'tree> {
    pub fn flatten(root: &'tree Tree) -> anyhow::Result<Self> {
        let reader = reader::Reader::new(&root.files);
        let win32 = reader
            .tree("Windows.Win32", &[])
            .context("failed to find Windows.Win32 namespace")?;

        let trees = reader::Tree {
            namespace: "Windows",
            nested: BTreeMap::from([("Win32", win32)]),
        }
        .flatten();
        let merged = trees
            .par_iter()
            .map(|tree| Self::get_items(&reader, tree))
            .try_reduce(
                || TreeItems::default(),
                |mut a, b| {
                    a.try_merge(b)?;
                    Ok(a)
                },
            )?;

        Ok(Self {
            trees,
            reader,
            constants: merged.constants,
            functions: merged.functions,
            records: merged.records,
            enums: merged.enums,
            function_pointers: merged.function_pointers,
        })
    }

    fn get_items(reader: &Reader<'tree>, tree: &reader::Tree) -> anyhow::Result<TreeItems<'tree>> {
        use reader::TypeKind;

        let mut constants = BTreeMap::new();
        let mut functions = BTreeMap::new();
        let mut records = BTreeMap::new();
        let mut enums = BTreeMap::new();
        let mut function_pointers = BTreeMap::new();

        for def in reader.namespace_types(tree.namespace) {
            let type_name = reader.type_def_type_name(def);
            let name = type_name.name;

            let kind = reader.type_def_kind(def);
            match kind {
                // Because winmd is a .NET format, and .NET doesn't have free functions
                // nor constants, we need to look in "classes" to get the actual win32
                // function signatures and constants
                TypeKind::Class => {
                    // ...except WinRT has actual classes, but we don't care about WinRT
                    if reader.type_def_flags(def).winrt() {
                        continue;
                    }

                    let (funcs, consts) = rayon::join(
                        || -> anyhow::Result<()> {
                            for method in reader.type_def_methods(def) {
                                let func_name = reader.method_def_name(method);
                                let func = Self::get_func(reader, method)?;
                                if let Some(old) = functions.insert(func_name, func) {
                                    anyhow::bail!(
                                        "a function named '{func_name}' already existed: {old:?}"
                                    );
                                }
                            }

                            Ok(())
                        },
                        || -> anyhow::Result<()> {
                            for field in reader.type_def_fields(def) {
                                let name = reader.field_name(field);
                                let constant = Self::get_constant(reader, field)?;
                                if let Some(old) = constants.insert(name, constant) {
                                    anyhow::bail!(
                                        "a constant named '{name}' already existed: {old:?}"
                                    );
                                }
                            }

                            Ok(())
                        },
                    );

                    funcs?;
                    consts?;
                }
                // Unlike the name suggests, this also includes unions
                TypeKind::Struct => {
                    if reader.type_def_is_contract(def) {
                        tracing::trace!("we don't care about contract '{name}'");
                        continue;
                    } else if reader
                        .type_def_attributes(def)
                        .any(|tda| reader.attribute_name(tda) == "NativeTypedefAttribute")
                    {
                        tracing::debug!("encountered handle '{name}'");
                        continue;
                    }

                    let record = Self::get_record(reader, def)?;

                    records.entry(name).or_insert(Vec::new()).push(record);
                }
                TypeKind::Enum => {
                    let enm = Self::get_enum(reader, def)?;
                    if let Some(old) = enums.insert(name, enm) {
                        anyhow::bail!("an enum named '{name}' already existed: {old:?}");
                    }
                }
                // Delegates are .NET speak for function pointer signatures
                TypeKind::Delegate => {
                    let fptr = Self::get_func_ptr(reader, def)?;
                    if let Some(old) = function_pointers.insert(name, fptr) {
                        anyhow::bail!("a function pointer named '{name}' already existed: {old:?}");
                    }
                }
                TypeKind::Interface => {
                    tracing::trace!("we don't care about interface '{name}'");
                }
            }
        }

        Ok(TreeItems {
            constants,
            functions,
            records,
            enums,
            function_pointers,
        })
    }

    fn get_func(reader: &Reader<'tree>, def: reader::MethodDef) -> anyhow::Result<Func<'tree>> {
        let name = reader.method_def_name(def);
        let sig = reader.method_def_signature(def, &[]);

        let ret = if let Some(return_type) = sig.return_type {
            Some(return_type.try_into()?)
        } else if reader.method_def_does_not_return(sig.def) {
            panic!("interesting, {name} doesn't return");
        } else {
            None
        };

        let impl_map = reader
            .method_def_impl_map(def)
            .with_context(|| format!("ImplMap not found for function '{name}'"))?;

        // Determine whether the calling convention is "system" or "C"
        let is_system = {
            let inv_attrs = reader.impl_map_flags(impl_map);

            if inv_attrs.conv_platform() {
                true
            } else if inv_attrs.conv_cdecl() {
                false
            } else {
                anyhow::bail!(
                    "function {name} has invalid invoke attributes {:08x}",
                    inv_attrs.0
                );
            }
        };

        let module = {
            let scope = reader.impl_map_scope(impl_map);
            reader.module_ref_name(scope)
        };

        let params = sig
            .params
            .into_iter()
            .map(|param| {
                let name = reader.param_name(param.def);
                let kind = param.ty.try_into()?;

                Ok(Item { name, kind })
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        Ok(Func {
            params,
            ret,
            module: Some(module),
            is_system,
        })
    }

    fn get_func_ptr(reader: &Reader<'tree>, def: reader::TypeDef) -> anyhow::Result<Func<'tree>> {
        let name = reader.type_def_name(def);
        let method = reader.type_def_invoke_method(def);
        let sig = reader.method_def_signature(method, &[]);

        let ret = if let Some(return_type) = sig.return_type {
            Some(return_type.try_into()?)
        } else if reader.method_def_does_not_return(sig.def) {
            panic!("interesting, function pointer {name} doesn't return");
        } else {
            None
        };

        let params = sig
            .params
            .into_iter()
            .map(|param| {
                let name = reader.param_name(param.def);
                let kind = param.ty.try_into()?;

                Ok(Item { name, kind })
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        Ok(Func {
            params,
            ret,
            module: None,
            is_system: true,
        })
    }

    fn get_record(reader: &Reader<'tree>, def: reader::TypeDef) -> anyhow::Result<Record<'tree>> {
        let name = reader.type_def_name(def);

        // Check if this is actually only used as an opaque pointer
        if reader.type_def_fields(def).next().is_none() {
            tracing::debug!("found opaque struct {name}");
            return Ok(Record {
                fields: vec![Item {
                    name: "_unused",
                    kind: QualType::Array {
                        element: Box::new(QualType::Builtin(Builtin::UChar)),
                        len: 0,
                    },
                }],
                layout: None,
                attrs: RecAttrs::empty(),
                nested: Vec::new(),
            });
        }

        let layout = if let Some(layout) = reader.type_def_class_layout(def) {
            Some(Layout::Packed(
                reader.class_layout_packing_size(layout) as u32
            ))
        } else if let Some(alignment) = None
        /* TODO: Generate list of records with alignment via clang++ */
        {
            Some(Layout::Align(alignment))
        } else {
            None
        };

        let fields = reader
            .type_def_fields(def)
            .filter_map(|f| {
                let name = reader.field_name(f);
                let ty = reader.field_type(f, Some(def));

                if reader.field_flags(f).literal() {
                    return None;
                }

                let kind = match ty.try_into() {
                    Ok(kind) => kind,
                    Err(err) => return Some(Err(err)),
                };

                Some(Ok(Item { name, kind }))
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        let flags = reader.type_def_flags(def);

        let mut attrs = RecAttrs::empty();

        if flags.explicit_layout() {
            attrs.insert(RecAttrs::UNION);
        }

        for attr in reader.type_def_attributes(def) {
            match reader.attribute_name(attr) {
                "SupportedArchitectureAttribute" => {
                    if let Some((_, reader::Value::I32(value))) = reader.attribute_args(attr).get(0)
                    {
                        if value & 1 != 0 {
                            attrs.insert(RecAttrs::X86);
                        }
                        if value & 2 != 0 {
                            attrs.insert(RecAttrs::X86_64);
                        }
                        if value & 4 != 0 {
                            attrs.insert(RecAttrs::AARCH64);
                        }
                    }
                }
                "DeprecatedAttribute" => {
                    attrs.insert(RecAttrs::DEPRECATED);
                }
                _ => {}
            }
        }

        let nested = reader
            .nested_types(def)
            .map(|td| Self::get_record(reader, td))
            .collect::<anyhow::Result<Vec<_>>>()?;

        Ok(Record {
            fields,
            layout,
            attrs,
            nested,
        })
    }

    fn get_enum(reader: &Reader<'tree>, def: reader::TypeDef) -> anyhow::Result<Enum<'tree>> {
        let type_name = reader.type_def_type_name(def);
        let repr = reader.type_def_underlying_type(def).try_into()?;

        let variants = reader
            .type_def_fields(def)
            .filter_map(|field| {
                if reader.field_flags(field).literal() {
                    let name = reader.field_name(field);
                    let constant = reader.field_constant(field)?;
                    let value = reader.constant_value(constant);

                    Some(EnumVariant {
                        name,
                        value: value.into(),
                    })
                } else {
                    None
                }
            })
            .collect();

        Ok(Enum { repr, variants })
    }

    fn get_constant(reader: &Reader<'tree>, def: reader::Field) -> anyhow::Result<Constant<'tree>> {
        let name = reader.field_name(def);

        let Some(constant) = reader.field_constant(def) else { anyhow::bail!("{name} is an invalid constant") };
        let kind = reader.constant_type(constant);
        let needs_conversion = kind != reader.field_type(def, None).to_const();

        let kind = kind.try_into()?;

        let value = reader.constant_value(constant);

        Ok(Constant {
            value: value.into(),
            needs_conversion,
            kind,
            is_ansi: reader.field_is_ansi(def),
        })

        // if ty == constant_type {
        //     if ty == Type::String {
        //         let crate_name = gen.crate_name();
        //         if gen.reader.field_is_ansi(def) {
        //             let value = gen.value(&reader.constant_value(constant));
        //             quote! {
        //                 #doc
        //                 #features
        //                 pub const #name: ::#crate_name::core::PCSTR = ::#crate_name::s!(#value);
        //             }
        //         } else {
        //             let value = gen.value(&gen.reader.constant_value(constant));
        //             quote! {
        //                 #doc
        //                 #features
        //                 pub const #name: ::#crate_name::core::PCWSTR = ::#crate_name::w!(#value);
        //             }
        //         }
        //     } else {
        //         let value = gen.typed_value(&gen.reader.constant_value(constant));
        //         quote! {
        //             #doc
        //             #features
        //             pub const #name: #value;
        //         }
        //     }
        // } else {
        //     let kind = gen.type_default_name(&ty);
        //     let value = gen.value(&gen.reader.constant_value(constant));

        //     let value = if gen.reader.type_underlying_type(&ty) == constant_type {
        //         value
        //     // TODO: workaround for https://github.com/microsoft/win32metadata/issues/1029
        //     } else if ty == Type::PCWSTR && value.0.starts_with('-') {
        //         quote! { #value as u16 as _ }
        //     } else {
        //         quote! { #value as _ }
        //     };

        //     if !gen.sys && gen.reader.type_has_replacement(&ty) {
        //         quote! {
        //             #doc
        //             #features
        //             pub const #name: #kind = #kind(#value);
        //         }
        //     } else {
        //         quote! {
        //             #doc
        //             #features
        //             pub const #name: #kind = #value;
        //         }
        //     }
        // }
    }
}

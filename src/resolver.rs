use anyhow::Context as _;
use rayon::prelude::*;
use std::collections::BTreeMap;
use ustr::{Ustr, UstrMap};
use windows_metadata::{
    reader::{self, File, Reader},
    FieldAttributes, PInvokeAttributes, TypeAttributes,
};

#[derive(serde::Deserialize, Debug, Clone)]
pub struct ArchLayout {
    #[serde(default)]
    pub a: u8,
    pub l: Layout,
}

#[derive(serde::Deserialize, Debug, Clone, Default)]
pub struct Layouts(Vec<ArchLayout>);

impl Layouts {
    #[inline]
    pub fn get_layout(&self, arch: u8) -> Option<Layout> {
        self.0.iter().find_map(|al| {
            if al.a == 0 {
                Some(al.l)
            } else if arch & al.a != 0 {
                Some(al.l)
            } else {
                None
            }
        })
    }

    #[inline]
    fn iter(&self) -> impl Iterator<Item = ArchLayout> + '_ {
        self.0.iter().cloned()
    }
}

impl From<Vec<ArchLayout>> for Layouts {
    fn from(v: Vec<ArchLayout>) -> Self {
        Self(v)
    }
}

pub struct MetadataFiles {
    files: Vec<File>,
    layouts: UstrMap<Layouts>,
}

impl MetadataFiles {
    pub fn new() -> anyhow::Result<Self> {
        let mds = ["Windows", "Windows.Win32", "Windows.Win32.Interop"];

        let (md_files, layouts) = rayon::join(
            || {
                let mut files = Vec::new();
                mds.into_par_iter()
                    .map(|p| -> anyhow::Result<File> {
                        let compressed =
                            std::fs::read(format!("/home/jake/code/minwin/md/{p}.winmd.zstd"))?;
                        let decompressed = zstd::decode_all(std::io::Cursor::new(compressed))?;
                        Ok(File::from_buffer(decompressed)?)
                    })
                    .collect_into_vec(&mut files);
                files
            },
            || -> anyhow::Result<_> {
                let compressed = std::fs::read("/home/jake/code/minwin/md/layouts.json.zstd")?;
                let decompressed = zstd::decode_all(std::io::Cursor::new(compressed))?;
                let layouts: UstrMap<Layouts> = serde_json::from_slice(&decompressed)?;
                Ok(layouts)
            },
        );

        Ok(Self {
            files: md_files.into_iter().collect::<anyhow::Result<Vec<_>>>()?,
            layouts: layouts?,
        })
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Builtin {
    Void,
    Never,
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

impl Builtin {
    pub fn as_repr(self) -> anyhow::Result<&'static str> {
        let rep = match self {
            Self::Char => "i8",
            Self::UChar => "u8",
            Self::Short => "i16",
            Self::UShort => "u16",
            Self::Int => "i32",
            Self::UInt => "u32",
            Self::Long => "i64",
            Self::ULong => "u64",
            _ => anyhow::bail!("{self:?} is an invalid enum representation"),
        };

        Ok(rep)
    }
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
pub enum QualType {
    Pointer {
        is_const: bool,
        pointee: Box<QualType>,
    },
    Builtin(Builtin),
    FunctionPointer {
        name: Ustr,
    },
    Array {
        element: Box<QualType>,
        len: u32,
    },
    Enum {
        name: Ustr,
    },
    Record {
        name: Ustr,
    },
    Typedef {
        name: Ustr,
    },
}

#[derive(Debug)]
pub struct Item {
    pub name: Ustr,
    pub kind: QualType,
}

#[derive(Debug, Copy, Clone, serde::Deserialize)]
#[serde(tag = "l", content = "s")]
pub enum Layout {
    Packed(u8),
    Align(u8),
}

bitflags::bitflags! {
    #[derive(Debug, Copy, Clone)]
    pub struct RecAttrs: u8 {
        const X86 = 1 << 0;
        const X86_64 = 1 << 1;
        const AARCH64 = 1 << 2;

        const COPY_CLONE = 1 << 3;
        const UNION = 1 << 4;
        const DEPRECATED = 1 << 5;

        const ARCH = RecAttrs::X86.bits() | RecAttrs::X86_64.bits() | RecAttrs::AARCH64.bits();
    }
}

#[derive(Debug)]
pub struct Record {
    pub fields: Vec<Item>,
    pub layouts: Layouts,
    pub attrs: RecAttrs,
    pub nested: Vec<Record>,
}

#[derive(Debug)]
pub struct Func {
    pub params: Vec<Item>,
    pub ret: Option<QualType>,
    pub module: Option<Ustr>,
    pub is_system: bool,
}

pub struct Value {
    pub val: reader::Value,
    pub is_wide_str: bool,
}

impl From<reader::Value> for Value {
    fn from(val: reader::Value) -> Self {
        Self {
            val,
            is_wide_str: false,
        }
    }
}

use std::fmt;

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use reader::Value;
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
            Value::Enum(td, v) => {
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

#[derive(Debug)]
pub struct EnumVariant {
    pub name: Ustr,
    pub value: Value,
}

#[derive(Debug)]
pub struct Enum {
    pub repr: Builtin,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug)]
pub struct Constant {
    pub value: Value,
    pub kind: QualType,
    pub needs_conversion: bool,
}

pub struct Resolver {
    pub namespaces: Vec<(Ustr, TreeItems)>,
}

#[derive(Default)]
pub struct TreeItems {
    pub constants: UstrMap<Constant>,
    pub functions: UstrMap<Vec<Func>>,
    pub records: UstrMap<Vec<Record>>,
    pub enums: UstrMap<Enum>,
    pub function_pointers: UstrMap<Vec<Func>>,
    pub typedefs: UstrMap<Vec<QualType>>,
}

impl Resolver {
    pub fn flatten(md: &MetadataFiles) -> anyhow::Result<Self> {
        let reader = reader::Reader::new(&md.files);

        // windows-sys doesn't care about these, so neither do we
        const EXCLUDED_NAMESPACES: &[&str] = &[
            "Windows.Win32.AI.MachineLearning",
            "Windows.Win32.Graphics.CompositionSwapchain",
            "Windows.Win32.Graphics.Direct2D",
            "Windows.Win32.Graphics.Direct3D",
            "Windows.Win32.Graphics.Direct3D10",
            "Windows.Win32.Graphics.Direct3D11",
            "Windows.Win32.Graphics.Direct3D11on12",
            "Windows.Win32.Graphics.Direct3D12",
            "Windows.Win32.Graphics.Direct3D9",
            "Windows.Win32.Graphics.Direct3D9on12",
            "Windows.Win32.Graphics.DirectComposition",
            "Windows.Win32.Graphics.DirectDraw",
            "Windows.Win32.Graphics.DirectManipulation",
            "Windows.Win32.Graphics.DirectWrite",
            "Windows.Win32.Graphics.DXCore",
            "Windows.Win32.Graphics.Dxgi",
            "Windows.Win32.Graphics.Imaging",
            "Windows.Win32.Interop",
            "Windows.Win32.Media.Audio.DirectSound",
            "Windows.Win32.Media.DirectShow",
            "Windows.Win32.Media.MediaFoundation",
            "Windows.Win32.Media.PictureAcquisition",
            "Windows.Win32.System.Diagnostics.Debug.WebApp",
            "Windows.Win32.System.SideShow",
            "Windows.Win32.System.TransactionServer",
            "Windows.Win32.System.WinRT",
            "Windows.Win32.Web.MsHtml",
            "Windows.Win32.UI.Xaml",
        ];

        let win32 = reader
            .tree("Windows.Win32", EXCLUDED_NAMESPACES)
            .context("failed to find Windows.Win32 namespace")?;

        let root = reader::Tree {
            namespace: "Windows",
            nested: BTreeMap::from([("Win32", win32)]),
        };
        let trees = root.flatten();

        let namespaces = trees
            .par_iter()
            .map(|tree| {
                Self::get_items(&reader, tree, &md.layouts)
                    .map(|ti| (Ustr::from(tree.namespace), ti))
            })
            .try_fold(
                || Vec::with_capacity(trees.len()),
                |mut v, t| -> anyhow::Result<_> {
                    v.push(t?);
                    Ok(v)
                },
            )
            .try_reduce(
                || Vec::with_capacity(trees.len()),
                |mut a, mut b| {
                    a.append(&mut b);
                    Ok(a)
                },
            )?;

        Ok(Self { namespaces })
    }

    fn resolve_ptr(
        reader: &Reader<'_>,
        inner: Box<reader::Type>,
        depth: usize,
        is_const: bool,
    ) -> anyhow::Result<Option<QualType>> {
        let pointee = if depth > 1 {
            let Some(inner) = Self::resolve_ptr(reader, inner, depth - 1, is_const)? else { return Ok(None) };
            inner
        } else if let Some(inner) = Self::resolve_type(reader, *inner)? {
            inner
        } else {
            return Ok(None);
        };

        Ok(Some(QualType::Pointer {
            is_const,
            pointee: Box::new(pointee),
        }))
    }

    fn resolve_type(reader: &Reader<'_>, ty: reader::Type) -> anyhow::Result<Option<QualType>> {
        use reader::Type;

        let qt = match ty {
            Type::MutPtr((inner, depth)) => {
                let Some(inner) = Self::resolve_ptr(reader, inner, depth, false)? else { return Ok(None) };
                inner
            }
            Type::ConstPtr((inner, depth)) => {
                let Some(inner) = Self::resolve_ptr(reader, inner, depth, true)? else { return Ok(None) };
                inner
            }
            Type::Win32Array((ele, len)) => {
                if let Some(ele) = Self::resolve_type(reader, *ele)? {
                    QualType::Array {
                        element: Box::new(ele),
                        len: len as u32,
                    }
                } else {
                    tracing::error!("array element was not valid");
                    return Ok(None);
                }
            }
            Type::TypeDef((td, _)) => {
                // A few structs/unions have anonymous nested records inside them, this is a bit
                // ugly since we need to mirror the naming when we actually emit the Rust definition
                fn scoped_name(reader: &Reader<'_>, td: reader::TypeDef) -> Ustr {
                    let name = reader.type_def_name(td);
                    if let Some(enclosing_type) = reader.type_def_enclosing_type(td) {
                        for (index, nested_type) in reader.nested_types(enclosing_type).enumerate()
                        {
                            if reader.type_def_name(nested_type) == name {
                                return format!("{}_{index}", scoped_name(reader, enclosing_type))
                                    .into();
                            }
                        }
                    }

                    name.into()
                }

                let name = scoped_name(reader, td);
                let kind = reader.type_def_kind(td);

                use reader::TypeKind;
                match kind {
                    TypeKind::Struct => {
                        if reader
                            .type_def_attributes(td)
                            .any(|tda| reader.attribute_name(tda) == "NativeTypedefAttribute")
                        {
                            QualType::Typedef { name }
                        } else {
                            QualType::Record { name }
                        }
                    }
                    TypeKind::Delegate => QualType::FunctionPointer { name },
                    TypeKind::Enum => QualType::Enum { name },
                    invalid => {
                        tracing::error!("encountered typedef '{name}' which is the invalid type kind '{invalid:?}'");
                        return Ok(None);
                    }
                }
            }
            Type::String
            | Type::IUnknown
            | Type::IInspectable
            | Type::TypeName
            | Type::GenericParam(_)
            | Type::WinrtArray(_)
            | Type::WinrtArrayRef(_)
            | Type::WinrtConstRef(_) => {
                tracing::debug!("type is not supported");
                return Ok(None);
            }
            other => QualType::Builtin(other.try_into()?),
        };

        Ok(Some(qt))
    }

    fn get_items(
        reader: &Reader<'_>,
        tree: &reader::Tree<'_>,
        layouts: &UstrMap<Layouts>,
    ) -> anyhow::Result<TreeItems> {
        use reader::TypeKind;

        let mut constants = UstrMap::default();
        let mut functions = UstrMap::default();
        let mut records = UstrMap::default();
        let mut enums = UstrMap::default();
        let mut function_pointers = UstrMap::default();
        let mut typedefs = UstrMap::default();

        for def in reader.namespace_types(tree.namespace) {
            let type_name = reader.type_def_type_name(def);
            let name = type_name.name;
            let kind = reader.type_def_kind(def);

            let s = tracing::debug_span!("get_def", typedef = name, kind = ?kind);
            let _s = s.enter();

            let name = name.into();

            match kind {
                // Because winmd is a .NET format, and .NET doesn't have free functions
                // nor constants, we need to look in "classes" to get the actual win32
                // function signatures and constants
                TypeKind::Class => {
                    // ...except WinRT has actual classes, but we don't care about WinRT
                    if reader.type_def_flags(def).contains(TypeAttributes::WINRT) {
                        continue;
                    }

                    let (funcs, consts) = rayon::join(
                        || -> anyhow::Result<()> {
                            for method in reader.type_def_methods(def) {
                                let func_name = reader.method_def_name(method);
                                let s = tracing::debug_span!("get_func", function = func_name);
                                let _s = s.enter();
                                let Some(func) = Self::get_func(reader, method).with_context(|| {
                                    format!("failed to resolve function '{func_name}'")
                                })? else { continue };

                                functions
                                    .entry(func_name.into())
                                    .or_insert(Vec::new())
                                    .push(func);
                            }

                            Ok(())
                        },
                        || -> anyhow::Result<()> {
                            for field in reader.type_def_fields(def) {
                                let cname = reader.field_name(field);
                                let s = tracing::debug_span!("get_constant", constant = cname);
                                let _s = s.enter();
                                let Some(constant) =
                                    Self::get_constant(reader, field).with_context(|| {
                                        format!("failed to resolve constant '{cname}'")
                                    })? else { continue; };
                                if let Some(old) = constants.insert(cname.into(), constant) {
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
                        typedefs
                            .entry(name)
                            .or_insert(Vec::new())
                            .extend(Self::resolve_type(
                                reader,
                                reader.type_def_underlying_type(def),
                            )?);
                        continue;
                    }

                    let Some(record) = Self::get_record(reader, def, layouts, None)
                        .with_context(|| format!("failed to resolve record '{name}'"))? else { continue; };

                    records.entry(name).or_insert(Vec::new()).push(record);
                }
                TypeKind::Enum => {
                    let enm = Self::get_enum(reader, def)
                        .with_context(|| format!("failed to resolve enum '{name}'"))?;
                    if let Some(old) = enums.insert(name, enm) {
                        anyhow::bail!("an enum named '{name}' already existed: {old:?}");
                    }
                }
                // Delegates are .NET speak for function pointer signatures
                TypeKind::Delegate => {
                    let Some(fptr) = Self::get_func_ptr(reader, def)
                        .with_context(|| format!("failed to resolve function pointer '{name}'"))? else { continue; };

                    function_pointers
                        .entry(name)
                        .or_insert(Vec::new())
                        .push(fptr);
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
            typedefs,
        })
    }

    fn get_func(reader: &Reader<'_>, def: reader::MethodDef) -> anyhow::Result<Option<Func>> {
        let name = reader.method_def_name(def);
        let sig = reader.method_def_signature(def, &[]);

        let ret = if let Some(return_type) = sig.return_type {
            let Some(ret) = Self::resolve_type(reader, return_type)? else {
                tracing::debug!("skipping due to return type");
                return Ok(None)
            };
            Some(ret)
        } else if reader.method_def_does_not_return(sig.def) {
            // See eg ExitProcess
            Some(QualType::Builtin(Builtin::Never))
        } else {
            None
        };

        let impl_map = reader
            .method_def_impl_map(def)
            .context("ImplMap not found")?;

        // Determine whether the calling convention is "system" or "C"
        let is_system = {
            let inv_attrs = reader.impl_map_flags(impl_map);

            if inv_attrs.contains(PInvokeAttributes::CONV_PLATFORM) {
                true
            } else if inv_attrs.contains(PInvokeAttributes::CONV_CDECL) {
                false
            } else {
                anyhow::bail!("function has invalid invoke attributes {:08x}", inv_attrs.0);
            }
        };

        let module = {
            let scope = reader.impl_map_scope(impl_map);
            // lowercase the name, we want to emit library names that will work
            // on case sensitive file systems (though the windows sdk has screwed up
            // names for many libraries with mixed case...)
            let dname = reader.module_ref_name(scope).to_lowercase();
            dname.strip_suffix(".dll").unwrap_or(&dname).into()
        };

        let mut params = Vec::new();

        for param in sig.params.into_iter() {
            let pname = reader.param_name(param.def).into();
            let Some(kind) = Self::resolve_type(reader, param.ty)? else {
                tracing::debug!("skipping parameter '{pname}'");
                return Ok(None);
            };

            params.push(Item { name: pname, kind });
        }

        Ok(Some(Func {
            params,
            ret,
            module: Some(module),
            is_system,
        }))
    }

    fn get_func_ptr(reader: &Reader<'_>, def: reader::TypeDef) -> anyhow::Result<Option<Func>> {
        let method = reader.type_def_invoke_method(def);

        let sig = reader.method_def_signature(method, &[]);

        let ret = if let Some(return_type) = sig.return_type {
            let Some(ret) = Self::resolve_type(reader, return_type)? else {
                tracing::debug!("skipping due to return type");
                return Ok(None);
            };
            Some(ret)
        } else if reader.method_def_does_not_return(sig.def) {
            Some(QualType::Builtin(Builtin::Never))
        } else {
            None
        };

        let mut params = Vec::new();

        for param in sig.params.into_iter() {
            let pname = reader.param_name(param.def).into();
            let Some(kind) = Self::resolve_type(reader, param.ty)? else {
                tracing::debug!("skipping due to parameter '{pname}'");
                return Ok(None);
            };

            params.push(Item { name: pname, kind });
        }

        Ok(Some(Func {
            params,
            ret,
            module: None,
            is_system: true,
        }))
    }

    fn get_record(
        reader: &Reader<'_>,
        def: reader::TypeDef,
        clang_layouts: &UstrMap<Layouts>,
        parent: Option<reader::TypeDef>,
    ) -> anyhow::Result<Option<Record>> {
        // Check if this is actually only used as an opaque pointer
        if reader.type_def_fields(def).next().is_none() {
            tracing::debug!("found opaque struct");
            return Ok(Some(Record {
                fields: vec![Item {
                    name: "_unused".into(),
                    kind: QualType::Array {
                        element: Box::new(QualType::Builtin(Builtin::UChar)),
                        len: 0,
                    },
                }],
                layouts: Layouts::default(),
                attrs: RecAttrs::empty(),
                nested: Vec::new(),
            }));
        }

        let fields = {
            let mut fields = Vec::new();
            for field in reader.type_def_fields(def) {
                if reader.field_flags(field).contains(FieldAttributes::LITERAL) {
                    continue;
                }

                let fname = reader.field_name(field).into();
                let ty = reader.field_type(field, Some(def));

                let Some(kind) = Self::resolve_type(reader, ty)? else {
                    tracing::debug!("skipping due to field {fname}");
                    return Ok(None);
                };

                fields.push(Item { name: fname, kind });
            }
            fields
        };

        let flags = reader.type_def_flags(def);

        let mut attrs = RecAttrs::empty();

        if flags.contains(TypeAttributes::EXPLICIT_LAYOUT) {
            attrs.insert(RecAttrs::UNION);
        }

        for attr in reader.type_def_attributes(def) {
            match reader.attribute_name(attr) {
                "SupportedArchitectureAttribute" => {
                    if let Some((_, reader::Value::Enum(_, reader::Integer::I32(value)))) =
                        reader.attribute_args(attr).get(0)
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

        let name = reader.type_def_name(def).into();

        // The windows metadata is missing vital layout information
        // 1. Alignment isn't collected at all https://github.com/microsoft/win32metadata/issues/1044
        // 2. While packing information is collected there are some that are missing! :p
        let clang_layout = if let Some(parent) = parent {
            // Alignment doesn't propagate to nested types, and AFAICT there are
            // no explicit alignments done for nested types
            let pname = reader.type_def_name(parent).into();
            clang_layouts
                .get(&pname)
                .filter(|l| l.0.iter().all(|al| matches!(al.l, Layout::Packed(_))))
        } else {
            clang_layouts.get(&name)
        };

        let layouts = if let Some(cl) = reader.type_def_class_layout(def) {
            let md_packing = reader.class_layout_packing_size(cl) as u8;

            // We only gather types via clang that are reachable from windows.h, so
            // we just accept what the metadata says in the cases we don't collect for now
            if let Some(layouts) = clang_layout {
                for layout in layouts.iter() {
                    match layout.l {
                        Layout::Align(a) => {
                            anyhow::bail!("windows metadata for {name} has a packing of {md_packing}, but we detected it was aligned by {a} via clang");
                        }
                        Layout::Packed(cp) => {
                            anyhow::ensure!(md_packing == cp, "windows metadata for {name} has packing of {md_packing} but we detected a packing of {cp} via clang");
                        }
                    }
                }

                layouts.clone()
            } else {
                vec![ArchLayout {
                    a: attrs.intersection(RecAttrs::ARCH).bits(),
                    l: Layout::Packed(md_packing),
                }]
                .into()
            }
        } else {
            clang_layout.cloned().unwrap_or_default()
        };

        let nested = {
            let mut nested = Vec::new();
            for (i, td) in reader.nested_types(def).enumerate() {
                let Some(nest) = Self::get_record(reader, td, clang_layouts, parent.or(Some(def)))? else {
                    tracing::debug!("skipping due to nested record {i}");
                    return Ok(None);
                };

                nested.push(nest);
            }
            nested
        };

        Ok(Some(Record {
            fields,
            layouts,
            attrs,
            nested,
        }))
    }

    fn get_enum(reader: &Reader<'_>, def: reader::TypeDef) -> anyhow::Result<Enum> {
        //let type_name = reader.type_def_type_name(def);
        let repr = reader.type_def_underlying_type(def).try_into()?;

        let variants = reader
            .type_def_fields(def)
            .filter_map(|field| {
                if reader.field_flags(field).contains(FieldAttributes::LITERAL) {
                    let name = reader.field_name(field).into();
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

    fn get_constant(reader: &Reader<'_>, def: reader::Field) -> anyhow::Result<Option<Constant>> {
        let name = reader.field_name(def);

        let Some(constant) = reader.field_constant(def) else { return Ok(None) };
        let kind = reader.constant_type(constant);
        let needs_conversion = kind != reader.field_type(def, None).to_const();

        let mut is_wide_str = false;

        let kind = if kind == reader::Type::String {
            if reader.field_is_ansi(def) {
                QualType::Builtin(Builtin::Pcstr)
            } else {
                is_wide_str = true;
                QualType::Builtin(Builtin::Pcwstr)
            }
        } else if let Some(kind) = Self::resolve_type(reader, kind)? {
            kind
        } else {
            return Ok(None);
        };

        let mut value: Value = reader.constant_value(constant).into();
        value.is_wide_str = is_wide_str;

        Ok(Some(Constant {
            value,
            needs_conversion,
            kind,
        }))

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
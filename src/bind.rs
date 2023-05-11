mod emit;
mod gather;

pub use gather::{Disambiguate, Impls, Item};

use anyhow::Context as _;
use rayon::prelude::*;
use windows_metadata::reader as wmr;

pub type InterfaceMap = std::collections::BTreeMap<String, BTreeSet<String>>;

/// All of the unique items gathered based on a root list of names
#[derive(Clone)]
pub struct ItemSet {
    /// Set of structs, unions, interfaces, classes, enums, type aliases, and function pointers
    pub types: BTreeMap<wmr::Type, Impls>,
    /// Set of functions
    pub functions: BTreeSet<wmr::MethodDef>,
    /// Set of constants, including individual enum values
    pub constants: BTreeSet<wmr::Field>,
}

fn qualify_item(reader: &wmr::Reader, bname: String) -> anyhow::Result<(String, Item)> {
    let (mut item, bname) = Item::parse(bname)?;

    if bname.starts_with("Windows.") {
        Ok((bname, item))
    } else {
        for ns in reader.namespaces() {
            // We could cheat and take casing into account, but there
            // are of course exceptions, and it's fast enough to just
            // brute force it

            // We _do_ consider which items are most likely to be bound,
            // namely functions -> constants/enums -> records
            let mut find = || {
                if matches!(item.dis, Disambiguate::Any | Disambiguate::Function)
                    && reader
                        .namespace_functions(ns)
                        .any(|f| reader.method_def_name(f) == bname)
                {
                    item.dis = Disambiguate::Function;
                    return true;
                }

                if matches!(item.dis, Disambiguate::Any | Disambiguate::Constant)
                    && reader
                        .namespace_constants(ns)
                        .any(|c| reader.field_name(c) == bname)
                {
                    item.dis = Disambiguate::Constant;
                    return true;
                }

                reader.namespace_types(ns, &Default::default()).any(|td| {
                    if reader.type_def_kind(td) == wmr::TypeKind::Enum {
                        if reader
                            .type_def_fields(td)
                            .any(|f| reader.field_name(f) == bname)
                        {
                            item.dis = Disambiguate::Constant;
                            return true;
                        }
                    } else if reader.type_def_name(td) == bname {
                        return true;
                    }

                    false
                })
            };

            if find() {
                let qualified = format!("{ns}.{bname}");
                tracing::debug!("resolved {bname} => {qualified}");
                return Ok((qualified, item));
            }
        }

        anyhow::bail!("unable to resolve '{bname}'");
    }
}

pub fn qualify_items(
    reader: &wmr::Reader,
    items: Vec<String>,
) -> anyhow::Result<Vec<(String, Item)>> {
    // Allow the user to specify simple or fully qualified names
    items
        .into_par_iter()
        .map(|bname| qualify_item(reader, bname))
        .collect()
}

use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
};
pub struct Bucket<T> {
    pub items: BTreeSet<T>,
    pub num: u32,
}

impl<T: std::cmp::Ord> Bucket<T> {
    #[inline]
    fn new(items: BTreeSet<T>) -> Self {
        Self { items, num: 0 }
    }

    #[inline]
    fn has(&self, item: &T) -> bool {
        self.items.contains(item)
    }

    #[inline]
    fn inc(&mut self) {
        self.num += 1;
    }
}

pub struct Items {
    /// Type aliases, not super expensive, so worth differentiating from other types
    pub aliases: Bucket<wmr::Type>,
    /// Functions
    pub functions: Bucket<wmr::MethodDef>,
    /// Constants, both global and enum variants
    pub constants: Bucket<wmr::Field>,
    /// Structs and unions
    pub records: Bucket<wmr::TypeDef>,
    /// Function pointers, rare
    pub func_pointers: Bucket<wmr::TypeDef>,
    /// COM interfaces, only present when emitting COM
    pub interfaces: Bucket<wmr::TypeDef>,
}

/// Determines the style used to emit extern functions from Win32
#[derive(Copy, Clone, PartialEq, Eq, Default, serde::Deserialize)]
pub enum LinkingStyle {
    /// Emits normal `#[link(name = "<dll>")]` extern blocks collated by dynamic
    /// library.
    ///
    /// There are exactly 5 non-dll dynamic libraries in the Win32 bindings, this
    /// style won't work with them, and you will get a panic about it
    Normal,
    /// Emits 1 `windows_targets::link!` per extern function
    #[default]
    WindowsTargets,
    /// Emits extern blocks similarly to [`Self::Normal`], except each one is
    /// `kind = "raw-dylib"`.
    ///
    /// This allows linking without needing import libs, but requires 1.65 for
    /// `x86_64` and `aarch64`, and is currently unstable on `x86`, though will
    /// be stable in (presumably) 1.71 <https://github.com/rust-lang/rust/pull/109677>
    RawDylib,
}

impl fmt::Display for LinkingStyle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Normal => f.write_str("normal"),
            Self::WindowsTargets => f.write_str("targets"),
            Self::RawDylib => f.write_str("rawdylib"),
        }
    }
}

/// Determines the style used to emit enums
#[derive(Copy, Clone, PartialEq, Eq, Default, serde::Deserialize)]
pub enum EnumStyle {
    /// Emits bindgen style enums
    ///
    /// ```
    /// pub type ENUM_NAME = u32;
    /// pub const ENUM_VALUE_ONE: ENUM_NAME = 1;
    /// pub const ENUM_VALUE_TWO: ENUM_NAME = 2;
    ///
    /// fn function_that_takes_enum(enum_val: ENUM_NAME) {}
    ///
    /// fn main() {
    ///     function_that_takes_enum(ENUM_VALUE_ONE);
    /// }
    /// ```
    #[default]
    Bindgen,
    /// Emits minwin style enums
    ///
    /// ```
    /// pub mod ENUM_NAME {
    ///     pub type Enum = u32;
    ///     pub const ENUM_VALUE_ONE: Enum = 1;
    ///     pub const ENUM_VALUE_TWO: Enum = 2;
    /// }
    ///
    /// fn function_that_takes_enum(enum_val: ENUM_NAME::Enum) {}
    ///
    /// fn main() {
    ///     function_that_takes_enum(ENUM_NAME::ENUM_VALUE_ONE);
    /// }
    /// ```
    Minwin,
}

impl fmt::Display for EnumStyle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bindgen => f.write_str("bindgen"),
            Self::Minwin => f.write_str("minwin"),
        }
    }
}

#[derive(serde::Deserialize, Copy, Clone)]
#[serde(rename_all = "kebab-case")]
pub struct MinwinBindConfig {
    /// The linking style to use for extern functions
    pub linking_style: LinkingStyle,
    /// The style of enums emitted, affects both declaration and usage
    pub enum_style: EnumStyle,
    /// If true, the `windows-core` crate is used for various core types
    /// such as `HRESULT`
    pub use_core: bool,
    /// If true, identifiers are fixed to remove pointless Hungarian notation
    pub fix_naming: bool,
    /// If true, the casing of all items will be changed to follow Rust casing conventions
    pub use_rust_casing: bool,
    /// If true, formats the output
    pub pretty_print: bool,
    /// If true, emits a version header at the beginning of the bindings
    pub add_version_header: bool,
}

impl Default for MinwinBindConfig {
    fn default() -> Self {
        Self {
            linking_style: Default::default(),
            enum_style: Default::default(),
            use_core: false,
            fix_naming: false,
            use_rust_casing: false,
            pretty_print: true,
            // We don't want this in tests
            add_version_header: !cfg!(debug_assertions),
        }
    }
}

#[derive(serde::Deserialize)]
pub enum BindConfig {
    /// Uses `windows-bindgen` to generate bindings.
    ///
    /// This option gives limited configuration and no way to inspect the number
    /// and types of items emitted. Requires `rustfmt` is installed.
    ///
    /// Use this if you want the "official" bindings.
    Bindgen,
    /// Uses `minwin` to generate bindings.
    ///
    /// This option allows for various customizations to be applied to the
    /// emitted bindings, as well as complete introspection on all of the emitted
    /// items
    Minwin(MinwinBindConfig),
}

impl fmt::Display for BindConfig {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bindgen => f.write_str("bindgen"),
            Self::Minwin(mwbc) => {
                f.write_fmt(format_args!(
                    "ls-{}_es-{}",
                    mwbc.linking_style, mwbc.enum_style
                ))?;

                if mwbc.use_core {
                    f.write_str("_core")?;
                }

                if mwbc.fix_naming {
                    f.write_str("_fixnaming")?;
                }

                if mwbc.use_rust_casing {
                    f.write_str("_rustcasing")?;
                }

                Ok(())
            }
        }
    }
}

pub struct BindOutput {
    /// The full set of generated bindings
    pub bindings: String,
    /// The full set of items that were emitted. Not available if [`BindConfig::Bindgen`] is used.
    pub items: Option<BTreeMap<String, Items>>,
}

pub fn bind<S: Into<String>>(
    names: impl IntoIterator<Item = S>,
    interfaces: BTreeMap<String, BTreeSet<String>>,
    config: BindConfig,
) -> anyhow::Result<BindOutput> {
    let names: Vec<_> = names.into_iter().map(|n| n.into()).collect();
    anyhow::ensure!(
        !names.is_empty() || !interfaces.is_empty(),
        "1 or more items must be specified to bind"
    );

    let files = &wmr::File::with_default(&[]).unwrap();
    let reader = &wmr::Reader::new(files);

    let items = qualify_items(reader, names)?;
    let ifaces = interfaces
        .into_par_iter()
        .map(|(name, methods)| qualify_item(reader, name).map(|(name, _)| (name, methods)))
        .collect::<anyhow::Result<BTreeMap<_, _>>>()?;

    anyhow::ensure!(
        !items.is_empty() || !ifaces.is_empty(),
        "unable to locate any items to bind"
    );

    match config {
        BindConfig::Bindgen => {
            let items: Vec<_> = items
                .iter()
                .map(|(n, _i)| n.as_str())
                .chain(ifaces.keys().map(|k| k.as_str()))
                .collect();
            Ok(BindOutput {
                bindings: windows_bindgen::standalone(&items),
                items: None,
            })
        }
        BindConfig::Minwin(config) => {
            let gatherer =
                gather::Gatherer::new(reader, items.iter().map(|(n, i)| (n.as_str(), *i)), &ifaces);
            let items = gatherer.gather();

            let emit = emit::Emit {
                items,
                reader,
                ifaces,
                layouts: Some(emit::load_clang_layouts()),
                config,
            };

            do_minwin(emit)
        }
    }
}

pub fn do_minwin(emit: emit::Emit<'_>) -> anyhow::Result<BindOutput> {
    // Gather stats now that we have all of the items we want to emit
    let mut ns_items = BTreeMap::<String, Items>::new();
    let reader = emit.reader;

    let index_namespace = |name: &str, ns_items: &mut BTreeMap<String, Items>| {
        assert!(
            reader.namespaces().any(|ns| ns == name),
            "namespace {name} was not found"
        );

        let functions = reader.namespace_functions(name).collect();
        let mut constants: BTreeSet<_> = reader.namespace_constants(name).collect();

        let mut aliases = BTreeSet::new();
        let mut records = BTreeSet::new();
        let mut interfaces = BTreeSet::new();
        let mut func_pointers = BTreeSet::new();

        for ty in reader.namespace_types(name, &Default::default()) {
            let kind = reader.type_def_kind(ty);

            match kind {
                wmr::TypeKind::Struct => {
                    if reader.type_def_is_handle(ty) {
                        aliases.insert(wmr::Type::TypeDef((ty, Vec::new())));
                    } else {
                        records.insert(ty);
                    }
                }
                wmr::TypeKind::Enum => {
                    aliases.insert(wmr::Type::TypeDef((ty, Vec::new())));
                    constants.extend(reader.type_def_fields(ty));
                }
                wmr::TypeKind::Delegate => {
                    func_pointers.insert(ty);
                }
                wmr::TypeKind::Class | wmr::TypeKind::Interface => {
                    let ty_name = reader.type_def_name(ty);
                    let qname = format!("{name}.{ty_name}");
                    if emit.ifaces.contains_key(&qname) {
                        interfaces.insert(ty);
                    } else {
                        aliases.insert(wmr::Type::TypeDef((ty, Vec::new())));
                    }
                }
            }
        }

        let items = Items {
            functions: Bucket::new(functions),
            constants: Bucket::new(constants),
            records: Bucket::new(records),
            aliases: Bucket::new(aliases),
            func_pointers: Bucket::new(func_pointers),
            interfaces: Bucket::new(interfaces),
        };

        ns_items.insert(name.to_owned(), items);
    };

    for func in emit.items.functions.iter().cloned() {
        let items = match ns_items
            .iter_mut()
            .find(|(_ns, items)| items.functions.has(&func))
        {
            Some(nsi) => nsi.1,
            None => {
                let ns = reader
                    .namespaces()
                    .find(|ns| reader.namespace_functions(ns).any(|m| m == func))
                    .with_context(|| {
                        format!(
                            "unable to find namespace containing function {}",
                            reader.method_def_name(func)
                        )
                    })?;

                index_namespace(ns, &mut ns_items);
                ns_items.get_mut(ns).unwrap()
            }
        };

        items.functions.inc();
    }

    for constant in emit.items.constants.iter().cloned() {
        let items = match ns_items
            .iter_mut()
            .find(|(_ns, items)| items.constants.has(&constant))
        {
            Some(nsi) => nsi.1,
            None => {
                let ns = if let wmr::Type::TypeDef((td, _)) = reader.field_type(constant, None) {
                    reader.type_def_namespace(td)
                } else {
                    reader
                        .namespaces()
                        .find(|ns| reader.namespace_constants(ns).any(|c| c == constant))
                        .with_context(|| {
                            format!(
                                "unable to find namespace containing constant {}",
                                reader.field_name(constant)
                            )
                        })?
                };

                index_namespace(ns, &mut ns_items);
                ns_items.get_mut(ns).unwrap()
            }
        };

        items.constants.inc();
    }

    for ty in emit.items.types.keys() {
        // Just ignore the core types, they're not a big deal
        let wmr::Type::TypeDef((td, _)) = &ty else { continue; };
        let td = *td;

        let ns = reader.type_def_namespace(td);
        if !ns_items.contains_key(ns) {
            index_namespace(ns, &mut ns_items);
        }

        let items = ns_items.get_mut(ns).with_context(|| {
            format!(
                "failed to index namespace '{ns}' to locate typedef '{}'",
                reader.type_def_name(td)
            )
        })?;

        match reader.type_def_kind(td) {
            wmr::TypeKind::Enum => items.aliases.inc(),
            wmr::TypeKind::Struct => items.records.inc(),
            wmr::TypeKind::Class | wmr::TypeKind::Interface => {
                let ty_name = reader.type_def_name(td);
                let qname = format!("{ns}.{ty_name}");
                if emit.ifaces.contains_key(&qname) {
                    items.interfaces.inc();
                } else {
                    items.aliases.inc();
                }
            }
            wmr::TypeKind::Delegate => items.func_pointers.inc(),
        }
    }

    let bindings = emit.emit()?;

    Ok(BindOutput {
        bindings,
        items: Some(ns_items),
    })
}

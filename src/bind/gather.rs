use super::{InterfaceMap, ItemSet};
use std::collections::{BTreeMap, BTreeSet};
use windows_metadata::reader::{self, Reader, Type, TypeDef, TypeKind};

/// Takes a list of fully qualified type names and recursively gathers all of
/// the items needed to fully define them.
pub struct Gatherer<'names, 'r> {
    reader: &'r Reader<'r>,
    interfaces: &'names InterfaceMap,
    items: BTreeMap<&'names str, Item>,
}

impl<'names, 'r> Gatherer<'names, 'r> {
    pub fn new(
        reader: &'r Reader<'r>,
        items: impl Iterator<Item = (&'names str, Item)>,
        interfaces: &'names InterfaceMap,
    ) -> Self {
        Self {
            reader,
            interfaces,
            items: items
                .chain(
                    interfaces
                        .keys()
                        .map(|iname| (iname.as_str(), Default::default())),
                )
                .collect(),
        }
    }

    pub fn gather(self) -> ItemSet {
        let mut types = BTreeMap::new();
        let mut functions = BTreeSet::new();
        let mut constants = BTreeSet::new();

        let reader = self.reader;

        for (name, item) in &self.items {
            let type_name = reader::TypeName::parse(name);

            let mut found = false;

            if matches!(item.dis, Disambiguate::Any | Disambiguate::Record) {
                for def in reader.get(type_name) {
                    self.collect(&Type::TypeDef((def, vec![])), &mut types);
                    found = true;
                }
            }

            if found {
                continue;
            }

            if matches!(item.dis, Disambiguate::Any | Disambiguate::Function) {
                for method in reader
                    .namespace_functions(type_name.namespace)
                    .filter(|method| reader.method_def_name(*method) == type_name.name)
                {
                    functions.insert(method);
                    let signature = reader.method_def_signature(method, &[]);
                    signature
                        .return_type
                        .iter()
                        .for_each(|ty| self.collect(ty, &mut types));
                    signature
                        .params
                        .iter()
                        .for_each(|param| self.collect(&param.ty, &mut types));
                    found = true;
                }
            }

            if found || !matches!(item.dis, Disambiguate::Any | Disambiguate::Constant) {
                continue;
            }

            let field_type = |row: reader::Field| -> Type {
                let ft = reader.field_type(row, None);

                // Normally, this would be an HSTRING, but not for constants
                if ft == Type::String {
                    return if reader.field_is_ansi(row) {
                        Type::PCSTR
                    } else {
                        Type::PCWSTR
                    };
                }

                ft
            };

            if let Some(field) = reader
                .namespace_constants(type_name.namespace)
                .find(|field| reader.field_name(*field) == type_name.name)
            {
                constants.insert(field);
                let ty = field_type(field).to_const_type();
                self.collect(&ty, &mut types);
            }

            if let Some(field) = reader
                .namespace_types(type_name.namespace, &Default::default())
                .find_map(|def| {
                    if reader.type_def_kind(def) == TypeKind::Enum {
                        return reader
                            .type_def_fields(def)
                            .find(|field| reader.field_name(*field) == type_name.name);
                    }
                    None
                })
            {
                constants.insert(field);
                self.collect(&reader.field_type(field, None), &mut types);
            }
        }

        self.fixup_impls(&mut types);

        ItemSet {
            types,
            functions,
            constants,
        }
    }

    fn collect(&self, ty: &Type, collected: &mut BTreeMap<Type, Impls>) {
        // Get the underlying type, the pointers are irrelevant
        let ty = match ty {
            Type::MutPtr((ty, _))
            | Type::ConstPtr((ty, _))
            | Type::Win32Array((ty, _))
            | Type::WinrtArray(ty)
            | Type::WinrtArrayRef(ty)
            | Type::WinrtConstRef(ty) => &ty,
            _ => ty,
        };

        if collected.contains_key(ty) {
            return;
        }

        collected.insert(ty.clone(), Impls::default());

        let Type::TypeDef((def, generics)) = ty else { return; };
        let def = *def;
        let reader = self.reader;

        // Ensure that we get _all_ the types that match
        let type_name = reader.type_def_type_name(def);
        if !type_name.namespace.is_empty() {
            for row in reader.get(type_name) {
                if def != row {
                    self.collect(&Type::TypeDef((row, Vec::new())), collected);
                }
            }
        }

        for generic in generics {
            self.collect(&generic, collected);
        }

        for field in reader.type_def_fields(def) {
            let ty = reader.field_type(field, Some(def));
            self.collect(&ty, collected);
        }

        let kind = reader.type_def_kind(def);
        let ty_name = reader.type_def_name(def);

        let methods = if kind == TypeKind::Interface {
            let Some(im) = self.interfaces.get(ty_name) else { return; };
            Some(im)
        } else {
            None
        };

        for method in reader.type_def_methods(def) {
            // Skip delegate pseudo-constructors.
            let name = reader.method_def_name(method);
            if name == ".ctor" {
                continue;
            }

            if let Some(m) = &methods {
                if !m.contains(name) {
                    tracing::debug!("skipping {ty_name}::{name}");
                    continue;
                }
            }

            let sig = reader.method_def_signature(method, generics);
            if let Some(rt) = &sig.return_type {
                self.collect(rt, collected);
            }
            for param in &sig.params {
                self.collect(&param.ty, collected);
            }
        }

        for interface in reader.type_interfaces(&ty) {
            self.collect(&interface.ty, collected);
        }

        if kind == TypeKind::Struct
            && reader.type_def_fields(def).next().is_none()
            && reader.type_def_guid(def).is_some()
        {
            collected.insert(Type::GUID, Impls::empty());
        }

        self.collect_nested(def, collected);
    }

    fn collect_nested(&self, td: TypeDef, collected: &mut BTreeMap<Type, Impls>) {
        let reader = self.reader;
        for nested in reader.nested_types(td) {
            self.collect_nested(nested, collected);

            for field in reader.type_def_fields(nested) {
                let ty = reader.field_type(field, Some(nested));
                self.collect(&ty, collected);
            }
        }
    }

    /// Recursively adds implementations based upon the requested implementations
    /// for root items.
    ///
    /// We _could_ do this while collecting types, but it's kind of annoying and
    /// easier to just do it in a separate fixup pass
    fn fixup_impls(&self, collected: &mut BTreeMap<Type, Impls>) {
        for (name, impls) in self
            .items
            .iter()
            .filter_map(|(name, item)| (!item.ri.is_empty()).then_some((*name, item.ri)))
        {
            let type_name = reader::TypeName::parse(name);

            for def in self.reader.get(type_name) {
                self.fill_impls(Type::TypeDef((def, Vec::new())), impls, collected);
            }
        }
    }

    fn fill_impls(&self, ty: Type, impls: Impls, collected: &mut BTreeMap<Type, Impls>) {
        let ty = match ty {
            // We can skip fields that are behind a pointer
            Type::MutPtr(_) | Type::ConstPtr(_) => return,
            Type::Win32Array((ty, _))
            | Type::WinrtArray(ty)
            | Type::WinrtArrayRef(ty)
            | Type::WinrtConstRef(ty) => *ty,
            _ => ty,
        };

        let Type::TypeDef((def, _generics)) = &ty else { return; };
        let def = *def;
        let reader = self.reader;

        if let Some(imp) = collected.get_mut(&ty) {
            if imp.contains(impls) {
                // If all of the flags are already present we don't need to recurse the type
                return;
            }

            imp.insert(impls);
        }

        for field in reader.type_def_fields(def) {
            let ty = reader.field_type(field, Some(def));
            if let Type::TypeDef((fdef, _)) = &ty {
                if reader.type_def_namespace(*fdef).is_empty() {
                    continue;
                }
            }

            self.fill_impls(ty, impls, collected);
        }

        self.fill_impls_nested(def, impls, collected);
    }

    fn fill_impls_nested(&self, td: TypeDef, impls: Impls, collected: &mut BTreeMap<Type, Impls>) {
        let reader = self.reader;
        for nested in reader.nested_types(td) {
            self.fill_impls_nested(nested, impls, collected);
            self.fill_impls(Type::TypeDef((nested, Vec::new())), impls, collected);
        }
    }
}

/// Disambiguates items during gathering.
///
/// In [some](https://docs.rs/windows-sys/0.48.0/windows_sys/Win32/UI/Input/KeyboardAndMouse/struct.VK_F.html)
/// [cases](https://docs.rs/windows-sys/0.48.0/windows_sys/Win32/UI/Input/KeyboardAndMouse/constant.VK_F.html)
/// items with the exact same name can be located within the same namespace,
/// which can cause the incorrect item to be emitted and the desired item to be
/// skipped.
#[derive(Copy, Clone, Debug, Default, PartialEq, Eq)]
pub enum Disambiguate {
    /// The item is a constant or enum variant
    Constant,
    /// The item is a function
    Function,
    /// The item is a struct or union
    Record,
    #[default]
    Any,
}

bitflags::bitflags! {
    /// Decides additional implementations that are derived for the struct
    ///
    /// `windows-bindgen` unconditionally emits `Copy` and `Clone` for all records,
    /// but this increases compile times for:
    ///
    /// * No benefit because they are only ever moved
    /// * Worse, extremely large types are accidentally copied when normally the
    /// user would be given an error that it's been moved when doing trivial
    /// operations like accessing a single field etc.
    #[derive(Debug, Copy, Clone, Default)]
    pub struct Impls: u8 {
        const COPY = 1 << 0 | Impls::CLONE.bits();
        const CLONE = 1 << 1;
    }
}

#[derive(Default, Copy, Clone, Debug)]
pub struct Item {
    pub dis: Disambiguate,
    pub ri: Impls,
}

impl Item {
    pub(super) fn parse(bname: String) -> anyhow::Result<(Self, String)> {
        match (bname.find('-'), bname.find('+')) {
            (None, None) => Ok((Self::default(), bname)),
            (Some(_), Some(_)) => {
                anyhow::bail!("'{bname}' has both a disambiguation and record implementation, you can just specify the impl if you want a record");
            }
            (Some(i), None) => {
                let dis = match &bname[..i] {
                    "constant" => Disambiguate::Constant,
                    "function" => Disambiguate::Function,
                    "record" => Disambiguate::Record,
                    unknown => {
                        anyhow::bail!("unknown disambiguation prefix '{unknown}' for {bname}");
                    }
                };

                let stripped = bname[i + 1..].to_owned();

                Ok((
                    Self {
                        dis,
                        ..Default::default()
                    },
                    stripped,
                ))
            }
            (None, Some(i)) => {
                let mut ri = Impls::empty();

                for imp in bname[i + 1..].split('+') {
                    match imp {
                        "Copy" => ri.insert(Impls::COPY),
                        "Clone" => ri.insert(Impls::CLONE),
                        unknown => {
                            anyhow::bail!("unknown record impl '{unknown}' for {bname}");
                        }
                    };
                }

                let stripped = bname[..i].to_owned();

                Ok((
                    Self {
                        ri,
                        ..Default::default()
                    },
                    stripped,
                ))
            }
        }
    }
}

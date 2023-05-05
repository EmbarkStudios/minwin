use super::{InterfaceMap, ItemSet};
use std::collections::{BTreeMap, BTreeSet};
use windows_metadata::reader::{self, Reader, Type, TypeDef, TypeKind};

pub fn collect(
    reader: &Reader,
    interfaces: &InterfaceMap,
    ty: &Type,
    mut ri: Impls,
    set: &mut BTreeMap<Type, Impls>,
) {
    if let Type::TypeDef((def, _)) = ty {
        if !matches!(reader.type_def_kind(*def), TypeKind::Struct) {
            ri = Impls::empty();
        }
    } else {
        ri = Impls::empty();
    }

    // Get the underlying type, the pointers are irrelevant
    let ty = match ty {
        Type::MutPtr((ty, _)) => &ty,
        Type::ConstPtr((ty, _)) => &ty,
        Type::Win32Array((ty, _)) => &ty,
        Type::WinrtArray(ty) => &ty,
        Type::WinrtArrayRef(ty) => &ty,
        Type::WinrtConstRef(ty) => &ty,
        _ => ty,
    };

    if set.contains_key(ty) {
        return;
    }

    set.insert(ty.clone(), ri);

    let Type::TypeDef((def, generics)) = ty else { return; };
    let def = *def;

    for generic in generics {
        collect(reader, interfaces, &generic, ri, set);
    }

    for field in reader.type_def_fields(def) {
        let ty = reader.field_type(field, Some(def));
        if let Type::TypeDef((fdef, _)) = &ty {
            if reader.type_def_namespace(*fdef).is_empty() {
                continue;
            }
        }
        collect(reader, interfaces, &ty, ri, set);
    }

    let kind = reader.type_def_kind(def);
    let ty_name = reader.type_def_name(def);

    let methods = if kind == TypeKind::Interface {
        let Some(im) = interfaces.get(ty_name) else { return; };
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
            collect(reader, interfaces, rt, ri, set);
        }
        for param in &sig.params {
            collect(reader, interfaces, &param.ty, ri, set);
        }
    }

    for interface in reader.type_interfaces(&ty) {
        collect(reader, interfaces, &interface.ty, ri, set);
    }

    if kind == TypeKind::Struct
        && reader.type_def_fields(def).next().is_none()
        && reader.type_def_guid(def).is_some()
    {
        set.insert(Type::GUID, Impls::empty());
    }

    collect_nested(reader, interfaces, def, ri, set);
}

fn collect_nested(
    reader: &Reader,
    interfaces: &InterfaceMap,
    td: TypeDef,
    ri: Impls,
    set: &mut BTreeMap<Type, Impls>,
) {
    for nested in reader.nested_types(td) {
        collect_nested(reader, interfaces, nested, ri, set);

        for field in reader.type_def_fields(nested) {
            let ty = reader.field_type(field, Some(nested));
            if let Type::TypeDef((def, _)) = &ty {
                // Skip the fields that actually refer to the anonymous nested
                // type, otherwise it will get added to the typeset and emitted
                if reader.type_def_namespace(*def).is_empty() {
                    continue;
                }

                collect(reader, interfaces, &ty, ri, set);
            }
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

#[derive(Default, Copy, Clone)]
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
                    let ri = match imp {
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

/// Takes a list of fully qualified type names and recursively gathers all of
/// the items needed to fully define them.
pub fn gather_items<'names>(
    reader: &Reader,
    interfaces: &'names InterfaceMap,
    names: impl Iterator<Item = (&'names str, Item)>,
) -> ItemSet {
    let mut types = BTreeMap::new();
    let mut functions = BTreeSet::new();
    let mut constants = BTreeSet::new();

    for (name, item) in names.chain(
        interfaces
            .keys()
            .map(|iname| (iname.as_str(), Default::default())),
    ) {
        let type_name = reader::TypeName::parse(name);

        let mut found = false;

        if matches!(item.dis, Disambiguate::Any | Disambiguate::Record) {
            for def in reader.get(type_name) {
                collect(
                    reader,
                    interfaces,
                    &Type::TypeDef((def, vec![])),
                    item.ri,
                    &mut types,
                );
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
                    .for_each(|ty| collect(reader, interfaces, ty, Impls::empty(), &mut types));
                signature.params.iter().for_each(|param| {
                    collect(reader, interfaces, &param.ty, Impls::empty(), &mut types)
                });
                found = true;
            }
        }

        if found || !matches!(item.dis, Disambiguate::Any | Disambiguate::Constant) {
            continue;
        }

        if let Some(field) = reader
            .namespace_constants(type_name.namespace)
            .find(|field| reader.field_name(*field) == type_name.name)
        {
            constants.insert(field);
            collect(
                reader,
                interfaces,
                &reader.field_type(field, None),
                Impls::empty(),
                &mut types,
            );
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
            collect(
                reader,
                interfaces,
                &reader.field_type(field, None),
                Impls::empty(),
                &mut types,
            );
        }
    }

    ItemSet {
        types,
        functions,
        constants,
    }
}

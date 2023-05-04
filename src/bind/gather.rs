use super::{InterfaceMap, ItemSet};
use std::collections::BTreeSet;
use windows_metadata::reader::{self, Reader, Type, TypeDef, TypeKind};

pub fn collect(reader: &Reader, interfaces: &InterfaceMap, ty: &Type, set: &mut BTreeSet<Type>) {
    let ty = ty.to_underlying_type();
    if !set.insert(ty.clone()) {
        return;
    }

    let Type::TypeDef((def, generics)) = &ty else { return; };
    let def = *def;

    for generic in generics {
        collect(reader, interfaces, generic, set);
    }

    for field in reader.type_def_fields(def) {
        let ty = reader.field_type(field, Some(def));
        if let Type::TypeDef((fdef, _)) = &ty {
            if reader.type_def_namespace(*fdef).is_empty() {
                continue;
            }
        }
        collect(reader, interfaces, &ty, set);
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
            collect(reader, interfaces, rt, set);
        }
        for param in &sig.params {
            collect(reader, interfaces, &param.ty, set);
        }
    }

    for interface in reader.type_interfaces(&ty) {
        collect(reader, interfaces, &interface.ty, set);
    }

    if kind == TypeKind::Struct
        && reader.type_def_fields(def).next().is_none()
        && reader.type_def_guid(def).is_some()
    {
        set.insert(Type::GUID);
    }

    collect_nested(reader, interfaces, def, set);
}

fn collect_nested(
    reader: &Reader,
    interfaces: &InterfaceMap,
    td: TypeDef,
    set: &mut BTreeSet<Type>,
) {
    for nested in reader.nested_types(td) {
        collect_nested(reader, interfaces, nested, set);

        for field in reader.type_def_fields(nested) {
            let ty = reader.field_type(field, Some(nested));
            if let Type::TypeDef((def, _)) = &ty {
                // Skip the fields that actually refer to the anonymous nested
                // type, otherwise it will get added to the typeset and emitted
                if reader.type_def_namespace(*def).is_empty() {
                    continue;
                }

                collect(reader, interfaces, &ty, set);
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
#[derive(Copy, Clone, Debug)]
pub enum Disambiguate {
    /// The item is a constant or enum variant
    Constant,
    /// The item is a function
    Function,
    /// The item is a struct or union
    Record,
}

/// Takes a list of fully qualified type names and recursively gathers all of
/// the items needed to fully define them.
pub fn gather_items<'names>(
    reader: &Reader,
    interfaces: &InterfaceMap,
    names: impl Iterator<Item = (&'names str, Option<Disambiguate>)>,
) -> ItemSet {
    let mut types = BTreeSet::new();
    let mut functions = BTreeSet::new();
    let mut constants = BTreeSet::new();

    for (name, dis) in names.chain(interfaces.keys().map(|iname| (iname.as_str(), None))) {
        let type_name = reader::TypeName::parse(name);

        // We can't simply use `if let` here to find the types and functions as there may be multiple definitions
        // to cover multi-arch support.
        let mut found = false;

        if matches!(dis, None | Some(Disambiguate::Record)) {
            for def in reader.get(type_name) {
                collect(
                    reader,
                    interfaces,
                    &Type::TypeDef((def, vec![])),
                    &mut types,
                );
                found = true;
            }
        }

        if found {
            continue;
        }

        if matches!(dis, None | Some(Disambiguate::Function)) {
            for method in reader
                .namespace_functions(type_name.namespace)
                .filter(|method| reader.method_def_name(*method) == type_name.name)
            {
                functions.insert(method);
                let signature = reader.method_def_signature(method, &[]);
                signature
                    .return_type
                    .iter()
                    .for_each(|ty| collect(reader, interfaces, ty, &mut types));
                signature
                    .params
                    .iter()
                    .for_each(|param| collect(reader, interfaces, &param.ty, &mut types));
                found = true;
            }
        }

        if found || !matches!(dis, None | Some(Disambiguate::Constant)) {
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

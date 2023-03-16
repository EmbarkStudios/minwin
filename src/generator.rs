use crate::{
    parser::{BindItem, BindItemKind},
    resolver::*,
    BindingFile, Resolver,
};
use anyhow::Context as _;
use proc_macro2::{self as pm, TokenStream};
use quote::{format_ident, quote, TokenStreamExt};
use ustr::{Ustr, UstrSet};

#[derive(Debug)]
enum Item<'res> {
    Function(&'res Func),
    Record(&'res Record),
    FunctionPointer(&'res Func),
    Constant(&'res Constant),
    Enum(&'res Enum),
    Typedef(QualType),
    NotFound,
}

#[derive(Debug)]
struct ItemDef<'res> {
    item: Item<'res>,
    ident: Ustr,
    namespace: Option<Ustr>,
    dependencies: Vec<ItemDef<'res>>,
}

impl<'res> ItemDef<'res> {
    fn emit(&self, ts: &mut TokenStream) -> anyhow::Result<()> {
        let ident = format_ident!("{}", self.ident.as_str());

        match &self.item {
            Item::Function(func) => {
                let cc = if func.is_system { "system" } else { "C" };
                let lib = func
                    .module
                    .as_ref()
                    .with_context(|| format!("function '{ident}' did not state its library"))?
                    .as_str();

                let params = func.params.iter().map(|p| {
                    let pname = format_ident!("{}", p.name.as_str());
                    let mut q = quote! { #pname: };
                    p.kind.emit(&mut q);
                    q
                });

                ts.extend(quote! {
                    #[link(name = #lib)]
                    extern #cc {
                        pub fn #ident(#(#params),*)
                    }
                });

                let ret = if let Some(rt) = &func.ret {
                    ts.extend(quote! { -> });
                    rt.emit(ts);
                };

                ts.append(pm::TokenTree::Punct(pm::Punct::new(
                    ';',
                    pm::Spacing::Alone,
                )));
            }
            Item::FunctionPointer(func) => {
                let params = func.params.iter().map(|p| {
                    let pname = format_ident!("{}", p.name.as_str());
                    let mut q = quote! { #pname: };
                    p.kind.emit(&mut q);
                    q
                });

                ts.extend(quote! {
                    pub type #ident = Option<unsafe extern "system" fn(#(#params),*)
                });

                let ret = if let Some(rt) = &func.ret {
                    ts.extend(quote! { -> });
                    rt.emit(ts);
                };

                ts.extend(quote! {>;});
            }
            Item::Record(rec) => {
                fn emit_rec(rec: &Record, name: pm::Ident, ts: &mut TokenStream) {
                    for (i, nested) in rec.nested.iter().enumerate() {
                        emit_rec(nested, format_ident!("{name}_{i}"), ts);
                    }

                    let fields = rec.fields.iter().map(|f| {
                        let fname = format_ident!("{}", f.name.as_str());
                        let mut q = quote! { #fname: };
                        f.kind.emit(&mut q);
                        q.append(pm::Punct::new(',', pm::Spacing::Joint));
                        q
                    });

                    // let repr = if let Some(layout) = rec.layout {
                    //     match layout {
                    //         Layout::Align(align) => {
                    //             let align = pm::Literal::u8_unsuffixed(align);
                    //             quote! {#[repr(C, align(#align))]}
                    //         }
                    //         Layout::Packed(pack) => {
                    //             let pack = pm::Literal::u8_unsuffixed(pack);
                    //             quote! {#[repr(C, packed(#pack))]}
                    //         }
                    //     }
                    // } else {
                    let repr = quote! {#[repr(C)]};
                    //};

                    let rec_kind = if rec.attrs.contains(RecAttrs::UNION) {
                        pm::Ident::new("union", pm::Span::call_site())
                    } else {
                        pm::Ident::new("struct", pm::Span::call_site())
                    };

                    ts.extend(quote! {
                        #repr
                        pub #rec_kind #name {
                            #(#fields)*
                        }
                    });
                }

                emit_rec(rec, ident, ts);
            }
            Item::Constant(cnst) => {
                ts.extend(quote! {
                    const #ident:
                });

                cnst.kind.emit(ts);

                ts.append(pm::Punct::new('=', pm::Spacing::Alone));

                cnst.value.emit(ts);

                if cnst.needs_conversion {
                    ts.extend(quote! { as _;});
                } else {
                    ts.append(pm::Punct::new(';', pm::Spacing::Alone));
                }
            }
            Item::Typedef(td) => {
                ts.extend(quote! {
                    pub type #ident =
                });

                td.emit(ts);
                ts.append(pm::Punct::new(';', pm::Spacing::Joint));
            }
            Item::Enum(nm) => {
                let repr = format_ident!("{}", nm.repr.as_repr()?);

                let variants = nm.variants.iter().map(|v| {
                    let vname = format_ident!("{}", v.name.as_str());
                    let mut q = quote! { #vname = };
                    v.value.emit(&mut q);
                    q.append(pm::Punct::new(',', pm::Spacing::Joint));
                    q
                });

                ts.extend(quote! {
                    #[repr(#repr)]
                    pub enum #ident {
                        #(#variants)*
                    }
                });
            }
            Item::NotFound => unreachable!("uhh not found?"),
        }

        Ok(())
    }
}

impl crate::resolver::Builtin {
    fn emit(&self, ts: &mut TokenStream) {
        let q = match self {
            // We support bool even though it's literally only used in the Windows.System.JS namespace :p
            Self::Bool => quote! {bool},
            Self::Bstr => quote! {Bstr},
            Self::Char => quote! {std::ffi::c_char},
            Self::Double => quote! {f64},
            Self::Float => quote! {f32},
            Self::Guid => quote! {Guid},
            Self::Hresult => quote! {Hresult},
            Self::ISize => quote! {isize},
            Self::Int => quote! {i32},
            Self::Long => quote! {i64},
            Self::Never => quote! {!},
            Self::Pcstr => quote! {*const std::ffi::c_char},
            Self::Pcwstr => quote! {*const u16},
            Self::Pstr => quote! {*mut std::ffi::c_char},
            Self::Pwstr => quote! {*mut u16},
            Self::Short => quote! {i16},
            Self::UChar => quote! {u8},
            Self::UInt => quote! {u32},
            Self::ULong => quote! {u64},
            Self::UShort => quote! {u16},
            Self::USize => quote! {usize},
            Self::Void => quote! {std::ffi::c_void},
        };

        ts.extend(q);
    }
}

impl crate::resolver::Value {
    fn emit(&self, ts: &mut TokenStream) {
        use pm::Literal;
        use windows_metadata::reader::Value;

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
                if self.is_wide_str {
                    let lits = s.encode_utf16().map(|u| Literal::u16_unsuffixed(u));

                    let com = format!("// {s}");

                    ts.extend(quote! {
                        #com
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
            Value::TypeDef(_td) => unreachable!("windows_metadata parse this, at least, it didn't"),
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

impl crate::resolver::QualType {
    fn emit(&self, ts: &mut TokenStream) {
        match self {
            Self::Builtin(bi) => bi.emit(ts),
            Self::Record { name }
            | Self::Enum { name }
            | Self::FunctionPointer { name }
            | Self::Typedef { name } => {
                ts.append(pm::Ident::new(name.as_str(), pm::Span::call_site()));
            }
            Self::Pointer { is_const, pointee } => {
                ts.extend(if *is_const {
                    quote! {*const}
                } else {
                    quote! {*mut}
                });

                pointee.emit(ts);
            }
            Self::Array { element, len } => {
                ts.append(pm::Punct::new('[', pm::Spacing::Joint));

                element.emit(ts);

                ts.append(pm::Punct::new(';', pm::Spacing::Alone));
                ts.append(pm::TokenTree::Literal(pm::Literal::u32_unsuffixed(*len)));
                ts.append(pm::Punct::new(']', pm::Spacing::Joint));
            }
        }
    }
}

#[inline]
fn get_namespace<'res>(res: &'res Resolver, name: &Ustr) -> &'res TreeItems {
    res.namespaces
        .iter()
        .find_map(|(ns, items)| (ns == name).then_some(items))
        .unwrap()
}

fn def_for_type<'res>(res: &'res Resolver, qt: &QualType, ns: Option<&Ustr>) -> Vec<ItemDef<'res>> {
    let found = match qt {
        QualType::Record { name } => get_record(res, name, ns),
        QualType::Typedef { name } => get_typedef(res, name, ns),
        QualType::FunctionPointer { name } => get_func_ptr(res, name, ns),
        QualType::Array { element, .. } => def_for_type(res, element, ns),
        QualType::Enum { name } => get_enum(res, name, ns),
        QualType::Pointer { pointee, .. } => def_for_type(res, pointee, ns),
        QualType::Builtin(bi) => match bi {
            Builtin::Guid => {
                let mut items = Vec::new();
                items.append(&mut get_record(res, &"GUID".into(), None));
                items.push(ItemDef {
                    item: Item::Typedef(QualType::Record {
                        name: "GUID".into(),
                    }),
                    ident: "Guid".into(),
                    dependencies: Vec::new(),
                    namespace: None,
                });
                items
            }
            Builtin::Hresult => {
                vec![ItemDef {
                    item: Item::Typedef(QualType::Builtin(Builtin::Int)),
                    ident: "Hresult".into(),
                    dependencies: Vec::new(),
                    namespace: None,
                }]
            }
            Builtin::Bstr => {
                vec![ItemDef {
                    item: Item::Typedef(QualType::Pointer {
                        is_const: true,
                        pointee: Box::new(QualType::Builtin(Builtin::UShort)),
                    }),
                    ident: "Bstr".into(),
                    dependencies: Vec::new(),
                    namespace: None,
                }]
            }
            _ => return Vec::new(),
        },
    };

    // If we couldn't locate an item in the same namespace as it was requested,
    // try again but searching _all_ namespaces
    if found.is_empty() && ns.is_some() {
        def_for_type(res, qt, None)
    } else {
        found
    }
}

#[inline]
fn get_func_deps<'res>(res: &'res Resolver, func: &'res Func, ns: &Ustr) -> Vec<ItemDef<'res>> {
    let mut deps = Vec::new();
    if let Some(ret) = &func.ret {
        deps.extend(def_for_type(res, ret, Some(ns)));
    }

    deps.extend(
        func.params
            .iter()
            .flat_map(|p| def_for_type(res, &p.kind, Some(ns))),
    );
    deps
}

#[inline]
fn get_rec_deps<'res>(res: &'res Resolver, rec: &'res Record, ns: &Ustr) -> Vec<ItemDef<'res>> {
    rec.fields
        .iter()
        .flat_map(|f| def_for_type(res, &f.kind, Some(ns)))
        .collect()
}

fn get_func_ptr<'res>(res: &'res Resolver, name: &Ustr, ns: Option<&Ustr>) -> Vec<ItemDef<'res>> {
    if let Some(ns) = ns {
        let items = get_namespace(res, &ns);

        if let Some(funcs) = items.function_pointers.get(name) {
            funcs
                .iter()
                .map(|f| {
                    let dependencies = get_func_deps(res, f, ns);

                    ItemDef {
                        item: Item::FunctionPointer(f),
                        ident: *name,
                        namespace: Some(*ns),
                        dependencies,
                    }
                })
                .collect()
        } else {
            Vec::new()
        }
    } else {
        let mut items = Vec::new();
        for (ns, _) in &res.namespaces {
            let mut fi = get_func_ptr(res, name, Some(ns));
            items.append(&mut fi);
        }
        items
    }
}

fn get_function<'res>(res: &'res Resolver, name: &Ustr, ns: Option<&Ustr>) -> Vec<ItemDef<'res>> {
    if let Some(ns) = ns {
        let items = get_namespace(res, &ns);

        if let Some(funcs) = items.functions.get(name) {
            funcs
                .iter()
                .map(|f| {
                    let dependencies = get_func_deps(res, f, ns);

                    ItemDef {
                        item: Item::Function(f),
                        ident: *name,
                        namespace: Some(*ns),
                        dependencies,
                    }
                })
                .collect()
        } else {
            Vec::new()
        }
    } else {
        let mut items = Vec::new();
        for (ns, _) in &res.namespaces {
            let mut fi = get_function(res, name, Some(ns));
            items.append(&mut fi);
        }
        items
    }
}

fn get_record<'res>(res: &'res Resolver, name: &Ustr, ns: Option<&Ustr>) -> Vec<ItemDef<'res>> {
    if let Some(ns) = ns {
        let items = get_namespace(res, &ns);

        if let Some(recs) = items.records.get(name) {
            recs.iter()
                .map(|rec| {
                    let dependencies = get_rec_deps(res, rec, ns);

                    ItemDef {
                        item: Item::Record(rec),
                        ident: *name,
                        namespace: Some(*ns),
                        dependencies,
                    }
                })
                .collect()
        } else {
            Vec::new()
        }
    } else {
        let mut items = Vec::new();
        for (ns, _) in &res.namespaces {
            let mut fi = get_record(res, name, Some(ns));
            items.append(&mut fi);
        }
        items
    }
}

fn get_constant<'res>(res: &'res Resolver, name: &Ustr, ns: Option<&Ustr>) -> Vec<ItemDef<'res>> {
    if let Some(ns) = ns {
        let items = get_namespace(res, &ns);

        if let Some(cnst) = items.constants.get(name) {
            vec![ItemDef {
                item: Item::Constant(cnst),
                ident: *name,
                namespace: Some(*ns),
                dependencies: def_for_type(res, &cnst.kind, Some(ns))
                    .into_iter()
                    .collect(),
            }]
        } else {
            Vec::new()
        }
    } else {
        let mut items = Vec::new();
        for (ns, _) in &res.namespaces {
            let mut fi = get_constant(res, name, Some(ns));
            items.append(&mut fi);
        }
        items
    }
}

fn get_typedef<'res>(res: &'res Resolver, name: &Ustr, ns: Option<&Ustr>) -> Vec<ItemDef<'res>> {
    if let Some(ns) = ns {
        let items = get_namespace(res, &ns);

        if let Some(tds) = items.typedefs.get(name) {
            tds.iter()
                .map(|f| ItemDef {
                    item: Item::Typedef(f.clone()),
                    ident: *name,
                    namespace: Some(*ns),
                    dependencies: def_for_type(res, f, Some(ns)),
                })
                .collect()
        } else {
            Vec::new()
        }
    } else {
        let mut items = Vec::new();
        for (ns, _) in &res.namespaces {
            let mut fi = get_typedef(res, name, Some(ns));
            items.append(&mut fi);
        }
        items
    }
}

fn get_enum<'res>(res: &'res Resolver, name: &Ustr, ns: Option<&Ustr>) -> Vec<ItemDef<'res>> {
    if let Some(ns) = ns {
        let items = get_namespace(res, &ns);

        if let Some(enm) = items.enums.get(name) {
            vec![ItemDef {
                item: Item::Enum(enm),
                ident: *name,
                namespace: Some(*ns),
                dependencies: Vec::new(),
            }]
        } else {
            Vec::new()
        }
    } else {
        let mut items = Vec::new();
        for (ns, _) in &res.namespaces {
            let mut fi = get_enum(res, name, Some(ns));
            items.append(&mut fi);
        }
        items
    }
}

fn locate_items<'m, 'res>(
    res: &'res Resolver,
    modi: &'m syn::ItemMod,
) -> Vec<(BindItem<'m>, Vec<ItemDef<'res>>)> {
    BindingFile::iter_module(modi).map_or(Vec::new(), |i| {
        i.map(|bi| {
            let mut items = Vec::new();
            match bi.kind {
                BindItemKind::Function => {
                    let name = bi.ident.to_string().into();
                    let mut fi = get_function(res, &name, None);
                    items.append(&mut fi);
                }
                BindItemKind::FunctionPtr => {
                    let name = bi.ident.to_string().into();
                    let mut fi = get_func_ptr(res, &name, None);
                    items.append(&mut fi);

                    // In some cases, the user may want to create function pointers for
                    // actual concrete functions for use with eg. GetProcAddress
                    if items.is_empty() {
                        let mut fi = get_function(res, &name, None);

                        for f in &mut fi {
                            if let Item::Function(func) = f.item {
                                f.item = Item::FunctionPointer(func);
                            }
                        }

                        items.append(&mut fi);
                    }
                }
                BindItemKind::Struct | BindItemKind::Union => {}
                BindItemKind::Constant => {
                    let name = bi.ident.to_string().into();
                    let mut fi = get_constant(res, &name, None);
                    items.append(&mut fi);
                }
            }

            (bi, items)
        })
        .collect()
    })
}

fn emit_item(def: &ItemDef<'_>, ts: &mut TokenStream, emitted: &mut UstrSet) -> anyhow::Result<()> {
    // if !emitted.insert(def.ident) {
    //     return Ok(());
    // }

    for dep in &def.dependencies {
        emit_item(dep, ts, emitted)?;
    }

    def.emit(ts)?;
    Ok(())
}

pub fn generate(res: &Resolver, modi: &syn::ItemMod) -> anyhow::Result<TokenStream> {
    let items = locate_items(res, modi);

    let mut ts = TokenStream::new();
    let mut emitted = UstrSet::default();

    for (bi, defs) in items {
        for def in defs {
            emit_item(&def, &mut ts, &mut emitted)?;
        }
    }

    Ok(ts)
}

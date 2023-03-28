use crate::{
    parser::{BindItem, BindItemKind},
    resolver::*,
    BindingFile, Resolver,
};
use anyhow::Context as _;
use proc_macro2::{self as pm, TokenStream};
use quote::{format_ident, quote, ToTokens, TokenStreamExt};
use ustr::{Ustr, UstrMap, UstrSet};

impl ToTokens for Attrs {
    fn to_tokens(&self, ts: &mut TokenStream) {
        let arch = self.intersection(Attrs::ARCH);

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

struct OutputStream {
    root: TokenStream,
    libs: Vec<(Ustr, bool, TokenStream)>,
    arches: Vec<(Attrs, Ustr, TokenStream)>,
}

impl OutputStream {
    fn new() -> Self {
        Self {
            root: TokenStream::new(),
            libs: Vec::new(),
            arches: Vec::new(),
        }
    }

    fn get_arch_block(&mut self, attrs: Attrs) -> &mut TokenStream {
        let arches = attrs & Attrs::ARCH;

        if arches.is_empty() {
            &mut self.root
        } else if let Some(ab) = self
            .arches
            .iter()
            .position(|(attrs, _, _ts)| arches.bits() == attrs.bits())
        {
            &mut self.arches[ab].2
        } else {
            let mut mod_name = String::new();

            for (i, arch) in arches
                .iter()
                .map(|a| {
                    if a.contains(Attrs::X86) {
                        "x86"
                    } else if a.contains(Attrs::X86_64) {
                        "x86_64"
                    } else if a.contains(Attrs::AARCH64) {
                        "aarch64"
                    } else {
                        unreachable!()
                    }
                })
                .enumerate()
            {
                if i > 0 {
                    mod_name.push('_');
                }

                mod_name.push_str(arch);
            }

            self.arches
                .push((arches, mod_name.into(), TokenStream::new()));
            &mut self.arches.last_mut().unwrap().2
        }
    }

    fn get_extern_block(&mut self, name: &Ustr, is_system: bool) -> &mut TokenStream {
        let eb = if let Some(eb) = self
            .libs
            .iter()
            .position(|(lname, is, _ts)| name == lname && is_system == *is)
        {
            eb
        } else {
            self.libs.push((*name, is_system, TokenStream::new()));
            self.libs.len() - 1
        };

        &mut self.libs[eb].2
    }

    fn finalize(self) -> TokenStream {
        let mut root = self.root;

        for (attrs, mn, ts) in self.arches {
            let mn = format_ident!("{mn}");
            root.extend(quote! {
                #[cfg(#attrs)]
                mod #mn {
                    use super::*;

                    #ts
                }

                #[cfg(#attrs)]
                use #mn::*;
            });
        }

        for (lib, is_system, ts) in self.libs {
            let lib = lib.as_str();
            let cc = if is_system { "system" } else { "C" };

            root.extend(quote! {
                #[link(name = #lib)]
                extern #cc {
                    #ts
                }
            });
        }

        root
    }
}

#[derive(Debug)]
enum Item<'res> {
    Function(&'res Func),
    FunctionPointer((&'res Func, bool)),
    Record(&'res Record),
    Constant(&'res Constant),
    Enum(&'res Enum),
    Typedef(QualType),
}

impl<'res> Item<'res> {
    fn id(&self) -> (BindItemKind, Attrs) {
        let (bik, attrs) = match self {
            Self::Function(func) => (BindItemKind::Function, func.attrs),
            Self::FunctionPointer((func, ..)) => (BindItemKind::FunctionPtr, func.attrs),
            Self::Record(rec) => (
                if rec.attrs.contains(Attrs::UNION) {
                    BindItemKind::Union
                } else {
                    BindItemKind::Struct
                },
                rec.attrs,
            ),
            Self::Constant(..) => (BindItemKind::Constant, Attrs::empty()),
            Self::Enum(..) => (BindItemKind::Enum, Attrs::empty()),
            Self::Typedef(..) => (BindItemKind::Typedef, Attrs::empty()),
        };

        (bik, attrs.intersection(Attrs::ARCH))
    }
}

#[derive(Debug)]
struct ItemDef<'res> {
    item: Item<'res>,
    ident: Ustr,
    namespace: Option<Ustr>,
    dependencies: Vec<ItemDef<'res>>,
}

impl<'res> ItemDef<'res> {
    fn emit(&self, os: &mut OutputStream) -> anyhow::Result<()> {
        let ident = format_ident!("{}", self.ident.as_str());

        match &self.item {
            Item::Function(func) => {
                let lib = func
                    .module
                    .as_ref()
                    .with_context(|| format!("function '{ident}' did not state its library"))?;

                let params = func.params.iter().map(|p| {
                    let pname = format_ident!("{}", p.name.as_str());
                    let pkind = &p.kind;
                    quote! { #pname: #pkind }
                });

                let ts = os.get_extern_block(lib, func.is_system);

                if func.attrs.intersects(Attrs::ARCH) {
                    let attrs = func.attrs;
                    ts.extend(quote! {
                        #[cfg(#attrs)]
                    });
                }

                let ret = if let Some(rt) = &func.ret {
                    quote! { -> #rt }
                } else {
                    TokenStream::new()
                };

                ts.extend(quote! {
                    pub fn #ident(#(#params),*)#ret;
                });
            }
            Item::FunctionPointer((func, normal)) => {
                let params = func.params.iter().map(|p| {
                    let pname = format_ident!("{}", p.name.as_str());
                    let pkind = &p.kind;
                    quote! { #pname: #pkind }
                });

                let ts = &mut os.root;

                if func.attrs.intersects(Attrs::ARCH) {
                    let attrs = func.attrs;
                    ts.extend(quote! {
                        #[cfg(#attrs)]
                    });
                }

                let ret = if let Some(rt) = &func.ret {
                    quote! { -> #rt }
                } else {
                    TokenStream::new()
                };

                let ty = if *normal {
                    quote! {
                        pub type #ident = Option<unsafe extern "system" fn(#(#params),*)#ret>;
                    }
                } else {
                    quote! {
                        pub type #ident = unsafe extern "system" fn(#(#params),*)#ret;
                    }
                };

                ts.extend(ty);
            }
            Item::Record(rec) => {
                fn emit_rec(rec: &Record, name: pm::Ident, os: &mut OutputStream) {
                    for (i, nested) in rec.nested.iter().enumerate() {
                        emit_rec(nested, format_ident!("{name}_{i}"), os);
                    }

                    let is_union = rec.attrs.contains(Attrs::UNION);

                    let fields = rec.fields.iter().map(|f| {
                        let fname = format_ident!("{}", f.name.as_str());
                        let fkind = &f.kind;

                        if is_union && matches!(fkind, QualType::Record { .. }) {
                            quote! { pub #fname: std::mem::ManuallyDrop<#fkind>, }
                        } else {
                            quote! { pub #fname: #fkind, }
                        }
                    });

                    let ts = os.get_arch_block(rec.attrs);

                    let repr = &rec.layout;

                    let rec_kind = if is_union {
                        format_ident!("union")
                    } else {
                        format_ident!("struct")
                    };

                    ts.extend(quote! {
                        #repr
                        pub #rec_kind #name {
                            #(#fields)*
                        }
                    });
                }

                emit_rec(rec, ident, os);
            }
            Item::Constant(cnst) => {
                let ckind = &cnst.kind;
                let cval = &cnst.value;

                let cnst_item = if cnst.needs_conversion {
                    quote! { pub const #ident: #ckind = #cval as _; }
                } else {
                    quote! { pub const #ident: #ckind = #cval; }
                };

                os.root.extend(cnst_item);
            }
            Item::Typedef(td) => {
                os.root.extend(quote! { pub type #ident = #td; });
            }
            Item::Enum(nm) => {
                let repr = format_ident!("{}", nm.repr.as_repr()?);

                os.root.extend(quote! { pub type #ident = #repr; });

                let variants = nm.variants.iter().map(|v| {
                    let vname = format_ident!("{}", v.name.as_str());
                    let val = &v.value;
                    quote! { pub const #vname: #ident = #val; }
                });

                os.root.extend(quote! {
                    #(#variants)*
                });
            }
        }

        Ok(())
    }
}

impl ToTokens for crate::resolver::Builtin {
    fn to_tokens(&self, ts: &mut TokenStream) {
        let q = match self {
            // We support bool even though it's literally only used in the Windows.System.JS namespace :p
            Self::Bool => quote! {bool},
            Self::Bstr => quote! {Bstr},
            Self::Char => quote! {u8},
            Self::Double => quote! {f64},
            Self::Float => quote! {f32},
            Self::Guid => quote! {Guid},
            Self::Hresult => quote! {Hresult},
            Self::ISize => quote! {isize},
            Self::Int => quote! {i32},
            Self::Long => quote! {i64},
            Self::Never => quote! {!},
            Self::Pcstr => quote! {*const u8},
            Self::Pcwstr => quote! {*const u16},
            Self::Pstr => quote! {*mut u8},
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

impl ToTokens for crate::resolver::Value {
    fn to_tokens(&self, ts: &mut TokenStream) {
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
                let s = format!("{s}\0");
                if self.is_wide_str {
                    let lits = s.encode_utf16().map(|u| Literal::u16_unsuffixed(u));

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

impl ToTokens for crate::resolver::QualType {
    fn to_tokens(&self, ts: &mut TokenStream) {
        match self {
            Self::Builtin(bi) => bi.to_tokens(ts),
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

                pointee.to_tokens(ts);
            }
            Self::Array { element, len } => {
                ts.append(pm::Punct::new('[', pm::Spacing::Joint));

                element.to_tokens(ts);

                ts.append(pm::Punct::new(';', pm::Spacing::Alone));
                ts.append(pm::Literal::u32_unsuffixed(*len));
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
        QualType::Record { name } => get_items(res, BindItemKind::Struct, name, ns),
        QualType::Typedef { name } => get_items(res, BindItemKind::Typedef, name, ns),
        QualType::FunctionPointer { name } => get_items(res, BindItemKind::FunctionPtr, name, ns),
        QualType::Array { element, .. } => def_for_type(res, element, ns),
        QualType::Enum { name } => get_items(res, BindItemKind::Enum, name, ns),
        QualType::Pointer { pointee, .. } => def_for_type(res, pointee, ns),
        QualType::Builtin(bi) => match bi {
            Builtin::Guid => {
                let mut items = Vec::new();
                items.append(&mut get_items(
                    res,
                    BindItemKind::Struct,
                    &"GUID".into(),
                    None,
                ));
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

    found
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
fn get_rec_deps<'res>(
    res: &'res Resolver,
    rec: &'res Record,
    ns: &Ustr,
    deps: &mut Vec<ItemDef<'res>>,
) {
    for nested in &rec.nested {
        get_rec_deps(res, nested, ns, deps);
    }

    deps.extend(
        rec.fields
            .iter()
            .filter(|f| {
                // We need to account for self pointers so we don't recurse
                // infinitely and overflow the stack :p
                if let QualType::Record { name } = f.kind.get_inner() {
                    if name == &rec.name {
                        return false;
                    }
                }

                true
            })
            .flat_map(|f| def_for_type(res, &f.kind, Some(ns))),
    )
}

fn get_items<'res>(
    res: &'res Resolver,
    kind: BindItemKind,
    name: &Ustr,
    ns: Option<&Ustr>,
) -> Vec<ItemDef<'res>> {
    fn inner<'res>(
        res: &'res Resolver,
        kind: BindItemKind,
        name: &Ustr,
        ns_name: &Ustr,
        ns: &'res TreeItems,
    ) -> Option<Vec<ItemDef<'res>>> {
        match kind {
            BindItemKind::Function => ns.functions.get(name).map(|funcs| {
                funcs
                    .iter()
                    .map(|f| {
                        let dependencies = get_func_deps(res, f, ns_name);

                        ItemDef {
                            item: Item::Function(f),
                            ident: *name,
                            namespace: Some(*ns_name),
                            dependencies,
                        }
                    })
                    .collect()
            }),
            BindItemKind::FunctionPtr => ns.function_pointers.get(name).map(|funcs| {
                funcs
                    .iter()
                    .map(|f| {
                        let dependencies = get_func_deps(res, f, ns_name);

                        ItemDef {
                            item: Item::FunctionPointer((f, true)),
                            ident: *name,
                            namespace: Some(*ns_name),
                            dependencies,
                        }
                    })
                    .collect()
            }),
            BindItemKind::Struct | BindItemKind::Union => ns.records.get(name).map(|recs| {
                recs.iter()
                    .map(|r| {
                        let mut dependencies = Vec::new();
                        get_rec_deps(res, r, ns_name, &mut dependencies);

                        ItemDef {
                            item: Item::Record(r),
                            ident: *name,
                            namespace: Some(*ns_name),
                            dependencies,
                        }
                    })
                    .collect()
            }),
            BindItemKind::Constant => ns.constants.get(name).map(|cnst| {
                vec![ItemDef {
                    item: Item::Constant(cnst),
                    ident: *name,
                    namespace: Some(*ns_name),
                    dependencies: def_for_type(res, &cnst.kind, Some(ns_name))
                        .into_iter()
                        .collect(),
                }]
            }),
            BindItemKind::Typedef => ns.typedefs.get(name).map(|tds| {
                tds.iter()
                    .map(|f| ItemDef {
                        item: Item::Typedef(f.clone()),
                        ident: *name,
                        namespace: Some(*ns_name),
                        dependencies: def_for_type(res, f, Some(ns_name)),
                    })
                    .collect()
            }),
            BindItemKind::Enum => ns.enums.get(name).map(|enm| {
                vec![ItemDef {
                    item: Item::Enum(enm),
                    ident: *name,
                    namespace: Some(*ns_name),
                    dependencies: Vec::new(),
                }]
            }),
        }
    }

    if let Some(ns) = ns {
        if let Some(located) = inner(res, kind, name, ns, get_namespace(res, ns)) {
            return located;
        }
    }

    let v = res
        .namespaces
        .iter()
        .fold(Vec::new(), |mut v, (ns_name, ns)| {
            let Some(mut id) = inner(res, kind, name, ns_name, ns) else { return v };
            v.append(&mut id);
            v
        });

    v
}

fn locate_items<'m, 'res>(
    res: &'res Resolver,
    modi: &'m syn::ItemMod,
) -> Vec<(BindItem<'m>, Vec<ItemDef<'res>>)> {
    BindingFile::iter_module(modi).map_or(Vec::new(), |i| {
        i.map(|bi| {
            let name = bi.ident.to_string().into();

            let mut items = get_items(res, bi.kind, &name, None);

            // In some cases, the user may want to create function pointers for
            // actual concrete functions for use with eg. GetProcAddress
            if bi.kind == BindItemKind::FunctionPtr && items.is_empty() {
                let mut fi = get_items(res, BindItemKind::Function, &name, None);

                for f in &mut fi {
                    if let Item::Function(func) = f.item {
                        f.item = Item::FunctionPointer((func, false));
                    }
                }

                items.append(&mut fi);
            }

            (bi, items)
        })
        .collect()
    })
}

#[derive(Debug)]
struct Emitted {
    namespace: Option<Ustr>,
    kind: BindItemKind,
    attrs: Attrs,
}

#[inline]
fn insert(emitted: &mut UstrMap<Vec<Emitted>>, item: &ItemDef<'_>) -> bool {
    let (kind, attrs) = item.item.id();

    let Some(pi) = emitted.get_mut(&item.ident) else {
        emitted.insert(item.ident, vec![Emitted {
            namespace: item.namespace,
            kind,
            attrs,
        }]);
        return true;
    };

    if let Some(ind) = pi
        .iter()
        .position(|e| e.namespace == item.namespace && e.kind == kind)
    {
        let existing = &mut pi[ind];
        if attrs.is_empty() && existing.attrs.is_empty() || attrs.intersects(existing.attrs) {
            return false;
        } else {
            existing.attrs |= attrs;
            return true;
        }
    }

    pi.push(Emitted {
        namespace: item.namespace,
        kind,
        attrs,
    });
    true
}

#[inline]
fn emit_item(
    def: &ItemDef<'_>,
    os: &mut OutputStream,
    emitted: &mut UstrMap<Vec<Emitted>>,
) -> anyhow::Result<()> {
    if !insert(emitted, def) {
        return Ok(());
    }

    for dep in &def.dependencies {
        emit_item(dep, os, emitted)?;
    }

    def.emit(os)?;
    Ok(())
}

pub struct NsRatios {
    namespace: Ustr,
    constants: UstrSet,
    enum_values: UstrSet,
    records: UstrSet,
    functions: UstrSet,
    function_ptrs: UstrSet,
}

pub struct Ratios {
    ns: Vec<NsRatios>,
}



pub fn generate(res: &Resolver, modi: &syn::ItemMod, ratios: &mut Ratios) -> anyhow::Result<TokenStream> {
    let items = locate_items(res, modi);

    let mut os = OutputStream::new();

    let mut emitted = UstrMap::default();

    for (_bi, defs) in items {
        for def in defs {
            emit_item(&def, &mut os, &mut emitted)?;
        }
    }

    Ok(os.finalize())
}

use super::*;

impl<'r> super::Emit<'r> {
    pub(super) fn emit_record(
        &self,
        rec: TypeDef,
    ) -> anyhow::Result<Option<(pm::Ident, TokenStream)>> {
        let reader = self.reader;

        let ident = self.to_ident(reader.type_def_name(rec), IdentKind::Record);

        if reader.type_def_fields(rec).next().is_none() {
            if let Some(value) = reader.type_def_guid(rec) {
                let val = self.guid_printer(value);
                let guid_ty = self.type_printer(Type::GUID);

                let ts = quote! {
                    pub const #ident: #guid_ty = #val;
                };

                return Ok(Some((ident, ts)));
            }
        }

        if reader.type_def_is_contract(rec) {
            return Ok(None);
        }

        // Handle types like HWND are treated differently
        if reader.type_def_is_handle(rec) {
            return self.emit_handle(rec);
        }

        fn emit_rec(rec: &Record, name: pm::Ident, convert_case: bool, os: &mut OutputStream) {
            for (i, nested) in rec.nested.iter().enumerate() {
                emit_rec(nested, format_ident!("{name}_{i}"), convert_case, os);
            }

            let is_union = rec.attrs.contains(Attrs::UNION);

            let fields = rec.fields.iter().map(|f| {
                let fname = to_ident(&f.name, convert_case, IdentKind::Field);
                let fkind = &f.kind;
                let fqt = QtPrint::from((fkind, convert_case));

                if is_union && matches!(fkind, QualType::Record { .. }) {
                    quote! { pub #fname: std::mem::ManuallyDrop<#fqt>, }
                } else {
                    quote! { pub #fname: #fqt, }
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

        let mut ts = TokenStream::new();
        self.emit_rec(rec, &ident, &mut ts)?;

        Ok(Some(ident, ts))
    }

    fn emit_rec(
        &self,
        rec: TypeDef,
        ident: &pm::Ident,
        ts: &mut TokenStream,
    ) -> anyhow::Result<()> {
        let reader = self.reader;

        // Check for opaque types that are only used via pointer
        if reader.type_def_fields(rec).count() == 0 {
            tracing::trace!("found opaque struct '{ident}'");
            ts.extend(quote! {
                pub struct #ident(_opaque: [u8; 0]);
            });
            return Ok(());
        }

        const STRUCT: syn::Ident = syn::Ident::new("struct", pm::Span::call_site());
        const UNION: syn::Ident = syn::Ident::new("union", pm::Span::call_site());

        let flags = reader.type_def_flags(rec);

        let rec_kind = if flags.contains(wmr::TypeAttributes::EXPLICIT_LAYOUT) {
            UNION
        } else {
            STRUCT
        };

        let layout = if let Some(layout) = reader.type_def_class_layout(def) {
            RecordLayout::::Packed(reader.class_layout_packing_size(layout) as u8);
        } else {
            // The windows metadata is missing vital layout information
            // 1. Alignment isn't collected at all https://github.com/microsoft/win32metadata/issues/1044
            // 2. While packing information is collected there are some that are missing! <https://github.com/microsoft/win32metadata/issues/1562>
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

            quote! { #[repr(C)] }
        };

        ts.extend(quote! {
            #repr
            pub #rec_kind #ident {
                #(#fields)*
            }
        });

        for (i, nested) in reader.nested_types(rec).enumerate() {
            let nested_ident = quote::format_ident!("{ident}_{i}");
            self.emit_rec(nested, &nested_ident, ts)?;
        }

        Ok(())
    }
}

fn gen_struct_with_name(gen: &Gen, def: TypeDef, struct_name: &str, cfg: &Cfg) -> TokenStream {
    let flags = gen.reader.type_def_flags(def);
    let cfg = cfg.union(&gen.reader.type_def_cfg(def, &[]));

    let repr = if let Some(layout) = gen.reader.type_def_class_layout(def) {
        let packing = Literal::usize_unsuffixed(gen.reader.class_layout_packing_size(layout));
        quote! { #[repr(C, packed(#packing))] }
    } else {
        quote! { #[repr(C)] }
    };

    let fields = gen.reader.type_def_fields(def).map(|f| {
        let name = to_ident(gen.reader.field_name(f));
        let ty = gen.reader.field_type(f, Some(def));

        if gen.reader.field_flags(f).contains(FieldAttributes::LITERAL) {
            quote! {}
        } else if !gen.sys
            && flags.contains(TypeAttributes::EXPLICIT_LAYOUT)
            && !gen.reader.field_is_copyable(f, def)
        {
            let ty = gen.type_default_name(&ty);
            quote! { pub #name: ::std::mem::ManuallyDrop<#ty>, }
        } else if !gen.sys
            && !flags.contains(TypeAttributes::WINRT)
            && !gen.reader.field_is_blittable(f, def)
        {
            if let Type::Win32Array((ty, len)) = ty {
                let ty = gen.type_default_name(&ty);
                quote! { pub #name: [::std::mem::ManuallyDrop<#ty>; #len], }
            } else {
                let ty = gen.type_default_name(&ty);
                quote! { pub #name: ::std::mem::ManuallyDrop<#ty>, }
            }
        } else {
            let ty = gen.type_default_name(&ty);
            quote! { pub #name: #ty, }
        }
    });

    let struct_or_union = if flags.contains(TypeAttributes::EXPLICIT_LAYOUT) {
        quote! { union }
    } else {
        quote! { struct }
    };

    let doc = gen.cfg_doc(&cfg);
    let features = gen.cfg_features(&cfg);

    let mut tokens = quote! {
        #repr
        #doc
        #features
        pub #struct_or_union #name {#(#fields)*}
    };

    tokens.combine(&gen_struct_constants(gen, def, &name, &cfg));
    tokens.combine(&gen_copy_clone(gen, def, &name, &cfg));
    tokens.combine(&gen_debug(gen, def, &name, &cfg));
    tokens.combine(&gen_windows_traits(gen, def, &name, &cfg));
    tokens.combine(&gen_compare_traits(gen, def, &name, &cfg));

    if !gen.sys {
        tokens.combine(&quote! {
            #features
            impl ::core::default::Default for #name {
                fn default() -> Self {
                    unsafe { ::core::mem::zeroed() }
                }
            }
        });
    }

    for (index, nested_type) in gen.reader.nested_types(def).enumerate() {
        let nested_name = format!("{struct_name}_{index}");
        tokens.combine(&gen_struct_with_name(gen, nested_type, &nested_name, &cfg));
    }

    tokens
}

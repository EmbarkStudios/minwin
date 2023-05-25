use super::*;

impl<'r> super::Emit<'r> {
    pub(super) fn emit_record(&self, os: &mut OutputStream, rec: TypeDef) -> anyhow::Result<()> {
        let reader = self.reader;

        let attrs = self.attributes(reader.type_def_attributes(rec));
        let ident = self
            .config
            .make_ident(reader.type_def_name(rec), IdentKind::Record);

        if reader.type_def_fields(rec).count() == 0 {
            if let Some(value) = reader.type_def_guid(rec) {
                let val = self.guid_printer(Some(value));
                let guid_ty = self.type_printer(Type::GUID);

                let ts = quote! {
                    pub const #ident: #guid_ty = #val;
                };

                os.insert_record(rec, ident, attrs, ts);
                return Ok(());
            }
        }

        if reader.type_def_is_contract(rec) {
            return Ok(());
        }

        // Handle types like HWND are not actual records
        if reader.type_def_is_handle(rec) {
            // Note that this is the sys version, we could also do the "std"
            // version which uses *mut c_void for isize and u32/u64 for usize
            let underlying = reader.type_def_underlying_type(rec);
            let tp = self.type_printer(underlying);

            let ts = quote! {
                pub type #ident = #tp;
            };

            os.insert_record(rec, ident, attrs, ts);
            return Ok(());
        }

        let mut ts = TokenStream::new();
        self.emit_rec(rec, &ident, &mut ts)?;
        os.insert_record(rec, ident, attrs, ts);

        Ok(())
    }

    fn emit_rec(
        &self,
        rec: TypeDef,
        ident: &pm::Ident,
        ts: &mut TokenStream,
    ) -> anyhow::Result<()> {
        let reader = self.reader;

        // Check for opaque types that are only used via pointer
        // Note that we don't care about Copy/Clone for these as they will
        // always only be used by pointer
        if reader.type_def_fields(rec).count() == 0 {
            tracing::trace!("found opaque struct '{ident}'");
            ts.extend(quote! {
                pub struct #ident(_opaque: [u8; 0]);
            });
            return Ok(());
        }

        let flags = reader.type_def_flags(rec);

        let is_union = flags.contains(wmr::TypeAttributes::EXPLICIT_LAYOUT);

        let repr = if let Some(layout) = reader.type_def_class_layout(rec) {
            RecordLayout::Agnostic(Layout::Packed(
                reader.class_layout_packing_size(layout) as u8
            ))
        } else if let Some(clang_layout) = self.layouts.and_then(|l| l.get(&ident.to_string())) {
            // The windows metadata is missing vital layout information
            // 1. Alignment isn't collected at all https://github.com/microsoft/win32metadata/issues/1044
            // 2. While packing information is collected, there are some that are missing! <https://github.com/microsoft/win32metadata/issues/1562>
            clang_layout.get_layout(self.attributes(reader.type_def_attributes(rec)))
        } else {
            RecordLayout::None
        };

        let (fields, field_names): (Vec<_>, Vec<_>) = reader
            .type_def_fields(rec)
            .filter_map(|f| {
                if reader
                    .field_flags(f)
                    .contains(wmr::FieldAttributes::LITERAL)
                {
                    return None;
                }

                let fname = self
                    .config
                    .make_ident(reader.field_name(f), IdentKind::Field);
                let ty = reader.field_type(f, Some(rec));

                // Unlike windows-bindgen, we don't unconditionally emit Copy/Clone for
                // every record since it just increases compile times for no benefit
                // in many cases. However, this means we need to to check unions to
                // see if their field type is Copy, as if it is not, we need to emit
                // that the field is manually droppable
                let ts = if is_union && !self.is_copy(&ty) {
                    let tp = self.type_printer(ty);
                    quote! { pub #fname: ::std::mem::ManuallyDrop<#tp> }
                } else {
                    let tp = self.type_printer(ty);
                    quote! { pub #fname: #tp }
                };

                Some((ts, fname))
            })
            .unzip();

        fn rec_kind(is_union: bool) -> &'static syn::Ident {
            use std::sync::Once;

            static INIT: Once = Once::new();
            static mut KINDS: Option<(syn::Ident, syn::Ident)> = None;

            unsafe {
                INIT.call_once(|| {
                    KINDS = Some((
                        quote::format_ident!("struct"),
                        quote::format_ident!("union"),
                    ));
                });
                KINDS
                    .as_ref()
                    .map(|(s, u)| if is_union { u } else { s })
                    .expect("impossible")
            }
        }

        let rec_kind = rec_kind(is_union);

        let impls = self
            .items
            .types
            .get(&Type::TypeDef((rec, Vec::new())))
            .cloned()
            .unwrap();
        let implsp = self.impls(&ident, impls, (!is_union).then_some(&field_names));

        let docs = self.docs_link(reader.type_def_attributes(rec));

        ts.extend(quote! {
            #docs
            #repr
            pub #rec_kind #ident {
                #(#fields),*
            }

            #implsp
        });

        for (i, nested) in reader.nested_types(rec).enumerate() {
            let nested_ident = quote::format_ident!("{ident}_{i}");
            self.emit_rec(nested, &nested_ident, ts)?;
        }

        Ok(())
    }
}

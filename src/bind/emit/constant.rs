use super::*;

impl<'r> super::Emit<'r> {
    fn emit_constant(&self, os: &mut OutputStream, def: Field) -> anyhow::Result<()> {
        let reader = self.reader;
        let name = self.to_ident(reader.field_name(def), IdentKind::Variant(None));
        let ty = reader.field_type(def, None).to_const_type();

        let ts = if let Some(constant) = reader.field_constant(def) {
            let constant_type = reader.constant_type(constant);
            let value = Value {
                val: reader.constant_value(constant),
                is_wide_str: !reader.field_is_ansi(def),
            };

            if ty == constant_type {
                let typename = self.type_printer(constant_type);

                Some(quote! {
                    pub const #name: #typename = #value;
                })
            } else {
                let Type::TypeDef((def, _)) = ty else { anyhow::bail!("constant '{name}' is not a typedef...") };

                let ts = os.get_enum_block(def, self);
                ts.extend(quote! {
                    pub const #name: Enum = #value;
                });
                None
            }
        } else if let Some(value) = reader.field_guid(def) {
            let val = self.guid_printer(value);
            let guid_ty = self.type_printer(Type::GUID);

            Some(quote! {
                pub const #name: #guid_ty = #val;
            })
        } else {
            // There are extremely few of these, concentrated in exactly 2 namespaces,
            // so just emit an error for now, not worth supporting unless it's needed
            anyhow::ensure!(
                !reader
                    .field_attributes(def)
                    .any(|attr| reader.attribute_name(attr) == "ConstantAttribute"),
                "constant '{name}' is a record type and isn't currently supported"
            );
            None
        };

        if let Some(ts) = ts {
            os.insert_constant(def, name, ts);
        }

        Ok(())
    }
}

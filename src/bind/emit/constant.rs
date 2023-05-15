use super::*;

impl<'r> super::Emit<'r> {
    pub(super) fn emit_constant(&self, os: &mut OutputStream, def: Field) -> anyhow::Result<()> {
        let reader = self.reader;
        let name = self
            .config
            .make_ident(reader.field_name(def), IdentKind::Variant(None));
        let ty = reader.field_type(def, None).to_const_type();

        let ts = if let Some(constant) = reader.field_constant(def) {
            let constant_type = reader.constant_type(constant);
            let value = Value {
                val: reader.constant_value(constant),
                is_wide_str: matches!(constant_type, Type::String) && !reader.field_is_ansi(def),
            };

            if ty == constant_type {
                // Type::String normally goes to HSTRING, but this is misleading
                // for constants, since it can be ansi or utf-16, so we need to
                // change it to the appropriate pointer type based on the field's
                // encoding
                let typename = self.type_printer(if matches!(constant_type, Type::String) {
                    if value.is_wide_str {
                        Type::PCWSTR
                    } else {
                        Type::PCSTR
                    }
                } else {
                    constant_type
                });

                // For utf-16 strings, add a doc comment on the comment so the
                // user knows exactly what the string's actual value is
                let string_value = if value.is_wide_str {
                    let wmr::Value::String(s) = &value.val else {
                        anyhow::bail!("constant {name} says it is a wide string...but it's not actually a string at all");
                    };

                    let s = format!(" \"{s}\"");

                    Some(quote! { #[doc = #s] })
                } else {
                    None
                };

                Some(quote! {
                    #string_value
                    pub const #name: #typename = #value;
                })
            } else {
                os.insert_enum_constant(ty, name, value);
                return Ok(());
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
            os.insert_constant(name, ts);
        }

        Ok(())
    }
}

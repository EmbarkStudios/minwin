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
            let val = self.guid_printer(Some(value));
            let guid_ty = self.type_printer(Type::GUID);

            Some(quote! {
                pub const #name: #guid_ty = #val;
            })
        } else {
            let value = reader
                .field_attributes(def)
                .find_map(|attr| if reader.attribute_name(attr) == "ConstantAttribute" {
                    let args = reader.attribute_args(attr);
                    let wmr::Value::String(val) = args.into_iter().next().unwrap().1 else { unimplemented!("these should only ever be strings") };
                    Some(val)
                } else {
                    None
                });

            value.map(|value| {
                let Type::TypeDef((def, _)) = reader.field_type(def, None) else {
                    unimplemented!("these should always be typedefs");
                };

                let mut fields = Vec::new();
                let mut input = value.as_str();
                for field in reader.type_def_fields(def) {
                    let (value, rest) = self.field_initializer(field, input);
                    input = rest;
                    fields.push(value);
                }

                let kind = self.type_printer(Type::TypeDef((def, Vec::new())));
                quote! { pub const #name: #kind = #kind { #(#fields),* }; }
            })
        };

        if let Some(ts) = ts {
            os.insert_constant(name, ts);
        }

        Ok(())
    }

    fn field_initializer<'i>(&self, field: Field, input: &'i str) -> (TokenStream, &'i str) {
        let reader = self.reader;
        let name = self
            .config
            .make_ident(reader.field_name(field), IdentKind::Field);

        match reader.field_type(field, None) {
            Type::GUID => {
                let (literals, rest) = Self::read_literal_array(input, 11);
                let value = self.guid_printer(Some(wmr::GUID::from_string_args(&literals)));
                (quote! { #name: #value }, rest)
            }
            Type::Win32Array((_, len)) => {
                let (literals, rest) = Self::read_literal_array(input, len);
                let literals = literals.into_iter().map(|literal| {
                    literal
                        .parse::<TokenStream>()
                        .expect("failed to parse array literal")
                });
                (quote! { #name: [#(#literals,)*] }, rest)
            }
            _ => {
                let (literal, rest) = Self::read_literal(input);
                let literal: pm::Literal = literal.parse().expect("failed to parse literal");
                (quote! { #name: #literal }, rest)
            }
        }
    }

    fn read_literal(input: &str) -> (&str, &str) {
        let mut start = None;
        let mut end = 0;

        for (pos, c) in input.bytes().enumerate() {
            if start.is_none() {
                if c != b' ' && c != b',' {
                    start = Some(pos);
                }
            } else if c == b' ' || c == b',' || c == b'}' {
                break;
            }
            end += 1;
        }

        let Some(start) = start else {
            unimplemented!();
        };

        (&input[start..end], &input[end..])
    }

    fn read_token(input: &str, token: u8) -> &str {
        for (pos, c) in input.bytes().enumerate() {
            if c == token {
                return &input[pos + 1..];
            } else if c != b' ' && c != b',' {
                break;
            }
        }

        panic!("`{}` expected", token.escape_ascii());
    }

    fn read_literal_array(input: &str, len: usize) -> (Vec<&str>, &str) {
        let mut input = Self::read_token(input, b'{');
        let mut result = vec![];

        for _ in 0..len {
            let (literal, rest) = Self::read_literal(input);
            result.push(literal);
            input = rest;
        }

        (result, Self::read_token(input, b'}'))
    }
}

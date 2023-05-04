use super::*;

impl Emit {
    fn emit_func(&self, os: &mut OutputStream, reader: &Reader, meth: MethodDef) {
    let sig = reader.method_def_signature(meth, &[]);

    if self.link_targets {
        let symbol = if symbol != name {
            format!(" \"{symbol}\"")
        } else {
            String::new()
        };

        let mut tokens = String::new();
        for param in params {
            tokens.push_str(&format!("{}, ", param.as_str()));
        }
        tokens.push_str(&vararg.0);
        let tokens = tokens.trim_end_matches(", ");
        format!(
            r#"::windows_targets::link!("{library}" "{abi}"{symbol} fn {name}({tokens}){return_type});"#
        )
        .into()
    } else {
        
    }
    
    tokens.combine(&gen_link(gen, &signature, &cfg));
    tokens

    let lib = func
                    .module
                    .as_ref()
                    .with_context(|| format!("function '{ident}' did not state its library"))?;

                let ts = os.get_extern_block(lib, func.is_system);

                if func.attrs.intersects(Attrs::ARCH) {
                    let attrs = func.attrs;
                    ts.extend(quote! {
                        #[cfg(#attrs)]
                    });
                }

                let params = get_params(func, convert_case);

                let ret = if let Some(rt) = &func.ret {
                    let rt = QtPrint::from((rt, convert_case));
                    quote! { -> #rt }
                } else {
                    TokenStream::new()
                };

                ts.extend(quote! {
                    pub fn #ident(#(#params),*)#ret;
                });
}

fn gen_link(gen: &Gen, signature: &Signature, cfg: &Cfg) -> TokenStream {
    let name = gen.reader.method_def_name(signature.def);
    let ident = to_ident(name);
    let library = gen.reader.method_def_module_name(signature.def);
    let abi = gen.reader.method_def_extern_abi(signature.def);

    let symbol = if let Some(impl_map) = gen.reader.method_def_impl_map(signature.def) {
        gen.reader.impl_map_import_name(impl_map)
    } else {
        name
    };

    let link_name = if symbol != name {
        quote! { #[link_name = #symbol] }
    } else {
        quote! {}
    };

    let params = signature.params.iter().map(|p| {
        let name = gen.param_name(p.def);
        let tokens = if p.kind == SignatureParamKind::ValueType {
            gen.type_default_name(&p.ty)
        } else {
            gen.type_abi_name(&p.ty)
        };
        quote! { #name: #tokens }
    });

    let return_type = gen.return_sig(signature);

    let vararg = if gen.sys && signature.vararg {
        "...".into()
    } else {
        quote! {}
    };

    if gen.std || !gen.namespace.starts_with("Windows.") {
        let library = library.trim_end_matches(".dll");

        quote! {
            #[link(name = #library)]
            extern #abi {
                #link_name
                pub fn #ident(#(#params,)* #vararg) #return_type;
            }
        }
    } else if let Some(library) = gen.reader.method_def_static_lib(signature.def) {
        quote! {
            #[link(name = #library, kind = "static")]
            extern #abi {
                #link_name
                pub fn #ident(#(#params,)* #vararg) #return_type;
            }
        }
    } else {
        
    }
}
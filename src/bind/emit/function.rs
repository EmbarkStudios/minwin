use super::*;

impl<'r> Emit<'r> {
    pub(super) fn emit_func(&self, os: &mut OutputStream, meth: wmr::MethodDef) {
        let reader = self.reader;
        let sig = reader.method_def_signature(meth, &[]);

        let name = reader.method_def_name(sig.def);
        let ident = self.config.make_ident(name, IdentKind::Function);

        let attrs = self.attributes(reader.method_def_attributes(meth));
        let docs = self.docs_link(reader.method_def_attributes(meth));

        let params = self.param_printer(&sig);
        let ret = sig
            .return_type
            .as_ref()
            .map(|rt| {
                let rt = self.type_printer(rt.clone());
                quote! { -> #rt }
            })
            .or_else(|| {
                reader
                    .method_def_does_not_return(meth)
                    .then(|| quote! { -> ! })
            });

        // Handle the 3! functions that have a different link name :crying:
        let symbol = if let Some(impl_map) = reader.method_def_impl_map(sig.def) {
            reader.impl_map_import_name(impl_map)
        } else {
            name
        };

        let module = reader.method_def_module_name(meth);

        // If this occurs we'd just need to add kind = "static" to the extern block...
        // but there are literally no cases of this, so not worth supporting
        if let Some(lib) = reader.method_def_static_lib(meth) {
            panic!("function '{name}' is defined within static lib '{lib}', please file an issue");
        }

        let abi = reader.method_def_extern_abi(meth);
        let is_system = abi == "system";

        let ts = if self.config.linking_style == crate::bind::LinkingStyle::WindowsTargets {
            let symbol = (self.config.use_rust_casing || symbol != name).then_some(symbol);

            // Unfortunately the link macro requires a return, even if it's just unit
            let ret = ret.unwrap_or_else(|| quote! { -> () });

            quote! {
                #attrs
                #docs
                ::windows_targets::link!(#module #abi #symbol fn #ident(#params)#ret);
            }
        } else {
            let link_name = (self.config.use_rust_casing || symbol != name).then(|| {
                quote! {
                    #[link_name = #symbol]
                }
            });

            quote! {
                #attrs
                #docs
                #link_name
                pub fn #ident(#params)#ret;
            }
        };

        os.insert_function(module.into(), is_system, meth, ident, attrs, ts);
    }
}

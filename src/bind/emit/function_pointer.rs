use super::*;

impl<'r> super::Emit<'r> {
    pub(super) fn emit_function_pointer(
        &self,
        fp: TypeDef,
    ) -> anyhow::Result<(pm::Ident, TokenStream)> {
        let reader = self.reader;

        anyhow::ensure!(
            !reader
                .type_def_flags(fp)
                .contains(wmr::TypeAttributes::WINRT),
            "WinRT delegates are not supported"
        );

        let ident = self.to_ident(reader.type_def_name(fp), IdentKind::FunctionPointer);

        let meth = reader.type_def_invoke_method(fp);
        let sig = reader.method_def_signature(meth, &[]);

        let attrs = self.attributes(reader.method_def_attributes(meth));
        let params = self.param_printer(&sig);
        let ret = sig.return_type.as_ref().map(|rt| {
            let rt = self.type_printer(rt.clone());
            quote! { -> #rt }
        });

        let ts = quote! {
            #attrs
            pub type #ident = ::core::option::Option<unsafe extern "system" fn(#params)#ret>;
        };

        Ok((ident, ts))
    }
}

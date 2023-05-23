use super::*;
use quote::format_ident;

impl<'r> Emit<'r> {
    pub(super) fn emit_interface(
        &self,
        os: &mut OutputStream,
        iface: TypeDef,
    ) -> anyhow::Result<()> {
        let reader = self.reader;
        let type_name = reader.type_def_type_name(iface);
        let ident = self.config.make_ident(type_name.name, IdentKind::Record);

        let Some(methods) = self.get_interface_methods(type_name) else {
            os.insert_interface(iface, ident, quote! { pub type #ident = *mut ::core::ffi::c_void; });
            return Ok(());
        };

        self.emit_vtable(os, iface, Some(methods))?;

        // Note this differs from the simple type alias above by allowing the user
        // to provide their own impl of the struct outside of the generated bindings
        let ts = if self.config.use_core {
            self.emit_interface_core(iface, &ident)?
        } else if self.config.emit_com_helpers {
            let vtbl_ident = format_ident!("{name}_Vtbl");
            quote! {
                #[repr(transparent)]
                pub struct #ident(ComObject<#vtbl_ident>);
            }
        } else {
            quote! {
                #[repr(transparent)]
                pub struct #ident(::core::ptr::NonNull<::core::ffi::c_void>);
            }
        };

        
        os.insert_interface(iface, ident, ts);

        Ok(())
    }

    /// Generate a vtable, replacing any methods that the user hasn't requested
    /// with a simple `usize` to preserve the layout of the vtable, without
    /// needing to pull in and compile any types exclusive to those skipped methods
    fn emit_vtable(
        &self,
        os: &mut OutputStream,
        iface: TypeDef,
        methods: Option<&std::collections::BTreeSet<String>>,
    ) -> anyhow::Result<()> {
        if os.has_vtable(iface) {
            return Ok(());
        }

        let reader = self.reader;
        let vtables = reader.type_def_vtables(iface);
        for base in &vtables {
            let Type::TypeDef((base, _)) = base else { continue; };
            self.emit_vtable(
                os,
                *base,
                self.get_interface_methods(reader.type_def_type_name(*base)),
            )?;
        }

        let name = reader.type_def_name(iface);
        let ident = format_ident!("{name}_Vtbl");

        let mut fields = Vec::new();

        {
            // The first field is always the vtable for the direct parent
            let parent = vtables.last().with_context(|| {
                format!("the interface '{name}' does not have a base interface")
            })?;

            let pfield = match parent {
                Type::IUnknown => {
                    if self.config.use_core {
                        quote! { pub base__: ::windows_core::IUnknown_Vtbl }
                    } else {
                        quote! { pub base__: IUnknown_Vtbl }
                    }
                }
                Type::IInspectable => {
                    if self.config.use_core {
                        quote! { pub base__: ::windows_core::IInspectable_Vtbl }
                    } else {
                        quote! { pub base__: IInspectable_Vtbl }
                    }
                }
                Type::TypeDef((def, _)) => {
                    let base_name = reader.type_def_name(*def);
                    let base_name = format_ident!("{base_name}_Vtbl");
                    quote! { pub base__: #base_name }
                }
                _ => {
                    anyhow::bail!("the interface '{name}' has a completely invalid base interface")
                }
            };

            fields.push(pfield);
        }

        for method in reader.type_def_methods(iface) {
            let name = reader.method_def_name(method)
            if name == ".ctor" {
                continue;
            }

            let fname = self.config.make_ident(name, IdentKind::Field);

            if !methods.map(|m| m.contains(name)).unwrap_or_default() {
                fields.push(quote! { #fname: usize });
                continue;
            }

            let sig = reader.method_def_signature(method, &[]);
            let params = self.param_printer(&sig);
            let ret = sig.return_type.as_ref().map(|rt| {
                let rt = self.type_printer(rt.clone());
                quote! { -> #rt }
            });

            let meth = quote! {
                pub #fname: unsafe extern "system" fn(#params)#ret
            };
            fields.push(meth);
        }

        let ts = quote! {
            #[repr(C)]
            pub struct #ident {
                #(#fields),*
            }
        };

        os.insert_vtable(iface, ident, ts);
        Ok(())
    }

    fn emit_interface_core(&self, iface: TypeDef, ident: &Ident) -> anyhow::Result<TokenStream> {
        let reader = self.reader;

        let mut methods = Vec::new();
        for meth in reader.type_def_methods(iface) {
            methods.push(self.emit_core_method(iface, meth));
        }

        let generics = self.generics_printer(iface);

        let com_iface = if generics.is_empty() {
            self.guid_printer(reader.type_def_guid(iface))

            quote! {
                unsafe impl ::windows_core::ComInterface for #ident {
                    const IID: ::windows_core::GUID = #guid;
                }
            }
        } else {
            quote! {
                unsafe impl ::windows_core::ComInterface for #ident {
                    const IID: ::windows_core::GUID = ::windows_core::GUID::from_signature(<Self as ::windows_core::RuntimeType>::SIGNATURE);
                }
            }
        };

        let its = quote! {
            #[repr(transparent)]
            pub struct #ident(::windows_core::IUnknown);

            #com_iface

            impl #ident {
                #(#methods)
            }
        };

        Ok(its)
    }

    fn emit_core_method(&self, iface: TypeDef, method: MethodDef) -> TokenStream {
        use wmr::SignatureKind;

        let reader = self.reader;
        let sig = reader.method_def_signature(method, &[]);
        
        let sig_kind = reader.signature_kind(&sig);

        match sig_kind {
            // Methods which essentially wrap IUnknown::QueryInterface but are
            // specific to a particular type
            SignatureKind::Query(_) => {
                let args = self.args(&signature.params, sig_kind);
                let params = self.params(&signature.params, sig_kind);
                let generics = expand_generics(generics, quote!(T));
                let where_clause =
                    expand_where_clause(where_clause, quote!(T: ::windows_core::ComInterface));
    
                quote! {
                    pub unsafe fn #name<#generics>(&self, #params) -> ::windows_core::Result<T> #where_clause {
                        let mut result__ = ::std::ptr::null_mut();
                        (::windows_core::Interface::vtable(self)#bases.#vname)(::windows_core::Interface::as_raw(self), #args).from_abi(result__)
                    }
                }
            }
        }
            
            let name = method_names.add(gen, method);
            let vname = virtual_names.add(gen, method);
            let generics = gen.constraint_generics(&signature.params);
            let where_clause = gen.where_clause(&signature.params);
            let mut cfg = gen.reader.signature_cfg(&signature);
            cfg.add_feature(gen.reader.type_def_namespace(def));
            let doc = gen.cfg_method_doc(&cfg);
            let features = gen.cfg_features(&cfg);
        
            if kind == InterfaceKind::None {
                return quote! {};
            }
        
            let mut bases = quote! {};
        
            for _ in 0..base_count {
                bases.combine(&quote! { .base__ });
            }
        
            let kind = gen.reader.signature_kind(&signature);
            match kind {
                
                SignatureKind::QueryOptional(_) => {
                    let args = gen.win32_args(&signature.params, kind);
                    let params = gen.win32_params(&signature.params, kind);
                    let generics = expand_generics(generics, quote!(T));
                    let where_clause =
                        expand_where_clause(where_clause, quote!(T: ::windows_core::ComInterface));
        
                    quote! {
                        #doc
                        #features
                        pub unsafe fn #name<#generics>(&self, #params result__: *mut ::core::option::Option<T>) -> ::windows_core::Result<()> #where_clause {
                            (::windows_core::Interface::vtable(self)#bases.#vname)(::windows_core::Interface::as_raw(self), #args).ok()
                        }
                    }
                }
                SignatureKind::ResultValue => {
                    let args = gen.win32_args(&signature.params, kind);
                    let params = gen.win32_params(&signature.params, kind);
                    let return_type = signature.params[signature.params.len() - 1].ty.deref();
                    let return_type = gen.type_name(&return_type);
        
                    quote! {
                        #doc
                        #features
                        pub unsafe fn #name<#generics>(&self, #params) -> ::windows_core::Result<#return_type> #where_clause {
                            let mut result__ = ::windows_core::zeroed::<#return_type>();
                            (::windows_core::Interface::vtable(self)#bases.#vname)(::windows_core::Interface::as_raw(self), #args).from_abi(result__)
                        }
                    }
                }
                SignatureKind::ResultVoid => {
                    let args = gen.win32_args(&signature.params, kind);
                    let params = gen.win32_params(&signature.params, kind);
        
                    quote! {
                        #doc
                        #features
                        pub unsafe fn #name<#generics>(&self, #params) -> ::windows_core::Result<()> #where_clause {
                            (::windows_core::Interface::vtable(self)#bases.#vname)(::windows_core::Interface::as_raw(self), #args).ok()
                        }
                    }
                }
                SignatureKind::ReturnValue => {
                    let args = gen.win32_args(&signature.params, kind);
                    let params = gen.win32_params(&signature.params, kind);
                    let return_type = signature.params[signature.params.len() - 1].ty.deref();
                    let is_nullable = gen.reader.type_is_nullable(&return_type);
                    let return_type = gen.type_name(&return_type);
        
                    if is_nullable {
                        quote! {
                            #doc
                            #features
                            pub unsafe fn #name<#generics>(&self, #params) -> ::windows_core::Result<#return_type> #where_clause {
                                let mut result__ = ::windows_core::zeroed::<#return_type>();
                                (::windows_core::Interface::vtable(self)#bases.#vname)(::windows_core::Interface::as_raw(self), #args);
                                ::windows_core::from_abi(result__)
                            }
                        }
                    } else {
                        quote! {
                            #doc
                            #features
                            pub unsafe fn #name<#generics>(&self, #params) -> #return_type #where_clause {
                                let mut result__ = ::windows_core::zeroed::<#return_type>();
                                (::windows_core::Interface::vtable(self)#bases.#vname)(::windows_core::Interface::as_raw(self), #args);
                                ::std::mem::transmute(result__)
                            }
                        }
                    }
                }
                SignatureKind::ReturnStruct => {
                    let args = gen.win32_args(&signature.params, kind);
                    let params = gen.win32_params(&signature.params, kind);
                    let return_type = gen.type_name(&signature.return_type);
        
                    quote! {
                        #doc
                        #features
                        pub unsafe fn #name<#generics>(&self, #params) -> #return_type #where_clause {
                            let mut result__: #return_type = ::core::mem::zeroed();
                            (::windows_core::Interface::vtable(self)#bases.#vname)(::windows_core::Interface::as_raw(self), &mut result__, #args);
                            result__
                        }
                    }
                }
                SignatureKind::PreserveSig => {
                    let args = gen.win32_args(&signature.params, kind);
                    let params = gen.win32_params(&signature.params, kind);
                    let return_type = gen.return_sig(&signature);
        
                    quote! {
                        #doc
                        #features
                        pub unsafe fn #name<#generics>(&self, #params) #return_type #where_clause {
                            (::windows_core::Interface::vtable(self)#bases.#vname)(::windows_core::Interface::as_raw(self), #args)
                        }
                    }
                }
                SignatureKind::ReturnVoid => {
                    let args = gen.win32_args(&signature.params, kind);
                    let params = gen.win32_params(&signature.params, kind);
        
                    quote! {
                        #doc
                        #features
                        pub unsafe fn #name<#generics>(&self, #params) #where_clause {
                            (::windows_core::Interface::vtable(self)#bases.#vname)(::windows_core::Interface::as_raw(self), #args)
                        }
                    }
                }
            }
        }

        pub fn win32_args(&self, params: &[SignatureParam], kind: SignatureKind) -> TokenStream {
            let mut tokens = quote! {};
    
            for (position, param) in params.iter().enumerate() {
                let new = match kind {
                    SignatureKind::Query(query) if query.object == position => {
                        quote! { &mut result__, }
                    }
                    SignatureKind::ReturnValue | SignatureKind::ResultValue
                        if params.len() - 1 == position =>
                    {
                        quote! { &mut result__, }
                    }
                    SignatureKind::QueryOptional(query) if query.object == position => {
                        quote! { result__ as *mut _ as *mut _, }
                    }
                    SignatureKind::Query(query) | SignatureKind::QueryOptional(query)
                        if query.guid == position =>
                    {
                        quote! { &<T as ::windows_core::ComInterface>::IID, }
                    }
                    _ => {
                        let name = self.param_name(param.def);
                        let flags = self.reader.param_flags(param.def);
                        match param.kind {
                            SignatureParamKind::ArrayFixed(_)
                            | SignatureParamKind::ArrayRelativeLen(_)
                            | SignatureParamKind::ArrayRelativeByteLen(_) => {
                                let map = if flags.contains(ParamAttributes::OPTIONAL) {
                                    quote! { #name.as_deref().map_or(::core::ptr::null(), |slice|slice.as_ptr()) }
                                } else {
                                    quote! { #name.as_ptr() }
                                };
                                quote! { ::core::mem::transmute(#map), }
                            }
                            SignatureParamKind::ArrayRelativePtr(relative) => {
                                let name = self.param_name(params[relative].def);
                                let flags = self.reader.param_flags(params[relative].def);
                                if flags.contains(ParamAttributes::OPTIONAL) {
                                    quote! { #name.as_deref().map_or(0, |slice|slice.len() as _), }
                                } else {
                                    quote! { #name.len() as _, }
                                }
                            }
                            SignatureParamKind::TryInto => {
                                quote! { #name.try_into_param()?.abi(), }
                            }
                            SignatureParamKind::IntoParam => {
                                quote! { #name.into_param().abi(), }
                            }
                            SignatureParamKind::OptionalPointer => {
                                if flags.contains(ParamAttributes::OUTPUT) {
                                    quote! { ::core::mem::transmute(#name.unwrap_or(::std::ptr::null_mut())), }
                                } else {
                                    quote! { ::core::mem::transmute(#name.unwrap_or(::std::ptr::null())), }
                                }
                            }
                            SignatureParamKind::ValueType => {
                                quote! { #name, }
                            }
                            SignatureParamKind::Blittable => {
                                quote! { ::core::mem::transmute(#name), }
                            }
                            SignatureParamKind::Other => {
                                quote! { ::core::mem::transmute_copy(#name), }
                            }
                        }
                    }
                };
                tokens.combine(&new)
            }
    
            tokens
        }
        pub fn win32_params(&self, params: &[SignatureParam], kind: SignatureKind) -> TokenStream {
            let mut tokens = quote! {};
    
            let mut generic_params = self.generic_params(params);
            for (position, param) in params.iter().enumerate() {
                match kind {
                    SignatureKind::Query(query) | SignatureKind::QueryOptional(query) => {
                        if query.object == position || query.guid == position {
                            continue;
                        }
                    }
                    SignatureKind::ReturnValue | SignatureKind::ResultValue
                        if params.len() - 1 == position =>
                    {
                        continue;
                    }
                    _ => {}
                }
    
                let name = self.param_name(param.def);
    
                match param.kind {
                    SignatureParamKind::ArrayFixed(fixed) => {
                        let ty = param.ty.deref();
                        let ty = self.type_default_name(&ty);
                        let len = Literal::u32_unsuffixed(fixed as _);
                        let ty = if self
                            .reader
                            .param_flags(param.def)
                            .contains(ParamAttributes::OUTPUT)
                        {
                            quote! { &mut [#ty; #len] }
                        } else {
                            quote! { &[#ty; #len] }
                        };
                        if self
                            .reader
                            .param_flags(param.def)
                            .contains(ParamAttributes::OPTIONAL)
                        {
                            tokens.combine(&quote! { #name: ::core::option::Option<#ty>, });
                        } else {
                            tokens.combine(&quote! { #name: #ty, });
                        }
                    }
                    SignatureParamKind::ArrayRelativeLen(_) => {
                        let ty = param.ty.deref();
                        let ty = self.type_default_name(&ty);
                        let ty = if self
                            .reader
                            .param_flags(param.def)
                            .contains(ParamAttributes::OUTPUT)
                        {
                            quote! { &mut [#ty] }
                        } else {
                            quote! { &[#ty] }
                        };
                        if self
                            .reader
                            .param_flags(param.def)
                            .contains(ParamAttributes::OPTIONAL)
                        {
                            tokens.combine(&quote! { #name: ::core::option::Option<#ty>, });
                        } else {
                            tokens.combine(&quote! { #name: #ty, });
                        }
                    }
                    SignatureParamKind::ArrayRelativeByteLen(_) => {
                        let ty = if self
                            .reader
                            .param_flags(param.def)
                            .contains(ParamAttributes::OUTPUT)
                        {
                            quote! { &mut [u8] }
                        } else {
                            quote! { &[u8] }
                        };
                        if self
                            .reader
                            .param_flags(param.def)
                            .contains(ParamAttributes::OPTIONAL)
                        {
                            tokens.combine(&quote! { #name: ::core::option::Option<#ty>, });
                        } else {
                            tokens.combine(&quote! { #name: #ty, });
                        }
                    }
                    SignatureParamKind::ArrayRelativePtr(_) => {}
                    SignatureParamKind::TryInto | SignatureParamKind::IntoParam => {
                        let (position, _) = generic_params.next().unwrap();
                        let kind: TokenStream = format!("P{position}").into();
                        tokens.combine(&quote! { #name: #kind, });
                    }
                    SignatureParamKind::OptionalPointer => {
                        let kind = self.type_default_name(&param.ty);
                        tokens.combine(&quote! { #name: ::core::option::Option<#kind>, });
                    }
                    SignatureParamKind::ValueType | SignatureParamKind::Blittable => {
                        let kind = self.type_default_name(&param.ty);
                        tokens.combine(&quote! { #name: #kind, });
                    }
                    SignatureParamKind::Other => {
                        let kind = self.type_default_name(&param.ty);
                        tokens.combine(&quote! { #name: &#kind, });
                    }
                }
            }
    
            tokens
        }
    }
}

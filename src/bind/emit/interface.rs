use crate::bind::COMStyle;

use super::*;
use syn::Ident;
use wmr::{ParamAttributes, Signature, SignatureKind, SignatureParamKind, TypeAttributes};

impl<'r> Emit<'r> {
    pub(super) fn emit_interface(
        &self,
        os: &mut OutputStream,
        iface: TypeDef,
    ) -> anyhow::Result<()> {
        let reader = self.reader;
        let type_name = reader.type_def_type_name(iface);
        let ident = self.config.make_ident(type_name.name, IdentKind::Record);

        if reader.type_def_flags(iface).contains(TypeAttributes::WINRT) {
            anyhow::bail!(
                "The WinRT interface '{}' is not supported in this mode, please use `bindgen` mode",
                type_name.name
            );
        }

        if self.get_interface_methods(type_name).is_none() {
            let ts = quote! { pub type #ident = *mut ::core::ffi::c_void; };
            os.insert_interface(iface, ident, ts);
            return Ok(());
        };

        self.emit_vtable(os, iface)?;

        let ts = self.emit_interface_impl(iface, &ident)?;
        os.insert_interface(iface, ident, ts);

        Ok(())
    }

    /// Generates an interfaces vtable, and all ancestors vtables.
    ///
    /// This replaces any methods that the user hasn't requested with a simple
    /// `usize` to preserve the layout of the vtable, but avoiding the need to
    /// emit any types exclusive to those skipped methods
    ///
    /// ```
    /// #[repr(C)]
    /// pub struct IModalWindow_Vtbl {
    ///     pub base__: ::windows_core::IUnknown_Vtbl,
    ///     pub Show: unsafe extern "system" fn(this: *mut ::core::ffi::c_void, hwndowner: HWND) -> ::windows_core::HRESULT,
    /// }
    /// ```
    fn emit_vtable(&self, os: &mut OutputStream, iface: TypeDef) -> anyhow::Result<()> {
        if os.has_vtable(iface) {
            return Ok(());
        }

        let reader = self.reader;
        let vtables = reader.type_def_vtables(iface);
        for base in &vtables {
            let Type::TypeDef((base, _)) = base else { continue; };
            self.emit_vtable(os, *base)?;
        }

        let name = reader.type_def_name(iface);
        let ident = quote::format_ident!("{name}_Vtbl");

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
                    let base_name = quote::format_ident!("{base_name}_Vtbl");
                    quote! { pub base__: #base_name }
                }
                _ => {
                    anyhow::bail!("the interface '{name}' has a completely invalid base interface")
                }
            };

            fields.push(pfield);
        }

        fields.extend(self.method_iter(iface).map(|meth| match meth {
            Method::Skip { ident } => {
                quote! { #ident: usize }
            }
            Method::Emit { ident, sig, .. } => {
                let params = self.param_printer(&sig);
                let ret = sig.return_type.as_ref().map(|rt| {
                    let rt = self.type_printer(rt.clone());
                    quote! { -> #rt }
                });

                quote! {
                    pub #ident: unsafe extern "system" fn(this: *mut ::core::ffi::c_void, #params)#ret
                }
            }
        }));

        let generics: Vec<_> = reader.type_def_generics(iface).collect();

        let (generics, phantoms) = (!generics.is_empty())
            .then(|| {
                let names = generics.iter().map(|g| self.type_printer(g.clone()));
                let names = quote! { <#(#names),*> };

                let phantoms = generics.into_iter().map(|g| {
                    let tp = self.type_printer(g);
                    quote! { #tp: ::core::marker::PhantomData<#tp> }
                });

                (
                    names,
                    quote! {
                        #(#phantoms),*
                    },
                )
            })
            .unzip();

        let ts = quote! {
            #[repr(C)]
            pub struct #ident #generics {
                #(#fields),*
                #phantoms
            }
        };

        os.insert_vtable(iface, ident, ts);
        Ok(())
    }

    /// Returns a token stream of the interface implementations and all of the
    /// requested items
    ///
    /// ```
    /// pub struct IModalWindow(::windows_core::IUnknown);
    ///
    /// impl IModalWindow {
    ///     // methods which go via vtable
    /// }
    ///
    /// ::windows_core::imp::interface_hierarchy!(IModalWindow, ::windows_core::IUnknown);
    /// impl ::core::cmp::PartialEq for IModalWindow {
    ///     fn eq(&self, other: &Self) -> bool {
    ///         self.0 == other.0
    ///     }
    /// }
    ///
    /// impl ::core::cmp::Eq for IModalWindow {}
    /// impl ::core::fmt::Debug for IModalWindow {
    ///     fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
    ///         f.debug_tuple("IModalWindow").field(&self.0).finish()
    ///     }
    /// }
    ///
    /// unsafe impl ::windows_core::Interface for IModalWindow {
    ///     type Vtable = IModalWindow_Vtbl;
    /// }
    ///
    /// impl ::core::clone::Clone for IModalWindow {
    ///     fn clone(&self) -> Self {
    ///         Self(self.0.clone())
    ///     }
    /// }
    ///
    /// unsafe impl ::windows_core::ComInterface for IModalWindow {
    ///     const IID: ::windows_core::GUID = ::windows_core::GUID::from_u128(0xb4db1657_70d7_485e_8e3e_6fcb5a5c1802);
    /// }
    /// ```
    fn emit_interface_impl(&self, iface: TypeDef, ident: &Ident) -> anyhow::Result<TokenStream> {
        if self.config.com_style == COMStyle::None {
            return Ok(quote! {
                #[repr(transparent)]
                pub struct #ident(::core::ptr::NonNull<::core::ffi::c_void>);
            });
        }

        let reader = self.reader;

        let methods: Vec<_> = self
            .method_iter(iface)
            .filter_map(|meth| match meth {
                Method::Emit { ident, sig, .. } => {
                    let mts = self.emit_method_impl(iface, ident, sig);
                    Some(mts)
                }
                Method::Skip { .. } => None,
            })
            .collect();

        let has_generics = reader.type_def_generics(iface).count() > 0;
        let tp = self.type_printer(Type::GUID);

        let com_iface = if !has_generics {
            let guid = self.guid_printer(reader.type_def_guid(iface));
            quote! {
                unsafe impl ::windows_core::ComInterface for #ident {
                    const IID: #tp = #guid;
                }
            }
        } else {
            quote! {
                unsafe impl ::windows_core::ComInterface for #ident {
                    const IID: #tp = #tp::from_signature(<Self as ::windows_core::RuntimeType>::SIGNATURE);
                }
            }
        };

        //let impls = self.emit_interface_impls(iface);

        let imp = if methods.is_empty() {
            None
        } else {
            Some(quote! {
                impl #ident {
                    #(#methods)*
                }
            })
        };

        let its = quote! {
            #[repr(transparent)]
            pub struct #ident(::windows_core::IUnknown);

            #imp

            #com_iface
        };

        Ok(its)
    }

    /// Emits methods which wrap the vtable function pointer, handling arguments
    /// and return values
    ///
    /// ```rust
    /// impl IModalWindow {
    ///     pub unsafe fn Show<P0>(&self, hwndowner: P0) -> ::windows_core::Result<()>
    ///     where
    ///         P0: ::windows_core::IntoParam<super::super::Foundation::HWND>,
    ///     {
    ///         (::windows_core::Interface::vtable(self).Show)(::windows_core::Interface::as_raw(self), hwndowner.into_param().abi()).ok()
    ///     }
    /// }
    /// ```
    fn emit_method_impl(&self, _iface: TypeDef, ident: Ident, sig: Signature) -> TokenStream {
        let reader = self.reader;

        let MethodParts {
            generics,
            where_clause,
            params,
            args,
        } = self.make_method_parts(&sig);

        let vtable = quote! { ::windows_core::Interface::vtable(self).#ident };
        let this = quote! { ::windows_core::Interface::as_raw(self) };

        let mut rt = None;
        let mut prefix = None;
        let mut suffix = None;

        let sig_kind = reader.signature_kind(&sig);
        match sig_kind {
            // Methods which essentially wrap IUnknown::QueryInterface but are
            // specific to a particular type
            SignatureKind::Query(_) => {
                rt = Some(quote! { -> ::windows_core::Result<T> });
                prefix = Some(quote! { let mut result__ = ::std::mem::MaybeUninit::uninit(); });
                suffix = Some(quote! { .from_abi(result__) });
            }
            // Ditto as above, but the result is optional and thus an out param instead
            SignatureKind::QueryOptional(_) => {
                rt = Some(quote! { -> ::windows_core::Result<()> });
                suffix = Some(quote! { .ok() });
            }
            // Typical methods that returns a simple type or error
            SignatureKind::ResultValue => {
                let return_type = sig.params.last().unwrap().ty.deref();
                let ret = self.type_printer(return_type);

                rt = Some(quote! { -> ::windows_core::Result<#ret> });
                prefix = Some(quote! { let mut result__ = ::windows_core::zeroed::<#ret>(); });
                suffix = Some(quote! { .from_abi(result__) });
            }
            // Methods that don't return a value other than HRESULT
            SignatureKind::ResultVoid => {
                rt = Some(quote! { -> ::windows_core::Result<()> });
                suffix = Some(quote! { .ok() });
            }
            // Methods that infallibly return a value
            SignatureKind::ReturnValue => {
                let return_type = sig.params.last().unwrap().ty.deref();
                let is_nullable = reader.type_is_nullable(&return_type);
                let ret = self.type_printer(return_type);

                let res = if is_nullable {
                    quote! { ::windows_core::from_abi(result__) }
                } else {
                    quote! { ::std::mem::transmute(result__) }
                };

                rt = Some(quote! { -> #ret });
                prefix = Some(quote! { let mut result__ = ::windows_core::zeroed::<#ret>(); });
                suffix = Some(quote! { ;
                    #res
                });
            }
            // Methods that infallibly return a record via out param
            SignatureKind::ReturnStruct => {
                let ret = self.type_printer(sig.return_type.as_ref().unwrap().clone());

                rt = Some(quote! { -> #ret });
                prefix = Some(quote! { let mut result__ = ::windows_core::zeroed::<#ret>(); });
                suffix = Some(quote! { ;
                    result__
                });
            }
            // The return type is forwarded
            SignatureKind::PreserveSig => {
                rt = sig.return_type.as_ref().and_then(|rt| match rt {
                    Type::Void if reader.method_def_does_not_return(sig.def) => {
                        Some(quote! { -> ! })
                    }
                    Type::Void => None,
                    _ => {
                        let rt = self.type_printer(rt.clone());
                        Some(quote! { -> #rt })
                    }
                });
            }
            // Methods that don't return anything
            SignatureKind::ReturnVoid => {}
        }

        quote! {
            pub unsafe fn #ident #generics (&self, #params) #rt #where_clause {
                #prefix
                (#vtable)(#this, #args)#suffix
            }
        }
    }

    fn make_method_parts(&self, sig: &Signature) -> MethodParts {
        let reader = self.reader;
        let sig_kind = reader.signature_kind(sig);

        let mut generics = Vec::new();
        let mut params = Vec::new();
        let mut args = Vec::new();

        if sig_kind == SignatureKind::ReturnStruct {
            args.push(quote! { &mut result__ });
        }

        // For QueryOptional methods that return an pointer rather than an interface directly
        let mut push_result_param = false;
        // When emitting bindgen-style COM bindings every many params use generic
        // conversions to make the API easier to use but vastly more complicated :p
        let mut generic_index = -1;

        for (pos, param) in sig.params.iter().enumerate() {
            if reader.signature_param_is_convertible(param) {
                generic_index += 1;
            }

            match sig_kind {
                SignatureKind::Query(query) if query.object == pos => {
                    args.push(quote! { &mut result__ });
                }
                SignatureKind::Query(query) | SignatureKind::QueryOptional(query)
                    if query.guid == pos =>
                {
                    args.push(quote! { &<T as ::windows_core::ComInterface>::IID });
                }
                SignatureKind::QueryOptional(query) if query.object == pos => {
                    args.push(quote! { result__ as *mut _ as *mut _ });
                    push_result_param = true;
                }
                SignatureKind::ReturnValue | SignatureKind::ResultValue
                    if sig.params.len() - 1 == pos =>
                {
                    args.push(quote! { &mut result__ });
                }
                _ => {
                    let name = self
                        .config
                        .make_ident(reader.param_name(param.def), IdentKind::Param);
                    let flags = reader.param_flags(param.def);
                    let param_ty = match param.kind {
                        SignatureParamKind::ArrayFixed(fixed) => {
                            let ty = param.ty.deref();
                            let tp = self.type_printer(ty);
                            let len = pm::Literal::u32_unsuffixed(fixed as _);
                            let ty = if reader
                                .param_flags(param.def)
                                .contains(ParamAttributes::OUTPUT)
                            {
                                quote! { &mut [#tp; #len] }
                            } else {
                                quote! { &[#tp; #len] }
                            };

                            let ts = if reader
                                .param_flags(param.def)
                                .contains(ParamAttributes::OPTIONAL)
                            {
                                quote! { ::core::option::Option<#ty> }
                            } else {
                                ty
                            };

                            Some(ts)
                        }
                        SignatureParamKind::ArrayRelativeLen(_) => {
                            let ty = param.ty.deref();
                            let tp = self.type_printer(ty);
                            let ty = if reader
                                .param_flags(param.def)
                                .contains(ParamAttributes::OUTPUT)
                            {
                                quote! { &mut [#tp] }
                            } else {
                                quote! { &[#tp] }
                            };

                            let ts = if reader
                                .param_flags(param.def)
                                .contains(ParamAttributes::OPTIONAL)
                            {
                                quote! { #name: ::core::option::Option<#ty> }
                            } else {
                                ty
                            };

                            Some(ts)
                        }
                        SignatureParamKind::ArrayRelativeByteLen(_) => {
                            let ty = if reader
                                .param_flags(param.def)
                                .contains(ParamAttributes::OUTPUT)
                            {
                                quote! { &mut [u8] }
                            } else {
                                quote! { &[u8] }
                            };

                            let ts = if reader
                                .param_flags(param.def)
                                .contains(ParamAttributes::OPTIONAL)
                            {
                                quote! { ::core::option::Option<#ty> }
                            } else {
                                ty
                            };

                            Some(ts)
                        }
                        SignatureParamKind::ArrayRelativePtr(_) => None,
                        SignatureParamKind::TryInto | SignatureParamKind::IntoParam => {
                            let name = quote::format_ident!("P{generic_index}");
                            let tp = self.type_printer(param.ty.clone());

                            if self.config.com_style == COMStyle::Bindgen {
                                let constraint = if param.kind == SignatureParamKind::TryInto {
                                    quote! { ::windows_core::TryIntoParam<#tp> }
                                } else {
                                    quote! { ::windows_core::IntoParam<#tp> }
                                };

                                generics.push((name.clone(), constraint));
                                Some(quote::ToTokens::into_token_stream(name))
                            } else {
                                Some(quote! { #tp })
                            }
                        }
                        SignatureParamKind::OptionalPointer => {
                            let tp = self.type_printer(param.ty.clone());
                            Some(quote! { ::core::option::Option<#tp> })
                        }
                        SignatureParamKind::ValueType | SignatureParamKind::Blittable => {
                            let tp = self.type_printer(param.ty.clone());
                            Some(quote! { #tp })
                        }
                        SignatureParamKind::Other => {
                            let tp = self.type_printer(param.ty.clone());
                            Some(quote! { &#tp })
                        }
                    };

                    if let Some(ty) = param_ty {
                        params.push(quote! { #name: #ty });
                    }

                    let arg_ts = match param.kind {
                        SignatureParamKind::ArrayFixed(_)
                        | SignatureParamKind::ArrayRelativeLen(_)
                        | SignatureParamKind::ArrayRelativeByteLen(_) => {
                            let map = if flags.contains(ParamAttributes::OPTIONAL) {
                                quote! { #name.as_deref().map_or(::core::ptr::null(), |slice| slice.as_ptr()) }
                            } else {
                                quote! { #name.as_ptr() }
                            };
                            quote! { ::core::mem::transmute(#map) }
                        }
                        SignatureParamKind::ArrayRelativePtr(relative) => {
                            let name = self.config.make_ident(
                                reader.param_name(sig.params[relative].def),
                                IdentKind::Param,
                            );
                            let flags = reader.param_flags(sig.params[relative].def);
                            if flags.contains(ParamAttributes::OPTIONAL) {
                                quote! { #name.as_deref().map_or(0, |slice| slice.len() as _) }
                            } else {
                                quote! { #name.len() as _ }
                            }
                        }
                        SignatureParamKind::TryInto => {
                            if self.config.com_style == COMStyle::Bindgen {
                                quote! { #name.try_into_param()?.abi() }
                            } else {
                                quote! { #name }
                            }
                        }
                        SignatureParamKind::IntoParam => {
                            if self.config.com_style == COMStyle::Bindgen {
                                quote! { #name.into_param().abi() }
                            } else {
                                quote! { #name }
                            }
                        }
                        SignatureParamKind::OptionalPointer => {
                            if flags.contains(ParamAttributes::OUTPUT) {
                                quote! { ::core::mem::transmute(#name.unwrap_or(::std::ptr::null_mut())) }
                            } else {
                                quote! { ::core::mem::transmute(#name.unwrap_or(::std::ptr::null())) }
                            }
                        }
                        SignatureParamKind::ValueType => {
                            quote! { #name }
                        }
                        SignatureParamKind::Blittable => {
                            quote! { ::core::mem::transmute(#name) }
                        }
                        SignatureParamKind::Other => {
                            quote! { ::core::mem::transmute_copy(#name) }
                        }
                    };

                    args.push(arg_ts);
                }
            }
        }

        if push_result_param {
            params.push(quote! { result__: *mut ::core::option::Option<T> });
        }

        if matches!(
            sig_kind,
            SignatureKind::Query(_) | SignatureKind::QueryOptional(_)
        ) {
            generics.push((
                quote::format_ident!("T"),
                quote! { ::windows_core::ComInterface },
            ));
        }

        let (generics, where_clause) = if generics.is_empty() {
            (None, None)
        } else {
            let generic = generics.iter().map(|(n, _)| n);
            let generic_clause = quote! {
                <#(#generic),*>
            };
            let constraint = generics.into_iter().map(|(n, c)| {
                quote! { #n: #c }
            });
            let where_clause = quote! {
                where #(#constraint),*
            };

            (Some(generic_clause), Some(where_clause))
        };

        let params = quote! {
            #(#params),*
        };

        let args = quote! {
            #(#args),*
        };

        MethodParts {
            generics,
            where_clause,
            params,
            args,
        }
    }
}

struct MethodParts {
    generics: Option<TokenStream>,
    where_clause: Option<TokenStream>,
    params: TokenStream,
    args: TokenStream,
}

enum Method<'r> {
    Skip {
        ident: Ident,
    },
    Emit {
        /// The ident for the method, might be the same as `name`
        ident: Ident,
        /// The original name of the method
        name: &'r str,
        /// The method's signature
        sig: Signature,
    },
}

impl<'r> Emit<'r> {
    fn method_iter(&self, iface: TypeDef) -> impl Iterator<Item = Method<'r>> + '_ {
        let reader = self.reader;
        let type_name = reader.type_def_type_name(iface);

        let req_methods = self.get_interface_methods(type_name);

        reader.type_def_methods(iface).filter_map(move |meth| {
            let name = reader.method_def_name(meth);
            if name == ".ctor" {
                return None;
            }

            let ident = self.config.make_ident(name, IdentKind::Field);

            let meth = if !req_methods.map(|m| m.contains(name)).unwrap_or_default() {
                Method::Skip { ident }
            } else {
                let sig = reader.method_def_signature(meth, &[]);
                Method::Emit { ident, name, sig }
            };

            Some(meth)
        })
    }
}

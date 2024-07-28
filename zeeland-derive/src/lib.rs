use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_quote, punctuated::Punctuated, spanned::Spanned, Ident, TraitItemFn};

#[proc_macro_attribute]
pub fn zeeland(
    _attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let input = syn::parse_macro_input!(input as syn::ItemTrait);
    let r = derive_impl(input)
        .unwrap_or_else(|e| e.to_compile_error())
        .into();

    eprintln!("{r}");

    r
}

enum HttpMethod {
    Get,
    Post,
    Put,
    Delete,
    Patch,
    Options,
    Head,
    Connect,
    Trace,
}

impl HttpMethod {
    fn allows_body(&self) -> bool {
        match self {
            HttpMethod::Get | HttpMethod::Head | HttpMethod::Delete | HttpMethod::Options => false,
            _ => true,
        }
    }
}

struct Arg {
    span: proc_macro2::Span,
    ty: syn::Type,
    location: DataPassSource,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum ReqBodyKind {
    None,
    JsonIndividualKeys,
    JsonWholeBody,
    FormIndividualKeys,
    FormWholeBody,
    MultipartIndividualKeys,
    MultipartWholeBody,
}

struct RouteDef {
    method: HttpMethod,
    path: String,
    name: Ident,
    args: Vec<Arg>,
    ret_ty: syn::ReturnType,
    req_body: ReqBodyKind,
    impl_code: syn::Block,
}

fn http_method_from_prefix(prefix: &str) -> Option<HttpMethod> {
    if prefix.starts_with("get") {
        Some(HttpMethod::Get)
    } else if prefix.starts_with("post") {
        Some(HttpMethod::Post)
    } else if prefix.starts_with("put") {
        Some(HttpMethod::Put)
    } else if prefix.starts_with("delete") {
        Some(HttpMethod::Delete)
    } else if prefix.starts_with("patch") {
        Some(HttpMethod::Patch)
    } else if prefix.starts_with("options") {
        Some(HttpMethod::Options)
    } else if prefix.starts_with("head") {
        Some(HttpMethod::Head)
    } else if prefix.starts_with("connect") {
        Some(HttpMethod::Connect)
    } else if prefix.starts_with("trace") {
        Some(HttpMethod::Trace)
    } else {
        None
    }
}

enum DataPassSource {
    Header { header_name: String },
    Query { param_name: String },
    WholeJsonBody,
    InJsonBody { key: String },
    ServerState,
}

#[cfg(not(debug_assertions))]
fn hash_str(s: &str) -> String {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    format!("{:x}", hasher.finish())
}

fn convert_trait_fn_to_route_def(f: &TraitItemFn) -> syn::Result<RouteDef> {
    let Some(impl_code) = &f.default else {
        return Err(syn::Error::new_spanned(
            f,
            "Zeeland trait associated functions must have default implementations",
        ));
    };

    if f.sig.asyncness.is_none() {
        return Err(syn::Error::new_spanned(
            f,
            "Zeeland trait associated functions must be async",
        ));
    }

    let name = f.sig.ident.clone();
    let name_str = name.to_string();
    let http_method_from_name = http_method_from_prefix(&name_str);
    #[cfg(debug_assertions)]
    let path = format!("/{name}");
    #[cfg(not(debug_assertions))]
    let path = format!("/{}", hash_str(&name_str));

    let mut default_method = HttpMethod::Get;
    let mut req_body = ReqBodyKind::None;

    let mut args = vec![];

    for (idx, arg) in f.sig.inputs.iter().enumerate() {
        let syn::FnArg::Typed(arg_pat) = arg else {
            return Err(syn::Error::new_spanned(
                arg,
                "Zeeland trait associated functions cannot take self as a parameter",
            ));
        };

        let key = match &*arg_pat.pat {
            syn::Pat::Ident(ident) => ident.ident.to_string(),
            _ => format!("arg_{idx}"),
        };

        let default_data_pass_source = DataPassSource::InJsonBody { key: key.clone() };
        let mut explicit_data_pass_source = None;
        for attr in &arg_pat.attrs {
            if !attr.path().is_ident("zeeland") {
                continue;
            }

            let metas =
                attr.parse_args_with(Punctuated::<syn::Meta, syn::Token![,]>::parse_terminated)?;
            for meta in metas {
                match meta {
                    syn::Meta::NameValue(nv) if nv.path.is_ident("source") => {
                        let source = nv.value;
                        match source {
                            syn::Expr::Lit(syn::ExprLit {
                                lit: syn::Lit::Str(lit),
                                ..
                            }) => {
                                let source = lit.value();
                                if let Some(header_name) = source.strip_prefix("header:") {
                                    if header_name.is_empty() {
                                        return Err(syn::Error::new_spanned(
                                            source,
                                            "Empty header name",
                                        ));
                                    }

                                    if explicit_data_pass_source.is_some() {
                                        return Err(syn::Error::new_spanned(
                                            arg,
                                            "Multiple sources specified",
                                        ));
                                    }

                                    explicit_data_pass_source = Some(DataPassSource::Header {
                                        header_name: header_name.to_owned(),
                                    });
                                } else if let Some(param_name) = source.strip_prefix("query:") {
                                    if param_name.is_empty() {
                                        return Err(syn::Error::new_spanned(
                                            source,
                                            "Empty query parameter name",
                                        ));
                                    }

                                    if explicit_data_pass_source.is_some() {
                                        return Err(syn::Error::new_spanned(
                                            arg,
                                            "Multiple sources specified",
                                        ));
                                    }

                                    explicit_data_pass_source = Some(DataPassSource::Query {
                                        param_name: param_name.to_owned(),
                                    });
                                } else if let Some(key) = source.strip_prefix("json:") {
                                    if key.is_empty() {
                                        return Err(syn::Error::new_spanned(
                                            source,
                                            "Empty json key",
                                        ));
                                    }

                                    if explicit_data_pass_source.is_some() {
                                        return Err(syn::Error::new_spanned(
                                            arg,
                                            "Multiple sources specified",
                                        ));
                                    }

                                    explicit_data_pass_source = Some(DataPassSource::InJsonBody {
                                        key: key.to_owned(),
                                    });
                                } else if source == "json" {
                                    if explicit_data_pass_source.is_some() {
                                        return Err(syn::Error::new_spanned(
                                            arg,
                                            "Multiple sources specified",
                                        ));
                                    }

                                    explicit_data_pass_source = Some(DataPassSource::WholeJsonBody);
                                } else {
                                    return Err(syn::Error::new_spanned(
                                        source,
                                        "Unknown source type",
                                    ));
                                }
                            }
                            _ => {
                                return Err(syn::Error::new_spanned(
                                    source,
                                    "Expected a string literal",
                                ));
                            }
                        }
                    }
                    _ => {
                        return Err(syn::Error::new_spanned(
                            meta,
                            "Unknown attribute on argument",
                        ));
                    }
                }
            }
        }

        let data_pass_source = explicit_data_pass_source.unwrap_or(default_data_pass_source);

        match &data_pass_source {
            DataPassSource::Header { .. } | DataPassSource::Query { .. } => {}
            DataPassSource::WholeJsonBody => match &req_body {
                ReqBodyKind::None => {
                    req_body = ReqBodyKind::JsonWholeBody;
                }
                ReqBodyKind::JsonIndividualKeys => {
                    return Err(syn::Error::new_spanned(
                        &f,
                        "Cannot have both json keys and whole json body parameters",
                    ));
                }
                other => {
                    return Err(syn::Error::new_spanned(
                        &f,
                        format!("Cannot have both json and {:?} body parameters", other),
                    ));
                }
            },
            DataPassSource::InJsonBody { .. } => match &req_body {
                ReqBodyKind::None => {
                    req_body = ReqBodyKind::JsonIndividualKeys;
                }
                ReqBodyKind::JsonIndividualKeys => {}
                ReqBodyKind::JsonWholeBody => {
                    return Err(syn::Error::new_spanned(
                        &f,
                        "Cannot have both json keys and whole json body parameters",
                    ));
                }
                other => {
                    return Err(syn::Error::new_spanned(
                        &f,
                        format!("Cannot have both json and {:?} body parameters", other),
                    ));
                }
            },
            DataPassSource::ServerState => {}
        }

        args.push(Arg {
            span: arg_pat.span(),
            ty: *arg_pat.ty.clone(),
            location: data_pass_source,
        });
    }

    if req_body != ReqBodyKind::None {
        default_method = HttpMethod::Post;

        if let Some(from_name) = &http_method_from_name {
            if !from_name.allows_body() {
                return Err(syn::Error::new_spanned(
                    f,
                    r#"
the parameters of this function require a body, but the function name
implies that it should not have one; change the function name to start
with 'post', 'put', 'patch', or change the parameters to use a different
source."#,
                ));
            }
        }
    }

    check_route_args(&args)?;

    Ok(RouteDef {
        method: http_method_from_name.unwrap_or(default_method),
        path,
        name,
        args,
        ret_ty: f.sig.output.clone(),
        req_body,
        impl_code: impl_code.clone(),
    })
}

fn check_route_args(args: &[Arg]) -> syn::Result<()> {
    let mut seen_headers = vec![];
    let mut seen_query = vec![];
    let mut seen_json_keys = vec![];
    let mut seen_whole_json = false;

    for arg in args {
        match &arg.location {
            DataPassSource::Header { header_name } => {
                if seen_headers.contains(&header_name) {
                    return Err(syn::Error::new(
                        arg.span,
                        format!("Duplicate header name: {header_name}"),
                    ));
                }
                seen_headers.push(header_name);
            }
            DataPassSource::Query { param_name } => {
                if seen_query.contains(&param_name) {
                    return Err(syn::Error::new(
                        arg.span,
                        format!("Duplicate query parameter name: {param_name}"),
                    ));
                }
                seen_query.push(param_name);
            }
            DataPassSource::InJsonBody { key } => {
                if seen_json_keys.contains(&key) {
                    return Err(syn::Error::new(
                        arg.span,
                        format!("Duplicate json key: {key}"),
                    ));
                }

                seen_json_keys.push(key);
            }
            DataPassSource::WholeJsonBody => {
                if seen_whole_json {
                    return Err(syn::Error::new(
                        arg.span,
                        "Multiple arguments that take the whole json body",
                    ));
                }

                seen_whole_json = true;
            }
            DataPassSource::ServerState => {}
        }
    }

    Ok(())
}

fn push_field(fields: &mut syn::Fields, field: syn::Field) {
    match fields {
        syn::Fields::Named(fields) => {
            fields.named.push(field);
        }
        syn::Fields::Unnamed(fields) => {
            unimplemented!()
        }
        syn::Fields::Unit => {
            unimplemented!()
        }
    }
}

fn create_rocket_route(
    api_impl_type: &syn::Ident,
    error_ty: syn::Type,
    rd: &RouteDef,
    unwrap_expr: impl Fn(&syn::Ident) -> TokenStream,
) -> syn::Result<(TokenStream, Ident)> {
    let method = match rd.method {
        HttpMethod::Get => quote! { get },
        HttpMethod::Post => quote! { post },
        HttpMethod::Put => quote! { put },
        HttpMethod::Delete => quote! { delete },
        HttpMethod::Patch => quote! { patch },
        HttpMethod::Options => quote! { options },
        HttpMethod::Head => quote! { head },
        HttpMethod::Connect => quote! { connect },
        HttpMethod::Trace => quote! { trace },
    };

    let path = &rd.path;
    let name = &rd.name;

    enum ReqBodyInfo {
        None,
        JsonWholeBody {
            inner_ty: syn::Type,
            ty_name: syn::Type,
        },
        Json {
            full_ty: syn::ItemStruct,
            ty_name: syn::Type,
        },
        Form {
            full_ty: Option<syn::ItemStruct>,
            ty_name: syn::Type,
        },
        Multipart {
            full_ty: Option<syn::ItemStruct>,
            ty_name: syn::Type,
        },
    }

    let mut body_ty = match rd.req_body {
        ReqBodyKind::None => ReqBodyInfo::None,
        ReqBodyKind::JsonWholeBody => {
            let ty_name = format_ident!("{}_body", name);
            ReqBodyInfo::JsonWholeBody {
                inner_ty: parse_quote!(::zeeland::serde_json::Value),
                ty_name: parse_quote!(#ty_name),
            }
        }
        ReqBodyKind::JsonIndividualKeys => {
            let ty_name = format_ident!("{}_body", name);
            ReqBodyInfo::Json {
                full_ty: parse_quote!(
                    #[derive(::zeeland::serde::Deserialize)]
                    #[allow(non_camel_case_types)]
                    struct #ty_name {}
                ),
                ty_name: parse_quote!(#ty_name),
            }
        }
        _ => unimplemented!(),
    };

    let mut rocket_args = quote! {};
    let mut args_values_list = quote! {};
    let mut extra_decls = quote! {};
    let mut handler_pre = quote! {};
    let mut extra_rocket_attributes = quote! {};

    for arg in &rd.args {
        match &arg.location {
            DataPassSource::WholeJsonBody => {
                if let ReqBodyInfo::JsonWholeBody { inner_ty, ty_name } = &mut body_ty {
                    *inner_ty = arg.ty.clone();
                    rocket_args.extend(quote! {
                        __body: rocket::serde::json::Json<#ty_name>,
                    });
                    args_values_list.extend(quote! {
                        __body.into_inner().0,
                    });
                    extra_rocket_attributes.extend(quote! {
                        format = "json", data = "<__body>",
                    });
                } else {
                    return Err(syn::Error::new(
                        arg.span,
                        "Internal error: expected json whole body but got something else",
                    ));
                }
            }
            DataPassSource::InJsonBody { key } => {
                if let ReqBodyInfo::Json { full_ty, .. } = &mut body_ty {
                    let field_name = format_ident!("{key}");
                    let ty = &arg.ty;
                    push_field(&mut full_ty.fields, parse_quote!(#field_name: #ty));
                    args_values_list.extend(quote! {
                        #field_name,
                    });
                } else {
                    return Err(syn::Error::new(
                        arg.span,
                        "Internal error: expected json body but got something else",
                    ));
                }
            }
            _ => {}
        }
    }

    match &body_ty {
        ReqBodyInfo::Json { full_ty, ty_name } => {
            let field_names = full_ty.fields.iter().map(|f| &f.ident);
            rocket_args.extend(quote! {
                __body: rocket::serde::json::Json<#ty_name>,
            });
            handler_pre.extend(quote! {
                let #ty_name { #(#field_names),* } = __body.into_inner();
            });

            extra_decls.extend(quote! {
                #full_ty
            });

            extra_rocket_attributes.extend(quote! {
                format = "json", data = "<__body>",
            });
        }
        ReqBodyInfo::JsonWholeBody { ty_name, inner_ty } => {
            extra_decls.extend(quote! {
                #[derive(rocket::serde::Deserialize)]
                #[allow(non_camel_case_types)]
                #[serde(transparent)]
                struct #ty_name(#inner_ty);
            });
        }
        _ => {}
    }

    let ret_ty = match &rd.ret_ty {
        syn::ReturnType::Type(_, ty) => (**ty).clone(),
        syn::ReturnType::Default => parse_quote! {()},
    };

    let ret_ty: syn::ReturnType = parse_quote! {-> ::core::result::Result<#ret_ty, #error_ty>};

    let api_param_name = format_ident!("__api");
    let unwrapped_api_type = unwrap_expr(&api_param_name);

    let api_route_name = format_ident!("__zeeland_{name}");

    Ok((
        quote! {
            #extra_decls

            #[rocket::#method(#path, #extra_rocket_attributes)]
            async fn #api_route_name(#api_param_name: &rocket::State<#api_impl_type>, #rocket_args) #ret_ty {
                #handler_pre
                #unwrapped_api_type.#name(#args_values_list).await
            }
        },
        api_route_name,
    ))
}

fn derive_impl(input: syn::ItemTrait) -> syn::Result<TokenStream> {
    if input.supertraits.len() != 0 {
        return Err(syn::Error::new_spanned(
            input,
            "Zeeland traits cannot have supertraits",
        ));
    }

    if input.generics.params.len() != 0 {
        return Err(syn::Error::new_spanned(
            input,
            "Zeeland traits cannot have generic parameters",
        ));
    }

    if let Some(where_clause) = &input.generics.where_clause {
        return Err(syn::Error::new_spanned(
            where_clause,
            "Zeeland traits cannot have where clauses",
        ));
    }

    let mut routes = vec![];
    let mut fns = vec![];
    let mut impl_fns = vec![];

    for item in &input.items {
        if let syn::TraitItem::Fn(f) = item {
            let route_def = convert_trait_fn_to_route_def(f)?;
            routes.push(route_def);

            // remove zeeland attributes from the function
            let mut f = f.clone();
            f.sig.inputs.iter_mut().for_each(|input| match input {
                syn::FnArg::Typed(arg) => {
                    arg.attrs.retain(|attr| !attr.path().is_ident("zeeland"));
                }
                syn::FnArg::Receiver(_) => {}
            });
            f.sig.inputs.insert(0, parse_quote!(&self));
            if f.sig.asyncness.is_none() {
                f.sig.asyncness = Some(parse_quote! {async});
            }

            let ret_ty = match &f.sig.output {
                syn::ReturnType::Type(_, ty) => (**ty).clone(),
                syn::ReturnType::Default => parse_quote! {()},
            };

            f.sig.output = parse_quote! {-> ::core::result::Result<#ret_ty, Self::Error>};

            impl_fns.push(f.clone());
            f.default = None;

            fns.push(f);
        } else {
            return Err(syn::Error::new_spanned(
                item,
                "Zeeland traits can only have function definitions",
            ));
        }
    }
    let name = &input.ident;

    let make_server_impls_macro_name = format_ident!("{}_make_server_impls", name);
    let impl_name = format_ident!("{}_impl", name);
    let impl_wrapper_name = format_ident!("{}_impl_wrapper", name);
    let vis = &input.vis;

    let other_err: syn::Type = parse_quote! {Error};

    let mut result = quote! {
        #[::zeeland::async_trait]
        #vis trait #name: Send + Sync + 'static {
            type Error: ::std::error::Error + Send + Sync + 'static;

            #(
                #fns
            )*
        }

        #[macro_export]
        macro_rules! #make_server_impls_macro_name {
            () => {
                #[derive(::zeeland::thiserror::Error, Debug)]
                #[error("internal error")]
                struct #other_err;

                impl<'r, 'o: 'r> rocket::response::Responder<'r, 'o> for #other_err {
                    fn respond_to(self, _: &'r rocket::Request) -> rocket::response::Result<'o> {
                        Err(rocket::http::Status::InternalServerError)
                    }
                }

                #[derive(::zeeland::thiserror::Error, Debug)]
                enum __zeeland_Error {
                    #[error("other error: {0}")]
                    Other(#[from] #other_err),
                }

                impl<'r, 'o: 'r> rocket::response::Responder<'r, 'o> for __zeeland_Error where #other_err: rocket::response::Responder<'r, 'o> {
                    fn respond_to(self, req: &'r rocket::Request) -> rocket::response::Result<'o> {
                        match self {
                            __zeeland_Error::Other(other) => other.respond_to(req),
                        }
                    }
                }

                #[allow(non_camel_case_types)]
                struct #impl_name;

                #[::zeeland::async_trait]
                impl #name for #impl_name {
                    type Error = __zeeland_Error;

                    #(
                        #impl_fns
                    )*
                }

                #[allow(non_camel_case_types)]
                struct #impl_wrapper_name(pub Box<dyn #name<Error=__zeeland_Error>>);
            };
        }
    };

    let mut rocket_code = quote! {};

    let mut routes_list = vec![];

    for route in &routes {
        let (route_impl, name) =
            create_rocket_route(&impl_wrapper_name, parse_quote!{<#impl_name as #name>::Error}, route, |name| quote! {#name.0})?;
        rocket_code.extend(route_impl);
        routes_list.push(name);
    }

    rocket_code.extend(quote! {
        #[rocket::launch]
        fn rocket() -> _ {
            rocket::build().mount("/", rocket::routes![#(#routes_list),*]).manage(#impl_wrapper_name(Box::new(#impl_name)))
        }
    });

    let macro_name = format_ident!("{}_rocket", name);

    result.extend(quote! {
        #[macro_export]
        macro_rules! #macro_name {
            () => {
                #rocket_code
            };
        }
    });

    Ok(result)
}

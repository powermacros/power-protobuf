use convert_case::Case;
use proc_macro2::TokenStream;
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{punctuated::Punctuated, spanned::Spanned, Ident, LitStr};
use syn_prelude::{ToIdent, ToIdentWithCase, ToLitStr, WithPrefix, WithSuffix};

use crate::{
    model::{
        DeclIndex, EnumValue, Enumeration, Field, FieldType, GetOption, Message, MessageElement,
        Method, Modifier, NestedTypeIndex, OneOf, Package, ProtobufConstant, ProtobufOption,
        ProtobufPath, Protocol, Service,
    },
    resolve::{GoogleBultin, InnerType, ProtocolInsideType, ResolvedType},
};

impl ToTokens for Protocol {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let decls = self
            .decls
            .iter()
            .map(|index| match index {
                DeclIndex::Message(idx) => self.messages.get(*idx).map(Message::to_tokens),
                DeclIndex::Enum(idx) => self.enums.get(*idx).map(Enumeration::to_tokens),
                DeclIndex::Service(idx) => {
                    self.services.get(*idx).map(|s| s.to_tokens(&self.package))
                }
            })
            .collect::<Vec<_>>();
        if let Some(Package { package }) = &self.package {
            let package = package.to_ident_with_case(Case::Snake);
            tokens.append_all(quote! {
                pub mod #package {
                    #(#decls)*
                }
            })
        } else {
            tokens.append_all(quote! {
                #(#decls)*
            })
        }
    }
}

impl Message {
    fn to_tokens(&self) -> TokenStream {
        let Self {
            struct_name,
            nested_mod_name,
            messages,
            enums,
            fields,
            nested_types,
            ..
        } = self;

        let field_tokens = fields.iter().map(|field| match field {
            MessageElement::Field(field) => field.to_tokens(),
            MessageElement::OneOf(OneOf {
                field_name,
                enum_name: type_name,
                field_lit,
                tags,
                options,
                ..
            }) => {
                let deprecated = options.deprecated();
                quote! {
                    #deprecated
                    #[prost(oneof=#field_lit, tags=#tags)]
                    pub #field_name: #nested_mod_name::#type_name
                }
            }
        });

        let nested = if !nested_types.is_empty() {
            let nested = nested_types
                .iter()
                .filter_map(|i| match i {
                    NestedTypeIndex::Message(idx) => {
                        messages.get(*idx).map(|mesage| mesage.to_tokens())
                    }
                    NestedTypeIndex::Enum(idx) => {
                        enums.get(*idx).map(|enumeration| enumeration.to_tokens())
                    }
                    NestedTypeIndex::Oneof(idx) => fields
                        .get(*idx)
                        .map(|f| match f {
                            MessageElement::Field(_) => None,
                            MessageElement::OneOf(oneof) => Some(oneof.to_tokens()),
                        })
                        .flatten(),
                })
                .collect::<Vec<_>>();

            Some(quote! {
                pub mod #nested_mod_name {
                    #(#nested)*
                }
            })
        } else {
            None
        };

        let mut derives = vec![quote!(Clone), quote!(PartialEq), quote!(prost::Message)];
        if cfg!(feature = "derive_serde") {
            derives.push(quote!(serde::Deserialize));
            derives.push(quote!(serde::Serialize));
        };

        quote! {
            #[allow(clippy::derive_partial_eq_without_eq)]
            #[derive(#(#derives),*)]
            pub struct #struct_name {
                #(#field_tokens),*
            }

            #nested
        }
    }
}

impl Field {
    fn to_tokens(&self) -> TokenStream {
        let Field {
            field_name,
            modifier,
            typ,
            tag: number,
            options,
            ..
        } = self;
        let deprecated = options.deprecated();
        let tag = number.to_lit_str();

        let field_type = typ.to_tokens(Some(options));
        let mut optional = false;
        let mut repeated = false;
        let field_type = if let Some(modifier) = modifier {
            match modifier {
                Modifier::Optional => {
                    optional = true;
                    quote!(Option<#field_type>)
                }
                Modifier::Repeated => {
                    repeated = true;
                    quote!(::prost::alloc::vec::Vec<#field_type>)
                }
                Modifier::Required => field_type,
            }
        } else {
            field_type
        };

        let mut prost_args = vec![];
        if let Some(ty) = typ.to_tag() {
            prost_args.push(ty);
        }
        if optional {
            prost_args.push(quote!(optional));
        } else if repeated {
            prost_args.push(quote!(repeated));
        }
        prost_args.push(quote!(tag=#tag));

        quote! {
            #deprecated
            #[prost(#(#prost_args),*)]
            pub #field_name: #field_type
        }
    }
}

impl Service {
    fn to_tokens(&self, package: &Option<Package>) -> TokenStream {
        let Self { name, .. } = self;

        let service_name = if let Some(package) = package.as_ref() {
            name.to_lit_str().with_prefix(package.package.to_string())
        } else {
            name.to_lit_str()
        };

        let client = self.impl_client(&service_name);
        let server = self.impl_server(&service_name);

        quote! {
            #client
            #server
        }
    }

    fn impl_client(&self, service_name: &LitStr) -> TokenStream {
        let client_mod = self
            .name
            .to_ident_with_case(Case::Snake)
            .with_suffix("_client");

        let client_name = self.code_name.with_suffix("Client");
        let methods = self
            .methods
            .iter()
            .map(|m| m.impl_client_method(service_name))
            .collect::<Vec<_>>();

        quote! {
            pub mod #client_mod {
                #![allow(unused_variables, dead_code, missing_docs, clippy::let_unit_value)]
                use tonic::codegen::*;
                use tonic::codegen::http::Uri;

                pub struct #client_name<T> {
                    inner: tonic::client::Grpc<T>,
                }

                impl #client_name<tonic::transport::Channel> {
                    pub async fn connect<D>(dst: D) -> Result<Self, tonic::transport::Error>
                    where
                        D: TryInto<tonic::transport::Endpoint>,
                        D::Error: Into<StdError>,
                    {
                        let conn = tonic::transport::Endpoint::new(dst)?.connect().await?;
                        Ok(Self::new(conn))
                    }
                }

                impl<T> #client_name<T>
                where
                    T: tonic::client::GrpcService<tonic::body::BoxBody>,
                    T::Error: Into<StdError>,
                    T::ResponseBody: Body<Data = Bytes> + Send + 'static,
                    <T::ResponseBody as Body>::Error: Into<StdError> + Send,
                {
                    pub fn new(inner: T) -> Self {
                        let inner = tonic::client::Grpc::new(inner);
                        Self { inner }
                    }
                    pub fn with_origin(inner: T, origin: Uri) -> Self {
                        let inner = tonic::client::Grpc::with_origin(inner, origin);
                        Self { inner }
                    }
                    pub fn with_interceptor<F>(
                        inner: T,
                        interceptor: F,
                    ) -> #client_name<InterceptedService<T, F>>
                    where
                        F: tonic::service::Interceptor,
                        T::ResponseBody: Default,
                        T: tonic::codegen::Service<
                            http::Request<tonic::body::BoxBody>,
                            Response = http::Response<
                                <T as tonic::client::GrpcService<tonic::body::BoxBody>>::ResponseBody,
                            >,
                        >,
                        <T as tonic::codegen::Service<
                            http::Request<tonic::body::BoxBody>,
                        >>::Error: Into<StdError> + Send + Sync,
                    {
                        #client_name::new(InterceptedService::new(inner, interceptor))
                    }
                    /// Compress requests with the given encoding.
                    ///
                    /// This requires the server to support it otherwise it might respond with an
                    /// error.
                    #[must_use]
                    pub fn send_compressed(mut self, encoding: CompressionEncoding) -> Self {
                        self.inner = self.inner.send_compressed(encoding);
                        self
                    }
                    /// Enable decompressing responses.
                    #[must_use]
                    pub fn accept_compressed(mut self, encoding: CompressionEncoding) -> Self {
                        self.inner = self.inner.accept_compressed(encoding);
                        self
                    }
                    /// Limits the maximum size of a decoded message.
                    ///
                    /// Default: `4MB`
                    #[must_use]
                    pub fn max_decoding_message_size(mut self, limit: usize) -> Self {
                        self.inner = self.inner.max_decoding_message_size(limit);
                        self
                    }
                    /// Limits the maximum size of an encoded message.
                    ///
                    /// Default: `usize::MAX`
                    #[must_use]
                    pub fn max_encoding_message_size(mut self, limit: usize) -> Self {
                        self.inner = self.inner.max_encoding_message_size(limit);
                        self
                    }

                    #(#methods)*
                }
            }
        }
    }

    fn impl_server(&self, service_name: &LitStr) -> TokenStream {
        let Self {
            name,
            code_name,
            methods,
            ..
        } = self;

        let server_mod = name.to_ident_with_case(Case::Snake).with_suffix("_server");
        let trait_methods = methods
            .iter()
            .map(|m| m.impl_server_trait_method())
            .collect::<Vec<_>>();

        let server_name = code_name.with_suffix("Server");
        let svc_cases = methods
            .iter()
            .map(|m| m.impl_server_method_switch(service_name, code_name))
            .collect::<Vec<_>>();

        quote! {
            pub mod #server_mod {
                #![allow(unused_variables, dead_code, missing_docs, clippy::let_unit_value)]
                use tonic::codegen::*;

                #[async_trait]
                pub trait #code_name: Send + Sync + 'static {
                    #(#trait_methods)*
                }

                #[derive(Debug)]
                pub struct #server_name<T: #code_name> {
                    inner: _Inner<T>,
                    accept_compression_encodings: EnabledCompressionEncodings,
                    send_compression_encodings: EnabledCompressionEncodings,
                    max_decoding_message_size: Option<usize>,
                    max_encoding_message_size: Option<usize>,
                }

                struct _Inner<T>(Arc<T>);

                impl<T: #code_name> #server_name<T> {
                    pub fn new(inner: T) -> Self {
                        Self::from_arc(Arc::new(inner))
                    }
                    pub fn from_arc(inner: Arc<T>) -> Self {
                        let inner = _Inner(inner);
                        Self {
                            inner,
                            accept_compression_encodings: Default::default(),
                            send_compression_encodings: Default::default(),
                            max_decoding_message_size: None,
                            max_encoding_message_size: None,
                        }
                    }
                    pub fn with_interceptor<F>(
                        inner: T,
                        interceptor: F,
                    ) -> InterceptedService<Self, F>
                    where
                        F: tonic::service::Interceptor,
                    {
                        InterceptedService::new(Self::new(inner), interceptor)
                    }
                    /// Enable decompressing requests with the given encoding.
                    #[must_use]
                    pub fn accept_compressed(mut self, encoding: CompressionEncoding) -> Self {
                        self.accept_compression_encodings.enable(encoding);
                        self
                    }
                    /// Compress responses with the given encoding, if the client supports it.
                    #[must_use]
                    pub fn send_compressed(mut self, encoding: CompressionEncoding) -> Self {
                        self.send_compression_encodings.enable(encoding);
                        self
                    }
                    /// Limits the maximum size of a decoded message.
                    ///
                    /// Default: `4MB`
                    #[must_use]
                    pub fn max_decoding_message_size(mut self, limit: usize) -> Self {
                        self.max_decoding_message_size = Some(limit);
                        self
                    }
                    /// Limits the maximum size of an encoded message.
                    ///
                    /// Default: `usize::MAX`
                    #[must_use]
                    pub fn max_encoding_message_size(mut self, limit: usize) -> Self {
                        self.max_encoding_message_size = Some(limit);
                        self
                    }
                }
                impl<T, B> tonic::codegen::Service<http::Request<B>> for #server_name<T>
                where
                    T: #code_name,
                    B: Body + Send + 'static,
                    B::Error: Into<StdError> + Send + 'static,
                {
                    type Response = http::Response<tonic::body::BoxBody>;
                    type Error = std::convert::Infallible;
                    type Future = BoxFuture<Self::Response, Self::Error>;
                    fn poll_ready(
                        &mut self,
                        _cx: &mut Context<'_>,
                    ) -> Poll<std::result::Result<(), Self::Error>> {
                        Poll::Ready(Ok(()))
                    }
                    fn call(&mut self, req: http::Request<B>) -> Self::Future {
                        let inner = self.inner.clone();
                        match req.uri().path() {
                            #(#svc_cases)*
                            _ => {
                                Box::pin(async move {
                                    Ok(
                                        http::Response::builder()
                                            .status(200)
                                            .header("grpc-status", "12")
                                            .header("content-type", "application/grpc")
                                            .body(empty_body())
                                            .unwrap(),
                                    )
                                })
                            }
                        }
                    }
                }
                impl<T: #code_name> Clone for #server_name<T> {
                    fn clone(&self) -> Self {
                        let inner = self.inner.clone();
                        Self {
                            inner,
                            accept_compression_encodings: self.accept_compression_encodings,
                            send_compression_encodings: self.send_compression_encodings,
                            max_decoding_message_size: self.max_decoding_message_size,
                            max_encoding_message_size: self.max_encoding_message_size,
                        }
                    }
                }
                impl<T: #code_name> Clone for _Inner<T> {
                    fn clone(&self) -> Self {
                        Self(Arc::clone(&self.0))
                    }
                }
                impl<T: std::fmt::Debug> std::fmt::Debug for _Inner<T> {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        write!(f, "{:?}", self.0)
                    }
                }
                impl<T: #code_name> tonic::server::NamedService for #server_name<T> {
                    const NAME: &'static str = #service_name;
                }
            }
        }
    }
}

impl Method {
    fn impl_client_method(&self, service_name: &LitStr) -> TokenStream {
        let Self {
            method_name,
            input_type,
            input_type_ref,
            output_type,
            output_type_ref,
            ..
        } = self;

        let input_type = Self::param_type(input_type, input_type_ref);
        let output_type = Self::param_type(output_type, output_type_ref);

        let method_name_lit = method_name
            .to_ident_with_case(Case::UpperCamel)
            .to_lit_str();
        let request_path = method_name_lit.with_prefix(format!("/{}/", service_name.value()));

        quote! {
            pub async fn #method_name(&mut self, request: impl tonic::IntoRequest<#input_type>) -> std::result::Result<tonic::Response<#output_type>, tonic::Status> {
                self.inner
                    .ready()
                    .await
                    .map_err(|e| {
                        tonic::Status::new(
                            tonic::Code::Unknown,
                            format!("Service was not ready: {}", e.into()),
                        )
                    })?;
                let codec = tonic::codec::ProstCodec::default();
                let path = http::uri::PathAndQuery::from_static(#request_path);
                let mut req = request.into_request();
                req.extensions_mut().insert(GrpcMethod::new(#service_name, #method_name_lit));
                self.inner.unary(req, path, codec).await
            }
        }
    }

    fn impl_server_trait_method(&self) -> TokenStream {
        let Self {
            method_name,
            input_type,
            input_type_ref,
            output_type,
            output_type_ref,
            ..
        } = self;

        let input_type = Self::param_type(input_type, input_type_ref);
        let output_type = Self::param_type(output_type, output_type_ref);

        quote! {
            async fn #method_name(
                &self,
                request: tonic::Request<#input_type>,
            ) -> std::result::Result<
                tonic::Response<#output_type>,
                tonic::Status,
            >;
        }
    }

    fn impl_server_method_switch(
        &self,
        service_name: &LitStr,
        server_trait: &Ident,
    ) -> TokenStream {
        let Self {
            name, method_name, ..
        } = self;
        let case_lit = service_name
            .with_prefix("/")
            .with_suffix("/")
            .with_suffix(name.to_ident_with_case(Case::UpperCamel).to_string());

        let struct_name = name.to_ident_with_case(Case::Camel).with_suffix("Svc");

        let input_type = Self::param_type(&self.input_type, &self.input_type_ref);
        let output_type = Self::param_type(&self.output_type, &self.output_type_ref);

        quote! {
            #case_lit => {
                #[allow(non_camel_case_types)]
                struct #struct_name<T: #server_trait>(pub Arc<T>);
                impl<T: #server_trait> tonic::server::UnaryService<#input_type>
                for #struct_name<T> {
                    type Response = #output_type;
                    type Future = BoxFuture<
                        tonic::Response<Self::Response>,
                        tonic::Status,
                    >;
                    fn call(
                        &mut self,
                        request: tonic::Request<#input_type>,
                    ) -> Self::Future {
                        let inner = Arc::clone(&self.0);
                        let fut = async move {
                            <T as #server_trait>::#method_name(&inner, request).await
                        };
                        Box::pin(fut)
                    }
                }
                let accept_compression_encodings = self.accept_compression_encodings;
                let send_compression_encodings = self.send_compression_encodings;
                let max_decoding_message_size = self.max_decoding_message_size;
                let max_encoding_message_size = self.max_encoding_message_size;
                let inner = self.inner.clone();
                let fut = async move {
                    let inner = inner.0;
                    let method = #struct_name(inner);
                    let codec = tonic::codec::ProstCodec::default();
                    let mut grpc = tonic::server::Grpc::new(codec)
                        .apply_compression_config(
                            accept_compression_encodings,
                            send_compression_encodings,
                        )
                        .apply_max_message_size_config(
                            max_decoding_message_size,
                            max_encoding_message_size,
                        );
                    let res = grpc.unary(method, req).await;
                    Ok(res)
                };
                Box::pin(fut)
            }
        }
    }

    fn param_type(typ: &ProtobufPath, resolved_type: &ResolvedType) -> TokenStream {
        if typ.is_local() {
            quote!(super::#typ)
        } else if typ.is_relative() {
            if let ResolvedType::External(x) = resolved_type {
                let segs = x
                    .type_path_segments
                    .iter()
                    .map(|s| (s, typ.span()).to_ident())
                    .collect::<Vec<_>>();
                quote!(#(#segs)::*)
            } else {
                unreachable!()
            }
        } else {
            typ.to_token_stream()
        }
    }
}

impl Enumeration {
    fn to_tokens(&self) -> TokenStream {
        let Self { name, values, .. } = self;
        let variants = values
            .iter()
            .map(
                |EnumValue {
                     variant_name,
                     tag: number,
                     ..
                 }| {
                    quote! { #variant_name = #number }
                },
            )
            .collect::<Vec<_>>();

        let name = name.to_ident_with_case(Case::UpperCamel);

        let as_str_cases = values
            .iter()
            .map(
                |EnumValue {
                     variant_name,
                     proto_name,
                     ..
                 }| {
                    quote! {Self::#variant_name => #proto_name}
                },
            )
            .collect::<Vec<_>>();

        let from_str_cases = values.iter().map(
            |EnumValue {
                 variant_name,
                 proto_name,
                 ..
             }| {
                quote! { #proto_name => Some(Self::#variant_name) }
            },
        );

        let mut derives = vec![
            quote!(Clone),
            quote!(Copy),
            quote!(Debug),
            quote!(Eq),
            quote!(PartialEq),
            quote!(PartialOrd),
            quote!(Ord),
            quote!(Hash),
            quote!(prost::Enumeration),
        ];
        if cfg!(feature = "derive_serde") {
            derives.push(quote!(serde::Deserialize));
            derives.push(quote!(serde::Serialize));
        }

        quote! {
            #[derive(#(#derives),*)]
            #[repr(i32)]
            pub enum #name {
                #(#variants),*
            }

            impl #name {
                /// String value of the enum field names used in the ProtoBuf definition.
                ///
                /// The values are not transformed in any way and thus are considered stable
                ///(if the ProtoBuf definition does not change) and safe for programmatic use.
                pub fn as_str_name(&self) -> &'static str {
                    match self {
                        #(#as_str_cases),*
                    }
                }

                /// Creates an enum from field names used in the ProtoBuf definition.
                pub fn from_str_name(value: &str) -> Option<Self> {
                    match value {
                        #(#from_str_cases),*
                        _ => None,
                    }
                }
            }

        }
    }
}

impl OneOf {
    fn to_tokens(&self) -> TokenStream {
        let Self {
            enum_name, fields, ..
        } = self;
        let field_tokens = fields.iter().map(Field::to_tokens).collect::<Vec<_>>();

        let mut derives = vec![quote!(Clone), quote!(PartialEq), quote!(prost::OneOf)];
        if cfg!(feature = "derive_serde") {
            derives.push(quote!(serde::Deserialize));
            derives.push(quote!(serde::Serialize));
        }

        quote! {
            #[allow(clippy::derive_partial_eq_without_eq)]
            #[derive(#(#derives),*)]
            pub enum #enum_name {
                #(#field_tokens),*
            }
        }
    }
}

impl ToTokens for ProtobufPath {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let idents = self.segments.iter().collect::<Vec<_>>();
        tokens.append_all(quote!(#(#idents)::*))
    }
}

impl FieldType {
    fn to_tokens(&self, options: Option<&Vec<ProtobufOption>>) -> TokenStream {
        match self {
            FieldType::Int32(span) => Ident::new("i32", *span).to_token_stream(),
            FieldType::Int64(span) => Ident::new("i64", *span).to_token_stream(),
            FieldType::Uint32(span) => Ident::new("u32", *span).to_token_stream(),
            FieldType::Uint64(span) => Ident::new("u64", *span).to_token_stream(),
            FieldType::Sint32(span) => Ident::new("i32", *span).to_token_stream(),
            FieldType::Sint64(span) => Ident::new("i64", *span).to_token_stream(),
            FieldType::Bool(span) => Ident::new("bool", *span).to_token_stream(),
            FieldType::Fixed32(span) => Ident::new("u32", *span).to_token_stream(),
            FieldType::Sfixed32(span) => Ident::new("i32", *span).to_token_stream(),
            FieldType::Fixed64(span) => Ident::new("u64", *span).to_token_stream(),
            FieldType::Sfixed64(span) => Ident::new("i64", *span).to_token_stream(),
            FieldType::Float(span) => Ident::new("f32", *span).to_token_stream(),
            FieldType::Double(span) => Ident::new("f64", *span).to_token_stream(),
            FieldType::String(span) => quote_spanned!(*span => ::prost::alloc::string::String),
            FieldType::Bytes(span) => quote_spanned!(*span => ::prost::alloc::vec::Vec<u8>),
            FieldType::MessageOrEnum(ty) => {
                let span = ty.type_path.span();
                match &ty.resolved_type {
                    ResolvedType::External(_external) => {
                        let idents = ty.type_path.segments.iter().collect::<Vec<_>>();
                        quote!(#(#idents)::*)
                    }
                    ResolvedType::ProtocolInside(ProtocolInsideType { name, .. }) => {
                        name.to_token_stream()
                    }
                    ResolvedType::Inner(InnerType {
                        inner_mod_name,
                        inner_type_name,
                        ..
                    }) => {
                        quote!(#inner_mod_name::#inner_type_name)
                    }
                    ResolvedType::Google(google) => match google {
                        GoogleBultin::Any => todo!(),
                        GoogleBultin::Empty => quote_spanned!(span => ()),
                        GoogleBultin::Timestamp => todo!(),
                        GoogleBultin::Duration => todo!(),
                        GoogleBultin::Struct => todo!(),
                        GoogleBultin::Value => todo!(),
                        GoogleBultin::Null => todo!(),
                        GoogleBultin::List => todo!(),
                        GoogleBultin::Type => todo!(),
                        GoogleBultin::Field => todo!(),
                        GoogleBultin::Enum => todo!(),
                        GoogleBultin::EnumValue => todo!(),
                        GoogleBultin::Option => todo!(),
                        GoogleBultin::Api => todo!(),
                        GoogleBultin::Method => todo!(),
                        GoogleBultin::Mixin => todo!(),
                        GoogleBultin::FieldMask => todo!(),
                        GoogleBultin::Double | GoogleBultin::Float => {
                            Ident::new("f64", span).to_token_stream()
                        }
                        GoogleBultin::Int64 => Ident::new("i64", span).to_token_stream(),
                        GoogleBultin::Uint64 => Ident::new("u64", span).to_token_stream(),
                        GoogleBultin::Int32 => Ident::new("i32", span).to_token_stream(),
                        GoogleBultin::Uint32 => Ident::new("u32", span).to_token_stream(),
                        GoogleBultin::Bool => Ident::new("bool", span).to_token_stream(),
                        GoogleBultin::String => {
                            quote_spanned!(span => ::prost::alloc::string::String)
                        }
                        GoogleBultin::Bytes => quote_spanned!(span => ::prost::alloc::vec::Vec<u8>),
                    },
                    ResolvedType::Unresolved => unreachable!(),
                }
            }
            FieldType::Map(map) => {
                let key_type = map.key.as_ref().to_tokens(None);
                let value_type = map.value.as_ref().to_tokens(None);

                let opt = options
                    .map(|opts| opts.iter().find(|opt| opt.name.is_option("map_type")))
                    .flatten();

                if let Some(ProtobufOption {
                    value: ProtobufConstant::String(value),
                    ..
                }) = opt
                {
                    let opt = value.value();
                    if opt.eq("hash") || opt.eq("HashMap") {
                        quote_spanned!(value.span() => std::collections::HashMap<#key_type, #value_type>)
                    } else if opt.eq("btree_map") || opt.eq("BTreeMap") {
                        quote_spanned!(value.span() => std::collections::BTreeMap<#key_type, #value_type>)
                    } else {
                        // default is hash map
                        quote_spanned!(value.span() => std::collections::HashMap<#key_type, #value_type>)
                    }
                } else {
                    quote!(std::collections::HashMap<#key_type, #value_type>)
                }
            }
            FieldType::Group(_group) => todo!(),
        }
    }

    fn to_tag(&self) -> Option<TokenStream> {
        Some(match self {
            Self::Int32(span) => ("int32", *span).to_ident().to_token_stream(),
            Self::Int64(span) => ("int64", *span).to_ident().to_token_stream(),
            Self::Uint32(span) => ("uint32", *span).to_ident().to_token_stream(),
            Self::Uint64(span) => ("uint64", *span).to_ident().to_token_stream(),
            Self::Sint32(span) => ("sint32", *span).to_ident().to_token_stream(),
            Self::Sint64(span) => ("sint64", *span).to_ident().to_token_stream(),
            Self::Bool(span) => ("bool", *span).to_ident().to_token_stream(),
            Self::Fixed64(span) => ("fixed64", *span).to_ident().to_token_stream(),
            Self::Sfixed64(span) => ("sfixed64", *span).to_ident().to_token_stream(),
            Self::Double(span) => ("double", *span).to_ident().to_token_stream(),
            Self::String(span) => ("string", *span).to_ident().to_token_stream(),
            Self::Bytes(span) => ("bytes", *span).to_ident().to_token_stream(),
            Self::Fixed32(span) => ("fixed32", *span).to_ident().to_token_stream(),
            Self::Sfixed32(span) => ("sfixed32", *span).to_ident().to_token_stream(),
            Self::Float(span) => ("float", *span).to_ident().to_token_stream(),
            Self::MessageOrEnum(ty) => match &ty.resolved_type {
                ResolvedType::External(x) => {
                    if x.is_message {
                        ("message", ty.type_path.span())
                            .to_ident()
                            .to_token_stream()
                    } else {
                        let e = (x.type_name.as_str(), ty.type_path.span()).to_lit_str();
                        quote!(enumeration = #e)
                    }
                }
                ResolvedType::ProtocolInside(inside) => {
                    if inside.is_message {
                        ("message", inside.name.span()).to_ident().to_token_stream()
                    } else {
                        let e = inside.name.to_lit_str();
                        quote!(enumeration = #e)
                    }
                }
                ResolvedType::Inner(inner) => {
                    if inner.is_message {
                        ("message", inner.inner_type_name.span())
                            .to_ident()
                            .to_token_stream()
                    } else {
                        let e = inner.inner_type_name.to_lit_str();
                        quote!(enumeration = #e)
                    }
                }
                ResolvedType::Google(_) => ("message", ty.type_path.span())
                    .to_ident()
                    .to_token_stream(),
                ResolvedType::Unresolved => unimplemented!(),
            },
            Self::Group(g) => ("group", g.name.span()).to_ident().to_token_stream(),
            Self::Map(_) => return None,
        })
    }
}

trait Deprecated: GetOption {
    fn deprecated(&self) -> Option<TokenStream> {
        self.get_option("deprecated")
            .map(|opt| {
                if let ProtobufOption {
                    value: ProtobufConstant::Bool(value),
                    ..
                } = opt
                {
                    if value.value() {
                        Some(quote!(#[deprecated]))
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .flatten()
    }
}

impl Deprecated for Vec<ProtobufOption> {}
impl<P> Deprecated for Punctuated<ProtobufOption, P> {}

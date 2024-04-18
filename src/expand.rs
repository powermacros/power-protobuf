use convert_case::Case;
use proc_macro2::TokenStream;
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{punctuated::Punctuated, spanned::Spanned, Ident};
use syn_prelude::{ToIdent, ToIdentWithCase, ToLitStr};

use crate::{
    model::{
        DeclIndex, EnumValue, Enumeration, Field, FieldType, GetOption, Message, MessageElement,
        Method, Modifier, NestedTypeIndex, OneOf, ProtobufConstant, ProtobufOption, ProtobufPath,
        Protocol, Service,
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
                DeclIndex::Service(idx) => self.services.get(*idx).map(Service::to_tokens),
            })
            .collect::<Vec<_>>();
        tokens.append_all(quote! {
            #(#decls)*
        })
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
                    #field_name: #nested_mod_name::#type_name
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
                mod #nested_mod_name {
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
                    quote!(Vec<#field_type>)
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
            #field_name: #field_type
        }
    }
}

impl Service {
    fn to_tokens(&self) -> TokenStream {
        let Self {
            code_name: trait_name,
            methods,
            ..
        } = self;

        let methods = methods
            .iter()
            .map(
                |Method {
                     method_name,
                     input_type,
                     output_type,
                     options,
                     ..
                 }| {
                    let deprecated = options.deprecated();
                    quote! {
                        #deprecated
                        fn #method_name(_: #input_type) -> #output_type;
                    }
                },
            )
            .collect::<Vec<_>>();

        quote! {
            trait #trait_name {
                #(#methods)*
            }
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
            FieldType::Group(group) => todo!(),
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
                // FIXME: google
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

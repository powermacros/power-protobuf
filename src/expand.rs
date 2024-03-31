use core::num;
use std::{collections::HashMap, process::id};

use convert_case::Case;
use proc_macro2::TokenStream;
use quote::{quote, quote_spanned, ToTokens, TokenStreamExt};
use syn::{
    punctuated::Punctuated, spanned::Spanned, AngleBracketedGenericArguments, Ident, LitStr,
    PathArguments, PathSegment, Token,
};
use syn_prelude::{ToIdentWithCase, ToLitStr};

use crate::model::{
    DeclIndex, EnumValue, Enumeration, Field, FieldOrOneOf, FieldType, GetOption, Message, Method,
    Modifier, NestedTypeIndex, OneOf, ProtobufConstant, ProtobufOption, ProtobufOptionName,
    ProtobufPath, Protocol, Service,
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
            FieldOrOneOf::Field(Field {
                field_name,
                modifier,
                typ,
                number,
                options,
                ..
            }) => {
                let deprecated = options.deprecated();
                let typ = typ.to_tokens(Some(options));
                let tag = (number.base10_digits(), number.span()).to_lit_str();

                let typ = if let Some(modifier) = modifier {
                    match modifier {
                        Modifier::Optional => quote!(Option<#typ>),
                        Modifier::Repeated => quote!(Vec<#typ>),
                        Modifier::Required => typ,
                    }
                } else {
                    typ
                };

                quote! {
                    #deprecated
                    #[prost(tag=#tag)]
                    #field_name: #typ
                }
            }
            FieldOrOneOf::OneOf(OneOf {
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
                            FieldOrOneOf::Field(_) => None,
                            FieldOrOneOf::OneOf(oneof) => Some(oneof.to_tokens()),
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

        quote! {
            #[derive(prost::Message)]
            pub struct #struct_name {
                #(#field_tokens),*
            }

            #nested
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
                     number,
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

        quote! {
            #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, PartialOrd, Ord, prost::Enumeration)]
            #[repr(i32)]
            pub enum #name {
                #(#variants),*
            }

            impl #name {
                /// String value of the enum field names used in the ProtoBuf definition.
                ///
                /// The values are not transformed in any way and thus are considered stable
                ///(if the ProtoBuf definition does not change) and safe for programmatic use.
                pub fn  as_str_name(&self) -> &'static str {
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
        let field_tokens = fields
            .iter()
            .map(
                |Field {
                     field_name,
                     modifier,
                     typ,
                     number,
                     options,
                     ..
                 }| {
                    let deprecated = options.deprecated();
                    let type_tag = typ.to_tag(|enum_path| None).map(|tokens| quote!(#tokens,));

                    let typ = typ.to_tokens(Some(options));
                    let tag = (number.base10_digits(), number.span()).to_lit_str();

                    let typ = if let Some(modifier) = modifier {
                        match modifier {
                            Modifier::Optional => quote!(Option<#typ>),
                            Modifier::Repeated => quote!(Vec<#typ>),
                            Modifier::Required => typ,
                        }
                    } else {
                        typ
                    };

                    quote! {
                        #deprecated
                        #[prost(#type_tag, tag=#tag)]
                        #field_name: #typ
                    }
                },
            )
            .collect::<Vec<_>>();

        quote! {
            #[allow(clippy::derive_partial_eq_without_eq)]
            #[derive(Clone, PartialEq, prost::Oneof)]
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
            FieldType::String(span) => Ident::new("String", *span).to_token_stream(),
            FieldType::Bytes(span) => quote_spanned!(*span => Vec<u8>),
            FieldType::MessageOrEnum(path) => {
                if let Some(first) = path.segments.first() {
                    if first.eq("google") {
                        for (index, ident) in path.segments.iter().enumerate() {
                            if index > 0 {
                                match index {
                                    2 => {
                                        if ident.eq("protobuf") {
                                            continue;
                                        }
                                    }
                                    3 => {
                                        // prost types refers to https://docs.rs/prost-types/latest/prost_types/index.html
                                        return match ident.to_string().as_str() {
                                            "BoolValue" => {
                                                Ident::new("bool", ident.span()).to_token_stream()
                                            }
                                            "BytesValue" => quote_spanned!(ident.span() => Vec<u8>),
                                            "DoubleValue" => {
                                                Ident::new("f64", ident.span()).to_token_stream()
                                            }
                                            "Empty" => quote_spanned!(ident.span() => ()),
                                            "FloatValue" => {
                                                Ident::new("f32", ident.span()).to_token_stream()
                                            }
                                            "Int32Value" => {
                                                Ident::new("i32", ident.span()).to_token_stream()
                                            }
                                            "Int64Value" => {
                                                Ident::new("i64", ident.span()).to_token_stream()
                                            }
                                            "StringValue" => {
                                                Ident::new("String", ident.span()).to_token_stream()
                                            }
                                            "Uint32Value" => {
                                                Ident::new("u32", ident.span()).to_token_stream()
                                            }
                                            "Uint64Value" => {
                                                Ident::new("u64", ident.span()).to_token_stream()
                                            }
                                            _ => {
                                                quote_spanned!(ident.span() => prost_types::#ident)
                                            }
                                        };
                                    }
                                    _ => {}
                                }
                                break;
                            }
                        }
                    }
                }
                let idents = path.segments.iter().collect::<Vec<_>>();
                quote!(#(#idents)::*)
            }
            FieldType::Map(map) => {
                let key_type = map.as_ref().0.to_tokens(None);
                let value_type = map.as_ref().1.to_tokens(None);

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

    fn to_tag<CheckEnum: Fn(&ProtobufPath) -> Option<String>>(
        &self,
        check_enum: CheckEnum,
    ) -> Option<LitStr> {
        Some(match self {
            Self::Int32(span) => ("int32", *span).to_lit_str(),
            Self::Int64(span) => ("int64", *span).to_lit_str(),
            Self::Uint32(span) => ("uint32", *span).to_lit_str(),
            Self::Uint64(span) => ("uint64", *span).to_lit_str(),
            Self::Sint32(span) => ("sint32", *span).to_lit_str(),
            Self::Sint64(span) => ("sint64", *span).to_lit_str(),
            Self::Bool(span) => ("bool", *span).to_lit_str(),
            Self::Fixed64(span) => ("fixed64", *span).to_lit_str(),
            Self::Sfixed64(span) => ("sfixed64", *span).to_lit_str(),
            Self::Double(span) => ("double", *span).to_lit_str(),
            Self::String(span) => ("string", *span).to_lit_str(),
            Self::Bytes(span) => ("bytes", *span).to_lit_str(),
            Self::Fixed32(span) => ("fixed32", *span).to_lit_str(),
            Self::Sfixed32(span) => ("sfixed32", *span).to_lit_str(),
            Self::Float(span) => ("float", *span).to_lit_str(),
            Self::MessageOrEnum(path) => {
                if let Some(enumeration) = check_enum(path) {
                    (format!("enumeration={}", enumeration), path.segments.span()).to_lit_str()
                } else {
                    ("message", path.segments.span()).to_lit_str()
                }
            }
            Self::Group(g) => ("group", g.name.span()).to_lit_str(),
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

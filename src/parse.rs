use std::ops::RangeInclusive;

use convert_case::Case;
use syn::{
    parse::{Parse, ParseBuffer, ParseStream},
    punctuated::Punctuated,
    token, Ident, LitBool, LitInt, LitStr, Token,
};
use syn_prelude::{
    ParseAsIdent, ToErr, ToIdentWithCase, ToSynError, TryParseAsIdent, TryParseOneOfIdents,
};

use crate::model::{
    AnyTypeUrl, DeclIndex, EnumValue, Enumeration, Extension, Field, FieldOrOneOf, FieldType,
    Group, Import, ImportVis, Message, Method, OneOf, Package, ProtobufConstant,
    ProtobufConstantMessage, ProtobufConstantMessageFieldName, ProtobufOption, ProtobufOptionName,
    ProtobufOptionNameExt, ProtobufOptionNamePart, ProtobufPath, Protocol, Rule, Service, Syntax,
};

impl Parse for Protocol {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let syntax: Syntax = input.parse()?;
        let mut protocol = Self {
            imports: Default::default(),
            syntax,
            package: None,
            messages: Default::default(),
            enums: Default::default(),
            extensions: Default::default(),
            services: Default::default(),
            options: Default::default(),
            decls: Default::default(),
        };

        let proto2 = if let Syntax::Proto2(_) = &syntax {
            true
        } else {
            false
        };

        while !input.is_empty() {
            if let Some(message) = Message::try_parse(input, proto2)? {
                protocol
                    .decls
                    .push(DeclIndex::Message(protocol.messages.len()));
                protocol.messages.push(message);
            } else if let Some(service) = Service::try_parse(input, proto2)? {
                protocol
                    .decls
                    .push(DeclIndex::Service(protocol.services.len()));
                protocol.services.push(service);
            } else if let Some(enumeration) = Enumeration::try_parse(input, proto2)? {
                protocol.decls.push(DeclIndex::Enum(protocol.enums.len()));
                protocol.enums.push(enumeration);
            } else if let Some(import) = input.parse::<OptionalImport>()?.0 {
                protocol.imports.push(import);
            } else if let Some(package) = input.parse::<OptionalPackage>()?.0 {
                protocol.package = Some(package);
            } else if let Some(ext) = Extension::try_parse(input, proto2)? {
                protocol.extensions.extend(ext);
            } else {
                input.span().to_syn_error("unexpected token").to_err()?;
            }
            input.parse::<Token![;]>()?;
        }

        protocol.syntax = syntax;

        Ok(protocol)
    }
}

impl Parse for ProtobufPath {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Self {
            segments: input.parse_terminated(Ident::parse, Token![.])?,
        })
    }
}

impl Parse for ProtobufOptionNamePart {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(token::Paren) {
            let inner: ParseBuffer;
            syn::parenthesized!(inner in input);
            Ok(Self::Ext(inner.parse()?))
        } else {
            Ok(Self::Direct(input.parse()?))
        }
    }
}

impl Parse for ProtobufOptionName {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut components = vec![input.parse()?];
        while input.peek(Token![.]) {
            input.parse::<Token![.]>()?;
            components.push(input.parse()?);
        }
        if components.len() == 1 {
            if let ProtobufOptionNamePart::Direct(n) = &components[0] {
                return Ok(ProtobufOptionName::Builtin(n.clone()));
            }
        }
        Ok(Self::Ext(ProtobufOptionNameExt(components)))
    }
}

impl Parse for ProtobufOption {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        input.parse_as_named_ident("option", false)?;
        Self::continue_to_parse(input)
    }
}

impl ProtobufOption {
    fn continue_to_parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![=]>()?;
        let value = input.parse()?;
        Ok(Self { name, value })
    }
}

impl Parse for ProtobufConstant {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(token::Brace) {
            Ok(Self::Message(input.parse()?))
        } else if input.peek(LitBool) {
            Ok(Self::Bool(input.parse()?))
        } else if input.peek(LitStr) {
            Ok(Self::String(input.parse()?))
        } else if input.peek(Token![-]) {
            Self::parse_num_lit(input, true)
        } else if input.peek(Token![+]) {
            Self::parse_num_lit(input, false)
        } else {
            Self::parse_num_lit(input, false)
        }
    }
}

impl ProtobufConstant {
    fn parse_num_lit(input: ParseStream, neg: bool) -> syn::Result<Self> {
        Ok(if input.peek(LitInt) {
            Self::U64(input.parse()?, neg)
        } else {
            Self::F64(input.parse()?, neg)
        })
    }
}

impl Parse for ProtobufConstantMessageFieldName {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(if input.peek(token::Bracket) {
            let inner: ParseBuffer;
            syn::bracketed!(inner in input);
            let n = inner.parse()?;
            if input.peek(Token![/]) {
                inner.parse::<Token![/]>()?;
                let fullname = inner.parse()?;
                Self::AnyTypeUrl(AnyTypeUrl {
                    prefix: n,
                    full_type_name: fullname,
                })
            } else {
                Self::Extension(n)
            }
        } else {
            Self::Regular(input.parse()?)
        })
    }
}

impl Parse for ProtobufConstantMessage {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut message = Self {
            fields: Default::default(),
        };
        let inner: ParseBuffer;
        syn::braced!(inner in input);
        while !inner.is_empty() {
            let name = inner.parse()?;
            let value = if inner.peek(Token![:]) {
                inner.parse::<Token![:]>()?;
                inner.parse()?
            } else {
                ProtobufConstant::Message(inner.parse()?)
            };
            message.fields.insert(name, value);
        }
        Ok(message)
    }
}

impl Parse for Syntax {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(if let Some(_) = input.try_parse_as_ident("syntax", false) {
            input.parse::<Token![=]>()?;
            let lit: LitStr = input.parse()?;
            let proto = lit.value();
            let proto = if proto.eq("proto2") {
                Self::Proto2(Some(lit.span()))
            } else if proto.eq("proto3") {
                Self::Proto3(lit.span())
            } else {
                lit.to_syn_error("unknown syntax").to_err()?
            };
            input.parse::<Token![;]>()?;
            proto
        } else {
            Self::Proto2(None)
        })
    }
}

struct OptionalImport(Option<Import>);
impl Parse for OptionalImport {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Some(i) = input.try_parse_as_ident("import", false) {
            let vis = if let Some(weak) = input.try_parse_as_ident("weak", false) {
                ImportVis::Weak(weak.span())
            } else if let Some(public) = input.try_parse_as_ident("public", false) {
                ImportVis::Public(public.span())
            } else {
                ImportVis::Default
            };
            let path = input.parse()?;
            input.parse::<Token![;]>()?;
            Ok(Self(Some(Import {
                import_token: i.span(),
                vis,
                path,
            })))
        } else {
            Ok(Self(None))
        }
    }
}

struct OptionalPackage(Option<Package>);
impl Parse for OptionalPackage {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Some(_) = input.try_parse_as_ident("package", false) {
            let package = input.parse::<Ident>()?;
            input.parse::<Token![;]>()?;
            Ok(Self(Some(Package { package })))
        } else {
            Ok(Self(None))
        }
    }
}

impl Service {
    fn try_parse(input: ParseStream, proto2: bool) -> syn::Result<Option<Self>> {
        if let Some(_) = input.try_parse_as_ident("service", false) {
            let name = input.parse_as_ident()?;
            let inner: ParseBuffer;
            syn::braced!(inner in input);
            let mut methods = vec![];
            let mut options = vec![];
            while !inner.is_empty() {
                if inner.peek(Token![;]) {
                    inner.parse::<Token![;]>()?;
                    continue;
                }
                if let Some(_) = inner.try_parse_as_ident("rpc", false) {
                    methods.push(Method::continue_to_parse(&inner, false)?);
                } else if let Some(stream) = inner.try_parse_as_ident("stream", false) {
                    if proto2 {
                        let mut method = Method::continue_to_parse(&inner, true)?;
                        method.client_streaming = Some(stream.span());
                        method.server_streaming = Some(stream.span());
                        methods.push(method);
                    }
                    stream.to_syn_error("unexpect stream in proto3").to_err()?;
                } else if let Some(_) = inner.try_parse_as_ident("option", false) {
                    let option = ProtobufOption::continue_to_parse(&inner)?;
                    options.push(option);
                }
            }
            Ok(Some(Self {
                name,
                methods,
                options,
            }))
        } else {
            Ok(None)
        }
    }
}

impl Method {
    fn continue_to_parse(input: ParseStream, proto2_stream: bool) -> syn::Result<Self> {
        let name = input.parse_as_ident()?;
        let (client_streaming, input_type) = {
            let inner: ParseBuffer;
            syn::parenthesized!(inner in input);
            let client_stream = if !proto2_stream {
                inner.try_parse_as_ident("stream", false)
            } else {
                None
            };
            let input_type: ProtobufPath = inner.parse()?;
            if !inner.is_empty() {
                inner.span().to_syn_error("expect ')'").to_err()?;
            }
            (client_stream.map(|i| i.span()), input_type)
        };
        input.parse_as_named_ident("returns", false)?;
        let (server_streaming, output_type) = {
            let inner: ParseBuffer;
            syn::parenthesized!(inner in input);
            let server_stream = if !proto2_stream {
                inner.try_parse_as_ident("stream", false)
            } else {
                None
            };
            let output_type: ProtobufPath = inner.parse()?;
            (server_stream.map(|i| i.span()), output_type)
        };

        let options = input.parse::<TryAsOptions>()?.0;

        input.parse::<Token![;]>()?;

        Ok(Self {
            name,
            client_streaming,
            input_type,
            server_streaming,
            output_type,
            options,
        })
    }
}

struct TryAsOptions(Punctuated<ProtobufOption, Token![;]>);
impl Parse for TryAsOptions {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(token::Brace) {
            let inner: ParseBuffer;
            syn::braced!(inner in input);
            inner
                .parse_terminated(ProtobufOption::parse, Token![;])
                .map(|options| Self(options))
        } else {
            Ok(Self(Punctuated::new()))
        }
    }
}

impl Message {
    fn try_parse(input: ParseStream, proto2: bool) -> syn::Result<Option<Self>> {
        if let Some(_) = input.try_parse_as_ident("message", false) {
            let name = input.parse_as_ident()?;
            let MessageBody {
                fields,
                reserved_nums,
                reserved_names,
                messages,
                enums,
                options,
                extension_ranges,
                extensions,
            } = MessageBody::parse(
                input,
                proto2,
                if proto2 {
                    MessageBodyParseMode::MessageProto2
                } else {
                    MessageBodyParseMode::MessageProto3
                },
            )?;
            Ok(Some(Self {
                name,
                fields,
                reserved_nums,
                reserved_names,
                messages,
                enums,
                options,
                extension_ranges,
                extensions,
            }))
        } else {
            Ok(None)
        }
    }
}

#[derive(Copy, Clone)]
enum MessageBodyParseMode {
    MessageProto2,
    MessageProto3,
    Oneof,
    ExtendProto2,
    ExtendProto3,
}

impl MessageBodyParseMode {
    fn to_str(&self) -> &'static str {
        match self {
            Self::MessageProto2 => "parsing message in proto2",
            Self::MessageProto3 => "parsing message in proto3",
            Self::Oneof => "parsing oneof",
            Self::ExtendProto2 => "parsing extension in proto2",
            Self::ExtendProto3 => "parsing extension in proto3",
        }
    }
}

impl MessageBodyParseMode {
    fn label_allowed(&self, label: Rule) -> bool {
        match label {
            Rule::Repeated => match *self {
                MessageBodyParseMode::MessageProto2
                | MessageBodyParseMode::MessageProto3
                | MessageBodyParseMode::ExtendProto2
                | MessageBodyParseMode::ExtendProto3 => true,
                MessageBodyParseMode::Oneof => false,
            },
            Rule::Optional => match *self {
                MessageBodyParseMode::MessageProto2 | MessageBodyParseMode::ExtendProto2 => true,
                MessageBodyParseMode::MessageProto3 | MessageBodyParseMode::ExtendProto3 => true,
                MessageBodyParseMode::Oneof => false,
            },
            Rule::Required => match *self {
                MessageBodyParseMode::MessageProto2 | MessageBodyParseMode::ExtendProto2 => true,
                MessageBodyParseMode::MessageProto3 | MessageBodyParseMode::ExtendProto3 => false,
                MessageBodyParseMode::Oneof => false,
            },
        }
    }

    fn some_label_required(&self) -> bool {
        match *self {
            MessageBodyParseMode::MessageProto2 | MessageBodyParseMode::ExtendProto2 => true,
            MessageBodyParseMode::MessageProto3
            | MessageBodyParseMode::ExtendProto3
            | MessageBodyParseMode::Oneof => false,
        }
    }

    fn map_allowed(&self) -> bool {
        match *self {
            MessageBodyParseMode::MessageProto2
            | MessageBodyParseMode::MessageProto3
            | MessageBodyParseMode::ExtendProto2
            | MessageBodyParseMode::ExtendProto3 => true,
            MessageBodyParseMode::Oneof => false,
        }
    }

    fn is_most_non_fields_allowed(&self) -> bool {
        match *self {
            MessageBodyParseMode::MessageProto2 | MessageBodyParseMode::MessageProto3 => true,
            MessageBodyParseMode::ExtendProto2
            | MessageBodyParseMode::ExtendProto3
            | MessageBodyParseMode::Oneof => false,
        }
    }

    fn is_option_allowed(&self) -> bool {
        match *self {
            MessageBodyParseMode::MessageProto2
            | MessageBodyParseMode::MessageProto3
            | MessageBodyParseMode::Oneof => true,
            MessageBodyParseMode::ExtendProto2 | MessageBodyParseMode::ExtendProto3 => false,
        }
    }

    fn is_extensions_allowed(&self) -> bool {
        match self {
            MessageBodyParseMode::MessageProto2 => true,
            _ => false,
        }
    }
}

#[derive(Default)]
pub(crate) struct MessageBody {
    pub fields: Vec<FieldOrOneOf>,
    pub reserved_nums: Vec<RangeInclusive<i32>>,
    pub reserved_names: Vec<Ident>,
    pub messages: Vec<Message>,
    pub enums: Vec<Enumeration>,
    pub options: Vec<ProtobufOption>,
    pub extension_ranges: Vec<RangeInclusive<i32>>,
    pub extensions: Vec<Extension>,
}

fn parse_range(input: ParseStream) -> syn::Result<RangeInclusive<i32>> {
    let from = input.parse::<LitInt>()?.base10_parse::<i32>()?;
    let to = if let Some(_) = input.try_parse_as_ident("to", false) {
        if let Some(_) = input.try_parse_as_ident("max", false) {
            0x20000000 - 1
        } else {
            input.parse::<LitInt>()?.base10_parse::<i32>()?
        }
    } else {
        from
    };
    Ok(from..=to)
}

fn parse_ranges(input: ParseStream) -> syn::Result<Vec<RangeInclusive<i32>>> {
    let mut ranges = vec![parse_range(input)?];
    while input.peek(Token![,]) {
        input.parse::<Token![,]>()?;
        ranges.push(parse_range(input)?);
    }
    Ok(ranges)
}

impl MessageBody {
    fn parse(input: ParseStream, proto2: bool, mode: MessageBodyParseMode) -> syn::Result<Self> {
        let mut r = MessageBody::default();
        let inner: ParseBuffer;
        syn::braced!(inner in input);
        while !inner.is_empty() {
            if inner.peek(Token![;]) {
                inner.parse::<Token![;]>()?;
                continue;
            }
            if mode.is_most_non_fields_allowed() {
                if let Some((field_nums, field_names)) = Self::try_parse_reserved(input)? {
                    inner.parse::<Token![;]>()?;
                    r.reserved_nums.extend(field_nums);
                    r.reserved_names.extend(field_names);
                    continue;
                }
                if let Some(oneof) = OneOf::try_parse(&inner, proto2)? {
                    inner.parse::<Token![;]>()?;
                    r.fields.push(FieldOrOneOf::OneOf(oneof));
                    continue;
                }
                if let Some(extensions) = Extension::try_parse(&inner, proto2)? {
                    r.extensions.extend(extensions);
                    continue;
                }
                if let Some(nested) = Message::try_parse(&inner, proto2)? {
                    r.messages.push(nested);
                    continue;
                }
                if let Some(nested) = Enumeration::try_parse(&inner, proto2)? {
                    r.enums.push(nested);
                    continue;
                }
            } else {
                if let Some(ident) = inner
                    .try_parse_one_of_idents(vec!["reserved", "oneof", "extend", "message", "enum"])
                {
                    ident
                        .to_syn_error(format!("not allowed while {}", mode.to_str()))
                        .to_err()?;
                }
            }

            if mode.is_extensions_allowed() {
                if let Some(extensions) = Extension::try_parse(&inner, proto2)? {
                    r.extensions.extend(extensions);
                    continue;
                }
            } else {
                if let Some(ident) = inner.try_parse_as_ident("extensions", false) {
                    ident
                        .to_syn_error(format!("not allowed while {}", mode.to_str()))
                        .to_err()?;
                }
            }

            if mode.is_option_allowed() {
                if let Some(_) = inner.try_parse_as_ident("option", false) {
                    r.options.push(ProtobufOption::continue_to_parse(&inner)?);
                    inner.parse::<Token![;]>()?;
                    continue;
                }
            } else {
                if let Some(ident) = inner.try_parse_as_ident("option", false) {
                    ident
                        .to_syn_error(format!("not allowed while {}", mode.to_str()))
                        .to_err()?;
                }
            }

            r.fields
                .push(FieldOrOneOf::Field(Field::parse(&inner, proto2, mode)?));
        }

        Ok(r)
    }

    fn try_parse_reserved(
        input: ParseStream,
    ) -> syn::Result<Option<(Vec<RangeInclusive<i32>>, Vec<Ident>)>> {
        Ok(
            if let Some(_) = input.try_parse_as_ident("reserved", false) {
                Some(if input.peek(LitStr) {
                    let mut names = vec![input.parse_as_ident()?];
                    while input.peek(Token![,]) {
                        input.parse::<Token![,]>()?;
                        names.push(input.parse_as_ident()?);
                    }
                    (vec![], names)
                } else {
                    (parse_ranges(input)?, vec![])
                })
            } else {
                None
            },
        )
    }
}

impl Rule {
    fn try_parse(input: ParseStream, mode: MessageBodyParseMode) -> syn::Result<Option<Self>> {
        if let Some(label) = input.try_parse_as_ident("optional", false) {
            if !mode.label_allowed(Rule::Optional) {
                label
                    .to_syn_error(format!("not allowed in {}", mode.to_str()))
                    .to_err()
            } else {
                Ok(Some(Self::Optional))
            }
        } else if let Some(label) = input.try_parse_as_ident("repeated", false) {
            if !mode.label_allowed(Rule::Repeated) {
                label
                    .to_syn_error(format!("not allowed in {}", mode.to_str()))
                    .to_err()
            } else {
                Ok(Some(Self::Repeated))
            }
        } else if let Some(label) = input.try_parse_as_ident("required", false) {
            if !mode.label_allowed(Rule::Required) {
                label
                    .to_syn_error(format!("not allowed in {}", mode.to_str()))
                    .to_err()
            } else {
                Ok(Some(Self::Required))
            }
        } else {
            Ok(None)
        }
    }
}

impl Field {
    fn parse(input: ParseStream, proto2: bool, mode: MessageBodyParseMode) -> syn::Result<Self> {
        let rule = if input.peek_as_ident("map", false) {
            if !mode.map_allowed() {
                let ident = input.parse_as_named_ident("map", false)?;
                ident
                    .to_syn_error(format!("not allowed in {}", mode.to_str()))
                    .to_err()?;
            }
            None
        } else {
            Rule::try_parse(input, mode)?
        };

        if let Some(_) = input.try_parse_as_ident("group", false) {
            let name = input.parse_as_ident()?;
            if !name
                .to_string()
                .chars()
                .next()
                .unwrap()
                .is_ascii_uppercase()
            {
                name.to_syn_error("expect Upper case group name").to_err()?;
            }
            input.parse::<Token![=]>()?;
            let number = input.parse::<LitInt>()?;
            let MessageBody { fields, .. } = MessageBody::parse(
                input,
                proto2,
                if proto2 {
                    MessageBodyParseMode::MessageProto2
                } else {
                    MessageBodyParseMode::MessageProto3
                },
            )?;
            let fields = fields
                .into_iter()
                .map(|fo| match fo {
                    FieldOrOneOf::Field(f) => Ok(f),
                    FieldOrOneOf::OneOf(o) => o
                        .name
                        .to_syn_error("unexpected 'oneof' in 'group'")
                        .to_err(),
                })
                .collect::<syn::Result<_>>()?;
            Ok(Self {
                // The field name is a lowercased version of the type name
                // (which has been verified to start with an uppercase letter).
                // https://git.io/JvxAP
                name: name.to_ident_with_case(Case::Lower),
                rule,
                typ: FieldType::Group(Group { name, fields }),
                number,
                options: Vec::new(),
            })
        } else {
            let typ = input.parse()?;
            let name = input.parse()?;
            input.parse::<Token![=]>()?;
            let number = input.parse()?;
            let mut options = vec![];
            let inner: ParseBuffer;
            syn::bracketed!(inner in input);
            while !inner.is_empty() {
                options.push(inner.parse()?);
            }
            input.parse::<Token![;]>()?;
            Ok(Self {
                name,
                rule,
                typ,
                number,
                options,
            })
        }
    }
}

impl Parse for FieldType {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if let Some(ty) = input.try_parse_one_of_idents(vec![
            "int32", "int64", "uint32", "uint64", "sint32", "sint64", "fixed32", "sfixed32",
            "fixed64", "sfixed64", "bool", "string", "bytes", "float", "double",
        ]) {
            Ok(match ty.to_string().as_str() {
                "int32" => Self::Int32(ty.span()),
                "int64" => Self::Int64(ty.span()),
                "uint32" => Self::Uint32(ty.span()),
                "uint64" => Self::Uint64(ty.span()),
                "sint32" => Self::Sint32(ty.span()),
                "sint64" => Self::Sint64(ty.span()),
                "fixed32" => Self::Fixed32(ty.span()),
                "sfixed32" => Self::Sfixed32(ty.span()),
                "fixed64" => Self::Fixed64(ty.span()),
                "sfixed64" => Self::Sfixed64(ty.span()),
                "bool" => Self::Bool(ty.span()),
                "string" => Self::String(ty.span()),
                "bytes" => Self::Bytes(ty.span()),
                "float" => Self::Float(ty.span()),
                "double" => Self::Double(ty.span()),
                _ => {
                    unreachable!()
                }
            })
        } else if let Some(_) = input.try_parse_as_ident("map", false) {
            input.parse::<Token![<]>()?;

            let key = FieldType::parse(input)?;

            input.parse::<Token![,]>()?;
            let value = FieldType::parse(input)?;

            input.parse::<Token![>]>()?;
            Ok(Self::Map(Box::new((key, value))))
        } else {
            Ok(Self::MessageOrEnum(input.parse()?))
        }
    }
}

impl OneOf {
    fn try_parse(input: ParseStream, proto2: bool) -> syn::Result<Option<Self>> {
        if let Some(_) = input.try_parse_as_ident("oneof", false) {
            let name = input.parse_as_ident()?;
            let MessageBody {
                fields, options, ..
            } = MessageBody::parse(input, proto2, MessageBodyParseMode::Oneof)?;
            let fields = fields
                .into_iter()
                .map(|fo| match fo {
                    FieldOrOneOf::Field(f) => Ok(f),
                    FieldOrOneOf::OneOf(o) => o.name.to_syn_error("oneof in oneof").to_err(),
                })
                .collect::<syn::Result<_>>()?;
            Ok(Some(OneOf {
                name,
                fields,
                options,
            }))
        } else {
            Ok(None)
        }
    }
}

impl Extension {
    fn try_parse(input: ParseStream, proto2: bool) -> syn::Result<Option<Vec<Extension>>> {
        if let Some(_) = input.try_parse_as_ident("extend", false) {
            let extendee: ProtobufPath = input.parse()?;
            let MessageBody { fields, .. } = MessageBody::parse(
                input,
                proto2,
                if proto2 {
                    MessageBodyParseMode::ExtendProto2
                } else {
                    MessageBodyParseMode::ExtendProto3
                },
            )?;

            let fields: Vec<Field> = fields
                .into_iter()
                .map(|fo| match fo {
                    FieldOrOneOf::Field(f) => Ok(f),
                    FieldOrOneOf::OneOf(o) => o.name.to_syn_error("oneof in extend").to_err(),
                })
                .collect::<syn::Result<_>>()?;

            let extensions = fields
                .into_iter()
                .map(|field| {
                    let extendee = extendee.clone();
                    let extension = Extension { extendee, field };
                    extension
                })
                .collect();
            Ok(Some(extensions))
        } else {
            Ok(None)
        }
    }
}

impl Enumeration {
    fn try_parse(input: ParseStream, _proto2: bool) -> syn::Result<Option<Self>> {
        if let Some(_) = input.try_parse_as_ident("enum", false) {
            let name = input.parse_as_ident()?;
            let mut values = Vec::new();
            let mut options = Vec::new();
            let mut reserved_nums = Vec::new();
            let mut reserved_names = Vec::new();

            let inner: ParseBuffer;
            syn::braced!(inner in input);
            while !inner.is_empty() {
                if inner.peek(Token![;]) {
                    inner.parse::<Token![;]>()?;
                    continue;
                }
                if let Some((field_nums, field_names)) = MessageBody::try_parse_reserved(&inner)? {
                    reserved_nums.extend(field_nums);
                    reserved_names.extend(field_names);
                    continue;
                }
                if let Some(_) = inner.try_parse_as_ident("option", false) {
                    options.push(ProtobufOption::continue_to_parse(&inner)?);
                    continue;
                }
                values.push(input.parse()?);
            }

            Ok(Some(Self {
                name,
                values,
                options,
                reserved_names,
                reserved_nums,
            }))
        } else {
            Ok(None)
        }
    }
}

impl Parse for EnumValue {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse_as_ident()?;
        input.parse::<Token![=]>()?;
        let value = input.parse::<LitInt>()?;
        let options = if input.peek(token::Bracket) {
            let inner: ParseBuffer;
            syn::bracketed!(inner in input);
            Some(inner.parse_terminated(ProtobufOption::parse_enum_option, Token![,])?)
        } else {
            None
        };
        Ok(Self {
            name,
            number: value,
            options,
        })
    }
}

impl ProtobufOption {
    fn parse_enum_option(input: ParseStream) -> syn::Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![=]>()?;
        let value = input.parse()?;
        Ok(Self { name, value })
    }
}

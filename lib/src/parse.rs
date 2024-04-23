use std::{collections::HashMap, ops::RangeInclusive, path::PathBuf};

use convert_case::Case;
use proc_macro2::Span;
use syn::{
    parse::{Parse, ParseBuffer, ParseStream},
    punctuated::Punctuated,
    token, Ident, LitBool, LitInt, LitStr, Token,
};
use syn_prelude::{
    AmmendSynError, JoinSynErrors, ParseAsIdent, ToErr, ToIdentWithCase, ToLitStr, ToSynError,
    TryParseAsIdent, TryParseOneOfIdents, WithPrefix, WithSuffix,
};

use crate::{
    dep::Deps,
    model::{
        AnyTypeUrl, DeclIndex, EnumValue, Enumeration, Extension, Field, FieldType, Group, Import,
        ImportVis, MapType, Message, MessageElement, Method, Modifier, NestedTypeIndex, OneOf,
        Package, ProtobufConstant, ProtobufConstantMessage, ProtobufConstantMessageFieldName,
        ProtobufOption, ProtobufOptionName, ProtobufOptionNameExt, ProtobufOptionNamePart,
        ProtobufPath, Protocol, Service, Syntax, TagValue, Type,
    },
    resolve::{InsideType, PathMod, ResolveContext},
};

impl Protocol {
    pub fn parse_from_call_site<BeforeCheck: Fn(&mut Protocol) -> syn::Result<()>>(
        input: ParseStream,
        call_site_path: PathBuf,
        before_check: BeforeCheck,
    ) -> syn::Result<Self> {
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
            deps: Deps::new(&call_site_path, input)?,
        };

        let proto_version = syntax.version();

        protocol
            .imports
            .push(Import::call_site(&protocol.deps, &call_site_path)?);
        while !input.is_empty() {
            if input.peek(Token![;]) {
                input.parse::<Token![;]>()?;
                continue;
            }
            if let Some(message) = Message::try_parse(input, proto_version, None)
                .add_error_suffix("while parsing message")?
            {
                protocol
                    .decls
                    .push(DeclIndex::Message(protocol.messages.len()));
                protocol.messages.push(message);
            } else if let Some(service) = Service::try_parse(input, proto_version)
                .add_error_suffix("while parsing service")?
            {
                protocol
                    .decls
                    .push(DeclIndex::Service(protocol.services.len()));
                let messages = service
                    .methods
                    .iter()
                    .map(|s| match (&s.input_message, &s.output_message) {
                        (None, None) => None,
                        (None, Some(o)) => Some(vec![o.clone()]),
                        (Some(i), None) => Some(vec![i.clone()]),
                        (Some(i), Some(o)) => Some(vec![i.clone(), o.clone()]),
                    })
                    .flatten()
                    .flatten()
                    .collect::<Vec<_>>();

                protocol.services.push(service);
                for message in messages {
                    protocol
                        .decls
                        .push(DeclIndex::Message(protocol.messages.len()));
                    protocol.messages.push(message);
                }
            } else if let Some(enumeration) = Enumeration::try_parse(input, proto_version, None)
                .add_error_suffix("while parsing enumeration")?
            {
                protocol.decls.push(DeclIndex::Enum(protocol.enums.len()));
                protocol.enums.push(enumeration);
            } else if let Some(import) = Import::try_parse(&protocol.deps, input)
                .add_error_suffix("while parsing import clause")?
            {
                protocol.imports.push(import);
            } else if let Some(option) =
                ProtobufOption::try_parse(input).add_error_prefix("while parsing global option")?
            {
                protocol.options.push(option);
            } else if let Some(package) =
                Package::try_parse(input).add_error_suffix("while parsing 'package'")?
            {
                protocol.package = Some(package);
            } else if let Some(ext) = Extension::try_parse(input, proto_version)
                .add_error_suffix("while parsing protobuf syntax version")?
            {
                protocol.extensions.extend(ext);
            } else {
                input.span().to_syn_error("unexpected token").to_err()?;
            }
        }

        protocol.syntax = syntax;
        before_check(&mut protocol)?;

        for (import_index, import) in protocol.imports.iter().enumerate() {
            protocol.deps.scan(import_index, import)?;
        }

        let mut types = protocol
            .messages
            .iter()
            .map(|m| (m.name.clone(), InsideType::Message((&vec![], m).into())))
            .collect::<HashMap<_, _>>();
        protocol.enums.iter().for_each(|e| {
            types.insert(e.name.clone(), InsideType::Enum(e.name.clone()));
        });
        protocol.services.iter().for_each(|s| {
            s.methods.iter().for_each(|m| {
                if let Some(m) = &m.input_message {
                    types.insert(m.name.clone(), InsideType::Message((&vec![], m).into()));
                }
                if let Some(m) = &m.output_message {
                    types.insert(m.name.clone(), InsideType::Message((&vec![], m).into()));
                }
            })
        });
        let ctx = ResolveContext {
            package: &protocol.package,
            types,
            deps: &protocol.deps,
            imports: &protocol.imports,
        };

        let check_result1 = protocol
            .messages
            .iter_mut()
            .map(|m| m.resolve(&ctx))
            .collect::<Vec<syn::Result<_>>>()
            .join_errors();

        let check_result2 = protocol
            .services
            .iter_mut()
            .map(|s| {
                s.methods
                    .iter_mut()
                    .map(|m| {
                        if let Some(message) = &mut m.input_message {
                            message.resolve(&ctx)?;
                        }
                        ctx.resolve_type(None, &mut m.input_type)?;
                        if let Some(message) = &mut m.output_message {
                            message.resolve(&ctx)?;
                        }
                        ctx.resolve_type(None, &mut m.output_type)?;
                        Ok(())
                    })
                    .collect::<Vec<_>>()
                    .join_errors()
            })
            .collect::<Vec<_>>()
            .join_errors();

        (check_result1, check_result2).join_errors()?;

        Ok(protocol)
    }
}

impl Parse for ProtobufPath {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut segments = Punctuated::new();
        while !input.is_empty() {
            segments.push(input.parse_as_ident()?);
            if input.peek(Token![.]) {
                segments.push_punct(input.parse::<Token![.]>()?);
            } else {
                break;
            }
        }
        Ok(Self { segments })
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
    fn try_parse(input: ParseStream) -> syn::Result<Option<Self>> {
        if input.peek_as_ident("option", false) {
            Ok(Some(Self::parse(input)?))
        } else {
            Ok(None)
        }
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
                Self::Proto2(lit.span())
            } else if proto.eq("proto3") {
                Self::Proto3(Some(lit.span()))
            } else {
                lit.to_syn_error("unknown syntax").to_err()?
            };
            proto
        } else {
            Self::Proto3(None)
        })
    }
}

impl Import {
    // call_site_path = proc_macro::Span::call_site().source_file().path();
    fn call_site(deps: &Deps, call_site_path: &PathBuf) -> syn::Result<Self> {
        let span = Span::call_site();
        let path_value = call_site_path.to_string_lossy().to_string();
        let file_path = deps.resolve_path(&path_value, span)?;
        let path = (path_value, span).to_lit_str();
        Ok(Self {
            import_token: span,
            path,
            builtin: false,
            vis: ImportVis::Default,
            file_path: Some(file_path),
        })
    }
    fn try_parse(deps: &Deps, input: ParseStream) -> syn::Result<Option<Self>> {
        if let Some(i) = input.try_parse_as_ident("import", false) {
            let vis = if let Some(weak) = input.try_parse_as_ident("weak", false) {
                ImportVis::Weak(weak.span())
            } else if let Some(public) = input.try_parse_as_ident("public", false) {
                ImportVis::Public(public.span())
            } else {
                ImportVis::Default
            };
            let path: LitStr = input.parse()?;
            let path_value = path.value();
            let builtin = if path_value.starts_with("google/protobuf/") {
                true
            } else if !path_value.ends_with(".rs") {
                path.span()
                    .to_syn_error("only support for importing '.rs' yet")
                    .to_err()?
            } else {
                false
            };
            let file_path = if !builtin {
                Some(deps.resolve_path(&path_value, path.span())?)
            } else {
                None
            };

            Ok(Some(Self {
                import_token: i.span(),
                vis,
                builtin,
                path,
                file_path,
            }))
        } else {
            Ok(None)
        }
    }
}

impl Package {
    fn try_parse(input: ParseStream) -> syn::Result<Option<Self>> {
        if let Some(_) = input.try_parse_as_ident("package", false) {
            let package = input.parse::<Ident>()?;
            Ok(Some(Self { package }))
        } else {
            Ok(None)
        }
    }
}

impl Service {
    fn try_parse(input: ParseStream, proto_version: usize) -> syn::Result<Option<Self>> {
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
                    methods.push(Method::continue_to_parse(&inner, proto_version)?);
                } else if let Some(stream) = inner.try_parse_as_ident("stream", false) {
                    if proto_version == 2 {
                        let mut method = Method::continue_to_parse(&inner, proto_version)?;
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
                code_name: name.to_ident_with_case(Case::UpperCamel),
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
    fn parse_param(
        input: ParseStream,
        rpc_name: &Ident,
        proto_version: usize,
        suffix: &'static str,
    ) -> syn::Result<(Option<Span>, Type, Option<Message>)> {
        let inner: ParseBuffer;
        syn::parenthesized!(inner in input);
        let stream = if proto_version != 2 {
            inner.try_parse_as_ident("stream", false)
        } else {
            None
        };
        let (message, param_type) = if cfg!(feature = "simplize_rpc_params") {
            if inner.is_empty() {
                (None, ProtobufPath::new_empty(rpc_name.span()))
            } else if inner.peek(token::Brace) {
                let MessageBody {
                    fields,
                    options,
                    reserved_names,
                    reserved_nums,
                    extension_ranges,
                    extensions,
                    messages,
                    enums,
                    nested_types,
                    ..
                } = MessageBody::parse(
                    &inner,
                    proto_version,
                    MessageBodyParseMode::parse_message(proto_version),
                    None,
                    false,
                )?;
                let message_name = rpc_name
                    .to_ident_with_case(Case::UpperCamel)
                    .with_suffix(suffix);
                let message = Message {
                    nested_mod_name: None,
                    struct_name: message_name.clone(),
                    name: message_name.clone(),
                    fields,
                    options,
                    reserved_names,
                    reserved_nums,
                    extension_ranges,
                    extensions,
                    messages,
                    enums,
                    nested_types,
                };
                (Some(message), ProtobufPath::from_ident(message_name))
            } else if inner.peek(Ident) && inner.peek2(token::Brace) {
                let message_name: Ident = inner.parse()?;
                let MessageBody {
                    fields,
                    options,
                    reserved_names,
                    reserved_nums,
                    extension_ranges,
                    extensions,
                    messages,
                    enums,
                    nested_types,
                    ..
                } = MessageBody::parse(
                    &inner,
                    proto_version,
                    MessageBodyParseMode::parse_message(proto_version),
                    None,
                    false,
                )?;
                let message = Message {
                    nested_mod_name: None,
                    struct_name: message_name.clone(),
                    name: message_name.clone(),
                    fields,
                    options,
                    reserved_names,
                    reserved_nums,
                    extension_ranges,
                    extensions,
                    messages,
                    enums,
                    nested_types,
                };
                (Some(message), ProtobufPath::from_ident(message_name))
            } else {
                (None, inner.parse()?)
            }
        } else {
            (None, inner.parse()?)
        };
        if !inner.is_empty() {
            inner.span().to_syn_error("expect ')'").to_err()?;
        }
        Ok((
            stream.map(|i| i.span()),
            Type {
                type_path: param_type,
                target_is_message: true,
                ty: syn::Type::new(),
            },
            message,
        ))
    }

    fn continue_to_parse(input: ParseStream, proto_version: usize) -> syn::Result<Self> {
        let name = input.parse_as_ident()?;
        let (client_streaming, input_type, input_message) =
            Self::parse_param(input, &name, proto_version, "Request")?;
        input.parse_as_named_ident("returns", false)?;
        let (server_streaming, output_type, output_message) =
            Self::parse_param(input, &name, proto_version, "Response")?;

        let options = input.parse::<TryAsOptions>()?.0;

        Ok(Self {
            method_name: name.to_ident_with_case(Case::Snake),
            name,
            client_streaming,
            input_message,
            input_type,
            server_streaming,
            output_message,
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
    fn try_parse(
        input: ParseStream,
        proto_version: usize,
        parent_name: Option<&Ident>,
    ) -> syn::Result<Option<Self>> {
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
                nested_types,
            } = MessageBody::parse(
                input,
                proto_version,
                MessageBodyParseMode::parse_message(proto_version),
                Some(&name),
                false,
            )?;

            Ok(Some(Self {
                nested_types,
                struct_name: name.to_ident_with_case(Case::UpperCamel),
                nested_mod_name: parent_name.map(|name| name.to_ident_with_case(Case::Snake)),
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
    fn parse_message(version: usize) -> Self {
        if version == 2 {
            Self::MessageProto2
        } else {
            Self::MessageProto3
        }
    }

    fn parse_extension(version: usize) -> Self {
        if version == 2 {
            Self::ExtendProto2
        } else {
            Self::ExtendProto3
        }
    }

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

#[allow(unused)]
impl MessageBodyParseMode {
    fn label_allowed(&self, label: Modifier) -> bool {
        match label {
            Modifier::Repeated => match *self {
                MessageBodyParseMode::MessageProto2
                | MessageBodyParseMode::MessageProto3
                | MessageBodyParseMode::ExtendProto2
                | MessageBodyParseMode::ExtendProto3 => true,
                MessageBodyParseMode::Oneof => false,
            },
            Modifier::Optional => match *self {
                MessageBodyParseMode::MessageProto2 | MessageBodyParseMode::ExtendProto2 => true,
                MessageBodyParseMode::MessageProto3 | MessageBodyParseMode::ExtendProto3 => true,
                MessageBodyParseMode::Oneof => false,
            },
            Modifier::Required => match *self {
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
    pub fields: Vec<MessageElement>,
    pub reserved_nums: Vec<RangeInclusive<i32>>,
    pub reserved_names: Vec<Ident>,
    pub messages: Vec<Message>,
    pub enums: Vec<Enumeration>,
    pub options: Vec<ProtobufOption>,
    pub extension_ranges: Vec<RangeInclusive<i32>>,
    pub extensions: Vec<Extension>,
    pub nested_types: Vec<NestedTypeIndex>,
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
    fn parse(
        input: ParseStream,
        proto_version: usize,
        mode: MessageBodyParseMode,
        parent_name: Option<&Ident>,
        enum_flag: bool,
    ) -> syn::Result<Self> {
        let mut r = MessageBody::default();
        let inner: ParseBuffer;
        syn::braced!(inner in input);
        while !inner.is_empty() {
            if inner.peek(Token![;]) {
                inner.parse::<Token![;]>()?;
                continue;
            }
            if mode.is_most_non_fields_allowed() {
                if let Some((field_nums, field_names)) = Self::try_parse_reserved(&inner)? {
                    r.reserved_nums.extend(field_nums);
                    r.reserved_names.extend(field_names);
                    continue;
                }
                if let Some(oneof) = OneOf::try_parse(&inner, proto_version, parent_name)? {
                    r.nested_types.push(NestedTypeIndex::Oneof(r.fields.len()));
                    r.fields.push(MessageElement::OneOf(oneof));
                    continue;
                }
                if let Some(extensions) = Extension::try_parse(&inner, proto_version)? {
                    r.extensions.extend(extensions);
                    continue;
                }
                if let Some(nested) = Message::try_parse(&inner, proto_version, parent_name)? {
                    r.nested_types
                        .push(NestedTypeIndex::Message(r.messages.len()));
                    r.messages.push(nested);
                    continue;
                }
                if let Some(nested) = Enumeration::try_parse(&inner, proto_version, parent_name)? {
                    r.nested_types.push(NestedTypeIndex::Enum(r.enums.len()));
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
                if let Some(extensions) = Extension::try_parse(&inner, proto_version)? {
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

            r.fields.push(MessageElement::Field(Field::parse(
                &inner,
                proto_version,
                mode,
                parent_name,
                enum_flag,
            )?));
        }

        r.check_tags()?;

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

    fn check_tags(&mut self) -> syn::Result<()> {
        let mut tag_used = HashMap::<i32, Span>::new();
        let mut ref_tag_span = None;
        let mut next_tag = 1;

        fn check_field(
            field: &mut Field,
            reserved_nums: &Vec<RangeInclusive<i32>>,
            tag_used: &mut HashMap<i32, Span>,
            next_tag: i32,
            ref_tag_span: Option<Span>,
        ) -> syn::Result<(i32, Option<Span>)> {
            macro_rules! throw_if_reserved {
                ($value:expr, $span:expr, $reserved_error:expr, $occupied_error:expr) => {
                    for range in reserved_nums.iter() {
                        if range.contains($value) {
                            return Err(syn::Error::new($span, $reserved_error));
                        }
                    }
                    if let Some(prev_span) = tag_used.get($value) {
                        let span = if let Some(new_span) = ($span).join(*prev_span) {
                            new_span
                        } else {
                            $span
                        };
                        return Err(syn::Error::new(span, $occupied_error));
                    }
                };
                ($value:expr, $span:expr) => {
                    throw_if_reserved!($value, $span, "this tag is reserved!", "The tag is used")
                };
            }
            match &field.tag {
                TagValue::Value(span, value) => {
                    throw_if_reserved!(value, *span);
                    tag_used.insert(*value, *span);
                    Ok((*value + 1, Some(*span)))
                }
                TagValue::AutoIncr => {
                    throw_if_reserved!(
                        &next_tag,
                        field.field_name.span(),
                        &format!("tag number({}) is reserved", next_tag),
                        &format!("tag number({}) is used", next_tag)
                    );
                    tag_used.insert(next_tag, field.field_name.span());
                    field.tag =
                        TagValue::Value(ref_tag_span.unwrap_or(field.field_name.span()), next_tag);
                    Ok((next_tag + 1, ref_tag_span))
                }
            }
        }

        for field in self.fields.iter_mut() {
            match field {
                MessageElement::Field(field) => {
                    (next_tag, ref_tag_span) = check_field(
                        field,
                        &self.reserved_nums,
                        &mut tag_used,
                        next_tag,
                        ref_tag_span,
                    )?;
                }
                MessageElement::OneOf(oneof) => {
                    for field in oneof.fields.iter_mut() {
                        (next_tag, ref_tag_span) = check_field(
                            field,
                            &self.reserved_nums,
                            &mut tag_used,
                            next_tag,
                            ref_tag_span,
                        )?;
                    }
                }
            }
        }
        Ok(())
    }
}

impl Modifier {
    fn try_parse(input: ParseStream, mode: MessageBodyParseMode) -> syn::Result<Option<Self>> {
        if let Some(label) = input.try_parse_as_ident("optional", false) {
            if !mode.label_allowed(Modifier::Optional) {
                label
                    .to_syn_error(format!("not allowed in {}", mode.to_str()))
                    .to_err()
            } else {
                Ok(Some(Self::Optional))
            }
        } else if let Some(label) = input.try_parse_as_ident("repeated", false) {
            if !mode.label_allowed(Modifier::Repeated) {
                label
                    .to_syn_error(format!("not allowed in {}", mode.to_str()))
                    .to_err()
            } else {
                Ok(Some(Self::Repeated))
            }
        } else if let Some(label) = input.try_parse_as_ident("required", false) {
            if !mode.label_allowed(Modifier::Required) {
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

impl TagValue {
    fn parse(input: ParseStream, error_span: Span) -> syn::Result<Self> {
        if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            let value = input.parse::<LitInt>()?;
            let span = value.span();
            let value: i32 = value.base10_parse()?;
            Ok(Self::Value(span, value))
        } else {
            if cfg!(not(feature = "tagless")) {
                return Err(syn::Error::new(
                    error_span,
                    "missing tag number here, you can enable 'tagless' feature to omit the tag.",
                ));
            }
            Ok(Self::AutoIncr)
        }
    }
}

impl Field {
    fn parse(
        input: ParseStream,
        proto_version: usize,
        mode: MessageBodyParseMode,
        parent_name: Option<&Ident>,
        enum_field: bool,
    ) -> syn::Result<Self> {
        let rule = if input.peek_as_ident("map", false) {
            if !mode.map_allowed() {
                let ident = input.parse_as_named_ident("map", false)?;
                ident
                    .to_syn_error(format!("not allowed in {}", mode.to_str()))
                    .to_err()?;
            }
            None
        } else {
            Modifier::try_parse(input, mode)?
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
            let tag = TagValue::parse(input, name.span())?;
            let MessageBody { fields, .. } = MessageBody::parse(
                input,
                proto_version,
                MessageBodyParseMode::parse_message(proto_version),
                parent_name,
                enum_field,
            )?;
            let fields = fields
                .into_iter()
                .map(|fo| match fo {
                    MessageElement::Field(f) => Ok(f),
                    MessageElement::OneOf(o) => o
                        .name
                        .to_syn_error("unexpected 'oneof' in 'group'")
                        .to_err(),
                })
                .collect::<syn::Result<_>>()?;
            Ok(Self {
                field_name: name.to_ident_with_case(Case::Snake),
                // The field name is a lowercased version of the type name
                // (which has been verified to start with an uppercase letter).
                // https://git.io/JvxAP
                name: name.to_ident_with_case(Case::Lower),
                modifier: rule,
                typ: FieldType::Group(Group { name, fields }),
                tag,
                options: Vec::new(),
                enum_field,
            })
        } else {
            let typ = FieldType::parse(input)?;
            let name = input.parse_as_ident()?;
            let tag = TagValue::parse(input, name.span())?;
            let mut options = vec![];
            if input.peek(token::Bracket) {
                let inner: ParseBuffer;
                syn::bracketed!(inner in input);
                while !inner.is_empty() {
                    options.push(inner.parse()?);
                }
            }
            Ok(Self {
                field_name: name.to_ident_with_case(Case::Snake),
                name,
                modifier: rule,
                typ,
                tag,
                options,
                enum_field,
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
        } else if let Some(ident) = input.try_parse_as_ident("map", false) {
            input.parse::<Token![<]>()?;

            let key = FieldType::parse(input)?;

            input.parse::<Token![,]>()?;
            let value = FieldType::parse(input)?;

            let a = input.parse::<Token![>]>()?;
            let span = ident.span().join(a.span).unwrap_or(ident.span());
            Ok(Self::Map(MapType {
                span,
                key: Box::new(key),
                value: Box::new(value),
            }))
        } else {
            let type_path: ProtobufPath = input.parse()?;
            Ok(Self::MessageOrEnum(Type {
                type_path,
                target_is_message: true,
                ty: syn::Type::new(),
            }))
        }
    }
}

impl OneOf {
    fn try_parse(
        input: ParseStream,
        proto_version: usize,
        parent_name: Option<&Ident>,
    ) -> syn::Result<Option<Self>> {
        if let Some(_) = input.try_parse_as_ident("oneof", false) {
            let name = input.parse_as_ident()?;
            let MessageBody {
                fields, options, ..
            } = MessageBody::parse(
                input,
                proto_version,
                MessageBodyParseMode::Oneof,
                parent_name,
                true,
            )?;
            let fields = fields
                .into_iter()
                .map(|fo| match fo {
                    MessageElement::Field(f) => Ok(f),
                    MessageElement::OneOf(o) => o.name.to_syn_error("oneof in oneof").to_err(),
                })
                .collect::<syn::Result<Vec<_>>>()?;

            if fields.len() == 0 {
                name.to_syn_error("missing fields").to_err()?;
            }

            let mut span = fields.first().unwrap().name.span();
            if let Some(last) = fields.last() {
                if let Some(s) = span.join(last.name.span()) {
                    span = s;
                }
            }
            let tags = LitStr::new(
                &fields
                    .iter()
                    .map(|f| f.tag.to_lit_str().value())
                    .collect::<Vec<_>>()
                    .join(", "),
                span,
            );

            let nested_mod_name = parent_name
                .ok_or(syn::Error::new(name.span(), "missing parent message"))?
                .to_ident_with_case(Case::Snake);

            let enum_name = name.to_ident_with_case(Case::UpperCamel);

            Ok(Some(OneOf {
                field_name: name.clone(),
                field_lit: enum_name
                    .to_lit_str()
                    .with_prefix(format!("{}::", nested_mod_name.to_string())),
                enum_name,
                nested_mod_name,
                tags,
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
    fn try_parse(input: ParseStream, proto_version: usize) -> syn::Result<Option<Vec<Extension>>> {
        if let Some(_) = input.try_parse_as_ident("extend", false) {
            let extendee: ProtobufPath = input.parse()?;
            let MessageBody { fields, .. } = MessageBody::parse(
                input,
                proto_version,
                MessageBodyParseMode::parse_extension(proto_version),
                None,
                false,
            )?;

            let fields: Vec<Field> = fields
                .into_iter()
                .map(|fo| match fo {
                    MessageElement::Field(f) => Ok(f),
                    MessageElement::OneOf(o) => o.name.to_syn_error("oneof in extend").to_err(),
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
    fn try_parse(
        input: ParseStream,
        _proto_version: usize,
        parent_name: Option<&Ident>,
    ) -> syn::Result<Option<Self>> {
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
                nested_mod_name: parent_name.map(|name| name.to_ident_with_case(Case::Snake)),
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
        let tag = if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            Some(input.parse::<LitInt>()?)
        } else {
            None
        };
        let options = if input.peek(token::Bracket) {
            let inner: ParseBuffer;
            syn::bracketed!(inner in input);
            Some(inner.parse_terminated(ProtobufOption::parse_enum_option, Token![,])?)
        } else {
            None
        };
        Ok(Self {
            proto_name: name.to_lit_str(),
            variant_name: name.to_ident_with_case(Case::UpperCamel),
            name,
            tag,
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

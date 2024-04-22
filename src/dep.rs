use std::{
    collections::HashMap,
    fs::{self, read_to_string},
    ops::Range,
    path::PathBuf,
};

use proc_macro2::Span;
use syn::parse::ParseStream;
use syn_prelude::{ToErr, ToSynError};

use crate::model::Import;

#[derive(Debug, Clone)]
pub struct ExternalTypeRef {
    pub is_message: bool,
    pub prost_type: bool,
    pub import_index: usize,
}

#[derive(Debug, Clone)]
pub struct Deps {
    pub current_source_range: Range<usize>,
    pub scopes: HashMap<String, HashMap<String, ExternalTypeRef>>,
}

impl Deps {
    pub fn new(call_site_path: &PathBuf, input: ParseStream) -> syn::Result<Self> {
        let mut slf = Self {
            current_source_range: Span::call_site().byte_range(),
            scopes: Default::default(),
        };
        let contents = fs::read_to_string(call_site_path)
            .map_err(|_err| input.span().to_syn_error("cannot read source file"))?;
        slf.scan_with_contents(&contents, 0)?;

        Ok(slf)
    }
    pub fn scan(&mut self, import_index: usize, import: &Import) -> syn::Result<()> {
        if import.builtin {
            match import.path.value().as_str() {
                "google/protobuf/any.proto" => {
                    self.add_well_known_type("google.protobuf.Any", true, import_index);
                }
                "google/protobuf/api.proto" => {
                    self.add_well_known_type("google.protobuf.Api", true, import_index);
                    self.add_well_known_type("google.protobuf.Method", true, import_index);
                    self.add_well_known_type("google.protobuf.Mixin", true, import_index);
                }
                "google/protobuf/duration.proto" => {
                    self.add_well_known_type("google.protobuf.Duration", true, import_index);
                }
                "google/protobuf/empty.proto" => {
                    self.add_well_known_type("google.protobuf.Empty", true, import_index);
                }
                "google/protobuf/field_mask.proto" => {
                    self.add_well_known_type("google.protobuf.FieldMask", true, import_index);
                }
                "google/protobuf/source_context.proto" => {
                    self.add_well_known_type("google.protobuf.SourceContext", true, import_index);
                }
                "google/protobuf/struct.proto" => {
                    self.add_well_known_type("google.protobuf.Struct", true, import_index);
                    self.add_well_known_type("google.protobuf.Value", true, import_index);
                    self.add_well_known_type("google.protobuf.NullValue", false, import_index);
                    self.add_well_known_type("google.protobuf.ListValue", true, import_index);
                }
                "google/protobuf/timestamp.proto" => {
                    self.add_well_known_type("google.protobuf.Timestamp", true, import_index);
                }
                "google/protobuf/type.proto" => {
                    self.add_well_known_type("google.protobuf.Type", true, import_index);
                    self.add_well_known_type("google.protobuf.Field", true, import_index);
                    self.add_well_known_type("google.protobuf.Field.Kind", false, import_index);
                    self.add_well_known_type(
                        "google.protobuf.Field.Cardinality",
                        false,
                        import_index,
                    );
                    self.add_well_known_type("google.protobuf.Enum", true, import_index);
                    self.add_well_known_type("google.protobuf.EnumValue", true, import_index);
                    self.add_well_known_type("google.protobuf.Option", true, import_index);
                    self.add_well_known_type("google.protobuf.Syntax", false, import_index);
                }
                "google/protobuf/wrappers.proto" => {
                    self.add_well_known_type("google.protobuf.DoubleValue", true, import_index);
                    self.add_well_known_type("google.protobuf.FloatValue", true, import_index);
                    self.add_well_known_type("google.protobuf.Int64Value", true, import_index);
                    self.add_well_known_type("google.protobuf.UInt64Value", true, import_index);
                    self.add_well_known_type("google.protobuf.Int32Value", true, import_index);
                    self.add_well_known_type("google.protobuf.UInt32Value", true, import_index);
                    self.add_well_known_type("google.protobuf.BoolValue", true, import_index);
                    self.add_well_known_type("google.protobuf.StringValue", true, import_index);
                    self.add_well_known_type("google.protobuf.BytesValue", true, import_index);
                }
                _ => import
                    .path
                    .to_syn_error("unsupported .proto as for importing well known types.")
                    .to_err()?,
            };
            Ok(())
        } else if let Some(file_path) = &import.file_path {
            let contents = read_to_string(&file_path.path).map_err(|_| {
                import.path.span().to_syn_error(
                    "fails to read contents while scanning potential types for importing",
                )
            })?;
            self.scan_with_contents(&contents, import_index)
        } else {
            // unreachable!()
            Ok(())
        }
    }

    fn add_well_known_type(&mut self, type_name: &str, is_message: bool, import_index: usize) {
        if let Some(scope) = self.scopes.get_mut("") {
            scope.insert(
                type_name.to_owned(),
                ExternalTypeRef {
                    is_message,
                    prost_type: true,
                    import_index,
                },
            );
        } else {
            let mut scope = HashMap::new();
            scope.insert(
                type_name.to_owned(),
                ExternalTypeRef {
                    is_message,
                    prost_type: true,
                    import_index,
                },
            );
            self.scopes.insert("".to_owned(), scope);
        }
    }

    fn scan_with_contents(&mut self, contents: &str, import_index: usize) -> syn::Result<()> {
        let mut source = contents;
        let mut source_pos = 0usize;
        while !source.is_empty() {
            if let Some(macro_pos) = source.find("protobuf!") {
                source = source.split_at(macro_pos + 10).1;
                source_pos += 10 + macro_pos;
                if self.current_source_range.contains(&source_pos) {
                    source_pos = self.current_source_range.end;
                    source = contents.split_at(self.current_source_range.end).1;
                    continue;
                }
                let before = source.split_at(macro_pos).0;
                if let Some(nl) = before.rfind("\n") {
                    // check is commented to this macro
                    if before.split_at(nl).1.contains("//") {
                        // has comment before protobuf!
                        continue;
                    }
                }
            } else {
                break;
            }

            if let Ok((rest, (package, types))) = fast_pb_parser::pb_proto(source) {
                let eaten = source.len() - rest.len();
                source_pos += eaten;
                source = rest;

                let package = package.unwrap_or_default().to_owned();
                if let Some(map) = self.scopes.get_mut(&package) {
                    types
                        .iter()
                        .for_each(|t| t.register_type(import_index, map, ""));
                } else {
                    let mut map = HashMap::<String, ExternalTypeRef>::new();
                    types
                        .iter()
                        .for_each(|t| t.register_type(import_index, &mut map, ""));
                    self.scopes.insert(package.to_owned(), map);
                }
            } else {
                // broken but can still parse next protobuf!
            }
        }
        Ok(())
    }
}

trait RegisterType {
    fn register_type(
        &self,
        import_index: usize,
        map: &mut HashMap<String, ExternalTypeRef>,
        parent: &str,
    );
}

mod fast_pb_parser {
    use nom::{
        branch::alt,
        bytes::complete::{is_not, tag, take_until, take_while1},
        character::{complete::digit1, is_newline},
        combinator::{map, opt},
        multi::{many0, separated_list1},
        sequence::{delimited, preceded, terminated, tuple},
        IResult,
    };

    use super::RegisterType;

    #[derive(Debug, PartialEq)]
    pub enum Element<'a> {
        Message(Message<'a>),
        Enum(Enum<'a>),
    }

    impl RegisterType for Element<'_> {
        fn register_type(
            &self,
            import_index: usize,
            map: &mut std::collections::HashMap<String, super::ExternalTypeRef>,
            parent: &str,
        ) {
            match self {
                Element::Message(m) => m.register_type(import_index, map, parent),
                Element::Enum(e) => e.register_type(import_index, map, parent),
            }
        }
    }

    fn line_comment(input: &str) -> IResult<&str, ()> {
        let (rest, _) = tag("//")(input)?;
        if let Some(c) = rest.chars().nth(0) {
            if is_newline(c as u8) {
                Ok((rest, ()))
            } else {
                map(is_not("\n\r"), |_| ())(rest)
            }
        } else {
            Ok((rest, ()))
        }
    }

    fn block_commnet(input: &str) -> IResult<&str, ()> {
        map(delimited(tag("/*"), take_until("*/"), tag("*/")), |_| ())(input)
    }

    fn ws(input: &str) -> IResult<&str, ()> {
        map(
            many0(alt((
                map(take_while1(|c: char| c.is_whitespace()), |_| ()),
                line_comment,
                block_commnet,
            ))),
            |_| (),
        )(input)
    }

    macro_rules! tag_ws_around {
        ($tag:expr) => {
            tuple((ws, tag($tag), ws))
        };
        ($tag1:expr,$tag2:expr) => {
            tuple((ws, tag($tag1), ws, tag($tag2), ws))
        };
    }

    fn ident(input: &str) -> IResult<&str, &str> {
        take_while1(|c: char| c.is_alphanumeric() || c == '_')(input)
    }

    pub fn pb_proto(input: &str) -> IResult<&str, (Option<&str>, Vec<Element>)> {
        let mut package = None;
        let (rest, elements) = delimited(
            tag_ws_around!("{"),
            map(many0(pb_decl), |result| {
                result
                    .into_iter()
                    .map(|(p, r)| {
                        if p.is_some() {
                            package = p;
                        }
                        r
                    })
                    .flatten()
                    .flatten()
                    .collect::<Vec<_>>()
            }),
            tag_ws_around!("}"),
        )(input)?;
        Ok((rest, (package, elements)))
    }

    fn pb_decl(input: &str) -> IResult<&str, (Option<&str>, Option<Vec<Element>>)> {
        let (rest, _) = ws(input)?;
        let (_, next) = alt((
            tag("message"),
            tag("enum"),
            tag("option"),
            tag("import"),
            tag("service"),
            tag("syntax"),
            tag("package"),
        ))(rest)?;
        let mut package = None;
        match next {
            "message" => map(pb_message, |m| Some(vec![Element::Message(m)]))(rest),
            "enum" => map(pb_enum, |e| Some(vec![Element::Enum(e)]))(rest),
            "option" => map(pb_option, |_| None)(rest),
            "import" => map(pb_import, |_| None)(rest),
            "service" => map(pb_service, |messages| {
                Some(
                    messages
                        .into_iter()
                        .map(|m| Element::Message(m))
                        .collect::<Vec<_>>(),
                )
            })(rest),
            "syntax" => map(pb_syntax, |_| None)(rest),
            "package" => map(pb_package, |name| {
                package = Some(name);
                None
            })(rest),
            _ => unreachable!(),
        }
        .map(|(rest, result)| (rest, (package, result)))
    }

    fn pb_syntax(input: &str) -> IResult<&str, &str> {
        delimited(
            tag_ws_around!("syntax", "="),
            delimited(tag("\""), ident, tag("\"")),
            tag_ws_around!(";"),
        )(input)
    }

    fn pb_import(input: &str) -> IResult<&str, &str> {
        delimited(
            tag_ws_around!("import"),
            delimited(tag("\""), take_until("\""), tag("\"")),
            tag_ws_around!(";"),
        )(input)
    }

    fn pb_option(input: &str) -> IResult<&str, ()> {
        map(
            tuple((
                tag_ws_around!("option"),
                take_while1(|c: char| c != ';'),
                tag_ws_around!(";"),
            )),
            |_| (),
        )(input)
    }

    fn pb_package(input: &str) -> IResult<&str, &str> {
        delimited(tag_ws_around!("package"), ident, tag_ws_around!(";"))(input)
    }

    fn pb_service(input: &str) -> IResult<&str, Vec<Message>> {
        let (rest, _name) =
            delimited(tag_ws_around!("service"), ident, tag_ws_around!("{"))(input)?;
        terminated(
            map(many0(pb_rpc), |messages| {
                messages
                    .into_iter()
                    .map(|rpc| match (rpc.gen_request, rpc.gen_response) {
                        (None, None) => vec![],
                        (None, Some((name, suffix, inner_types))) => vec![Message {
                            name,
                            suffix,
                            inner_types,
                        }],
                        (Some((name, suffix, inner_types)), None) => {
                            vec![Message {
                                name,
                                suffix,
                                inner_types,
                            }]
                        }
                        (Some(r), Some(p)) => vec![
                            Message {
                                name: r.0,
                                suffix: r.1,
                                inner_types: r.2,
                            },
                            Message {
                                name: p.0,
                                suffix: p.1,
                                inner_types: p.2,
                            },
                        ],
                    })
                    .flatten()
                    .collect()
            }),
            tag_ws_around!("}"),
        )(rest)
    }

    struct Rpc<'a> {
        gen_request: Option<(&'a str, &'static str, Vec<Element<'a>>)>,
        gen_response: Option<(&'a str, &'static str, Vec<Element<'a>>)>,
    }

    fn pb_rpc(input: &str) -> IResult<&str, Rpc> {
        let (rest, _) = tag_ws_around!("rpc")(input)?;
        let (rest, rpc_name) = ident(rest)?;
        let (rest, gen_request) = delimited(
            tag_ws_around!("("),
            preceded(
                opt(tag_ws_around!("stream")),
                alt((
                    map(pb_message_body, |inner_types| {
                        Some((rpc_name, "Request", inner_types))
                    }),
                    map(tuple((ident, pb_message_body)), |(name, types)| {
                        Some((name, "", types))
                    }),
                    map(ident, |_| None),
                )),
            ),
            tag_ws_around!(")"),
        )(rest)?;
        let (rest, _) = tag_ws_around!("returns")(rest)?;
        let (rest, gen_response) = delimited(
            tag_ws_around!("("),
            preceded(
                opt(tag_ws_around!("stream")),
                alt((
                    map(pb_message_body, |inner_types| {
                        Some((rpc_name, "Response", inner_types))
                    }),
                    map(tuple((ident, pb_message_body)), |(name, inner_types)| {
                        Some((name, "", inner_types))
                    }),
                    map(ident, |_| None),
                )),
            ),
            tag_ws_around!(")"),
        )(rest)?;
        // rpc options
        let (rest, _) = opt(tuple((
            tag_ws_around!("{"),
            many0(tuple((pb_option, opt(tag_ws_around!(";"))))),
            tag_ws_around!("}"),
        )))(rest)?;
        let (rest, _) = opt(tag_ws_around!(";"))(rest)?;
        Ok((
            rest,
            Rpc {
                gen_request,
                gen_response,
            },
        ))
    }

    fn pb_message_body(input: &str) -> IResult<&str, Vec<Element>> {
        map(
            delimited(
                tag_ws_around!("{"),
                many0(alt((
                    map(pb_reserved, |_| None),
                    map(pb_extend, |_| None),
                    map(pb_message, |m| Some(Element::Message(m))),
                    map(pb_enum, |e| Some(Element::Enum(e))),
                    map(pb_field, |_| None),
                ))),
                tag_ws_around!("}"),
            ),
            |results| results.into_iter().flatten().collect::<Vec<_>>(),
        )(input)
    }

    fn pb_path(input: &str) -> IResult<&str, ()> {
        separated_list1(tag("."), ident)(input).map(|(rest, _)| (rest, ()))
    }

    fn pb_extend(input: &str) -> IResult<&str, ()> {
        let (rest, _) = tuple((ws, tag("extend"), ws, pb_path))(input)?;
        pb_message_body(rest).map(|(rest, _)| (rest, ()))
    }

    fn pb_reserved(input: &str) -> IResult<&str, ()> {
        preceded(
            tag_ws_around!("reserved"),
            map(
                many0(terminated(
                    alt((
                        map(digit1, |_| ()),
                        map(tuple((digit1, ws, tag("to"), ws, digit1)), |_| ()),
                        map(delimited(tag("\""), ident, tag("\"")), |_| ()),
                    )),
                    opt(tag_ws_around!(";")),
                )),
                |_| (),
            ),
        )(input)
    }

    fn pb_field(input: &str) -> IResult<&str, ()> {
        let (rest, _) = alt((
            map(tuple((tag_ws_around!("group"), pb_message_body)), |_| ()),
            map(
                tuple((
                    tag_ws_around!("map", "<"),
                    pb_path,
                    tag_ws_around!(","),
                    pb_path,
                    tag_ws_around!(">"),
                    ident,
                    ws,
                    opt(tuple((tag_ws_around!("="), digit1, ws))),
                    opt(tuple((tag("["), is_not("]"), tag("]"), ws))),
                )),
                |_| (),
            ),
            map(
                tuple((tag_ws_around!("oneof"), ident, ws, pb_message_body)),
                |_| (),
            ),
            map(
                tuple((
                    ws,
                    opt(tag("repeated")),
                    ws,
                    pb_path,
                    ws,
                    ident,
                    ws,
                    opt(tuple((tag_ws_around!("="), digit1, ws))),
                    opt(tuple((tag("["), is_not("]"), tag("]"), ws))),
                )),
                |_| (),
            ),
        ))(input)?;
        let (rest, _) = opt(tag_ws_around!(";"))(rest)?;
        Ok((rest, ()))
    }

    #[derive(Debug, PartialEq)]
    pub struct Message<'a> {
        pub name: &'a str,
        pub suffix: &'static str,
        pub inner_types: Vec<Element<'a>>,
    }

    impl RegisterType for Message<'_> {
        fn register_type(
            &self,
            import_index: usize,
            map: &mut std::collections::HashMap<String, super::ExternalTypeRef>,
            parent: &str,
        ) {
            let type_name = format!("{}{}{}", parent, self.name, self.suffix);

            for el in self.inner_types.iter() {
                el.register_type(import_index, map, &format!("{}.", &type_name));
            }

            map.insert(
                type_name,
                super::ExternalTypeRef {
                    is_message: true,
                    prost_type: false,
                    import_index,
                },
            );
        }
    }

    fn pb_message(input: &str) -> IResult<&str, Message> {
        let (rest, message_name) = delimited(tag_ws_around!("message"), ident, ws)(input)?;
        let (rest, inner_types) = pb_message_body(rest)?;
        Ok((
            rest,
            Message {
                name: message_name,
                suffix: "",
                inner_types,
            },
        ))
    }

    #[derive(Debug, PartialEq)]
    pub struct Enum<'a> {
        pub name: &'a str,
    }

    impl RegisterType for Enum<'_> {
        fn register_type(
            &self,
            import_index: usize,
            map: &mut std::collections::HashMap<String, super::ExternalTypeRef>,
            parent: &str,
        ) {
            map.insert(
                format!("{}{}", parent, self.name),
                super::ExternalTypeRef {
                    is_message: false,
                    prost_type: false,
                    import_index,
                },
            );
        }
    }

    fn pb_enum(input: &str) -> IResult<&str, Enum> {
        map(
            delimited(
                tag_ws_around!("enum"),
                ident,
                tuple((
                    ws,
                    tag("{"),
                    ws,
                    many0(tuple((
                        alt((
                            pb_reserved,
                            pb_option,
                            map(
                                tuple((
                                    ident,
                                    opt(tuple((tag_ws_around!("="), digit1, ws))),
                                    opt(tuple((tag("["), is_not("]"), tag("]"), ws))),
                                    ws,
                                )),
                                |_| (),
                            ),
                        )),
                        opt(tag_ws_around!(";")),
                    ))),
                    tag("}"),
                    ws,
                )),
            ),
            |name| Enum { name },
        )(input)
    }

    #[cfg(test)]
    mod test_fast_pb_parser {
        use crate::dep::fast_pb_parser::{self, Element, Message};

        fn test_parser(
            pb_text: &str,
            expected_package: Option<&str>,
            expected_elements: Vec<Element>,
        ) {
            let (rest, (package, result)) = fast_pb_parser::pb_proto(pb_text).unwrap();
            if let Some(pos) = pb_text.rfind("}") {
                assert_eq!(
                    rest,
                    pb_text.split_at(pos + 1).1,
                    "unexpected rest text: {rest}"
                );
            }
            assert!(
                package.eq(&expected_package),
                "unexpected package {:#?}",
                package
            );
            assert!(
                result.eq(&expected_elements),
                "unexpected result {:#?}",
                result
            );
        }

        #[test]
        fn test_parse_protocol() {
            test_parser(
                r#"{
                    message A {
                        message AInner {
                        }
                    }
                }"#,
                None,
                vec![Element::Message(Message {
                    name: "A",
                    suffix: "",
                    inner_types: vec![Element::Message(Message {
                        name: "AInner",
                        suffix: "",
                        inner_types: vec![],
                    })],
                })],
            );

            test_parser(
                r#"{
                    package abc;
                    service SomeService {
                        rpc hello({
                            string name
                            message ReqInner {
                            }
                        }) returns({
                            string words
                        })
                    }
                }"#,
                Some("abc"),
                vec![
                    Element::Message(Message {
                        name: "hello",
                        suffix: "Request",
                        inner_types: vec![Element::Message(Message {
                            name: "ReqInner",
                            suffix: "",
                            inner_types: vec![],
                        })],
                    }),
                    Element::Message(Message {
                        name: "hello",
                        suffix: "Response",
                        inner_types: vec![],
                    }),
                ],
            );
        }
    }
}

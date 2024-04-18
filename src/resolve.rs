use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
    str::FromStr,
};

use convert_case::{Case, Casing};
use proc_macro2::Span;
use syn::{spanned::Spanned, Ident};

use crate::model::ProtobufPath;

#[derive(Debug, Clone)]
pub enum GoogleBultin {
    Any,
    Empty,
    Timestamp,
    Duration,
    Struct,
    Value,
    Null,
    List,
    Type,
    Field,
    Enum,
    EnumValue,
    Option,
    Api,
    Method,
    Mixin,
    FieldMask,
    Double,
    Float,
    Int64,
    Uint64,
    Int32,
    Uint32,
    Bool,
    String,
    Bytes,
}

#[derive(Debug, Clone)]
pub struct ExternalType {
    pub source_path: PathBuf,
    pub package: String,
    pub type_name: String,
    pub is_message: bool,
}

#[derive(Debug, Clone)]
pub struct InnerType {
    pub message_name: Ident,
    pub inner_mod_name: Ident,
    pub inner_type_name: Ident,
    pub is_message: bool,
}

#[derive(Debug, Clone)]
pub struct ProtocolInsideType {
    pub name: Ident,
    pub is_message: bool,
}

#[derive(Debug, Clone)]

pub enum ResolvedType {
    External(ExternalType),
    Inner(InnerType),
    ProtocolInside(ProtocolInsideType),
    Google(GoogleBultin),
    Unresolved,
}

pub struct TypeResolver {
    _call_site: proc_macro::Span,
    pub call_path: PathBuf,
    src_dir: PathBuf,
    cache: HashMap<PathBuf, HashMap<String, PbTypes>>,
}

impl TypeResolver {
    pub fn new() -> Self {
        let call_site = proc_macro::Span::call_site();
        let src_dir = Self::find_cargo_dir(call_site.source_file().path())
            .unwrap_or(PathBuf::from_str(env!("CARGO_MANIFEST_DIR")).unwrap())
            .join("src");
        Self {
            call_path: call_site.source_file().path(),
            _call_site: call_site,
            src_dir,
            cache: Default::default(),
        }
    }

    fn find_cargo_dir(path: impl AsRef<Path>) -> Option<PathBuf> {
        if let Some(parent) = path.as_ref().parent() {
            if parent.join("Cargo.toml").exists() {
                Some(parent.to_path_buf())
            } else {
                Self::find_cargo_dir(parent)
            }
        } else {
            None
        }
    }

    pub fn resolve(&mut self, typ_path: &ProtobufPath) -> syn::Result<Option<ResolvedType>> {
        let mut iter = typ_path.segments.iter().enumerate();
        let first = if let Some((_, ident)) = iter.next() {
            ident
        } else {
            Err(syn::Error::new(typ_path.span(), "missing type name"))?
        };

        if first.eq("crate") {
            self.lookup(
                typ_path.span(),
                self.src_dir.clone(),
                &mut iter,
                typ_path.segments.len() - 1,
            )
            .map(|r| r.map(|x| ResolvedType::External(x)))
        } else if first.eq("super") {
            self.lookup(
                typ_path.span(),
                self.call_path.parent().unwrap().to_path_buf(),
                &mut iter,
                typ_path.segments.len() - 1,
            )
            .map(|r| r.map(|x| ResolvedType::External(x)))
        } else {
            if first.eq("google") {
                if let Some((_, next)) = iter.next() {
                    if next.eq("protobuf") {
                        if let Some((_, next)) = iter.next() {
                            let ty = match next.to_string().as_str() {
                                "Any" => GoogleBultin::Any,
                                "Empty" => GoogleBultin::Empty,
                                "Timestamp" => GoogleBultin::Timestamp,
                                "Duration" => GoogleBultin::Duration,
                                "Struct" => GoogleBultin::Struct,
                                "Value" => GoogleBultin::Value,
                                "NullValue" => GoogleBultin::Null,
                                "ListValue" => GoogleBultin::List,
                                "Type" => GoogleBultin::Type,
                                "Field" => GoogleBultin::Field,
                                "Enum" => GoogleBultin::Enum,
                                "EnumValue" => GoogleBultin::EnumValue,
                                "Option" => GoogleBultin::Option,
                                "Api" => GoogleBultin::Api,
                                "Method" => GoogleBultin::Method,
                                "Mixin" => GoogleBultin::Mixin,
                                "FieldMask" => GoogleBultin::FieldMask,
                                // wrappers
                                "DoubleValue" => GoogleBultin::Double,
                                "FloatValue" => GoogleBultin::Float,
                                "Int64Value" => GoogleBultin::Int64,
                                "UInt64Value" => GoogleBultin::Uint64,
                                "Int32Value" => GoogleBultin::Int32,
                                "UInt32Value" => GoogleBultin::Uint32,
                                "BoolValue" => GoogleBultin::Bool,
                                "StringValue" => GoogleBultin::String,
                                "BytesValue" => GoogleBultin::Bytes,
                                _ => Err(syn::Error::new(next.span(), "unsupported type"))?,
                            };
                            return Ok(Some(ResolvedType::Google(ty)));
                        }
                    }
                }
            }
            // resolve in same file
            self.lookup(
                typ_path.span(),
                self.call_path.clone(),
                &mut typ_path.segments.iter().enumerate(),
                typ_path.segments.len() - 1,
            )
            .map(|r| r.map(|x| ResolvedType::External(x)))
        }
    }

    fn lookup<'a>(
        &mut self,
        error_span: Span,
        mut path: PathBuf,
        iter: &'a mut impl Iterator<Item = (usize, &'a Ident)>,
        last_seg_number: usize,
    ) -> syn::Result<Option<ExternalType>> {
        while let Some((_, seg)) = iter.next() {
            let p = path.join(seg.to_string());
            if p.exists() {
                // mod exists
                path = p;
                continue;
            }
            let p = path.join(format!("{}.rs", seg.to_string()));

            if p.eq(&self.call_path) {
                // in same file
            }

            if p.exists() {
                if let Some(types) = self.cache.get(&p) {
                    return Self::lookup_in_types(error_span, &p, types, iter, last_seg_number);
                } else {
                    let types = scan_types(error_span, &p)?;
                    let result =
                        Self::lookup_in_types(error_span, &p, &types, iter, last_seg_number);
                    self.cache.insert(p, types);
                    return result;
                }
            } else {
                Err(syn::Error::new(error_span, "no such type"))?;
            }
        }
        Ok(None)
    }
    fn lookup_in_types<'a>(
        error_span: Span,
        path: impl AsRef<Path>,
        types: &HashMap<String, PbTypes>,
        iter: &'a mut impl Iterator<Item = (usize, &'a Ident)>,
        last_seg_number: usize,
    ) -> syn::Result<Option<ExternalType>> {
        if let Some((index, mut name)) = iter.next() {
            let mut package_name = "".to_owned();
            if let Some(elements) = if index == last_seg_number {
                types.get("")
            } else {
                package_name = name.to_string();
                if let Some((index, n)) = iter.next() {
                    if index != last_seg_number {
                        return Err(syn::Error::new(
                            error_span,
                            &format!("cannot lookup inner types yet in {:?}", path.as_ref()),
                        ));
                    }
                    name = n;
                    types.get(&package_name)
                } else {
                    unreachable!()
                }
            } {
                let ty_name = name.to_string();
                if elements.messages.contains(&ty_name) {
                    Ok(Some(ExternalType {
                        source_path: path.as_ref().to_path_buf(),
                        package: package_name,
                        type_name: ty_name,
                        is_message: true,
                    }))
                } else if elements.enums.contains(&ty_name) {
                    Ok(Some(ExternalType {
                        source_path: path.as_ref().to_path_buf(),
                        package: package_name,
                        type_name: ty_name,
                        is_message: true,
                    }))
                } else {
                    Err(syn::Error::new(error_span, "no such type"))
                }
            } else if index == last_seg_number {
                Err(syn::Error::new(error_span, &{
                    let candidates = types
                        .keys()
                        .filter_map(|k| {
                            if k.is_empty() {
                                None
                            } else {
                                Some(k.to_owned())
                            }
                        })
                        .collect::<Vec<_>>()
                        .join(",");
                    format!(
                        "no such type in {:?}, you can try with package name: {}",
                        path.as_ref(),
                        candidates
                    )
                }))
            } else {
                Ok(None)
            }
        } else {
            Err(syn::Error::new(error_span, "missing type name"))
        }
    }
}

#[derive(Default)]
struct PbTypes {
    _scope: String,
    messages: HashSet<String>,
    enums: HashSet<String>,
}

fn scan_types(
    span: proc_macro2::Span,
    source_file_path: impl AsRef<Path>,
) -> syn::Result<HashMap<String, PbTypes>> {
    let contents = fs::read_to_string(source_file_path.as_ref()).map_err(|err| {
        syn::Error::new(
            span,
            format!(
                "cannot read source file: {:?}({err})",
                source_file_path.as_ref()
            ),
        )
    })?;
    let mut source = contents.as_str();
    let mut default_scope = PbTypes::default();
    let mut scopes = HashMap::new();
    while !source.is_empty() {
        if let Some(macro_pos) = source.find("protobuf!") {
            if macro_pos > 0 {
                let before = source.split_at(macro_pos).0;
                if let Some(nl) = before.rfind("\n") {
                    // check is commented to this macro
                    if before.split_at(nl).1.contains("//") {
                        // has comment before protobuf!
                        continue;
                    }
                }
            }
            source = source.split_at(macro_pos + 10).1;
        } else {
            break;
        }

        if let Ok((rest, (package, types))) = fast_pb_parser::pb_proto(source) {
            source = rest;
            let messages = types.iter().filter_map(|t| {
                if let fast_pb_parser::Element::Message(m) = t {
                    Some(if m.suffix.is_empty() {
                        m.name.to_owned()
                    } else {
                        format!("{}{}", m.name.to_case(Case::UpperCamel), m.suffix)
                    })
                } else {
                    None
                }
            });
            let enums = types.iter().filter_map(|t| {
                if let fast_pb_parser::Element::Enum(e) = t {
                    Some(e.name.to_owned())
                } else {
                    None
                }
            });
            if let Some(package) = package {
                scopes.insert(
                    package.to_owned(),
                    PbTypes {
                        _scope: package.to_owned(),
                        messages: messages.collect(),
                        enums: enums.collect(),
                    },
                );
            } else {
                default_scope.messages.extend(messages);
                default_scope.enums.extend(enums);
            }
        } else {
            // broken but can still parse next protobuf!
        }
    }
    scopes.insert("".to_owned(), default_scope);
    Ok(scopes)
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

    #[derive(Debug)]
    pub enum Element<'a> {
        Message(Message<'a>),
        Enum(Enum<'a>),
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
                    .map(|rpc| {
                        if rpc.gen_request && rpc.gen_response {
                            vec![
                                Message {
                                    name: rpc.name,
                                    suffix: "Request",
                                },
                                Message {
                                    name: rpc.name,
                                    suffix: "Response",
                                },
                            ]
                        } else if rpc.gen_request {
                            vec![Message {
                                name: rpc.name,
                                suffix: "Request",
                            }]
                        } else if rpc.gen_response {
                            vec![Message {
                                name: rpc.name,
                                suffix: "Response",
                            }]
                        } else {
                            vec![]
                        }
                    })
                    .flatten()
                    .collect()
            }),
            tag_ws_around!("}"),
        )(rest)
    }

    struct Rpc<'a> {
        name: &'a str,
        gen_request: bool,
        gen_response: bool,
    }

    fn pb_rpc(input: &str) -> IResult<&str, Rpc> {
        let (rest, _) = tag_ws_around!("rpc")(input)?;
        let (rest, rpc_name) = ident(rest)?;
        let (rest, gen_request) = delimited(
            tag_ws_around!("("),
            preceded(
                opt(tag_ws_around!("stream")),
                alt((map(pb_message_body, |_| true), map(ident, |_| false))),
            ),
            tag_ws_around!(")"),
        )(rest)?;
        let (rest, _) = tag_ws_around!("returns")(rest)?;
        let (rest, gen_response) = delimited(
            tag_ws_around!("("),
            preceded(
                opt(tag_ws_around!("stream")),
                alt((map(pb_message_body, |_| true), map(ident, |_| false))),
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
                name: rpc_name,
                gen_request,
                gen_response,
            },
        ))
    }

    fn pb_message_body(input: &str) -> IResult<&str, ()> {
        map(
            delimited(
                tag_ws_around!("{"),
                many0(alt((
                    pb_reserved,
                    pb_extend,
                    map(pb_message, |_| ()),
                    map(pb_enum, |_| ()),
                    pb_field,
                ))),
                tag_ws_around!("}"),
            ),
            |_| (),
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

    #[derive(Debug)]
    pub struct Message<'a> {
        pub name: &'a str,
        pub suffix: &'static str,
    }

    fn pb_message(input: &str) -> IResult<&str, Message> {
        let (rest, message_name) = delimited(tag_ws_around!("message"), ident, ws)(input)?;
        let (rest, _) = pb_message_body(rest)?;
        Ok((
            rest,
            Message {
                name: message_name,
                suffix: "",
            },
        ))
    }

    #[derive(Debug)]
    pub struct Enum<'a> {
        pub name: &'a str,
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
}

use std::ops::RangeInclusive;
use std::path::PathBuf;

use indexmap::IndexMap;
use proc_macro2::Span;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::Ident;
use syn::LitBool;
use syn::LitFloat;
use syn::LitInt;
use syn::LitStr;
use syn::Token;
use syn_prelude::ToIdent;
use syn_prelude::ToLitStr;

use crate::dep::Deps;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ProtobufPath {
    pub segments: Punctuated<Ident, Token![.]>,
}

impl ProtobufPath {
    pub fn from_ident(name: Ident) -> Self {
        let mut slf = Self {
            segments: Punctuated::new(),
        };
        slf.segments.push(name);
        slf
    }

    pub fn new_empty(span: Span) -> Self {
        let mut segments = Punctuated::new();
        segments.push_value(("google", span).to_ident());
        segments.push_punct(Token![.](span));
        segments.push_value(("protobuf", span).to_ident());
        segments.push_punct(Token![.](span));
        segments.push(("Empty", span).to_ident());
        Self { segments }
    }
}

impl ProtobufPath {
    pub fn local_name(&self) -> &Ident {
        if let Some(last) = self.segments.last() {
            return last;
        } else {
            unreachable!()
        }
    }
}

/// Protobuf syntax.
#[derive(Debug, Clone, Copy)]
pub enum Syntax {
    /// Protobuf syntax [2](https://developers.google.com/protocol-buffers/docs/proto)
    Proto2(Span),
    /// Protobuf syntax [3](https://developers.google.com/protocol-buffers/docs/proto3) (default)
    Proto3(Option<Span>),
}

impl Syntax {
    pub fn version(&self) -> usize {
        match self {
            Self::Proto2(_) => 2,
            Self::Proto3(_) => 3,
        }
    }
}

/// A field rule
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum Modifier {
    /// A well-formed message can have zero or one of this field (but not more than one).
    Optional,
    /// This field can be repeated any number of times (including zero) in a well-formed message.
    /// The order of the repeated values will be preserved.
    Repeated,
    /// A well-formed message must have exactly one of this field.
    Required,
}

/// Protobuf group
#[derive(Debug, Clone)]
pub struct Group {
    /// Group name
    pub name: Ident,
    pub fields: Vec<Field>,
}

/// Protobuf supported field types
#[derive(Debug, Clone)]
pub enum FieldType {
    /// Protobuf int32
    ///
    /// # Remarks
    ///
    /// Uses variable-length encoding. Inefficient for encoding negative numbers – if
    /// your field is likely to have negative values, use sint32 instead.
    Int32(Span),
    /// Protobuf int64
    ///
    /// # Remarks
    ///
    /// Uses variable-length encoding. Inefficient for encoding negative numbers – if
    /// your field is likely to have negative values, use sint64 instead.
    Int64(Span),
    /// Protobuf uint32
    ///
    /// # Remarks
    ///
    /// Uses variable-length encoding.
    Uint32(Span),
    /// Protobuf uint64
    ///
    /// # Remarks
    ///
    /// Uses variable-length encoding.
    Uint64(Span),
    /// Protobuf sint32
    ///
    /// # Remarks
    ///
    /// Uses ZigZag variable-length encoding. Signed int value. These more efficiently
    /// encode negative numbers than regular int32s.
    Sint32(Span),
    /// Protobuf sint64
    ///
    /// # Remarks
    ///
    /// Uses ZigZag variable-length encoding. Signed int value. These more efficiently
    /// encode negative numbers than regular int32s.
    Sint64(Span),
    /// Protobuf bool
    Bool(Span),
    /// Protobuf fixed64
    ///
    /// # Remarks
    ///
    /// Always eight bytes. More efficient than uint64 if values are often greater than 2^56.
    Fixed64(Span),
    /// Protobuf sfixed64
    ///
    /// # Remarks
    ///
    /// Always eight bytes.
    Sfixed64(Span),
    /// Protobuf double
    Double(Span),
    /// Protobuf string
    ///
    /// # Remarks
    ///
    /// A string must always contain UTF-8 encoded or 7-bit ASCII text.
    String(Span),
    /// Protobuf bytes
    ///
    /// # Remarks
    ///
    /// May contain any arbitrary sequence of bytes.
    Bytes(Span),
    /// Protobut fixed32
    ///
    /// # Remarks
    ///
    /// Always four bytes. More efficient than uint32 if values are often greater than 2^28.
    Fixed32(Span),
    /// Protobut sfixed32
    ///
    /// # Remarks
    ///
    /// Always four bytes.
    Sfixed32(Span),
    /// Protobut float
    Float(Span),
    /// Protobuf message or enum (holds the name)
    MessageOrEnum(Type),
    /// Protobut map
    Map(MapType),
    /// Protobuf group (deprecated)
    Group(Group),
}

#[derive(Debug, Clone)]
pub struct MapType {
    pub span: Span,
    pub key: Box<FieldType>,
    pub value: Box<FieldType>,
}

impl FieldType {
    #[allow(unused)]
    pub fn span(&self) -> Span {
        match self {
            Self::Int32(span) => *span,
            Self::Int64(span) => *span,
            Self::Uint32(span) => *span,
            Self::Uint64(span) => *span,
            Self::Sint32(span) => *span,
            Self::Sint64(span) => *span,
            Self::Bool(span) => *span,
            Self::Fixed64(span) => *span,
            Self::Sfixed64(span) => *span,
            Self::Double(span) => *span,
            Self::String(span) => *span,
            Self::Bytes(span) => *span,
            Self::Fixed32(span) => *span,
            Self::Sfixed32(span) => *span,
            Self::Float(span) => *span,
            Self::MessageOrEnum(t) => t.type_path.span(),
            Self::Map(map) => map.span,
            Self::Group(group) => group.name.span(),
        }
    }
    pub fn is_message_or_enum(&self) -> bool {
        match self {
            Self::MessageOrEnum(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Type {
    pub type_path: ProtobufPath,
    pub target_is_message: bool,
    pub ty: syn::Type,
}

#[derive(Debug, Clone)]
pub enum TagValue {
    Value(Span, i32),
    AutoIncr,
}

impl ToLitStr for TagValue {
    fn to_lit_str(&self) -> LitStr {
        if let Self::Value(span, value) = self {
            LitStr::new(&format!("{}", value), *span)
        } else {
            unreachable!()
        }
    }
}

/// A Protobuf Field
#[derive(Debug, Clone)]
pub struct Field {
    /// Field name
    pub name: Ident,
    pub field_name: Ident,
    /// Field `Rule`
    pub modifier: Option<Modifier>,
    /// Field type
    pub typ: FieldType,
    /// Tag number
    pub tag: TagValue,
    /// Non-builtin options
    pub options: Vec<ProtobufOption>,
    pub enum_field: bool,
}

/// A Protobuf field of oneof group
#[derive(Debug, Clone)]
pub enum MessageElement {
    Field(Field),
    OneOf(OneOf),
}

#[derive(Debug, Clone, Copy)]
pub enum NestedTypeIndex {
    Message(usize),
    Enum(usize),
    Oneof(usize),
}

/// A protobuf message
#[derive(Debug, Clone)]
pub struct Message {
    /// Message name
    pub name: Ident,
    pub struct_name: Ident,
    pub nested_mod_name: Option<Ident>,
    /// Message fields and oneofs
    pub fields: Vec<MessageElement>,
    /// Message reserved numbers
    pub reserved_nums: Vec<RangeInclusive<i32>>,
    /// Message reserved names
    pub reserved_names: Vec<Ident>,
    /// Nested messages
    pub messages: Vec<Message>,
    /// Nested enums
    pub enums: Vec<Enumeration>,
    /// Non-builtin options
    pub options: Vec<ProtobufOption>,
    /// Extension field numbers
    pub extension_ranges: Vec<RangeInclusive<i32>>,
    /// Extensions
    pub extensions: Vec<Extension>,
    pub nested_types: Vec<NestedTypeIndex>,
}

/// A protobuf enumeration field
#[derive(Debug, Clone)]
pub struct EnumValue {
    /// enum value name
    pub name: Ident,
    /// variant name from Self::name
    pub variant_name: Ident,
    /// proto_name from Self::name
    pub proto_name: LitStr,
    /// enum value number
    pub tag: Option<LitInt>,
    /// enum value options
    pub options: Option<Punctuated<ProtobufOption, Token![,]>>,
}

/// A protobuf enumerator
#[derive(Debug, Clone)]
pub struct Enumeration {
    pub nested_mod_name: Option<Ident>,
    /// enum name
    pub name: Ident,
    /// enum values
    pub values: Vec<EnumValue>,
    /// enum options
    pub options: Vec<ProtobufOption>,
    /// enum reserved numbers
    pub reserved_nums: Vec<RangeInclusive<i32>>,
    /// enum reserved names
    pub reserved_names: Vec<Ident>,
}

/// A OneOf
#[derive(Debug, Clone)]
pub struct OneOf {
    /// OneOf name
    pub name: Ident,
    pub field_name: Ident,
    pub nested_mod_name: Ident,
    pub field_lit: LitStr,
    pub enum_name: Ident,
    pub tags: LitStr,
    /// OneOf fields
    pub fields: Vec<Field>,
    /// oneof options
    pub options: Vec<ProtobufOption>,
}

#[derive(Debug, Clone)]
pub struct Extension {
    /// Extend this type with field
    pub extendee: ProtobufPath,
    /// Extension field
    pub field: Field,
}

/// Service method
#[derive(Debug, Clone)]
pub struct Method {
    /// Method name
    pub name: Ident,
    // snake case name
    pub method_name: Ident,
    // generated input message
    pub input_message: Option<Message>,
    /// Input type
    pub input_type: Type,
    // generated output message
    pub output_message: Option<Message>,
    /// Output type
    pub output_type: Type,
    /// If this method is client streaming
    #[allow(dead_code)] // TODO
    pub client_streaming: Option<Span>,
    /// If this method is server streaming
    #[allow(dead_code)] // TODO
    pub server_streaming: Option<Span>,
    /// Method options
    pub options: Punctuated<ProtobufOption, Token![;]>,
}

/// Service definition
#[derive(Debug, Clone)]
pub struct Service {
    /// Service name
    pub name: Ident,
    pub code_name: Ident,
    pub methods: Vec<Method>,
    pub options: Vec<ProtobufOption>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct AnyTypeUrl {
    pub prefix: ProtobufPath,
    pub full_type_name: ProtobufPath,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum ProtobufConstantMessageFieldName {
    Regular(Ident),
    Extension(ProtobufPath),
    AnyTypeUrl(AnyTypeUrl),
}

#[derive(Debug, Clone)]
pub struct ProtobufConstantMessage {
    pub fields: IndexMap<ProtobufConstantMessageFieldName, ProtobufConstant>,
}

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum ProtobufConstant {
    U64(LitInt, bool),
    F64(LitFloat, bool),
    Bool(LitBool),
    Ident(ProtobufPath),
    String(LitStr),
    Message(ProtobufConstantMessage),
}

/// Equivalent of `UninterpretedOption.NamePart`.
#[derive(Debug, Clone)]
pub enum ProtobufOptionNamePart {
    Direct(Ident),
    Ext(ProtobufPath),
}

#[derive(Debug, Clone)]
pub struct ProtobufOptionNameExt(pub Vec<ProtobufOptionNamePart>);

#[derive(Debug, Clone)]
pub enum ProtobufOptionName {
    Builtin(Ident),
    Ext(ProtobufOptionNameExt),
}

impl ProtobufOptionName {
    pub fn is_option(&self, opt_name: &str) -> bool {
        match self {
            ProtobufOptionName::Builtin(name) => name.eq(opt_name),
            ProtobufOptionName::Ext(ext) => {
                let mut compared_index = 0;
                for (index, n) in opt_name.split('.').enumerate() {
                    if let Some(seg) = ext.0.get(index) {
                        match seg {
                            ProtobufOptionNamePart::Direct(d) => {
                                if !d.eq(n) {
                                    return false;
                                }
                            }
                            ProtobufOptionNamePart::Ext(_ext) => return false,
                        }
                    } else {
                        return false;
                    }
                    compared_index = index;
                }
                ext.0.len() == compared_index + 1
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct ProtobufOption {
    pub name: ProtobufOptionName,
    pub value: ProtobufConstant,
}

pub trait GetOption {
    fn get_option<'a>(&'a self, name: &str) -> Option<&'a ProtobufOption>;
}

impl GetOption for Vec<ProtobufOption> {
    fn get_option<'a>(&'a self, name: &str) -> Option<&'a ProtobufOption> {
        self.iter().find(|opt| opt.name.is_option(name))
    }
}

impl<P> GetOption for Punctuated<ProtobufOption, P> {
    fn get_option<'a>(&'a self, name: &str) -> Option<&'a ProtobufOption> {
        self.iter().find(|opt| opt.name.is_option(name))
    }
}

/// Visibility of import statement
#[derive(Debug, Clone)]
pub enum ImportVis {
    Default,
    Public(Span),
    Weak(Span),
}

impl Default for ImportVis {
    fn default() -> Self {
        ImportVis::Default
    }
}

/// Import statement
#[derive(Debug, Clone)]
pub struct Import {
    pub import_token: Span,
    pub path: LitStr,
    pub builtin: bool,
    pub vis: ImportVis,
    pub file_path: Option<FilePath>,
}

#[derive(Debug, Clone)]
pub struct FilePath {
    pub root: bool,
    pub bin: bool,
    pub example: bool,
    pub is_mod: bool,
    pub path: PathBuf,
    pub mod_path: syn::Path,
}

#[derive(Debug, Clone)]
pub struct Package {
    pub package: Ident,
}

#[derive(Debug, Clone)]
pub enum DeclIndex {
    Message(usize),
    Enum(usize),
    Service(usize),
}

/// A File descriptor representing a whole .proto file
#[derive(Debug, Clone)]
pub struct Protocol {
    /// Imports
    pub imports: Vec<Import>,
    /// Package
    pub package: Option<Package>,
    /// Protobuf Syntax
    pub syntax: Syntax,
    /// Top level messages
    pub messages: Vec<Message>,
    /// Enums
    pub enums: Vec<Enumeration>,
    /// Extensions
    pub extensions: Vec<Extension>,
    /// Services
    pub services: Vec<Service>,
    /// Non-builtin options
    pub options: Vec<ProtobufOption>,
    pub decls: Vec<DeclIndex>,
    pub deps: Deps,
}

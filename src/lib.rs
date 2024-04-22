#![doc = include_str!("../README.md")]
#![feature(proc_macro_span)]

use model::Protocol;
use quote::ToTokens;
use syn::parse::Parse;

mod dep;
mod expand;
mod model;
mod parse;
mod resolve;

struct ProtobufMacro {
    inner: Protocol,
}

impl Parse for ProtobufMacro {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let span = proc_macro::Span::call_site();
        Ok(Self {
            inner: Protocol::parse_from_call_site(input, span.source_file().path())?,
        })
    }
}

impl ToTokens for ProtobufMacro {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.inner.to_tokens(tokens)
    }
}

#[proc_macro]
pub fn protobuf(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match syn::parse::<ProtobufMacro>(input) {
        Ok(protocol) => protocol.to_token_stream().into(),
        Err(err) => err.to_compile_error().into(),
    }
}

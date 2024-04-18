#![feature(proc_macro_span)]
use std::fs;

use quote::ToTokens;

mod expand;
mod model;
mod parse;
mod resolve;

#[proc_macro]
pub fn protobuf(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match syn::parse::<model::Protocol>(input) {
        Ok(protocol) => {
            fs::write(
                "/tmp/g.rs",
                protocol.to_token_stream().to_string().as_bytes(),
            );
            protocol.to_token_stream().into()
        }
        Err(err) => err.to_compile_error().into(),
    }
}

use quote::ToTokens;

mod expand;
mod model;
mod parse;

#[proc_macro]
pub fn protobuf(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match syn::parse::<model::Protocol>(input) {
        Ok(protocol) => protocol.to_token_stream().into(),
        Err(err) => err.to_compile_error().into(),
    }
}

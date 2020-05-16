extern crate proc_macro;

use proc_macro::TokenStream;

use syn::{parse_macro_input, DeriveInput};

mod expand;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    expand::derive(&input)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

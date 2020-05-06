extern crate proc_macro;

use proc_macro::TokenStream;

use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields, Ident};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    let struct_name = generate_builder_name(&name);
    let builder = generate_builder_struct(&struct_name, &input.data);

    let expanded = quote! {
        #builder
    };

    TokenStream::from(expanded)
}

fn generate_builder_name(name: &Ident) -> Ident {
    let builder_name = format!("{}Builder", name);
    Ident::new(&builder_name, Span::call_site())
}

fn generate_builder_struct(struct_name: &Ident, data: &Data) -> proc_macro2::TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let names = fields.named.iter().map(|f| {
                    let name = f.ident.as_ref().unwrap();
                    quote! { #name }
                });

                let names2 = names.clone();

                let tys = fields.named.iter().map(|f| {
                    let ty = &f.ty;
                    quote! { Option<#ty> }
                });

                quote! {
                    pub struct #struct_name {
                        #(#names2: #tys,)*
                    }


                    impl #struct_name {
                        fn builder() -> #struct_name {
                            #struct_name {
                                #(#names: None,)*
                            }
                        }
                    }
                }
            }
            Fields::Unnamed(ref fields) => unimplemented!(),
            Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

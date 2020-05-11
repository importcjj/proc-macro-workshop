extern crate proc_macro;

use proc_macro::TokenStream;

use proc_macro2::Span;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields, Ident, Path, Type, TypePath};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let name = input.ident;

    let builder_name = generate_builder_name(&name);
    let builder = generate_builder_struct(&name, &builder_name, &input.data);

    let expanded = quote! {
        #builder
    };

    TokenStream::from(expanded)
}

fn generate_builder_name(name: &Ident) -> Ident {
    let builder_name = format!("{}Builder", name);
    Ident::new(&builder_name, Span::call_site())
}

fn generate_builder_struct(
    struct_name: &Ident,
    builder_name: &Ident,
    data: &Data,
) -> proc_macro2::TokenStream {
    match *data {
        Data::Struct(ref data) => match data.fields {
            Fields::Named(ref fields) => {
                let names_tys = fields.named.iter().map(|f| {
                    let name = f.ident.as_ref().unwrap();
                    let ty = match parse_inner_ty("Option", &f.ty) {
                        Some(inner) => inner,
                        None => &f.ty
                    };
                    quote! {#name: Option<#ty>}
                });

                let names = fields.named.iter().map(|f| {
                    let name = f.ident.as_ref().unwrap();
                    quote! { #name }
                });

                let setter_functions = fields.named.iter().map(|f| {
                    let name = f.ident.as_ref().unwrap();
                    let ty = match parse_inner_ty("Option", &f.ty) {
                        Some(inner) => inner,
                        None => &f.ty,
                    };

                    quote! {
                        pub fn #name(&mut self, #name: #ty) -> &mut Self {
                            self.#name = Some(#name);
                            self
                        }
                    }
                });

                let set_fields_values = fields.named.iter().map(|f| {
                    let name = f.ident.as_ref().unwrap();

                    match parse_inner_ty("Option", &f.ty) {
                        Some(_) => {
                            quote! {
                                #name: self.#name.take()
                            }
                        }
                        None => {
                            let err = format!("field {} missed", name);
                            quote! {
                                #name: self.#name.take().ok_or_else(|| {
                                    let e: Box<dyn std::error::Error> = #err.into();
                                    e
                                })?
                            }
                        }
                    }
                });

                quote! {
                    impl #struct_name {
                        pub fn builder() -> #builder_name {
                            #builder_name {
                                #(#names: None,)*
                            }
                        }
                    }

                    pub struct #builder_name {
                        #(#names_tys,)*
                    }

                    impl #builder_name {
                        #(#setter_functions)*


                        pub fn build(&mut self) -> Result<#struct_name, Box<dyn std::error::Error>> {
                            std::result::Result::Ok(#struct_name{
                                #(#set_fields_values,)*
                            })
                        }
                    }
                }
            }
            Fields::Unnamed(..) => unimplemented!(),
            Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    }
}

fn parse_inner_ty<'a>(outer: &str, ty: &'a Type) -> Option<&'a Type> {
    if let Type::Path(TypePath {
        qself: None,
        path: Path { segments, .. },
    }) = ty
    {
        let first_seg = &segments[0];
        if first_seg.ident != outer {
            return None;
        }

        if let syn::PathArguments::AngleBracketed(ref args) = first_seg.arguments {
            if let syn::GenericArgument::Type(ref ty) = &args.args[0] {
                return Some(ty);
            }
        }
    }

    None
}

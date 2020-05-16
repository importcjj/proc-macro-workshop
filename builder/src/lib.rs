extern crate proc_macro;

use proc_macro::TokenStream;

use proc_macro2::Span;
use quote::quote;
use syn::parse::ParseStream;
use syn::{
    parse_macro_input, Attribute, Data, DeriveInput, Fields, Ident, LitStr, Path, Result, Token,
    Type, TypePath,
};

mod expand;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    // let input = parse_macro_input!(input as DeriveInput);
    // let name = input.ident;

    // let builder_name = generate_builder_name(&name);
    // let builder = generate_builder_struct(&name, &builder_name, &input.data);

    // let expanded = quote! {
    //     #builder
    // };

    // TokenStream::from(expanded)

    let input = parse_macro_input!(input as DeriveInput);
    expand::derive(&input)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
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
                    let ty = match get_inner_ty("Option", &f.ty) {
                        Some(inner) => inner,
                        None => &f.ty,
                    };
                    quote! {#name: Option<#ty>}
                });

                let names = fields.named.iter().map(|f| {
                    let name = f.ident.as_ref().unwrap();
                    quote! { #name }
                });

                let setter_functions = fields.named.iter().map(|f| {
                    let name = f.ident.as_ref().unwrap();
                    let ty = match get_inner_ty("Option", &f.ty) {
                        Some(inner) => inner,
                        None => &f.ty,
                    };

                    let each_attr = get_each_attr(&f.attrs).unwrap();
                    match each_attr.name {
                        Some(each_name) => {
                            let vec_ty = get_inner_ty("Vec", &ty).unwrap();

                            quote! {
                                pub fn #each_name(&mut self, #each_name: #vec_ty) -> &mut Self {
                                    match self.#name {
                                        Some(ref mut v) => v.push(#each_name),
                                        None => self.#name = Some(vec![#each_name])
                                    }
                                    self
                                }
                            }
                        }
                        None => {
                            quote! {
                                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                                    self.#name = Some(#name);
                                    self
                                }
                            }
                        }
                    }
                });

                let set_fields_values = fields.named.iter().map(|f| {
                    let name = f.ident.as_ref().unwrap();

                    match get_inner_ty("Option", &f.ty) {
                        Some(_) => {
                            quote! {
                                #name: self.#name.take()
                            }
                        }
                        None => {
                            let each_attr = get_each_attr(&f.attrs).unwrap();
                            let is_vec = each_attr.name.is_some();
                            if is_vec {
                                quote! {
                                    #name: self.#name.take().unwrap_or_else(|| {
                                        vec![]
                                    })
                                }
                            } else {
                                let err = format!("field {} missed", name);
                                quote! {
                                    #name: self.#name.take().ok_or_else(|| {
                                        let e: Box<dyn std::error::Error> = #err.into();
                                        e
                                    })?
                                }
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

fn get_inner_ty<'a>(outer: &str, ty: &'a Type) -> Option<&'a Type> {
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

struct EachAttr {
    name: Option<Ident>,
}

fn get_each_attr(input: &[Attribute]) -> Result<EachAttr> {
    let mut each_attr = EachAttr { name: None };

    syn::custom_keyword!(each);

    for attr in input {
        if attr.path.is_ident("builder") {
            attr.parse_args_with(|input: ParseStream| {
                input.parse::<Option<each>>()?;
                input.parse::<Token![=]>()?;
                let lit = input.parse::<LitStr>()?;
                let name = Ident::new(&lit.value(), lit.span());
                each_attr.name = Some(name);
                Ok(())
            })?;
        }
    }

    Ok(each_attr)
}

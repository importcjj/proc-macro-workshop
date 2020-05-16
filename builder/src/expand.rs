use proc_macro2::{Span, TokenStream};
use quote::{format_ident, quote, quote_spanned};
use syn::parse::ParseStream;
use syn::spanned::Spanned;
use syn::{
    Attribute, Data, DataStruct, DeriveInput, Error, Fields, Ident, LitStr, Result, Token, Type,
    Visibility,
};

pub fn derive(input: &DeriveInput) -> Result<TokenStream> {
    let input = match *&input.data {
        Data::Struct(ref data) => Struct::from_syn(input, &data)?,
        _ => return Err(Error::new(Span::call_site(), "only supports struct")),
    };

    Ok(impl_struct(input))
}

pub fn impl_struct(input: Struct) -> TokenStream {
    let Struct {
        vis,
        name,
        target_name,
        fields,
    } = input;

    let builder_default_values = fields.iter().map(|fd| {
        let &Field { name, ref ty, .. } = fd;

        match ty {
            FieldType::Normal(..) | FieldType::Option(..) => {
                quote! { #name: std::option::Option::None }
            }
            FieldType::Element(..) => {
                quote! { #name: std::option::Option::Some(vec![]) }
            }
        }
    });

    let builder_fields = fields.iter().map(|fd| {
        let &Field {
            name,
            original,
            vis,
            ref ty,
            ..
        } = fd;

        match *ty {
            FieldType::Normal(ty) | FieldType::Option(ty) => {
                quote_spanned!(original.span() => #vis #name: std::option::Option<#ty>)
            }
            FieldType::Element(_, ty) => {
                quote_spanned!(original.span() => #vis #name: std::option::Option<Vec<#ty>>)
            }
        }
    });

    let builder_setter = fields.iter().map(|fd| {
        let &Field { name, ref ty, .. } = fd;

        match ty {
            FieldType::Normal(ty) | FieldType::Option(ty) => quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            },
            FieldType::Element(ref each_name, ty) => quote! {
                pub fn #each_name(&mut self, #each_name: #ty) -> &mut Self {
                    let v = self.#name.as_mut().unwrap();
                    v.push(#each_name);
                    self
                }
            },
        }
    });

    let take_fields = fields.iter().map(|fd| {
        let &Field { name, ref ty, .. } = fd;

        match ty {
            FieldType::Normal(..) => {
                let err_missed = format!("field {} missed", name);
                quote! {
                    #name: self.#name.take().ok_or_else(|| {
                        let e: std::boxed::Box<dyn std::error::Error> = #err_missed.into();
                        e
                    })?
                }
            }
            FieldType::Option(..) => {
                quote! { #name: self.#name.take()}
            }
            FieldType::Element(..) => {
                quote! { #name: self.#name.take().unwrap()}
            }
        }
    });

    quote! {
            impl #target_name {
                pub fn builder() -> #name {
                    #name {
                        #(#builder_default_values),*
                    }
                }
            }

            #vis struct #name {
                #(#builder_fields),*
            }

            impl #name {
               #(#builder_setter)*


                pub fn build(&mut self) -> std::result::Result<#target_name, std::boxed::Box<dyn std::error::Error>> {
                    std::result::Result::Ok(#target_name {
                        #(
                        #take_fields,
                    )*
                })
            }
        }
    }
}

struct Field<'a> {
    original: &'a syn::Field,
    vis: &'a Visibility,
    name: &'a Ident,
    ty: FieldType<'a>,
}

enum FieldType<'a> {
    Normal(&'a Type),
    Option(&'a Type),
    Element(Ident, &'a Type),
}

impl<'a> Field<'a> {
    fn from_syn(input: &'a syn::Field) -> Result<Self> {
        let vis = &input.vis;
        let name = input.ident.as_ref().unwrap();
        let mut field = Field {
            original: input,
            vis,
            name,
            ty: FieldType::Normal(&input.ty),
        };

        let each_attr = get_each_attr(&input.attrs)?;
        match each_attr.name {
            Some(name) => {
                if let Some(element_ty) = get_inner_ty("Vec", &input.ty) {
                    field.ty = FieldType::Element(name, &element_ty);
                } else {
                    return Err(Error::new(input.span(), "vec type required"));
                }
            }
            None => match get_inner_ty("Option", &input.ty) {
                Some(inner_ty) => field.ty = FieldType::Option(inner_ty),
                None => field.ty = FieldType::Normal(&input.ty),
            },
        }

        Ok(field)
    }
}

pub struct Struct<'a> {
    vis: &'a Visibility,
    name: Ident,
    target_name: &'a Ident,
    fields: Vec<Field<'a>>,
}

impl<'a> Struct<'a> {
    pub fn from_syn(input: &'a DeriveInput, data: &'a DataStruct) -> Result<Self> {
        let vis = &input.vis;
        let target_name = &input.ident;
        let name = generate_name(&input.ident);
        let fields = generate_fields(&data.fields)?;

        Ok(Struct {
            vis,
            name,
            target_name,
            fields,
        })
    }
}

// Generate builder's name.
fn generate_name(name: &Ident) -> Ident {
    format_ident!("{}Builder", name)
}

fn generate_fields(input: &Fields) -> Result<Vec<Field>> {
    match *input {
        Fields::Named(ref named) => {
            let mut fields = vec![];
            for input in named.named.iter() {
                let field = Field::from_syn(&input)?;
                fields.push(field);
            }

            Ok(fields)
        }
        _ => Err(Error::new_spanned(input, "only support named fields")),
    }
}

fn get_inner_ty<'a>(outer: &str, ty: &'a Type) -> Option<&'a Type> {
    let path = match ty {
        Type::Path(ty) => &ty.path,
        _ => return None,
    };

    let last = path.segments.last().unwrap();
    if last.ident != outer {
        return None;
    }

    let args = match &last.arguments {
        syn::PathArguments::AngleBracketed(ref args) => args,
        _ => return None,
    };

    match args.args.first().unwrap() {
        syn::GenericArgument::Type(ref ty) => Some(ty),
        _ => None,
    }
}

struct EachAttr {
    name: Option<Ident>,
}

fn get_each_attr(input: &[Attribute]) -> Result<EachAttr> {
    let mut each_attr = EachAttr { name: None };

    syn::custom_keyword!(each);

    for attr in input {
        if attr.path.is_ident("builder") {
            let meta = attr.parse_meta()?;
            attr.parse_args_with(|input: ParseStream| {
                input
                    .parse::<each>()
                    .map_err(|_| Error::new_spanned(meta, r#"expected `builder(each = "...")`"#))?;
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

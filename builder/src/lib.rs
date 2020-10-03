extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use regex::Regex;
use syn::export::Span;
use syn::Data::Struct;
use syn::DeriveInput;
use syn::GenericArgument::Type;
use syn::PathArguments::AngleBracketed;

fn get_inner_type(segment: &syn::PathSegment) -> syn::Type {
    let args = &segment.arguments;
    match args {
        AngleBracketed(ref angle_args) => {
            let generic_arg = angle_args.args.first().unwrap();
            match generic_arg {
                Type(t) => t.clone(),
                _ => unimplemented!(),
            }
        }
        _ => unimplemented!(),
    }
}

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(input).unwrap();

    let identifier = ast.ident;
    let builder_name = format!("{}Builder", identifier);
    let builder_name = syn::Ident::new(&builder_name, Span::call_site());
    let data = match ast.data {
        Struct(d) => d.fields,
        _ => unimplemented!(),
    };
    let field_iter = data.iter();

    let mut option_field_names = vec![];
    let mut option_field_types = vec![];
    let mut normal_field_names = vec![];
    let mut normal_field_types = vec![];

    let mut builder_arg_fields = vec![];
    let mut builder_arg_field_names = vec![];
    let mut builder_arg_field_types = vec![];

    for f in field_iter {
        let name = &f.ident;
        if let Some(ref attr) = f.attrs.first() {
            let first_segment = attr.path.segments.first().unwrap();
            match first_segment.ident.to_string().as_ref() {
                "builder" => match &f.ty {
                    syn::Type::Path(t) => {
                        let token_str = attr.tokens.to_string();
                        let each_regex = Regex::new(r#"each = "([[:alnum:]]+)"#).unwrap();
                        if let Some(captures) = each_regex.captures(token_str.as_ref()) {
                        let arg_name = String::from(captures.get(1).unwrap().as_str());
                        builder_arg_fields.push(name.as_ref().unwrap());
                        builder_arg_field_names.push(syn::Ident::new(arg_name.as_str(), Span::call_site()));

                        let type_first_segment = t.path.segments.first().unwrap();
                        let inner_type = get_inner_type(type_first_segment);
                        builder_arg_field_types.push(inner_type);
                        } else {
                           let err = syn::Error::new(Span::call_site(), r#"expected `builder(each = "...")`"#).to_compile_error();
                           let tokens = quote! { #err };
                            
                           return tokens.into();
                        }

                    }
                    _ => unimplemented!(),
                },
                _ => unimplemented!(),
            }
        } else {
            if let syn::Type::Path(t) = &f.ty {
                let first_segment = t.path.segments.first().unwrap();

                if first_segment.ident.to_string().starts_with("Option") {
                    option_field_names.push(name.as_ref().unwrap());
                    option_field_types.push(get_inner_type(first_segment));
                } else {
                    normal_field_names.push(name.as_ref().unwrap());

                    normal_field_types.push(&f.ty);
                }
            }
        }
    }

    let mut field_names = vec![];
    let mut field_types = vec![];
    field_names.extend_from_slice(normal_field_names.as_slice());
    field_names.extend_from_slice(option_field_names.as_slice());
    field_types.extend_from_slice(normal_field_types.as_slice());
    for ref t in option_field_types.as_slice() {
        field_types.push(&t);
    }

    let tokens = quote! {
        pub struct #builder_name {
            #( #field_names: std::option::Option<#field_types>, )*
            
            #( #builder_arg_fields: std::option::Option<Vec<#builder_arg_field_types>>, )*
        }

        impl #builder_name {
            #( pub fn #field_names(&mut self, value: #field_types) -> &mut Self {
                self.#field_names = std::option::Option::Some(value);
                self
            })*

            #( pub fn #builder_arg_field_names(&mut self, value: #builder_arg_field_types) -> &mut Self {
                let vec = self.#builder_arg_fields.as_mut().unwrap();
                vec.push(value);
                self
            })*

            fn create_struct(&mut self) -> std::result::Result<#identifier, std::io::Error> {
                Ok(#identifier {
                    #( #normal_field_names: self.#normal_field_names.take().ok_or(std::io::ErrorKind::NotFound)?, )*
                    #( #option_field_names: self.#option_field_names.take(), )*
                    #( #builder_arg_fields: self.#builder_arg_fields.take().unwrap(), )*
                })
            }

            pub fn build(&mut self) -> std::option::Option<#identifier> {
                let x = self.create_struct();

                return x.ok()
            }
        }

        impl #identifier {
            pub fn builder() -> #builder_name {
               #builder_name {
                    #( #field_names: std::option::Option::None, )*
                    #( #builder_arg_fields: std::option::Option::Some(Vec::<#builder_arg_field_types>::new()), )*
               }
            }
        }

    };

    // eprintln!("TOKENS: {}", tokens);

    tokens.into()
}

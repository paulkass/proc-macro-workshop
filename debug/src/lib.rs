extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;
use syn::parse_quote;
use syn::token::Comma;
use syn::DeriveInput;
use syn::GenericArgument;
use syn::Lit;
use syn::Meta;
use syn::PathArguments;
use syn::WherePredicate;

fn add_debug_trait(mut generics: syn::Generics) -> syn::Generics {
    for param in &mut generics.params {
        if let syn::GenericParam::Type(p) = param {
            p.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }
    generics
}

enum TypeEnum<'a> {
    NormalType(&'a syn::Type),
    PhantomType(&'a syn::Type),
    AssociatedType(&'a syn::Type),
}

fn record_type<'a>(t: &'a syn::Type) -> TypeEnum<'a> {
    match t {
        syn::Type::Path(path) => {
            match &path.qself {
                Some(_) => TypeEnum::AssociatedType(&t),
                None => {
                    let last_segment = path.path.segments.last().unwrap();
                    match &last_segment.arguments {
                        syn::PathArguments::AngleBracketed(angle_args) => {
                            if last_segment.ident == syn::Ident::new("PhantomData", proc_macro2::Span::call_site()) {
                                TypeEnum::PhantomType(&t)
                            } else {
                                match angle_args.args.first().unwrap() {
                                    syn::GenericArgument::Type(arg_type) => {
                                        record_type(arg_type)
                                    },
                                    _ => unimplemented!(),
                                }
                            }
                        },
                        syn::PathArguments::Parenthesized(_) => unimplemented!(), 
                        syn::PathArguments::None => {
                            TypeEnum::NormalType(&t)
                        },
                    }
                },
            }
        },
        _ => unimplemented!(),
    }
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let mut generics = ast.generics;

    let struct_name = ast.ident;
    let struct_name_str = format!("{}", struct_name);

    let mut field_names = vec![];
    let mut field_names_str = vec![];
    let mut field_types = vec![];
    let mut field_formats = vec![];

    let mut normal_field_types = vec![];
    let mut phantom_data_types = vec![];
    let mut associated_data_types = vec![];

    match ast.data {
        syn::Data::Struct(data) => match data.fields {
            syn::Fields::Named(fields) => {
                for f in fields.named.iter() {
                    let field_name = f.ident.clone().unwrap();
                    field_names_str.push(format!("{}", field_name));
                    field_names.push(field_name);
                    field_types.push(&f.ty);

                    match &f.ty {
                        syn::Type::Path(path) => match &path.qself {
                            Some(_) => {
                                associated_data_types.push((&f.ty).clone());
                            },
                            None => {
                                let first_segment = path.path.segments.first().unwrap();
                                println!("{:?}", first_segment.ident);
                                match first_segment.ident.to_string().as_str() {
                                    "PhantomData" => match &first_segment.arguments {
                                        PathArguments::AngleBracketed(brackets) => {
                                            match brackets.args.first().unwrap() {
                                                GenericArgument::Type(t) => {
                                                    phantom_data_types.push(t.clone());
                                                }
                                                _ => unimplemented!(),
                                            }
                                        }
                                        _ => unimplemented!(),
                                    },
                                    _ => {
                                        normal_field_types.push(&f.ty);
                                    }
                                }
                            }
                        },
                        _ => {}
                    }

                    let mut format_option = None;
                    for attr in &f.attrs {
                        match attr.parse_meta().unwrap() {
                            Meta::NameValue(name_value_meta) => {
                                let name = name_value_meta
                                    .path
                                    .segments
                                    .last()
                                    .unwrap()
                                    .ident
                                    .to_string();
                                match name.as_str() {
                                    "debug" => match name_value_meta.lit {
                                        Lit::Str(lit_str) => {
                                            format_option = Some(lit_str.value());
                                        }
                                        _ => unimplemented!(),
                                    },
                                    _ => unimplemented!(),
                                }
                            }
                            _ => unimplemented!(),
                        }
                    }
                    field_formats.push(format_option.unwrap_or(String::from("{:?}")));
                }
            }
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }

    println!("We have {:?} associated types", associated_data_types.len());
    for param in &mut generics.params {
        if let syn::GenericParam::Type(p) = param {
            if phantom_data_types.iter().any(|t| match t {
                syn::Type::Path(path) => {
                    let path_ident = &path.path.segments.first().unwrap().ident;
                    path_ident == &p.ident
                }
                _ => false,
            }) {
                let where_token = syn::token::Where {
                    span: syn::export::Span::call_site(),
                };
                let mut where_clause_option = generics.where_clause.take();
                let where_clause = where_clause_option.get_or_insert(syn::WhereClause {
                    where_token: where_token,
                    predicates: syn::punctuated::Punctuated::<WherePredicate, Comma>::new(),
                });
                where_clause
                    .predicates
                    .push_value(parse_quote!(PhantomData<#p>: std::fmt::Debug));
                generics.where_clause = Some(where_clause_option.take().unwrap());
            } else if associated_data_types.iter().any(|t| match t {
                syn::Type::Path(path_type) => {
                    println!("choosing associated type");
                    let path_ident = &path_type.path.segments.first().unwrap().ident;
                    println!("{:?}", path_ident);
                    println!("{:?}", p.ident);
                    path_ident == &p.ident
                },
                _ => { println!("wow"); false },
            }) {
                println!("associated type here");
            } else {
                p.bounds.push(parse_quote!(std::fmt::Debug));
            }
        }
    }

    let (impl_generics, type_generics, where_clause) = generics.split_for_impl();
    let tokens = quote! {
        impl #impl_generics std::fmt::Debug for #struct_name #type_generics #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#struct_name_str)
                    #( .field(#field_names_str, &format_args!(#field_formats, &self.#field_names)) )*
                    .finish()
            }
        }
    };

    tokens.into()
}

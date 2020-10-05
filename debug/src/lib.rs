extern crate proc_macro;

use proc_macro::TokenStream;
use quote::ToTokens;
use core::str::FromStr;
use quote::quote;
use std::iter::FromIterator;
use syn::parse_macro_input;
use syn::parse_quote;
use syn::token::Comma;
use syn::DeriveInput;
use syn::Lit;
use syn::Meta;
use syn::WherePredicate;

enum TypeEnum {
    NormalType(syn::Type),
    PhantomType(syn::Type),
    AssociatedType(syn::Type),
    ParameterType(syn::Type),
}

fn record_type(t: syn::Type, v: &Vec<syn::Ident>) -> TypeEnum {
    let t_clone = t.clone();
    match t {
        syn::Type::Path(path) => match &path.qself {
            Some(qself) => {
                let q_clone = qself.ty.as_ref().clone();
                match qself.ty.as_ref() {
                    syn::Type::Path(ty) => {
                        let type_ident = &ty.path.segments.first().unwrap().ident;
                        if v.contains(&type_ident) {
                            TypeEnum::AssociatedType(t_clone)
                        } else {
                            record_type(q_clone, v)
                        }
                    }
                    _ => unimplemented!(),
                }
            }
            None => {
                let first_segment = path.path.segments.first().unwrap();
                match &first_segment.arguments {
                    syn::PathArguments::AngleBracketed(angle_args) => {
                        if first_segment.ident
                            == syn::Ident::new("PhantomData", proc_macro2::Span::call_site())
                        {
                            TypeEnum::PhantomType(t_clone)
                        } else {
                            match angle_args.args.first().unwrap() {
                                syn::GenericArgument::Type(arg_type) => {
                                    let arg_clone = arg_type.clone();
                                    record_type(arg_clone, &v)
                                }
                                _ => unimplemented!(),
                            }
                        }
                    }
                    syn::PathArguments::Parenthesized(_) => unimplemented!(),
                    syn::PathArguments::None => {
                        if v.contains(&first_segment.ident) && path.path.segments.len() > 1 {
                            TypeEnum::AssociatedType(t_clone)
                        } else if v.contains(&first_segment.ident) {
                            TypeEnum::ParameterType(t_clone)
                        } else {
                            TypeEnum::NormalType(t_clone)
                        }
                    }
                }
            }
        },
        syn::Type::Reference(reference) => {
            let ref_type = reference.elem.as_ref().clone();
            TypeEnum::NormalType(ref_type)
        }
        _ => TypeEnum::NormalType(t_clone),
    }
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);

    let mut generics = ast.generics;
    let generic_params = {
        let generics_bor = &generics.params;
        Vec::from_iter(
            generics_bor
                .iter()
                .map(|g| match g {
                    syn::GenericParam::Type(k) => Some(k.ident.clone()),
                    _ => None,
                })
                .filter(Option::is_some)
                .map(|g| g.unwrap()),
        )
    };

    let struct_name = ast.ident;
    let struct_name_str = format!("{}", struct_name);

    let mut field_names = vec![];
    let mut field_names_str = vec![];
    let mut field_types = vec![];
    let mut field_formats = vec![];

    let mut where_types = vec![];

    let mut where_clause_option = None;

    match ast.data {
        syn::Data::Struct(data) => match data.fields {
            syn::Fields::Named(fields) => {
                for f in fields.named.iter() {
                    let field_name = f.ident.clone().unwrap();
                    field_names_str.push(format!("{}", field_name));
                    field_names.push(field_name);
                    field_types.push(&f.ty);

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

                    where_types.push(record_type((&f.ty).clone(), &generic_params));
                }
            }
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    }

    for attr in ast.attrs {
        let attr_name = attr.path.segments.first().unwrap().ident.clone();
        match attr_name.to_string().as_str() {
            "debug" => match attr.parse_args().expect("Could not unwrap deubg attr") {
                Meta::NameValue(name_value_meta) => {
                    let name_ident = name_value_meta
                        .path
                        .segments
                        .first()
                        .unwrap()
                        .ident
                        .clone();
                    match name_ident.to_string().as_str() {
                        "bound" => {
                            if let Lit::Str(value) = name_value_meta.lit {
                                where_types.clear();

                                let v = format!("where {}", value.value());
                                where_clause_option = Some(v);
                            }
                        }
                        _ => unimplemented!(),
                    }
                }
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    drop(generic_params);

    for te in where_types {
        let where_token = syn::token::Where {
            span: syn::export::Span::call_site(),
        };

        let mut where_clause_option = generics.where_clause.take();

        let where_clause = where_clause_option.get_or_insert(syn::WhereClause {
            where_token: where_token,
            predicates: syn::punctuated::Punctuated::<WherePredicate, Comma>::new(),
        });

        match te {
            TypeEnum::NormalType(_) => {}
            TypeEnum::ParameterType(t) => {
                for param in &mut generics.params {
                    if let syn::GenericParam::Type(p) = param {
                        if let syn::Type::Path(k) = &t {
                            let first_segment = &k.path.segments.first().unwrap();
                            if first_segment.ident == p.ident {
                                p.bounds.push(parse_quote!(std::fmt::Debug));
                            }
                        }
                    }
                }
            }
            TypeEnum::AssociatedType(t) => {
                let predicate = parse_quote!(#t: std::fmt::Debug);

                where_clause.predicates.push_value(predicate);

                generics.where_clause = Some(where_clause_option.take().unwrap());
            }
            TypeEnum::PhantomType(t) => {
                let predicate = parse_quote!(PhantomData<#t>: std::fmt::Debug);
                where_clause.predicates.push_value(predicate);

                generics.where_clause = Some(where_clause_option.take().unwrap());
            }
        };
    }

    let (impl_generics, type_generics, where_clause_gen) = generics.split_for_impl();

    let where_clause = if let Some(s) = where_clause_option {
        proc_macro2::TokenStream::from_str(s.as_str()).unwrap()
    } else {
        match where_clause_gen {
            Some(s) => s.to_token_stream(),
            None => proc_macro2::TokenStream::from_str("").unwrap(),
        }
    };

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

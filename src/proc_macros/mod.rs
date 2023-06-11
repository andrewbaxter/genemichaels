use std::collections::HashSet;
use convert_case::{
    Case,
    Casing,
};
use proc_macro2::TokenStream;
use quote::{
    format_ident,
    quote,
    ToTokens,
};
use syn::{
    self,
    parse_macro_input,
    Type,
    Fields,
    Field,
    DeriveInput,
    Attribute,
};

fn get_docstr(attrs: &Vec<Attribute>) -> String {
    let mut out = String::new();
    for attr in attrs {
        if !attr.path.is_ident("doc") {
            continue;
        }
        match attr.parse_meta().unwrap() {
            syn::Meta::NameValue(syn::MetaNameValue { lit: syn::Lit::Str(v), .. }) => {
                out.push_str(&v.value());
            },
            _ => continue,
        }
    }
    return out.trim().to_string();
}

struct GenRec {
    vark: TokenStream,
    help_child_placeholders: Vec<TokenStream>,
    help_child_placeholders_detail: Vec<(TokenStream, String)>,
    help_recurse: Vec<syn::TypePath>,
}

fn gen_impl_type(ty: &Type, path: &str) -> GenRec {
    match ty {
        Type::Path(t) => {
            return GenRec {
                vark: quote!{
                    < #t >:: vark(state)
                },
                help_child_placeholders: vec![quote!{
                    &< #t >:: generate_help_placeholder()
                }],
                help_child_placeholders_detail: vec![],
                help_recurse: vec![t.clone()],
            };
        },
        Type::Tuple(t) => {
            return gen_impl_unnamed(
                path,
                quote!(),
                t.elems.iter().map(|e| (String::new(), e)).collect::<Vec<(String, &Type)>>().as_slice(),
            );
        },
        _ => panic!("Unsupported type {} in {}", ty.to_token_stream(), path),
    }
}

fn gen_impl_unnamed(path: &str, ident: TokenStream, d: &[(String, &Type)]) -> GenRec {
    let mut parse_positional = vec![];
    let mut copy_fields = vec![];
    let mut help_child_placeholders = vec![];
    let mut help_child_placeholder_detail = vec![];
    let mut help_recurse = vec![];
    for (i, ty) in d.iter().enumerate() {
        let eof_code = if i == 0 {
            quote!{
                break R::EOF;
            }
        } else {
            quote!{
                break state.r_err(format!("Missing argument {}", #i));
            }
        };
        let f_ident = format_ident!("v{}", i);
        let gen = gen_impl_type(ty.1, path);
        help_child_placeholders.extend(gen.help_child_placeholders);
        help_child_placeholder_detail.extend(gen.help_child_placeholders_detail);
        help_recurse.extend(gen.help_recurse);
        let vark = gen.vark;
        parse_positional.push(quote!{
            state.breadcrumbs.push(format!("{{{}}}", #i));
            let r = #vark;
            state.breadcrumbs.pop();
            let #f_ident = match r {
                R:: Help => {
                    break R::Help;
                },
                R:: Ok(v) => {
                    v
                },
                R:: Err => {
                    break R::Err;
                },
                R:: EOF => {
                    #eof_code
                }
            };
        });
        copy_fields.push(f_ident.to_token_stream());
    }
    return GenRec {
        vark: quote!{
            loop {
                #(#parse_positional) * break state.r_ok(#ident(#(#copy_fields), *));
            }
        },
        help_child_placeholders: help_child_placeholders,
        help_child_placeholders_detail: help_child_placeholder_detail,
        help_recurse: help_recurse,
    };
}

fn gen_impl_struct(ident: TokenStream, d: &Fields) -> GenRec {
    match d {
        Fields::Named(d) => {
            let mut help_placeholders = vec![];
            let mut help_placeholders_detail = vec![];
            let mut help_recurse = vec![];
            let mut vark_optional_fields = vec![];
            let mut vark_parse_optional_cases = vec![];
            let mut vark_parse_positional = vec![];
            let mut vark_copy_fields = vec![];
            let mut required_i = 0usize;
            for (i, f) in d.named.iter().enumerate() {
                let field_ident = f.ident.as_ref().expect("Named field missing name");
                let f_local_ident = format_ident!("v{}", i);
                let help_docstr = get_docstr(&f.attrs);

                // If an optional field, generate opt parsers and skip positional parsing
                if |f: &Field| -> bool {
                    // Confirm optional
                    let ty = match &f.ty {
                        Type::Path(t) => {
                            if t.qself.is_some() {
                                return false;
                            }
                            if t.path.leading_colon.is_some() {
                                return false;
                            }
                            if t.path.segments.len() != 1 {
                                return false;
                            }
                            let s = t.path.segments.first().unwrap();
                            if &s.ident.to_string() != "Option" {
                                return false;
                            }
                            match &s.arguments {
                                syn::PathArguments::None => return false,
                                syn::PathArguments::AngleBracketed(a) => {
                                    if a.args.len() != 1 {
                                        return false;
                                    }
                                    match a.args.first().unwrap() {
                                        syn::GenericArgument::Type(t) => t,
                                        _ => return false,
                                    }
                                },
                                syn::PathArguments::Parenthesized(_) => return false,
                            }
                        },
                        _ => return false,
                    };

                    // Do generation
                    let flag = format!("--{}", field_ident.to_string().to_case(Case::Kebab));
                    let help_docstr = get_docstr(&f.attrs);
                    vark_optional_fields.push(quote!{
                        #field_ident: Option < #ty >,
                    });
                    vark_copy_fields.push(quote!{
                        #field_ident: optional.#field_ident
                    });
                    let gen = gen_impl_type(ty, &field_ident.to_string());
                    help_recurse.extend(gen.help_recurse);
                    let help_child_placeholders = gen.help_child_placeholders;
                    help_placeholders_detail.push((quote!{
                        &format!(
                            "{}{}",
                            aargvark:: style_lit(#flag),
                            aargvark:: join_strs("", &[#(&format!(" {}", #help_child_placeholders)), *])
                        )
                    }, help_docstr));
                    let vark = gen.vark;
                    vark_parse_optional_cases.push(quote!{
                        #flag => {
                            if optional.#field_ident.is_some() {
                                return state.r_err(format!("The argument {} was already specified", #flag));
                            }
                            state.consume();
                            let #f_local_ident = match #vark {
                                R:: Help => {
                                    return R::Help;
                                },
                                R:: Ok(v) => {
                                    v
                                },
                                R:: Err => {
                                    return R::Err;
                                },
                                R:: EOF => {
                                    return state.r_err(format!("Missing argument for {}", #flag));
                                }
                            };
                            optional.#field_ident = Some(#f_local_ident);
                            return R::Ok(true);
                        }
                    });
                    return true;
                }(&f) {
                    continue;
                }

                // Positional/required parsing
                let help_placeholder = field_ident.to_string().to_case(Case::UpperKebab);
                let eof_code = if required_i == 0 {
                    quote!{
                        break R::EOF;
                    }
                } else {
                    quote!{
                        break state.r_err(format!("Missing argument {}", #help_placeholder));
                    }
                };
                let gen = gen_impl_type(&f.ty, &ident.to_string());
                help_recurse.extend(gen.help_recurse);
                help_placeholders.push(quote!(& aargvark:: style_name(#help_placeholder)));
                let help_child_placeholders = gen.help_child_placeholders;
                help_placeholders_detail.push((quote!{
                    &format!(
                        "{}:{}",
                        aargvark:: style_name(#help_placeholder),
                        aargvark:: join_strs("", &[#(&format!(" {}", #help_child_placeholders)), *])
                    )
                }, help_docstr));
                let vark = gen.vark;
                vark_parse_positional.push(quote!{
                    let #f_local_ident = loop {
                        if match state.peek() {
                            PeekR:: None => false,
                            PeekR:: Help => break R:: Help,
                            PeekR:: Ok(s) => match parse_optional(&mut optional, state, s.to_string()) {
                                R:: Help => {
                                    break R::Help
                                },
                                R:: Ok(v) => {
                                    v
                                },
                                R:: Err => {
                                    break R::Err;
                                },
                                R:: EOF => {
                                    unreachable!();
                                }
                            },
                        }
                        {
                            continue;
                        }
                        break #vark;
                    };
                    let #f_local_ident = match #f_local_ident {
                        R:: Help => {
                            break R::Help;
                        },
                        R:: Ok(v) => {
                            v
                        },
                        R:: Err => {
                            break R::Err;
                        },
                        R:: EOF => {
                            #eof_code
                        }
                    };
                });
                vark_copy_fields.push(quote!{
                    #field_ident: #f_local_ident
                });
                required_i += 1;
            }
            if vark_optional_fields.len() > 0 {
                help_placeholders.push(quote!("[OPT...]"));
            }

            // Assemble code
            let vark = quote!{
                {
                    state.simple_enum_root = false;
                    loop {
                        #[derive(Default)] struct Optional {
                            #(#vark_optional_fields) *
                        }
                        let mut optional = Optional::default();
                        fn parse_optional(
                            optional: &mut Optional,
                            state: &mut aargvark::VarkState,
                            s: String
                        ) -> R < bool > {
                            match s.as_str() {
                                #(#vark_parse_optional_cases) * _ => return R:: Ok(false),
                            };
                        }
                        #(#vark_parse_positional) * 
                        // Parse any remaining optional args
                        let opt_search_res = loop {
                            match state.peek() {
                                PeekR::None => {
                                    break state.r_ok(());
                                },
                                PeekR::Help => break R::Help,
                                PeekR::Ok(s) => match parse_optional(&mut optional, state, s.to_string()) {
                                    R::Help => {
                                        break R::Help;
                                    },
                                    R::Ok(v) => {
                                        if !v {
                                            break state.r_ok(());
                                        }
                                    },
                                    R::Err => {
                                        break R::Err;
                                    },
                                    R::EOF => {
                                        unreachable!();
                                    },
                                },
                            };
                        };
                        match opt_search_res {
                            R::Help => {
                                break R::Help;
                            },
                            R::Ok(()) => { },
                            R::Err => {
                                break R::Err;
                            },
                            R::EOF => {
                                unreachable!();
                            },
                        };
                        // Build obj + return
                        break state.r_ok(#ident {
                            #(#vark_copy_fields),
                            *
                        });
                    }
                }
            };
            return GenRec {
                vark: vark,
                help_child_placeholders: help_placeholders,
                help_child_placeholders_detail: help_placeholders_detail,
                help_recurse: help_recurse,
            };
        },
        Fields::Unnamed(d) => {
            return gen_impl_unnamed(
                &ident.to_string(),
                ident.to_token_stream(),
                d
                    .unnamed
                    .iter()
                    .map(|f| (get_docstr(&f.attrs), &f.ty))
                    .collect::<Vec<(String, &Type)>>()
                    .as_slice(),
            );
        },
        Fields::Unit => {
            return GenRec {
                vark: quote!{
                    state.r_ok(#ident)
                },
                help_child_placeholders: vec![],
                help_child_placeholders_detail: vec![],
                help_recurse: vec![],
            };
        },
    };
}

fn gen_impl(ast: syn::DeriveInput) -> TokenStream {
    let ident = &ast.ident;
    let help_placeholder = ident.to_string().to_case(Case::UpperKebab);
    let help_docstr = get_docstr(&ast.attrs);
    let help_child_placeholder_joiner;
    let gen = match &ast.data {
        syn::Data::Struct(d) => {
            help_child_placeholder_joiner = quote!(" ");
            gen_impl_struct(ast.ident.to_token_stream(), &d.fields)
        },
        syn::Data::Enum(d) => {
            let mut all_tags = vec![];
            let mut vark_cases = vec![];
            let mut help_recurse = vec![];
            let mut help_placeholders = vec![];
            let mut help_placeholders_detail = vec![];
            let mut help_short_placeholders = vec![];
            let mut help_short_placeholders_detail = vec![];
            help_child_placeholder_joiner = quote!(" | ");
            for v in &d.variants {
                let variant_ident = &v.ident;
                let name_str = variant_ident.to_string().to_case(Case::Kebab);
                let gen = gen_impl_struct(quote!(#ident:: #variant_ident), &v.fields);
                all_tags.push(name_str.clone());
                let help_docstr = get_docstr(&v.attrs);
                help_short_placeholders.push(quote!(& aargvark:: style_lit(#name_str)));
                help_short_placeholders_detail.push(
                    (quote!(& aargvark:: style_lit(#name_str)), help_docstr.clone()),
                );
                help_placeholders.push(quote!(& aargvark:: style_lit(#name_str)));
                let help_child_placeholders = gen.help_child_placeholders;
                help_placeholders_detail.push(
                    (
                        quote!(
                            &format!(
                                "{}{}",
                                aargvark:: style_lit(#name_str),
                                aargvark:: join_strs("", &[#(&format!(" {}", #help_child_placeholders)), *])
                            )
                        ),
                        help_docstr.clone(),
                    ),
                );
                help_recurse.extend(gen.help_recurse.clone());
                let child_help_recurse = gen.help_recurse;
                let vark = gen.vark;
                let help_child_placeholders_detail: Vec<TokenStream> =
                    gen
                        .help_child_placeholders_detail
                        .iter()
                        .map(|(placeholder, docstr)| quote!((#placeholder, #docstr)))
                        .collect();
                vark_cases.push(quote!{
                    #name_str => {
                        state.consume();
                        state.breadcrumbs.push(#name_str.to_string());
                        let v = #vark;
                        if simple_enum_root && matches !(v, R::Help) {
                            let (mut text, mut seen_sections) =
                                aargvark::generate_help_section_usage_prefix(state);
                            text.push_str(
                                & aargvark:: generate_help_section_suffix(
                                    #help_docstr,
                                    vec![#(#help_child_placeholders), *],
                                    vec![#(#help_child_placeholders_detail), *],
                                    " "
                                )
                            );
                            #(
                                < #child_help_recurse >:: generate_help_section(&mut text, &mut seen_sections);
                            ) * eprintln !("{}\n", text.trim());
                            std::process::exit(0);
                        }
                        state.breadcrumbs.pop();
                        v
                    }
                });
            }
            let help_short_placeholders_detail: Vec<TokenStream> =
                help_short_placeholders_detail
                    .iter()
                    .map(|(placeholder, docstr)| quote!((#placeholder, #docstr)))
                    .collect();
            GenRec {
                vark: quote!{
                    {
                        let simple_enum_root = state.simple_enum_root;
                        let tag = match state.peek() {
                            PeekR:: None => return R:: EOF,
                            PeekR:: Help => {
                                if simple_enum_root {
                                    let (mut text, mut seen_sections) =
                                        aargvark::generate_help_section_usage_prefix(state);
                                    text.push_str(
                                        & aargvark:: generate_help_section_suffix(
                                            #help_docstr,
                                            vec![#(#help_short_placeholders), *],
                                            vec![#(#help_short_placeholders_detail), *],
                                            #help_child_placeholder_joiner
                                        )
                                    );
                                    eprintln!("{}\n", text.trim());
                                    std::process::exit(0);
                                }
                                else {
                                    return R::Help;
                                }
                            },
                            PeekR:: Ok(s) => s,
                        };
                        match tag {
                            #(#vark_cases) * _ => {
                                state.r_err(
                                    format!("Unrecognized variant {}. Choices are {:?}", tag, vec![#(#all_tags), *]),
                                )
                            }
                        }
                    }
                },
                help_child_placeholders: help_placeholders,
                help_child_placeholders_detail: help_placeholders_detail,
                help_recurse: help_recurse,
            }
        },
        syn::Data::Union(_) => panic!("union not supported"),
    };
    let vark = gen.vark;
    let help_child_placeholders = gen.help_child_placeholders;
    let help_child_placeholders_detail: Vec<TokenStream> =
        gen
            .help_child_placeholders_detail
            .iter()
            .map(|(placeholder, docstr)| quote!((#placeholder, #docstr)))
            .collect();
    let mut seen_help_recurse = HashSet::new();
    let mut help_recurse = vec![];
    for r in gen.help_recurse {
        if !seen_help_recurse.insert(r.to_token_stream().to_string()) {
            continue;
        }
        help_recurse.push(r);
    }
    return quote!{
        impl aargvark:: AargvarkTrait for #ident {
            fn vark(state: &mut aargvark::VarkState) -> aargvark:: R < #ident > {
                use aargvark::R;
                use aargvark::PeekR;
                #vark
            }
            fn generate_help_placeholder() -> String {
                aargvark:: style_name(#help_placeholder)
            }
            fn generate_help_section_suffix(text: &mut String, seen_sections: &mut std::collections::HashSet<String>) {
                text.push_str(
                    & aargvark:: generate_help_section_suffix(
                        #help_docstr,
                        vec![#(#help_child_placeholders), *],
                        vec![#(#help_child_placeholders_detail), *],
                        #help_child_placeholder_joiner
                    )
                );
                #(< #help_recurse >:: generate_help_section(text, seen_sections);) *
            }
            fn generate_help_section(text: &mut String, seen_sections: &mut std::collections::HashSet<String>) {
                if ! seen_sections.insert(#help_placeholder.to_string()) {
                    return;
                }
                text.push_str(& aargvark:: style_name(#help_placeholder));
                text.push_str(":");
                < #ident >:: generate_help_section_suffix(text, seen_sections);
            }
        }
    };
}

#[proc_macro_derive(Aargvark)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    return gen_impl(parse_macro_input!(input as DeriveInput)).into();
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use proc_macro2::TokenStream;
    use quote::quote;
    use crate::gen_impl;

    #[test]
    fn newtype_string() {
        assert_eq!(
            gen_impl(
                syn::parse2(TokenStream::from_str("enum Yol {
        ToqQuol,
    }").unwrap()).unwrap(),
            ).to_string(),
            quote!().to_string()
        );
    }
}

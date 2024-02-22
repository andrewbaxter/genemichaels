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
    Attribute,
    DeriveInput,
    Fields,
    Lit,
    Meta,
    Type,
};

/// Break boundary - remove the footgunishness of using loop for this directly
macro_rules! bb{
    ($l: lifetime _; $($t: tt) *) => {
        $l: loop {
            #[allow(unreachable_code)] break {
                $($t) *
            };
        }
    };
    ($($t: tt) *) => {
        loop {
            #[allow(unreachable_code)] break {
                $($t) *
            };
        }
    };
}

#[derive(Default, Clone)]
struct VarkAttr {
    help_break: bool,
    literal: Option<String>,
    id: Option<String>,
}

fn get_vark(attrs: &Vec<Attribute>) -> VarkAttr {
    let mut help_break = false;
    let mut literal = None;
    let mut id = None;
    for a in attrs {
        let Ok(m) = a.parse_meta() else {
            continue;
        };
        let Meta:: List(m) = m else {
            continue;
        };
        if &m.path.to_token_stream().to_string() != "vark" {
            continue;
        }
        for m in m.nested {
            let syn:: NestedMeta:: Meta(m) = m else {
                continue;
            };
            match &m {
                Meta::Path(k) => {
                    match k.to_token_stream().to_string().as_str() {
                        "break" => {
                            help_break = true;
                        },
                        i => {
                            panic!("Unexpected argument in `vark` attr: {:?}", i);
                        },
                    }
                },
                Meta::List(_) => {
                    panic!("Unexpected tokens in `vark` attr arguments: {:?}", m.to_token_stream());
                },
                Meta::NameValue(kv) => {
                    match kv.path.to_token_stream().to_string().as_str() {
                        "literal" => {
                            literal = Some(match &kv.lit {
                                Lit::Str(s) => s.value(),
                                l => panic!("`vark` `literal` argument must be a string, got {}", l.to_token_stream()),
                            });
                        },
                        "id" => {
                            id = Some(match &kv.lit {
                                Lit::Str(s) => s.value(),
                                l => panic!("`vark` `id` argument must be a string, got {}", l.to_token_stream()),
                            });
                        },
                        i => {
                            panic!("Unexpected argument in `vark` attr: {:?}", i);
                        },
                    }
                },
            }
        }
    }
    return VarkAttr {
        help_break: help_break,
        literal: literal,
        id: id,
    };
}

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
    help_pattern: TokenStream,
}

fn gen_impl_type(ty: &Type, path: &str) -> GenRec {
    match ty {
        Type::Path(t) => {
            return GenRec {
                vark: quote!{
                    < #t >:: vark(state)
                },
                help_pattern: quote!{
                    < #t as aargvark:: AargvarkTrait >:: build_help_pattern(state)
                },
            };
        },
        Type::Tuple(t) => {
            return gen_impl_unnamed(
                path,
                ty.to_token_stream(),
                quote!(),
                "TUPLE",
                "",
                0,
                t.elems.iter().map(|e| (VarkAttr::default(), String::new(), e)).collect::<Vec<_>>().as_slice(),
            );
        },
        _ => panic!("Unsupported type {} in {}", ty.to_token_stream(), path),
    }
}

fn gen_impl_unnamed(
    path: &str,
    parent_ident: TokenStream,
    ident: TokenStream,
    help_placeholder: &str,
    help_docstr: &str,
    subtype_index: usize,
    d: &[(VarkAttr, String, &Type)],
) -> GenRec {
    let mut parse_positional = vec![];
    let mut copy_fields = vec![];
    let mut help_fields = vec![];
    let mut help_field_patterns = vec![];
    let help_unit_transparent = d.len() == 1 && d[0].1.is_empty();
    for (i, (field_vark_attr, field_help_docstr, field_ty)) in d.iter().enumerate() {
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
        let gen = gen_impl_type(field_ty, path);
        let vark = gen.vark;
        let placeholder = field_vark_attr.id.clone().unwrap_or_else(|| {
            let mut placeholder = vec![];
            let mut placeholder_i = i;
            loop {
                placeholder.push((('A' as u8) + (placeholder_i % 27) as u8) as char);
                if placeholder_i < 27 {
                    break;
                }
                placeholder_i = placeholder_i / 27;
            }
            placeholder.reverse();
            String::from_iter(placeholder)
        });
        parse_positional.push(quote!{
            //. .
            let r = #vark;
            //. .
            let #f_ident = match r {
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
        let field_help_pattern = gen.help_pattern;
        help_fields.push(quote!{
            struct_.fields.push(aargvark:: HelpField {
                id: #placeholder.to_string(),
                pattern: #field_help_pattern,
                description: #field_help_docstr.to_string(),
            });
        });
        help_field_patterns.push(field_help_pattern);
    }
    return GenRec {
        vark: quote!{
            loop {
                #(#parse_positional) * break state.r_ok(#ident(#(#copy_fields), *));
            }
        },
        help_pattern: if d.is_empty() {
            quote!{
                aargvark::HelpPattern(vec![])
            }
        } else if help_unit_transparent {
            help_field_patterns.pop().unwrap()
        } else {
            quote!{
                {
                    let(
                        key,
                        struct_
                    ) = state.add_struct(
                        std:: any:: TypeId:: of::< #parent_ident >(),
                        #subtype_index,
                        #help_placeholder,
                        #help_docstr
                    );
                    let mut struct_ = struct_.borrow_mut();
                    #(#help_fields) * 
                    //. .
                    aargvark:: HelpPattern(vec![aargvark::HelpPatternElement::Reference(key)])
                }
            }
        },
    };
}

fn gen_impl_struct(
    parent_ident: TokenStream,
    ident: TokenStream,
    decl_generics: &TokenStream,
    forward_generics: &TokenStream,
    help_placeholder: &str,
    vark_attr: &VarkAttr,
    help_docstr: &str,
    subtype_index: usize,
    d: &Fields,
) -> GenRec {
    match d {
        Fields::Named(d) => {
            let mut help_fields = vec![];
            let mut partial_help_fields = vec![];
            let mut vark_optional_fields = vec![];
            let mut vark_optional_fields_default = vec![];
            let mut vark_parse_optional_cases = vec![];
            let mut vark_parse_positional = vec![];
            let mut vark_copy_fields = vec![];
            let mut required_i = 0usize;
            'next_field: for (i, f) in d.named.iter().enumerate() {
                let field_vark_attr = get_vark(&f.attrs);
                let field_help_docstr = get_docstr(&f.attrs);
                let field_ident = f.ident.as_ref().expect("Named field missing name");
                let f_local_ident = format_ident!("v{}", i);

                // If an optional field, generate opt parsers and skip positional parsing
                bb!{
                    'not_optional _;
                    let ty;
                    {
                        let Type:: Path(t) =& f.ty else {
                            break 'not_optional;
                        };
                        if t.qself.is_some() {
                            break 'not_optional;
                        }
                        if t.path.leading_colon.is_some() {
                            break 'not_optional;
                        }
                        if t.path.segments.len() != 1 {
                            break 'not_optional;
                        }
                        let s = t.path.segments.first().unwrap();
                        if &s.ident.to_string() != "Option" {
                            break 'not_optional;
                        }
                        let syn:: PathArguments:: AngleBracketed(a) =& s.arguments else {
                            break 'not_optional;
                        };
                        if a.args.len() != 1 {
                            break 'not_optional;
                        }
                        let syn:: GenericArgument:: Type(t) =& a.args[0] else {
                            break 'not_optional;
                        };
                        ty = t;
                    }
                    let flag =
                        format!(
                            "--{}",
                            field_vark_attr
                                .literal
                                .clone()
                                .unwrap_or_else(|| field_ident.to_string().to_case(Case::Kebab))
                        );
                    vark_optional_fields.push(quote!{
                        #field_ident: Option < #ty >,
                    });
                    vark_optional_fields_default.push(quote!{
                        #field_ident: None,
                    });
                    vark_copy_fields.push(quote!{
                        #field_ident: optional.#field_ident
                    });
                    let gen = gen_impl_type(ty, &field_ident.to_string());
                    let vark = gen.vark;
                    vark_parse_optional_cases.push(quote!{
                        #flag => {
                            if optional.#field_ident.is_some() {
                                return state.r_err(format!("The argument {} was already specified", #flag));
                            }
                            state.consume();
                            let #f_local_ident = match #vark {
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
                    let field_help_pattern;
                    if vark_attr.help_break || field_vark_attr.help_break {
                        field_help_pattern = quote!(aargvark::HelpPattern(vec![]));
                    }
                    else {
                        field_help_pattern = gen.help_pattern;
                    }
                    let help_field = quote!{
                        aargvark:: HelpOptionalField {
                            literal: #flag.to_string(),
                            pattern: #field_help_pattern,
                            description: #field_help_docstr.to_string(),
                        }
                    };
                    help_fields.push(quote!{
                        struct_.optional_fields.push(#help_field);
                    });
                    partial_help_fields.push(quote!{
                        if optional.#field_ident.is_none() {
                            optional_fields.push(#help_field);
                        }
                    });
                    continue 'next_field;
                };

                // Positional/required parsing
                let field_help_placeholder = field_vark_attr.id.unwrap_or_else(|| field_ident.to_string().to_case(Case::UpperKebab));
                let eof_code = if required_i == 0 {
                    quote!{
                        break R::EOF;
                    }
                } else {
                    quote!{
                        break state.r_err(format!("Missing argument {}", #field_help_placeholder));
                    }
                };
                let gen = gen_impl_type(&f.ty, &ident.to_string());
                let vark = gen.vark;
                let field_help_pattern = gen.help_pattern;
                vark_parse_positional.push(quote!{
                    let #f_local_ident = loop {
                        if match state.peek() {
                            PeekR:: None => false,
                            PeekR:: Help => {
                                aargvark:: show_help_and_exit(state, | state | {
                                    return aargvark:: HelpPartialProduction {
                                        description: #help_docstr.to_string(),
                                        content: build_partial_help(state, #required_i, &optional),
                                    };
                                });
                            },
                            PeekR:: Ok(s) => match parse_optional(&mut optional, state, s.to_string()) {
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
                let help_field = quote!{
                    aargvark:: HelpField {
                        id: #field_help_placeholder.to_string(),
                        pattern: #field_help_pattern,
                        description: #field_help_docstr.to_string(),
                    }
                };
                help_fields.push(quote!{
                    struct_.fields.push(#help_field);
                });
                partial_help_fields.push(quote!{
                    if required_i <= #required_i {
                        fields.push(#help_field);
                    }
                });
                required_i += 1;
            }

            // Assemble code
            let vark = quote!{
                {
                    loop {
                        struct Optional #decl_generics {
                            #(#vark_optional_fields) *
                        }
                        let mut optional = Optional {
                            #(#vark_optional_fields_default) *
                        };
                        fn parse_optional #decl_generics(
                            optional:& mut Optional #forward_generics,
                            state: &mut aargvark::VarkState,
                            s: String
                        ) -> R < bool > {
                            match s.as_str() {
                                #(#vark_parse_optional_cases) * 
                                //. .
                                _ => return R:: Ok(false),
                            };
                        }
                        fn build_partial_help #decl_generics(
                            state: &mut aargvark::HelpState,
                            required_i: usize,
                            optional:& Optional #forward_generics,
                        ) -> aargvark:: HelpPartialContent {
                            let mut fields = vec![];
                            let mut optional_fields = vec![];
                            #(#partial_help_fields) * 
                            //. .
                            return aargvark:: HelpPartialContent:: struct_(fields, optional_fields);
                        }
                        #(#vark_parse_positional) * 
                        // Parse any remaining optional args
                        let opt_search_res = loop {
                            match state.peek() {
                                PeekR:: None => {
                                    break state.r_ok(());
                                },
                                PeekR:: Help => {
                                    aargvark:: show_help_and_exit(state, | state | {
                                        return aargvark:: HelpPartialProduction {
                                            description: #help_docstr.to_string(),
                                            content: build_partial_help(state, #required_i, &optional),
                                        };
                                    });
                                },
                                PeekR:: Ok(s) => match parse_optional(&mut optional, state, s.to_string()) {
                                    R:: Ok(v) => {
                                        if !v {
                                            break state.r_ok(());
                                        }
                                    },
                                    R:: Err => {
                                        break R::Err;
                                    },
                                    R:: EOF => {
                                        unreachable!();
                                    },
                                },
                            };
                        };
                        match opt_search_res {
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
                help_pattern: quote!{
                    {
                        let(
                            key,
                            struct_
                        ) = state.add_struct(
                            std::any::TypeId::of::<Self>(),
                            #subtype_index,
                            #help_placeholder,
                            #help_docstr
                        );
                        let mut struct_ = struct_.borrow_mut();
                        #(#help_fields) * 
                        //. .
                        aargvark:: HelpPattern(vec![aargvark::HelpPatternElement::Reference(key)])
                    }
                },
            };
        },
        Fields::Unnamed(d) => {
            return gen_impl_unnamed(
                &ident.to_string(),
                parent_ident,
                ident.to_token_stream(),
                help_placeholder,
                help_docstr,
                subtype_index,
                d
                    .unnamed
                    .iter()
                    .map(|f| (get_vark(&f.attrs), get_docstr(&f.attrs), &f.ty))
                    .collect::<Vec<_>>()
                    .as_slice(),
            );
        },
        Fields::Unit => {
            return GenRec {
                vark: quote!{
                    state.r_ok(#ident)
                },
                help_pattern: quote!{
                    aargvark::HelpPattern(vec![])
                },
            };
        },
    };
}

fn gen_impl(ast: syn::DeriveInput) -> TokenStream {
    let ident = &ast.ident;
    let decl_generics = ast.generics.to_token_stream();
    let forward_generics;
    {
        let mut parts = vec![];
        for p in ast.generics.params {
            match p {
                syn::GenericParam::Type(p) => parts.push(p.ident.to_token_stream()),
                syn::GenericParam::Lifetime(p) => parts.push(p.lifetime.to_token_stream()),
                syn::GenericParam::Const(p) => parts.push(p.ident.to_token_stream()),
            }
        }
        if parts.is_empty() {
            forward_generics = quote!();
        } else {
            forward_generics = quote!(< #(#parts), *>);
        }
    }
    let vark_attr = get_vark(&ast.attrs);
    let help_docstr = get_docstr(&ast.attrs);
    let help_placeholder = vark_attr.id.clone().unwrap_or_else(|| ident.to_string().to_case(Case::UpperKebab));
    let vark;
    let help_build;
    match &ast.data {
        syn::Data::Struct(d) => {
            let gen =
                gen_impl_struct(
                    ast.ident.to_token_stream(),
                    ast.ident.to_token_stream(),
                    &decl_generics,
                    &forward_generics,
                    &help_placeholder,
                    &vark_attr,
                    &help_docstr,
                    0,
                    &d.fields,
                );
            vark = gen.vark;
            help_build = gen.help_pattern;
        },
        syn::Data::Enum(d) => {
            let mut all_tags = vec![];
            let mut vark_cases = vec![];
            let mut help_variants = vec![];
            for (subtype_index, v) in d.variants.iter().enumerate() {
                let variant_vark_attr = get_vark(&v.attrs);
                let variant_help_docstr = get_docstr(&v.attrs);
                let variant_ident = &v.ident;
                let name_str =
                    variant_vark_attr
                        .literal
                        .clone()
                        .unwrap_or_else(|| variant_ident.to_string().to_case(Case::Kebab));
                let gen =
                    gen_impl_struct(
                        ident.to_token_stream(),
                        quote!(#ident:: #variant_ident),
                        &decl_generics,
                        &forward_generics,
                        &name_str,
                        &variant_vark_attr,
                        "",
                        subtype_index + 1,
                        &v.fields,
                    );
                all_tags.push(name_str.clone());
                let vark = gen.vark;
                let partial_help_variant_pattern = gen.help_pattern;
                vark_cases.push(quote!{
                    #name_str => {
                        state.consume();
                        #vark
                    }
                });
                let help_variant_pattern;
                if vark_attr.help_break || variant_vark_attr.help_break {
                    help_variant_pattern = quote!(aargvark::HelpPattern(vec![]));
                } else {
                    help_variant_pattern = partial_help_variant_pattern;
                }
                help_variants.push(quote!{
                    variants.push(aargvark:: HelpVariant {
                        literal: #name_str.to_string(),
                        pattern: #help_variant_pattern,
                        description: #variant_help_docstr.to_string(),
                    });
                });
            }
            vark = quote!{
                {
                    let tag = match state.peek() {
                        PeekR:: None => return R:: EOF,
                        PeekR:: Help => {
                            aargvark:: show_help_and_exit(state, | state | {
                                let mut variants = vec![];
                                #(#help_variants) * 
                                //. .
                                return aargvark:: HelpPartialProduction {
                                    description: #help_docstr.to_string(),
                                    content: aargvark::HelpPartialContent::enum_(variants),
                                };
                            });
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
            };
            help_build = quote!{
                let(
                    key,
                    variants
                ) = state.add_enum(std::any::TypeId::of::<Self>(), 0, #help_placeholder, #help_docstr);
                let mut variants = variants.borrow_mut();
                #(#help_variants) * 
                //. .
                return aargvark:: HelpPattern(vec![aargvark::HelpPatternElement::Reference(key)]);
            };
        },
        syn::Data::Union(_) => panic!("Union not supported"),
    };
    return quote!{
        impl #decl_generics aargvark:: AargvarkTrait for #ident #forward_generics {
            fn vark(state: &mut aargvark::VarkState) -> aargvark:: R < #ident #forward_generics > {
                use aargvark::R;
                use aargvark::PeekR;
                #vark
            }
            fn build_help_pattern(state: &mut aargvark::HelpState) -> aargvark:: HelpPattern {
                #help_build
            }
        }
    };
}

#[proc_macro_derive(Aargvark, attributes(vark))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    return gen_impl(parse_macro_input!(input as DeriveInput)).into();
}

#[cfg(test)]
mod tests {
    use std::str::FromStr;
    use genemichaels::FormatConfig;
    use proc_macro2::TokenStream;
    use quote::quote;
    use crate::gen_impl;

    #[test]
    fn dump() {
        let got = gen_impl(syn::parse2(quote!{
            #[derive(Aargvark, PartialEq, Debug)]
            struct Naya {
                b: Option<()>,
            }
        }).unwrap());
        let cfg = FormatConfig::default();
        let mut s =
            [&got].into_iter().map(|s| genemichaels::format_str(&s.to_string(), &cfg)).collect::<Vec<_>>();
        let got = s.remove(0).expect(&format!("Failed to format got code:\n{}", got.to_string())).rendered;
        panic!("{}", got);
    }

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

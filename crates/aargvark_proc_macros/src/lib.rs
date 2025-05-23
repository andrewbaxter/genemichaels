use {
    convert_case::{
        Case,
        Casing,
    },
    darling::{
        FromDeriveInput,
        FromField,
        FromVariant,
    },
    flowcontrol::shed,
    proc_macro2::TokenStream,
    quote::{
        format_ident,
        quote,
        ToTokens,
    },
    std::collections::HashSet,
    syn::{
        self,
        parse_macro_input,
        spanned::Spanned,
        Attribute,
        DeriveInput,
        Expr,
        Fields,
        Lit,
        Type,
    },
};

#[derive(Default, Clone, FromDeriveInput)]
#[darling(attributes(vark))]
#[darling(default)]
struct TypeAttr {
    break_help: bool,
    placeholder: Option<String>,
}

#[derive(Default, Clone, FromField)]
#[darling(attributes(vark))]
#[darling(default)]
struct FieldAttr {
    break_help: bool,
    #[darling(multiple)]
    flag: Vec<String>,
    placeholder: Option<String>,
}

#[derive(Default, Clone, FromVariant)]
#[darling(attributes(vark))]
#[darling(default)]
struct VariantAttr {
    break_help: bool,
    name: Option<String>,
    placeholder: Option<String>,
}

fn get_docstr(attrs: &Vec<Attribute>) -> String {
    let mut out = String::new();
    for attr in attrs {
        match &attr.meta {
            syn::Meta::NameValue(meta) => {
                if !meta.path.is_ident("doc") {
                    continue;
                }
                let Expr::Lit(syn::ExprLit { lit: Lit::Str(s), .. }) = &meta.value else {
                    continue;
                };
                out.push_str(&s.value());
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
                    < #t as a:: AargvarkTrait >:: build_help_pattern(state)
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
                t.elems.iter().map(|e| (FieldAttr::default(), String::new(), e)).collect::<Vec<_>>().as_slice(),
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
    fields: &[(FieldAttr, String, &Type)],
) -> GenRec {
    let mut parse_positional = vec![];
    let mut copy_fields = vec![];
    let mut help_fields = vec![];
    let mut help_field_patterns = vec![];
    let help_unit_transparent = fields.len() == 1 && fields[0].1.is_empty();
    for (i, (field_vark_attr, field_help_docstr, field_ty)) in fields.iter().enumerate() {
        let f_ident = format_ident!("v{}", i);
        let gen = gen_impl_type(field_ty, path);
        let vark = gen.vark;
        let placeholder = field_vark_attr.placeholder.clone().unwrap_or_else(|| {
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
                a:: R:: Ok(v) => v,
                a:: R:: Help(b) => break a:: R:: Help(b),
                a:: R:: Err => break a:: R:: Err,
            };
        });
        copy_fields.push(f_ident.to_token_stream());
        let field_help_pattern = gen.help_pattern;
        help_fields.push(quote!{
            struct_.fields.push(a:: HelpField {
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
                #(#parse_positional) * 
                //. .
                break state.r_ok(#ident(#(#copy_fields), *), None);
            }
        },
        help_pattern: if fields.is_empty() {
            quote!{
                a::HelpPattern(vec![])
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
                    let mut struct_ = struct_.as_ref().borrow_mut();
                    #(#help_fields) * 
                    //. .
                    a:: HelpPattern(vec![a::HelpPatternElement::Reference(key)])
                }
            }
        },
    };
}

fn get_optional_type(t: &Type) -> Option<&Type> {
    let Type::Path(t) = &t else {
        return None;
    };
    if t.qself.is_some() {
        return None;
    }
    if t.path.leading_colon.is_some() {
        return None;
    }
    if t.path.segments.len() != 1 {
        return None;
    }
    let s = t.path.segments.first().unwrap();
    if &s.ident.to_string() != "Option" {
        return None;
    }
    let syn::PathArguments::AngleBracketed(a) = &s.arguments else {
        return None;
    };
    if a.args.len() != 1 {
        return None;
    }
    let syn::GenericArgument::Type(t) = &a.args[0] else {
        return None;
    };
    return Some(t);
}

fn gen_impl_struct(
    parent_ident: TokenStream,
    ident: TokenStream,
    decl_generics: &TokenStream,
    forward_generics: &TokenStream,
    help_placeholder: &str,
    type_break_help: bool,
    help_docstr: &str,
    subtype_index: usize,
    d: &Fields,
) -> Result<GenRec, syn::Error> {
    match d {
        Fields::Named(d) => {
            let mut help_fields = vec![];
            let mut partial_help_fields = vec![];
            let mut vark_flag_fields = vec![];
            let mut vark_flag_fields_default = vec![];
            let mut vark_parse_flag_cases = vec![];
            let mut vark_parse_positional = vec![];
            let mut vark_copy_flag_fields = vec![];
            let mut seen_flags = HashSet::new();
            let mut required_i = 0usize;
            let mut init_need_flags = vec![];
            'next_field: for (i, f) in d.named.iter().enumerate() {
                let field_vark_attr = FieldAttr::from_field(f)?;
                let field_help_docstr = get_docstr(&f.attrs);
                let field_ident = f.ident.as_ref().expect("Named field missing name");
                let f_local_ident = format_ident!("v{}", i);

                // If a flag (non-positional) field, generate parsers and skip positional parsing
                shed!{
                    'no_flags _;
                    let mut flags = field_vark_attr.flag.clone();
                    if flags.is_empty() {
                        flags.push(format!("--{}", field_ident.to_string().to_case(Case::Kebab)));
                    }
                    for flag in &flags {
                        if !seen_flags.insert(flag.clone()) {
                            return Err(
                                syn::Error::new(f.span(), format!("Duplicate flag [{}] in [{}]", flag, ident)),
                            );
                        }
                    }
                    let ty;
                    let copy;
                    let optional;
                    if let Some(ty1) = get_optional_type(&f.ty) {
                        ty = ty1;
                        copy = quote!(flag_fields.#field_ident);
                        optional = true;
                    }
                    else if ! field_vark_attr.flag.is_empty() {
                        ty = &f.ty;
                        copy = quote!(if let Some(f) = flag_fields.#field_ident {
                            f
                        } else {
                            return state.r_err(
                                format!("Missing required flags: {:?}", need_flags),
                                Some(build_completer(need_flags)),
                            );
                        });
                        optional = false;
                    }
                    else {
                        break 'no_flags;
                    }
                    vark_flag_fields.push(quote!{
                        #field_ident: Option < #ty >,
                    });
                    vark_flag_fields_default.push(quote!{
                        #field_ident: None,
                    });
                    vark_copy_flag_fields.push(quote!{
                        #field_ident: #copy
                    });
                    let gen = gen_impl_type(ty, &field_ident.to_string());
                    let vark = gen.vark;
                    for flag in &flags {
                        if !optional {
                            init_need_flags.push(quote!{
                                #flag,
                            });
                        }
                        vark_parse_flag_cases.push(quote!{
                            #flag => {
                                need_flags.remove(#flag);
                                if flag_fields.#field_ident.is_some() {
                                    return state.r_err(
                                        format!("The argument {} was already specified", #flag),
                                        Some(a::empty_completer()),
                                    );
                                }
                                state.consume();
                                let #f_local_ident = match #vark {
                                    a:: R:: Ok(v) => v,
                                    a:: R:: Help(b) => return a:: R:: Help(b),
                                    a:: R:: Err => return a:: R:: Err,
                                };
                                flag_fields.#field_ident = Some(#f_local_ident);
                                return a::R::Ok(true);
                            }
                        });
                    }
                    let field_help_pattern;
                    if type_break_help || field_vark_attr.break_help {
                        field_help_pattern = quote!(a::HelpPattern(vec![]));
                    }
                    else {
                        field_help_pattern = gen.help_pattern;
                    }
                    let help_field = quote!{
                        a:: HelpFlagField {
                            option: #optional,
                            flags: vec ![#(#flags.to_string()), *],
                            pattern: #field_help_pattern,
                            description: #field_help_docstr.to_string(),
                        }
                    };
                    help_fields.push(quote!{
                        struct_.flag_fields.push(#help_field);
                    });
                    partial_help_fields.push(quote!{
                        if flag_fields.#field_ident.is_none() {
                            help_flag_fields.push(#help_field);
                        }
                    });
                    continue 'next_field;
                };

                // Positional/required parsing
                let field_help_placeholder =
                    field_vark_attr
                        .placeholder
                        .unwrap_or_else(|| field_ident.to_string().to_case(Case::UpperKebab));
                let gen = gen_impl_type(&f.ty, &ident.to_string());
                let vark = gen.vark;
                let field_help_pattern = gen.help_pattern;
                vark_parse_positional.push(quote!{
                    let #f_local_ident = loop {
                        let peek = state.peek();
                        if match peek {
                            a:: PeekR:: None => false,
                            a:: PeekR:: Help => return a:: R:: Help(Box:: new(move | state | {
                                return a:: HelpPartialProduction {
                                    description: #help_docstr.to_string(),
                                    content: build_partial_help(state, #required_i, &flag_fields),
                                };
                            })),
                            a:: PeekR:: Ok(
                                s
                            ) => match parse_flags(&mut need_flags, &mut flag_fields, state, s.to_string()) {
                                a:: R:: Ok(v) => v,
                                a:: R:: Help(b) => break a:: R:: Help(b),
                                a:: R:: Err => break a:: R:: Err,
                            },
                        }
                        {
                            continue;
                        }
                        break #vark;
                    };
                    let #f_local_ident = match #f_local_ident {
                        a:: R:: Ok(v) => v,
                        a:: R:: Help(b) => break a:: R:: Help(b),
                        a:: R:: Err => break a:: R:: Err,
                    };
                });
                vark_copy_flag_fields.push(quote!{
                    #field_ident: #f_local_ident
                });
                let help_field = quote!{
                    a:: HelpField {
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
                        help_fields.push(#help_field);
                    }
                });
                required_i += 1;
            }

            // Assemble code
            let vark = quote!{
                {
                    loop {
                        struct FlagFields #decl_generics {
                            #(#vark_flag_fields) *
                        }
                        let mut flag_fields = FlagFields {
                            #(#vark_flag_fields_default) *
                        };
                        type NeedFlags = std::collections::HashSet<&'static str>;
                        let mut need_flags =[#(#init_need_flags) *].into_iter().collect::< NeedFlags >();
                        fn parse_flags #decl_generics(
                            need_flags: & mut NeedFlags,
                            flag_fields:& mut FlagFields #forward_generics,
                            state:& mut a:: VarkState,
                            s: String
                        ) -> a:: R < bool > {
                            match s.as_str() {
                                #(#vark_parse_flag_cases) * 
                                //. .
                                _ => return a:: R:: Ok(false),
                            };
                        }
                        fn build_partial_help #decl_generics(
                            state:& mut a:: HelpState,
                            required_i: usize,
                            flag_fields:& FlagFields #forward_generics,
                        ) -> a:: HelpPartialContent {
                            let mut help_fields = vec![];
                            let mut help_flag_fields = vec![];
                            #(#partial_help_fields) * 
                            //. .
                            return a:: HelpPartialContent:: struct_(help_fields, help_flag_fields);
                        }
                        #(#vark_parse_positional) * 
                        // Parse any remaining optional args
                        let flag_search_res = loop {
                            match state.peek() {
                                a:: PeekR:: None => {
                                    break state.r_ok((), None);
                                },
                                a:: PeekR:: Help => return a:: R:: Help(Box:: new(move | state | {
                                    return a:: HelpPartialProduction {
                                        description: #help_docstr.to_string(),
                                        content: build_partial_help(state, #required_i, &flag_fields),
                                    };
                                })),
                                a:: PeekR:: Ok(
                                    s
                                ) => match parse_flags(&mut need_flags, &mut flag_fields, state, s.to_string()) {
                                    a:: R:: Ok(v) => {
                                        if !v {
                                            break state.r_ok((), None);
                                        }
                                    },
                                    a:: R:: Help(b) => break a:: R:: Help(b),
                                    a:: R:: Err => break a:: R:: Err,
                                },
                            };
                        };
                        match flag_search_res {
                            a::R::Ok(()) => { },
                            a::R::Help(b) => break a::R::Help(b),
                            a::R::Err => break a::R::Err,
                        };
                        fn build_completer(need_flags: NeedFlags) -> a::AargvarkCompleter {
                            return Box::new(move || {
                                return need_flags.iter().map(|v| vec![v.to_string()]).collect();
                            });
                        }
                        // Build obj + return
                        break state.r_ok(#ident {
                            #(#vark_copy_flag_fields),
                            *
                        }, None);
                    }
                }
            };
            return Ok(GenRec {
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
                        let mut struct_ = struct_.as_ref().borrow_mut();
                        #(#help_fields) * 
                        //. .
                        a:: HelpPattern(vec![a::HelpPatternElement::Reference(key)])
                    }
                },
            });
        },
        Fields::Unnamed(d) => {
            let mut fields = vec![];
            for f in &d.unnamed {
                fields.push((FieldAttr::from_field(f)?, get_docstr(&f.attrs), &f.ty));
            }
            return Ok(
                gen_impl_unnamed(
                    &ident.to_string(),
                    parent_ident,
                    ident.to_token_stream(),
                    help_placeholder,
                    help_docstr,
                    subtype_index,
                    &fields,
                ),
            );
        },
        Fields::Unit => {
            return Ok(GenRec {
                vark: quote!{
                    state.r_ok(#ident, None)
                },
                help_pattern: quote!{
                    a::HelpPattern(vec![])
                },
            });
        },
    };
}

fn gen_impl(ast: syn::DeriveInput) -> Result<TokenStream, syn::Error> {
    let ident = &ast.ident;
    let type_attr = TypeAttr::from_derive_input(&ast)?;
    let help_docstr = get_docstr(&ast.attrs);
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
    let help_placeholder =
        type_attr.placeholder.clone().unwrap_or_else(|| ident.to_string().to_case(Case::UpperKebab));
    let impl_vark;
    let impl_help_build;
    match &ast.data {
        syn::Data::Struct(d) => {
            let gen =
                gen_impl_struct(
                    ast.ident.to_token_stream(),
                    ast.ident.to_token_stream(),
                    &decl_generics,
                    &forward_generics,
                    &help_placeholder,
                    type_attr.break_help,
                    &help_docstr,
                    0,
                    &d.fields,
                )?;
            impl_vark = gen.vark;
            impl_help_build = gen.help_pattern;
        },
        syn::Data::Enum(d) => {
            let mut all_tags = vec![];
            let mut vark_cases = vec![];
            let mut help_variants = vec![];
            for (subtype_index, v) in d.variants.iter().enumerate() {
                let variant_vark_attr = VariantAttr::from_variant(v)?;
                let variant_help_docstr = get_docstr(&v.attrs);
                let variant_ident = &v.ident;
                let name_str =
                    variant_vark_attr
                        .name
                        .clone()
                        .unwrap_or_else(|| variant_ident.to_string().to_case(Case::Kebab));
                let help_placeholder =
                    variant_vark_attr
                        .placeholder
                        .unwrap_or_else(|| variant_ident.to_string().to_case(Case::UpperKebab));
                let gen =
                    gen_impl_struct(
                        ident.to_token_stream(),
                        quote!(#ident:: #variant_ident),
                        &decl_generics,
                        &forward_generics,
                        &help_placeholder,
                        variant_vark_attr.break_help,
                        "",
                        subtype_index + 1,
                        &v.fields,
                    )?;
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
                if type_attr.break_help || variant_vark_attr.break_help {
                    help_variant_pattern =
                        quote!(a::HelpPattern(vec![a::HelpPatternElement::PseudoReference("...".to_string())]));
                } else {
                    help_variant_pattern = partial_help_variant_pattern;
                }
                help_variants.push(quote!{
                    variants.push(a:: HelpVariant {
                        literal: #name_str.to_string(),
                        pattern: #help_variant_pattern,
                        description: #variant_help_docstr.to_string(),
                    });
                });
            }
            impl_vark = quote!{
                {
                    fn build_completer(arg: & str) -> a:: AargvarkCompleter {
                        let arg = arg.to_string();
                        return Box:: new(move || {
                            let mut out = vec![];
                            for want_arg in &[#(#all_tags), *] {
                                if want_arg.starts_with(&arg) {
                                    out.push(vec![want_arg.to_string()]);
                                }
                            }
                            return out;
                        });
                    }
                    let tag = match state.peek() {
                        a:: PeekR:: None => {
                            return state.r_err(
                                format!("Need variant tag - choices are {:?}", vec![#(#all_tags), *]),
                                Some(build_completer("")),
                            );
                        },
                        a:: PeekR:: Help => return a:: R:: Help(Box:: new(move | state | {
                            let mut variants = vec![];
                            #(#help_variants) * 
                            //. .
                            return a:: HelpPartialProduction {
                                description: #help_docstr.to_string(),
                                content: a:: HelpPartialContent:: enum_(variants),
                            };
                        })),
                        a:: PeekR:: Ok(s) => s,
                    };
                    match tag {
                        #(#vark_cases) * 
                        //. .
                        _ => {
                            state.r_err(
                                format!("Unrecognized variant {} - choices are {:?}", tag, vec![#(#all_tags), *]),
                                Some(build_completer(tag)),
                            )
                        }
                    }
                }
            };
            impl_help_build = quote!{
                let(
                    key,
                    variants
                ) = state.add_enum(std::any::TypeId::of::<Self>(), 0, #help_placeholder, #help_docstr);
                let mut variants = variants.as_ref().borrow_mut();
                #(#help_variants) * 
                //. .
                return a:: HelpPattern(vec![a::HelpPatternElement::Reference(key)]);
            };
        },
        syn::Data::Union(_) => panic!("Union not supported"),
    };
    return Ok(quote!{
        impl #decl_generics aargvark:: traits:: AargvarkTrait for #ident #forward_generics {
            fn vark(state:& mut aargvark:: base:: VarkState) -> aargvark:: base:: R < #ident #forward_generics > {
                mod a {
                    pub use aargvark::help::*;
                    pub use aargvark::base::*;
                    pub use aargvark::traits::*;
                }
                #impl_vark
            }
            fn build_help_pattern(state:& mut aargvark:: help:: HelpState) -> aargvark:: help:: HelpPattern {
                mod a {
                    pub use aargvark::help::*;
                    pub use aargvark::traits::*;
                }
                #impl_help_build
            }
        }
    });
}

#[proc_macro_derive(Aargvark, attributes(vark))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    return match gen_impl(parse_macro_input!(input as DeriveInput)) {
        Ok(x) => x,
        Err(e) => e.to_compile_error(),
    }.into();
}

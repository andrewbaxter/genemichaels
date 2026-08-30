use proc_macro2::{
    Delimiter,
    Group,
    Spacing,
    TokenStream,
    TokenTree,
};

fn fixup(ts: TokenStream, in_assoc_body: bool) -> TokenStream {
    let mut toks: Vec<TokenTree> = ts.into_iter().collect();
    let mut depth = 0i32;
    let mut at_prefix = true;
    let mut item_is_impl_or_trait = false;
    let mut prev_hash = false;
    let mut prev_pub = false;
    for idx in 0 .. toks.len() {
        let is_body_brace =
            depth == 0 && item_is_impl_or_trait &&
                matches!(&toks[idx], TokenTree:: Group(g) if g.delimiter() == Delimiter:: Brace);
        if matches!(&toks[idx], TokenTree::Group(_)) {
            let (delimiter, span, stream) = {
                let TokenTree::Group(g) = &toks[idx] else {
                    unreachable!();
                };
                (g.delimiter(), g.span(), g.stream())
            };
            let mut new_g = Group::new(delimiter, fixup(stream, is_body_brace));
            new_g.set_span(span);
            toks[idx] = TokenTree::Group(new_g);
        }
        let mut next_hash = false;
        let mut next_pub = false;
        match &toks[idx] {
            TokenTree::Punct(p) => {
                let c = p.as_char();
                if c == '<' {
                    depth += 1;
                } else if c == '>' {
                    let is_arrow =
                        idx > 0 &&
                            matches!(
                                &toks[idx - 1],
                                TokenTree:: Punct(prev) if prev.as_char() == '-' && prev.spacing() == Spacing:: Joint
                            );
                    if !is_arrow && depth > 0 {
                        depth -= 1;
                    }
                } else if depth == 0 {
                    if c == ';' {
                        at_prefix = true;
                        item_is_impl_or_trait = false;
                    } else if c == '#' && at_prefix {
                        next_hash = true;
                    } else {
                        at_prefix = false;
                    }
                }
            },
            TokenTree::Ident(id) if depth == 0 => {
                if at_prefix && (id == "impl" || id == "trait") {
                    item_is_impl_or_trait = true;
                    at_prefix = false;
                } else if at_prefix && (id == "pub" || id == "unsafe" || id == "default" || id == "auto") {
                    next_pub = id == "pub";
                } else {
                    at_prefix = false;
                }
            },
            TokenTree::Group(g) if depth == 0 => {
                if is_body_brace || g.delimiter() == Delimiter::Brace {
                    at_prefix = true;
                    item_is_impl_or_trait = false;
                } else if at_prefix && prev_hash && g.delimiter() == Delimiter::Bracket {
                } else if at_prefix && prev_pub && g.delimiter() == Delimiter::Parenthesis {
                } else {
                    at_prefix = false;
                }
            },
            _ => {
                if depth == 0 {
                    at_prefix = false;
                }
            },
        }
        prev_hash = next_hash;
        prev_pub = next_pub;
    }
    if in_assoc_body {
        fixup_type_where_clauses(&mut toks);
    }
    TokenStream::from_iter(toks)
}

pub(crate) fn fixup_deprecated_syntax(ts: TokenStream) -> TokenStream {
    fixup(ts, false)
}

fn fixup_type_where_clauses(toks: &mut Vec<TokenTree>) {
    let mut i = 0;
    while i < toks.len() {
        if !is_ident(&toks[i], "type") {
            i += 1;
            continue;
        }
        let mut depth = 0i32;
        let mut where_idx = None;
        let mut eq_idx = None;
        let mut semi_idx = None;
        let mut j = i + 1;
        while j < toks.len() {
            match &toks[j] {
                TokenTree::Punct(p) => {
                    let c = p.as_char();
                    if c == '<' {
                        depth += 1;
                    } else if c == '>' {
                        let is_arrow =
                            j > 0 &&
                                matches!(
                                    &toks[j - 1],
                                    TokenTree:: Punct(
                                        prev
                                    ) if prev.as_char() == '-' && prev.spacing() == Spacing:: Joint
                                );
                        if !is_arrow && depth > 0 {
                            depth -= 1;
                        }
                    } else if depth == 0 {
                        if c == ';' {
                            semi_idx = Some(j);
                            break;
                        }
                        if c == '=' && eq_idx.is_none() {
                            let joined =
                                p.spacing() == Spacing::Joint &&
                                    matches!(
                                        toks.get(j + 1),
                                        Some(TokenTree::Punct(next)) if next.as_char() == '=' || next.as_char() == '>'
                                    );
                            if !joined {
                                eq_idx = Some(j);
                            }
                        }
                    }
                },
                TokenTree::Ident(id) if depth == 0 && where_idx.is_none() && id == "where" => {
                    where_idx = Some(j);
                },
                _ => { },
            }
            j += 1;
        }
        if let (Some(w), Some(e), Some(s)) = (where_idx, eq_idx, semi_idx) {
            if w < e && e < s {
                let mut reordered = toks[e .. s].to_vec();
                reordered.extend_from_slice(&toks[w .. e]);
                toks.splice(w .. s, reordered);
                i = s + 1;
                continue;
            }
        }
        i += 1;
    }
}

fn is_ident(t: &TokenTree, name: &str) -> bool {
    matches!(t, TokenTree:: Ident(i) if i == name)
}

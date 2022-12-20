//! Types of lists:
//!
//! * inline raw: `a, b, c`, splits with `a` staying on first line
//!
//! * inline: `a, b, c`, splits with `a` on the next line, indented (comes after something
//!    else); where statements, generic + separated
//!
//! * bracketed: `(a, b, c)` - inline within brackets, comma after final element; [], <>,
//!    {} in use statements
//!
//! * curly bracketed: `{ a, b, c }` - inline within brackets with spaces
use proc_macro2::LineColumn;
use quote::ToTokens;
use syn::{
    punctuated::Punctuated,
    Expr,
};
use crate::{
    Formattable,
    FormattablePunct,
    MakeSegsState,
    Alignment,
    SplitGroupBuilder,
    sg_general::{
        append_comments,
        has_comments,
    },
    SplitGroupIdx,
    new_sg,
    comments::HashLineColumn,
};

pub(crate) enum InlineListSuffix<T: Formattable> {
    None,
    Punct,
    // unit tuples only
    UnitPunct,
    Extra(T),
}

pub(crate) fn append_inline_list_raw<
    E: Formattable + ToTokens,
    T: FormattablePunct,
    F: Formattable,
>(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    punct: &str,
    exprs: &Punctuated<E, T>,
    suffix: InlineListSuffix<F>,
) {
    if exprs.pairs().any(|s| has_comments(out, s.value()) || (s.value().has_attrs() && out.split_attributes)) {
        sg.initial_split();
    }
    let mut next_punct: Option<&T> = None;
    for (i, pair) in exprs.pairs().enumerate() {
        if i > 0 {
            if let Some(p) = next_punct.take() {
                append_comments(out, base_indent, sg, p.span_start());
                sg.seg(out, punct);
            }
            sg.split(out, base_indent.clone(), true);
            sg.seg_unsplit(out, " ");
        }
        sg.child(pair.value().make_segs(out, base_indent));
        next_punct = pair.punct().copied();
    }
    match suffix {
        InlineListSuffix::None => { },
        InlineListSuffix::UnitPunct if exprs.len() == 1 => {
            if let Some(p) = next_punct {
                append_comments(out, base_indent, sg, p.span_start());
                sg.seg(out, punct);
            } else if !exprs.is_empty() {
                sg.seg(out, punct);
            }
        },
        InlineListSuffix::Punct | InlineListSuffix::UnitPunct => {
            if let Some(p) = next_punct {
                append_comments(out, base_indent, sg, p.span_start());
                sg.seg_split(out, punct);
            } else if !exprs.is_empty() {
                sg.seg_split(out, punct);
            }
        },
        InlineListSuffix::Extra(e) => {
            if let Some(p) = next_punct {
                append_comments(out, base_indent, sg, p.span_start());
                sg.seg(out, punct);
            } else if !exprs.is_empty() {
                sg.seg(out, punct);
            }
            if !exprs.is_empty() {
                sg.split(out, base_indent.clone(), true);
                sg.seg_unsplit(out, " ");
            }
            e.make_segs(out, base_indent);
        },
    }
}

pub(crate) fn append_inline_list<
    E: Formattable + ToTokens,
    T: FormattablePunct,
    F: Formattable,
>(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    punct: &str,
    exprs: &Punctuated<E, T>,
    suffix: InlineListSuffix<F>,
) {
    let indent = base_indent.indent();
    sg.split(out, indent.clone(), true);
    append_inline_list_raw(out, &indent, sg, punct, exprs, suffix);
}

pub(crate) fn append_bracketed_list<
    E: Formattable + ToTokens,
    T: FormattablePunct,
    F: Formattable,
>(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    prefix_start: LineColumn,
    prefix: &str,
    bracket_space: bool,
    punct: &str,
    exprs: &Punctuated<E, T>,
    list_suffix: InlineListSuffix<F>,
    suffix_start: LineColumn,
    suffix: &str,
) {
    if out.comments.contains_key(&HashLineColumn(suffix_start)) {
        sg.initial_split();
    }
    append_comments(out, base_indent, sg, prefix_start);
    sg.seg(out, prefix);
    let need_pad = bracket_space && (!exprs.is_empty() || matches!(&list_suffix, InlineListSuffix::Extra(_)));
    if need_pad {
        sg.seg_unsplit(out, " ");
    }
    let indent = base_indent.indent();
    sg.split(out, indent.clone(), true);
    append_inline_list_raw(out, &indent, sg, punct, exprs, list_suffix);
    if need_pad {
        sg.seg_unsplit(out, " ");
    }
    append_comments(out, &indent, sg, suffix_start);
    sg.split(out, base_indent.clone(), false);
    sg.seg(out, suffix);
}

pub(crate) fn append_bracketed_list_common<
    E: Formattable + ToTokens,
    T: FormattablePunct,
>(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    prefix_start: LineColumn,
    prefix: &str,
    exprs: &Punctuated<E, T>,
    suffix_start: LineColumn,
    suffix: &str,
) {
    append_bracketed_list(
        out,
        base_indent,
        sg,
        prefix_start,
        prefix,
        false,
        ",",
        exprs,
        InlineListSuffix::<Expr>::Punct,
        suffix_start,
        suffix,
    );
}

pub(crate) fn append_bracketed_list_curly<
    E: Formattable + ToTokens,
    T: FormattablePunct,
>(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    sg: &mut SplitGroupBuilder,
    prefix_start: LineColumn,
    exprs: &Punctuated<E, T>,
    extra: Option<impl Formattable>,
    suffix_start: LineColumn,
) {
    append_bracketed_list(out, base_indent, sg, prefix_start, " {", true, ",", exprs, match extra {
        Some(e) => InlineListSuffix::Extra(e),
        None => InlineListSuffix::Punct,
    }, suffix_start, "}")
}

pub(crate) fn new_sg_bracketed_list<
    E: Formattable + ToTokens,
    T: FormattablePunct,
    F: Formattable,
>(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    prefix_start: LineColumn,
    prefix: &str,
    bracket_space: bool,
    punct: &str,
    exprs: &Punctuated<E, T>,
    list_suffix: InlineListSuffix<F>,
    suffix_start: LineColumn,
    suffix: &str,
) -> SplitGroupIdx {
    let mut sg = new_sg(out);
    append_bracketed_list(
        out,
        base_indent,
        &mut sg,
        prefix_start,
        prefix,
        bracket_space,
        punct,
        exprs,
        list_suffix,
        suffix_start,
        suffix,
    );
    sg.build(out)
}

pub(crate) fn new_sg_bracketed_list_common<
    E: Formattable + ToTokens,
    T: FormattablePunct,
>(
    out: &mut MakeSegsState,
    base_indent: &Alignment,
    prefix_start: LineColumn,
    prefix: &str,
    exprs: &Punctuated<E, T>,
    suffix_start: LineColumn,
    suffix: &str,
) -> SplitGroupIdx {
    new_sg_bracketed_list(
        out,
        base_indent,
        prefix_start,
        prefix,
        false,
        ",",
        exprs,
        InlineListSuffix::<Expr>::Punct,
        suffix_start,
        suffix,
    )
}

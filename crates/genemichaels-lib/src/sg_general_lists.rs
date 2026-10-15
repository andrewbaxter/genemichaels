//! Types of lists:
//!
//! * inline raw: `a, b, c`, splits with `a` staying on first line
//!
//! * inline: `a, b, c`, splits with `a` on the next line, indented (comes after
//!   something else); where statements, generic + separated
//!
//! * bracketed: `(a, b, c)` - inline within brackets, comma after final element; [],
//!   <>, {} in use statements
//!
//! * curly bracketed: `{ a, b, c }` - inline within brackets with spaces
use {
    crate::{
        Alignment,
        Formattable,
        FormattablePunct,
        MakeSegsState,
        SplitGroupBuilder,
        SplitGroupIdx,
        new_sg,
        sg_general::{
            append_whitespace,
            append_whitespace_list,
            has_comments,
        },
        whitespace::HashLineColumn,
    },
    proc_macro2::LineColumn,
    quote::ToTokens,
    syn::{
        Expr,
        punctuated::Punctuated,
        spanned::Spanned,
    },
};

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
    let suffix_whitespace = out.whitespaces.contains_key(&HashLineColumn(suffix_start));
    if suffix_whitespace {
        sg.initial_split();
    }
    append_whitespace(out, base_indent, sg, prefix_start);
    sg.seg(out, prefix);
    let indent = base_indent.indent();
    let empty = exprs.is_empty() && !matches!(&list_suffix, InlineListSuffix::Extra(_));
    let trailing_whitespace = if out.macro_depth.get() == 0 && matches!(&list_suffix, InlineListSuffix::Punct) {
        exprs.pairs().last().and_then(|p| p.punct().map(|p| p.span_start())).and_then(|loc| {
            let key = HashLineColumn(loc);
            out.whitespace_anchors.insert(key);
            return out.whitespaces.remove(&key);
        })
    } else {
        None
    };
    if trailing_whitespace.is_some() {
        sg.initial_split();
    }
    if !empty {
        if bracket_space {
            sg.seg_unsplit(out, " ");
        }
        sg.split(out, indent.clone(), true);
        append_inline_list_raw(out, &indent, sg, punct, exprs, list_suffix);
        if bracket_space {
            sg.seg_unsplit(out, " ");
        }
    }
    if let Some((_, ws)) = trailing_whitespace {
        append_whitespace_list(out, &indent, sg, ws);
    }
    append_whitespace(out, &indent, sg, suffix_start);
    if !empty || suffix_whitespace {
        sg.split(out, base_indent.clone(), false);
    }
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
        if out.macro_depth.get() == 0 {
            InlineListSuffix::<Expr>::Punct
        } else {
            InlineListSuffix::VerbatimPunct
        },
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
        None => if out.macro_depth.get() == 0 {
            InlineListSuffix::Punct
        } else {
            InlineListSuffix::VerbatimPunct
        },
    }, suffix_start, "}")
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
    if exprs
        .pairs()
        .any(
            |s| has_comments(out, s.value()) || (s.value().has_attrs() && out.config.split_attributes) ||
                s.punct().is_some_and(|p| out.whitespaces.contains_key(&HashLineColumn(p.span_start()))),
        ) {
        sg.initial_split();
    }
    let mut next_punct: Option<&T> = None;
    for (i, pair) in exprs.pairs().enumerate() {
        if i > 0 {
            let mut punct_comments_after = None;
            if let Some(p) = next_punct.take() {
                if out.macro_depth.get() > 0 {
                    append_whitespace(out, base_indent, sg, p.span_start());
                    sg.seg(out, punct);
                } else {
                    sg.seg(out, punct);
                    punct_comments_after = Some(p.span_start());
                }
            }
            sg.split(out, base_indent.clone(), true);
            sg.seg_unsplit(out, " ");
            if let Some(loc) = punct_comments_after {
                append_whitespace(out, base_indent, sg, loc);
            }
        }
        let span = pair.value().span();
        out.record_element(span);
        append_whitespace(out, base_indent, sg, span.start());
        sg.child(pair.value().make_segs(out, base_indent));
        next_punct = pair.punct().copied();
    }
    let suffix = match suffix {
        InlineListSuffix::None if out.macro_depth.get() > 0 => InlineListSuffix::VerbatimPunct,
        s => s,
    };
    match suffix {
        InlineListSuffix::None => {
            if let Some(p) = next_punct {
                append_whitespace(out, base_indent, sg, p.span_start());
            }
        },
        InlineListSuffix::UnitPunct if exprs.len() == 1 => {
            if let Some(p) = next_punct {
                sg.seg(out, punct);
                append_whitespace(out, base_indent, sg, p.span_start());
            } else if !exprs.is_empty() {
                sg.seg(out, punct);
            }
        },
        InlineListSuffix::Punct | InlineListSuffix::UnitPunct => {
            if let Some(p) = next_punct {
                sg.seg_split(out, punct);
                append_whitespace(out, base_indent, sg, p.span_start());
            } else if !exprs.is_empty() {
                sg.seg_split(out, punct);
            }
        },
        InlineListSuffix::VerbatimPunct => {
            if let Some(p) = next_punct {
                append_whitespace(out, base_indent, sg, p.span_start());
                sg.seg(out, punct);
            }
        },
        InlineListSuffix::Extra(e) => {
            if let Some(p) = next_punct {
                sg.seg(out, punct);
                append_whitespace(out, base_indent, sg, p.span_start());
            } else if !exprs.is_empty() {
                sg.seg(out, punct);
            }
            if !exprs.is_empty() {
                sg.split(out, base_indent.clone(), true);
                sg.seg_unsplit(out, " ");
            }
            let first_seg = out.segs.len();
            sg.child(e.make_segs(out, base_indent));
            if out.segs[first_seg..].iter().any(|s| match &s.content {
                crate::SegmentContent::Whitespace((_, ws)) => ws
                    .iter()
                    .any(|w| matches!(w.mode, crate::WhitespaceMode::Comment(_))),
                _ => false,
            }) {
                sg.initial_split();
            }
        },
    }
}

pub(crate) enum InlineListSuffix<T: Formattable> {
    // _Must_ have final punctuation, but followed by an extra pseudo-element like
    // `..Default::default()`
    Extra(T),
    // _Must_ have no final punctuation, like in `A: X + Y + C` (can't have final
    // `+`), `|` in where clauses
    None,
    // Add final punctuation as long as not empty (most `,` separated lists)
    Punct,
    // Unit tuples only - final punctuation mandatory
    UnitPunct,
    // Keep existing final punctuation, mostly for macros where this is significant
    VerbatimPunct,
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
    new_sg_bracketed_list(out, base_indent, prefix_start, prefix, false, ",", exprs, if out.macro_depth.get() == 0 {
        InlineListSuffix::<Expr>::Punct
    } else {
        InlineListSuffix::VerbatimPunct
    }, suffix_start, suffix)
}

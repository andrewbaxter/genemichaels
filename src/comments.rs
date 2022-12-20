use anyhow::{
    anyhow,
    Result,
};
use markdown::mdast::Node;
use crate::{
    Comment,
    CommentMode,
    es,
};
use proc_macro2::{
    LineColumn,
    TokenStream,
    Group,
};
use std::cell::RefCell;
use std::collections::{
    HashMap,
};
use std::hash::Hash;
use std::rc::Rc;
use std::str::FromStr;
use structre::UnicodeRegex;

#[derive(PartialEq, Eq, Debug)]
pub struct HashLineColumn(pub LineColumn);

impl Hash for HashLineColumn {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.0.line, self.0.column).hash(state);
    }
}

pub fn extract_comments(source: &str) -> Result<(HashMap<HashLineColumn, Vec<Comment>>, TokenStream)> {
    let mut line_lookup = vec![];
    {
        let mut offset = 0usize;
        loop {
            line_lookup.push(offset);
            offset += match source[offset..].find('\n') {
                Some(r) => r,
                None => {
                    break;
                },
            } + 1;
        }
    }

    struct State<'a> {
        source: &'a str,
        // starting offset of each line
        line_lookup: Vec<usize>,
        comments: HashMap<HashLineColumn, Vec<Comment>>,
        line_start: Option<LineColumn>,
        last_offset: usize,
        start_re: Option<UnicodeRegex>,
        block_event_re: Option<UnicodeRegex>,
    }

    impl<'a> State<'a> {
        fn to_offset(&self, loc: LineColumn) -> usize {
            if loc.line == 0 {
                return 0usize;
            }
            let line_start_offset = *self.line_lookup.get(loc.line - 1).unwrap();
            line_start_offset +
                self.source[line_start_offset..].chars().take(loc.column).map(char::len_utf8).sum::<usize>()
        }

        fn add_comments(&mut self, end: LineColumn, whole_text: &str) {
            let start_re =
                &self
                    .start_re
                    .get_or_insert_with(|| UnicodeRegex::new(r#"(?:(//)(/|!|\.)?)|(?:(/\*)(\*|!)?)"#).unwrap());
            let block_event_re =
                &self.block_event_re.get_or_insert_with(|| UnicodeRegex::new(r#"((?:/\*)|(?:\*/))"#).unwrap());

            struct CommentBuffer {
                out: Vec<Comment>,
                mode: CommentMode,
                lines: Vec<String>,
                loc: LineColumn,
            }

            impl CommentBuffer {
                fn flush(&mut self) {
                    if self.lines.is_empty() {
                        return;
                    }
                    self.out.push(Comment {
                        loc: self.loc,
                        mode: self.mode,
                        lines: self.lines.split_off(0).join("\n"),
                    });
                }

                fn add(&mut self, mode: CommentMode, line: &str) {
                    if self.mode != mode && !self.lines.is_empty() {
                        self.flush();
                    }
                    self.mode = mode;
                    self.lines.push(line.to_string());
                }
            }

            let mut buffer = CommentBuffer {
                out: vec![],
                mode: CommentMode::Normal,
                lines: vec![],
                loc: end,
            };
            let mut text = whole_text;
            'comment_loop : loop {
                match start_re.captures(text) {
                    Some(found_start) => {
                        let start_prefix_match = found_start.get(1).or_else(|| found_start.get(3)).unwrap();
                        match start_prefix_match.as_str() {
                            "//" => {
                                let mode = {
                                    let start_suffix_match = found_start.get(2);
                                    let (mode, match_end) = match start_suffix_match {
                                        Some(start_suffix_match) => (match start_suffix_match.as_str() {
                                            "/" => CommentMode::DocOuter,
                                            "!" => CommentMode::DocInner,
                                            "." => CommentMode::Verbatim,
                                            _ => unreachable!(),
                                        }, start_suffix_match.end()),
                                        None => (CommentMode::Normal, start_prefix_match.end()),
                                    };
                                    text = &text[match_end..];
                                    mode
                                };
                                let (line, next_start) = match text.find('\n') {
                                    Some(line_end) => (&text[..line_end], line_end + 1),
                                    None => (text, text.len()),
                                };
                                buffer.add(mode, line);
                                text = &text[next_start..];
                            },
                            "/*" => {
                                let mode = {
                                    let start_suffix_match = found_start.get(2);
                                    let (mode, match_end) = match start_suffix_match {
                                        Some(start_suffix_match) => (match start_suffix_match.as_str() {
                                            "*" => CommentMode::DocOuter,
                                            "!" => CommentMode::DocInner,
                                            _ => unreachable!(),
                                        }, start_suffix_match.end()),
                                        None => (CommentMode::Normal, start_prefix_match.end()),
                                    };
                                    text = &text[match_end..];
                                    mode
                                };
                                let mut nesting = 1;
                                let mut search_end_at = 0usize;
                                let (lines, next_start) = loop {
                                    let found_event =
                                        block_event_re.captures(&text[search_end_at..]).unwrap().get(1).unwrap();
                                    let event_start = search_end_at + found_event.start();
                                    search_end_at += found_event.end();
                                    match found_event.as_str() {
                                        "/*" => {
                                            nesting += 1;
                                        },
                                        "*/" => {
                                            nesting -= 1;
                                            if nesting == 0 {
                                                break (&text[..event_start], search_end_at);
                                            }
                                        },
                                        _ => unreachable!(),
                                    }
                                };
                                for line in lines.lines() {
                                    let mut line = line.trim();
                                    line = line.strip_prefix("* ").unwrap_or(line);
                                    buffer.add(mode, line);
                                }
                                text = &text[next_start..];
                            },
                            _ => unreachable!(),
                        }
                    },
                    None => {
                        break 'comment_loop;
                    },
                }
            }
            buffer.flush();
            if !buffer.out.is_empty() {
                self.comments.insert(HashLineColumn(end), buffer.out);
            }
        }

        fn extract(&mut self, mut start: usize, end: LineColumn) {
            // Transpose line-end comments to line-start
            if if let Some(previous_start) = &self.line_start {
                if end.line > previous_start.line {
                    let eol = match self.source[start..].find('\n') {
                        Some(n) => start + n,
                        None => self.source.len(),
                    };
                    let text = &self.source[start .. eol];
                    if text.trim_start().starts_with("//") {
                        self.add_comments(*previous_start, text);
                    }
                    start = eol;
                    true
                } else {
                    false
                }
            } else {
                true
            } {
                self.line_start = Some(end);
            }

            // Do normal comment extraction
            let end_offset = self.to_offset(end);
            if end_offset < start {
                return;
            }
            let whole_text = &self.source[start .. end_offset];
            self.add_comments(end, whole_text);
        }
    }

    // Extract comments
    let mut state = State {
        source,
        line_lookup,
        comments: HashMap::new(),
        last_offset: 0usize,
        line_start: None,
        start_re: None,
        block_event_re: None,
    };

    fn recurse(state: &mut State, ts: TokenStream) -> TokenStream {
        let mut out = vec![];
        let mut ts = ts.into_iter().peekable();
        while let Some(t) = ts.next() {
            match t {
                proc_macro2::TokenTree::Group(g) => {
                    state.extract(state.last_offset, g.span_open().start());
                    state.last_offset = state.to_offset(g.span_open().end());
                    let subtokens = recurse(state, g.stream());
                    state.extract(state.last_offset, g.span_close().start());
                    state.last_offset = state.to_offset(g.span_close().end());
                    let mut new_g = Group::new(g.delimiter(), subtokens);
                    new_g.set_span(g.span());
                    out.push(proc_macro2::TokenTree::Group(new_g));
                },
                proc_macro2::TokenTree::Ident(g) => {
                    state.extract(state.last_offset, g.span().start());
                    state.last_offset = state.to_offset(g.span().end());
                    out.push(proc_macro2::TokenTree::Ident(g));
                },
                proc_macro2::TokenTree::Punct(g) => {
                    let offset = state.to_offset(g.span().start());
                    if g.as_char() == '#' && &state.source[offset .. offset + 1] == "/" {
                        // Syn converts doc comments into doc attrs, work around that here by detecting a mismatch between the token
                        // and the source (written /, token is #) and skipping all tokens within the fake doc attr range
                        loop {
                            let in_comment = ts.peek().map(|n| n.span().start() < g.span().end()).unwrap_or(false);
                            if !in_comment {
                                break;
                            }
                            ts.next();
                        }
                    } else {
                        state.extract(state.last_offset, g.span().start());
                        state.last_offset = state.to_offset(g.span().end());
                        out.push(proc_macro2::TokenTree::Punct(g));
                    }
                },
                proc_macro2::TokenTree::Literal(g) => {
                    state.extract(state.last_offset, g.span().start());
                    state.last_offset = state.to_offset(g.span().end());
                    out.push(proc_macro2::TokenTree::Literal(g));
                },
            }
        }
        TokenStream::from_iter(out)
    }

    let tokens =
        recurse(
            &mut state,
            TokenStream::from_str(
                source,
            ).map_err(
                |e| anyhow!(
                    "Error reconverting token stream at {}:{}: {:?}",
                    e.span().start().line,
                    e.span().start().column,
                    e
                ),
            )?,
        );
    Ok((state.comments, tokens))
}

struct State {
    line_buffer: String,
    need_nl: bool,
}

#[derive(Debug)]
struct LineState_ {
    first_prefix: Option<String>,
    prefix: String,
    explicit_wrap: bool,
    max_width: usize,
    rel_max_width: Option<usize>,
}

impl LineState_ {
    fn flush_always(&mut self, state: &mut State, out: &mut String, wrapping: bool) {
        out.push_str(format!("{}{}{}{}", if state.need_nl {
            "\n"
        } else {
            ""
        }, match &self.first_prefix.take() {
            Some(t) => t,
            None => &*self.prefix,
        }, &state.line_buffer, if wrapping && self.explicit_wrap {
            " \\"
        } else {
            ""
        }).trim_end());
        state.line_buffer.clear();
        state.need_nl = true;
    }

    fn flush(&mut self, state: &mut State, out: &mut String, wrapping: bool) {
        if !state.line_buffer.trim().is_empty() {
            self.flush_always(state, out, wrapping);
        }
    }

    fn calc_max_width(&self) -> usize {
        match self.rel_max_width {
            Some(w) => self.prefix.len() + w,
            None => self.max_width - if self.explicit_wrap {
                2
            } else {
                0
            },
        }
    }
}

/// Line split points, must be interior indexes (no 0 and no text.len())
fn get_splits(text: &str) -> impl Iterator<Item = usize> + '_ {
    // let segmenter =
    // LineBreakSegmenter::try_new_unstable(&icu_testdata::unstable()).unwrap(); match
    // segmenter .segment_str(&text)
    text.char_indices().filter(|i| i.1 == ' ').map(|i| i.0 + 1)
}

struct LineState(Rc<RefCell<LineState_>>);

impl LineState {
    fn new(
        first_prefix: Option<String>,
        prefix: String,
        max_width: usize,
        rel_max_width: Option<usize>,
        explicit_wrap: bool,
    ) -> LineState {
        LineState(Rc::new(RefCell::new(LineState_ {
            first_prefix,
            prefix,
            explicit_wrap,
            max_width,
            rel_max_width,
        })))
    }

    fn clone_inline(&self) -> LineState {
        LineState(self.0.clone())
    }

    fn clone_zero_indent(&self) -> LineState {
        let mut s = self.0.as_ref().borrow_mut();
        LineState(Rc::new(RefCell::new(LineState_ {
            first_prefix: s.first_prefix.take(),
            prefix: s.prefix.clone(),
            explicit_wrap: s.explicit_wrap,
            max_width: s.max_width,
            rel_max_width: s.rel_max_width,
        })))
    }

    fn clone_indent(&self, first_prefix: Option<String>, prefix: String, explicit_wrap: bool) -> LineState {
        let mut s = self.0.as_ref().borrow_mut();
        LineState(Rc::new(RefCell::new(LineState_ {
            first_prefix: match (s.first_prefix.take(), first_prefix) {
                (None, None) => None,
                (None, Some(p)) => Some(format!("{}{}", s.prefix, p)),
                (Some(p), None) => Some(p),
                (Some(p1), Some(p2)) => Some(format!("{}{}", p1, p2)),
            },
            prefix: format!("{}{}", s.prefix, prefix),
            explicit_wrap: s.explicit_wrap || explicit_wrap,
            max_width: s.max_width,
            rel_max_width: s.rel_max_width,
        })))
    }

    fn write_breakable(&self, state: &mut State, out: &mut String, text: &str) {
        let mut s = self.0.as_ref().borrow_mut();
        let max_width = s.calc_max_width();
        let mut text = text;
        if state.line_buffer.is_empty() {
            text = text.trim_start();
        }
        while !text.is_empty() {
            if state.line_buffer.chars().count() + text.chars().count() > max_width {
                match get_splits(text).take_while(|b| state.line_buffer.chars().count() + *b < max_width).last() {
                    Some(b) => {
                        // Doesn't fit, but can split to get within line
                        state.line_buffer.push_str(text[..b].trim_end());
                        text = text[b..].trim_start();
                        s.flush(state, out, !text.is_empty());
                    },
                    None => {
                        if !state.line_buffer.is_empty() {
                            // Doesn't fit, can't split, but stuff in buffer - flush that first
                            s.flush(state, out, true);
                        } else {
                            // Doesn't fit, can't split, but buffer empty - just write it
                            state.line_buffer.push_str(text.trim_end());
                            text = &text[text.len()..];
                        }
                    },
                }
            } else {
                // Fits
                state.line_buffer.push_str(text);
                text = &text[text.len()..];
            }
        }
    }

    fn flush_always(&self, state: &mut State, out: &mut String) {
        self.0.as_ref().borrow_mut().flush_always(state, out, false);
    }

    fn write_unbreakable(&self, state: &mut State, out: &mut String, text: &str) {
        let mut s = self.0.as_ref().borrow_mut();
        let max_width = s.calc_max_width();
        if state.line_buffer.chars().count() + text.chars().count() > max_width {
            s.flush(state, out, true);
        }
        state.line_buffer.push_str(text);
    }

    fn write_newline(&self, state: &mut State, out: &mut String) {
        let mut s = self.0.as_ref().borrow_mut();
        if !state.line_buffer.is_empty() {
            panic!();
        }
        s.flush_always(state, out, false);
    }
}

fn recurse_write(state: &mut State, out: &mut String, line: LineState, node: &Node, inline: bool) {
    fn join_lines(text: &str) -> String {
        let lines = text.lines().collect::<Vec<&str>>();
        let mut joined = String::new();
        for (i, line) in lines.iter().enumerate() {
            let mut line = *line;
            if i > 0 {
                line = line.trim_start();
                joined.push(' ');
            }
            if i < lines.len() - 1 {
                line = line.trim_end();
            }
            joined.push_str(line);
        }
        joined
    }

    match node {
        // block->block elements (newline between)
        Node::Root(x) => {
            for (i, child) in x.children.iter().enumerate() {
                if i > 0 {
                    line.write_newline(state, out);
                }
                recurse_write(state, out, line.clone_zero_indent(), child, false);
            }
        },
        Node::BlockQuote(x) => {
            let line = line.clone_indent(None, "> ".into(), false);
            for (i, child) in x.children.iter().enumerate() {
                if i > 0 {
                    line.write_newline(state, out);
                }
                recurse_write(state, out, line.clone_inline(), child, false);
            }
        },
        Node::List(x) => {
            match &x.start {
                Some(i) => {
                    // bug in markdown lib, start is actually the number of the last child:
                    // https://github.com/wooorm/markdown-rs/issues/38
                    for (j, child) in x.children.iter().enumerate() {
                        if j > 0 {
                            line.write_newline(state, out);
                        }
                        recurse_write(
                            state,
                            out,
                            line.clone_indent(Some(format!("{}. ", *i as usize + j)), "   ".into(), false),
                            child,
                            false,
                        );
                    }
                },
                None => {
                    for (i, child) in x.children.iter().enumerate() {
                        if i > 0 {
                            line.write_newline(state, out);
                        }
                        recurse_write(
                            state,
                            out,
                            line.clone_indent(Some("* ".into()), "   ".into(), false),
                            child,
                            false,
                        );
                    }
                },
            };
        },
        Node::ListItem(x) => {
            for (i, child) in x.children.iter().enumerate() {
                if i > 0 {
                    line.write_newline(state, out);
                }
                recurse_write(state, out, line.clone_zero_indent(), child, false);
            }
        },
        // block->inline elements (flush after)
        Node::Code(x) => {
            line.write_unbreakable(state, out, &format!("```{}", match &x.lang {
                None => "",
                Some(x) => x,
            }));
            line.flush_always(state, out);
            for l in x.value.as_str().lines() {
                line.write_unbreakable(state, out, l);
                line.flush_always(state, out);
            }
            line.write_unbreakable(state, out, "```");
            line.flush_always(state, out);
        },
        Node::Heading(x) => {
            let line = line.clone_indent(Some(format!("{} ", "#".repeat(x.depth as usize))), "  ".into(), true);
            for child in &x.children {
                recurse_write(state, out, line.clone_inline(), child, true);
            }
            line.flush_always(state, out);
        },
        Node::FootnoteDefinition(x) => {
            let line = line.clone_indent(Some(format!("[^{}]: ", x.identifier)), "   ".into(), false);
            for child in &x.children {
                recurse_write(state, out, line.clone_inline(), child, true);
            }
            line.flush_always(state, out);
        },
        Node::ThematicBreak(_) => {
            line.write_unbreakable(state, out, "---");
            line.flush_always(state, out);
        },
        Node::Definition(x) => {
            line.write_unbreakable(state, out, &format!("[{}]: {}", x.identifier.trim(), x.url));
            if let Some(title) = &x.title {
                line.write_unbreakable(state, out, " \"");
                line.write_breakable(state, out, title);
                line.write_unbreakable(state, out, "\"");
            }
            line.flush_always(state, out);
        },
        Node::Paragraph(x) => {
            for child in &x.children {
                recurse_write(state, out, line.clone_inline(), child, true);
            }
            line.flush_always(state, out);
        },
        Node::Html(x) if !inline => {
            line.write_unbreakable(state, out, &format!("`{}`", join_lines(&x.value)));
            line.flush_always(state, out);
        },
        // inline elements
        Node::Text(x) => {
            line.write_breakable(state, out, &join_lines(&x.value));
        },
        Node::InlineCode(x) => {
            line.write_unbreakable(state, out, &format!("`{}`", join_lines(&x.value)));
        },
        Node::Strong(x) => {
            line.write_unbreakable(state, out, "**");
            for child in &x.children {
                recurse_write(state, out, line.clone_inline(), child, true);
            }
            line.write_unbreakable(state, out, "**");
        },
        Node::Delete(x) => {
            line.write_unbreakable(state, out, "~~");
            for child in &x.children {
                recurse_write(state, out, line.clone_inline(), child, true);
            }
            line.write_unbreakable(state, out, "~~");
        },
        Node::Emphasis(x) => {
            line.write_unbreakable(state, out, "_");
            for child in &x.children {
                recurse_write(state, out, line.clone_inline(), child, true);
            }
            line.write_unbreakable(state, out, "_");
        },
        Node::FootnoteReference(x) => {
            line.write_unbreakable(state, out, &format!("[^{}]", x.identifier));
        },
        Node::Html(x) => {
            line.write_unbreakable(state, out, &format!("`{}`", join_lines(&x.value)));
        },
        Node::Image(x) => {
            match (get_splits(&x.alt).next().is_some(), &x.title) {
                (false, None) => {
                    line.write_unbreakable(state, out, &format!("![{}]({})", x.alt, x.url));
                },
                (false, Some(t)) => {
                    line.write_unbreakable(state, out, &format!("![{}]({}", x.alt, x.url));
                    line.write_unbreakable(state, out, " \"");
                    line.write_breakable(state, out, t);
                    line.write_unbreakable(state, out, "\")");
                },
                (true, None) => {
                    line.write_unbreakable(state, out, "![");
                    line.write_breakable(state, out, &x.alt);
                    line.write_unbreakable(state, out, &format!("]({})", x.url));
                },
                (true, Some(t)) => {
                    line.write_unbreakable(state, out, "![");
                    line.write_breakable(state, out, &x.alt);
                    line.write_unbreakable(state, out, &format!("]({}", x.url));
                    line.write_unbreakable(state, out, " \"");
                    line.write_breakable(state, out, t);
                    line.write_unbreakable(state, out, "\")");
                },
            }
        },
        Node::ImageReference(x) => {
            line.write_unbreakable(state, out, &format!("![][{}]", x.identifier));
        },
        Node::Link(x) => {
            let simple_text = if x.children.len() != 1 {
                None
            } else {
                x.children.get(0)
            }.and_then(|c| match c {
                Node::Text(t) => if get_splits(&t.value).next().is_some() {
                    None
                } else {
                    Some(t.value.clone())
                },
                Node::InlineCode(t) => if get_splits(&t.value).next().is_some() {
                    None
                } else {
                    Some(format!("`{}`", t.value))
                },
                _ => None,
            });
            match (simple_text, &x.title) {
                (Some(unbroken_content), None) => {
                    if unbroken_content.as_str() == x.url.as_str() {
                        line.write_unbreakable(state, out, &format!("<{}>", x.url));
                    } else {
                        line.write_unbreakable(state, out, &format!("[{}]({})", unbroken_content, x.url));
                    }
                },
                (Some(c), Some(title)) => {
                    line.write_unbreakable(state, out, &format!("[{}]({}", c, x.url));
                    line.write_unbreakable(state, out, " \"");
                    line.write_breakable(state, out, title);
                    line.write_unbreakable(state, out, "\")");
                },
                (None, None) => {
                    line.write_unbreakable(state, out, "[");
                    for child in &x.children {
                        recurse_write(state, out, line.clone_inline(), child, true);
                    }
                    line.write_unbreakable(state, out, &format!("]({})", x.url));
                },
                (None, Some(title)) => {
                    line.write_unbreakable(state, out, "[");
                    for child in &x.children {
                        recurse_write(state, out, line.clone_inline(), child, true);
                    }
                    line.write_unbreakable(state, out, &format!("]({}", x.url));
                    line.write_unbreakable(state, out, " \"");
                    line.write_breakable(state, out, title);
                    line.write_unbreakable(state, out, "\")");
                },
            }
        },
        Node::LinkReference(x) => {
            let simple_text = if x.children.len() != 1 {
                None
            } else {
                x.children.get(0)
            }.and_then(|c| match c {
                Node::Text(t) => if get_splits(&t.value).next().is_some() {
                    None
                } else {
                    Some(t.value.clone())
                },
                Node::InlineCode(t) => if get_splits(&t.value).next().is_some() {
                    None
                } else {
                    Some(format!("`{}`", t.value))
                },
                _ => {
                    None
                },
            });
            match simple_text {
                Some(t) if t == x.identifier => {
                    line.write_unbreakable(state, out, &format!("[{}]", t));
                },
                _ => {
                    line.write_unbreakable(state, out, "[");
                    for child in &x.children {
                        recurse_write(state, out, line.clone_inline(), child, true);
                    }
                    line.write_unbreakable(state, out, &format!("][{}]", x.identifier));
                },
            }
        },
        Node::Break(_) => { 
            // normalized out
            },
        Node::Math(_) => unreachable!(),
        Node::Table(_) => unreachable!(),
        Node::TableRow(_) => unreachable!(),
        Node::TableCell(_) => unreachable!(),
        Node::MdxJsxTextElement(_) => unreachable!(),
        Node::MdxFlowExpression(_) => unreachable!(),
        Node::MdxJsxFlowElement(_) => unreachable!(),
        Node::MdxjsEsm(_) => unreachable!(),
        Node::Toml(_) => unreachable!(),
        Node::Yaml(_) => unreachable!(),
        Node::InlineMath(_) => unreachable!(),
        Node::MdxTextExpression(_) => unreachable!(),
    }
}

pub(crate) fn format_md(
    true_out: &mut String,
    max_width: usize,
    rel_max_width: Option<usize>,
    prefix: &str,
    source: &str,
) -> Result<()> {
    // TODO, due to a bug a bunch of unreachable branches might have had code added.  I'd
    // like to go back and see if some block-level starts can be removed in contexts they
    // shouldn't appear.
    match es!({
        let mut out = String::new();
        let mut state = State {
            line_buffer: String::new(),
            need_nl: false,
        };
        let ast = markdown::to_mdast(source, &markdown::ParseOptions {
            constructs: markdown::Constructs { ..Default::default() },
            ..Default::default()
        }).map_err(|e| anyhow!("{}", e))?;
        recurse_write(
            &mut state,
            &mut out,
            LineState::new(None, prefix.to_string(), max_width, rel_max_width, false),
            &ast,
            false,
        );
        Ok(out)
    }) {
        Ok(o) => {
            true_out.push_str(&o);
            Ok(())
        },
        Err(e) => {
            Err(e)
        },
    }
}

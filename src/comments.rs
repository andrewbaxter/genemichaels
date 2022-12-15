use anyhow::{
    anyhow,
    Result,
};
use crate::{
    Comment,
    CommentMode,
};
use proc_macro2::{
    LineColumn,
    TokenStream,
    Group,
};
use pulldown_cmark::Event;
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
            offset += match (&source[offset..]).find('\n') {
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
                (&self.source[line_start_offset..]).chars().take(loc.column).map(char::len_utf8).sum::<usize>()
        }

        fn add_comments(&mut self, end: LineColumn, whole_text: &str) {
            let start_re =
                &self
                    .start_re
                    .get_or_insert_with(|| UnicodeRegex::new(r#"(?:(//)(/|!)?)|(?:(/\*)(\*|!)?)"#).unwrap());
            let block_event_re =
                &self.block_event_re.get_or_insert_with(|| UnicodeRegex::new(r#"((?:/\*)|(?:\*/))"#).unwrap());

            struct CommentBuffer {
                out: Vec<Comment>,
                mode: CommentMode,
                lines: Vec<String>,
            }

            impl CommentBuffer {
                fn flush(&mut self) {
                    if self.lines.is_empty() {
                        return;
                    }
                    self.out.push(Comment{
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

            let mut buffer = CommentBuffer{
                out: vec![],
                mode: CommentMode::Normal,
                lines: vec![],
            };
            let mut text = whole_text;
            'comment_loop : loop {
                match start_re.captures(text) {
                    Some(found_start) => {
                        let start_prefix_match = found_start.get(1).or(found_start.get(3)).unwrap();
                        match start_prefix_match.as_str() {
                            "//" => {
                                let mode = {
                                    let start_suffix_match = found_start.get(2);
                                    let (mode, match_end) = match start_suffix_match {
                                        Some(start_suffix_match) => (match start_suffix_match.as_str() {
                                            "/" => CommentMode::DocOuter,
                                            "!" => CommentMode::DocInner,
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
                                assert_ne!(next_start, 0);
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
                                    search_end_at = search_end_at + found_event.end();
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
                                assert_ne!(next_start, 0);
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
                    let eol = match (&self.source[start..]).find('\n') {
                        Some(n) => start + n,
                        None => self.source.len(),
                    };
                    let text = &self.source[start .. eol];
                    if text.trim_start().starts_with("//") {
                        self.add_comments(previous_start.clone(), text);
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
    let mut state = State{
        source: source,
        line_lookup: line_lookup,
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

    let tokens = recurse(&mut state, TokenStream::from_str(source).map_err(|e| anyhow!("{:?}", e))?);
    Ok((state.comments, tokens))
}

struct State {
    stack: Vec<Box<dyn StackEl>>,
    line_buffer: String,
    need_nl: bool,
}

enum StackRes {
    Push(Box<dyn StackEl>),
    Keep,
    Pop,
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
    fn flush_always(&mut self, state: &mut State, out: &mut String) {
        out.push_str(&format!("{}{}{}{}", if state.need_nl {
            "\n"
        } else {
            ""
        }, match &self.first_prefix.take() {
            Some(t) => t,
            None => &*self.prefix,
        }, &state.line_buffer, if self.explicit_wrap {
            " \\"
        } else {
            ""
        }).trim_end());
        state.line_buffer.clear();
        state.need_nl = true;
    }

    fn flush(&mut self, state: &mut State, out: &mut String) {
        if !state.line_buffer.trim().is_empty() {
            self.flush_always(state, out);
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

struct LineState(Rc<RefCell<LineState_>>);

impl LineState {
    fn new(
        first_prefix: Option<String>,
        prefix: String,
        max_width: usize,
        rel_max_width: Option<usize>,
        explicit_wrap: bool,
    ) -> LineState {
        LineState(Rc::new(RefCell::new(LineState_{
            first_prefix: first_prefix,
            prefix: prefix,
            explicit_wrap: explicit_wrap,
            max_width: max_width,
            rel_max_width: rel_max_width,
        })))
    }

    fn share(&self) -> LineState {
        LineState(self.0.clone())
    }

    fn zero_indent(&self) -> LineState {
        let mut s = self.0.as_ref().borrow_mut();
        LineState(Rc::new(RefCell::new(LineState_{
            first_prefix: s.first_prefix.take(),
            prefix: s.prefix.clone(),
            explicit_wrap: s.explicit_wrap,
            max_width: s.max_width,
            rel_max_width: s.rel_max_width,
        })))
    }

    fn indent(&self, first_prefix: Option<String>, prefix: String, explicit_wrap: bool) -> LineState {
        let mut s = self.0.as_ref().borrow_mut();
        LineState(Rc::new(RefCell::new(LineState_{
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

        // let segmenter = LineBreakSegmenter::try_new_unstable(&icu_testdata::unstable()).unwrap();
        let mut text = text;
        if state.line_buffer.is_empty() {
            text = text.trim_start();
        }
        while !text.is_empty() {
            if state.line_buffer.len() + text.len() > max_width {
                // match segmenter .segment_str(&text)
                match text
                    .char_indices()
                    .filter(|i| i.1 == ' ')
                    .map(|i| i.0 + 1)
                    .take_while(|b| state.line_buffer.len() + *b < max_width)
                    .last() {
                    Some(b) => {
                        // Doesn't fit, but can split to get within line
                        state.line_buffer.push_str(&text[..b].trim_end());
                        s.flush(state, out);
                        text = (&text[b..]).trim_start();
                    },
                    None => {
                        if !state.line_buffer.is_empty() {
                            // Doesn't fit, can't split, but stuff in buffer - flush that first
                            s.flush(state, out);
                        } else {
                            // Doesn't fit, can't split, but buffer empty - just write it
                            state.line_buffer.push_str(text.trim_end());
                            s.flush(state, out);
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
        self.0.as_ref().borrow_mut().flush_always(state, out);
    }

    fn flush(&self, state: &mut State, out: &mut String) {
        self.0.as_ref().borrow_mut().flush(state, out);
    }

    fn write_unbreakable(&self, state: &mut State, out: &mut String, text: &str) {
        let mut s = self.0.as_ref().borrow_mut();
        let max_width = s.calc_max_width();
        if state.line_buffer.len() + text.len() > max_width {
            s.flush(state, out);
        }
        state.line_buffer.push_str(text);
    }

    fn write_whitespace(&self, state: &mut State, out: &mut String, text: &str) {
        let mut s = self.0.as_ref().borrow_mut();
        let max_width = s.max_width - if s.explicit_wrap {
            2
        } else {
            0
        };
        if state.line_buffer.len() + text.len() >= max_width {
            s.flush(state, out);
        } else {
            state.line_buffer.push_str(text);
        }
    }

    fn write_newline(&self, state: &mut State, out: &mut String) {
        let mut s = self.0.as_ref().borrow_mut();
        if !state.line_buffer.is_empty() {
            panic!();
        }
        s.flush_always(state, out);
    }
}

fn write_image(state: &mut State, out: &mut String, line: &LineState, url: &str, title: &str) {
    line.write_unbreakable(state, out, &format!("![]({}", url));
    if title.is_empty() {
        line.write_unbreakable(state, out, ")");
    } else {
        line.write_unbreakable(state, out, " \"");
        line.write_breakable(state, out, &title);
        line.write_unbreakable(state, out, "\")");
    }
}

fn push_emphasis(state: &mut State, out: &mut String, line: LineState, line_root: bool) -> StackRes {
    StackRes::Push(StackInline::new(state, out, StackInlineArgs{
        start_bound: Some("_".into()),
        end_bound: Some("_".into()),
        line_state: line,
        line_root: line_root,
    }))
}

fn push_strong(state: &mut State, out: &mut String, line: LineState, line_root: bool) -> StackRes {
    StackRes::Push(StackInline::new(state, out, StackInlineArgs{
        start_bound: Some("**".into()),
        end_bound: Some("**".into()),
        line_state: line,
        line_root: line_root,
    }))
}

fn push_strikethrough(state: &mut State, out: &mut String, line: LineState, line_root: bool) -> StackRes {
    StackRes::Push(StackInline::new(state, out, StackInlineArgs{
        start_bound: Some("~~".into()),
        end_bound: Some("~~".into()),
        line_state: line,
        line_root: line_root,
    }))
}

fn push_link(
    state: &mut State,
    out: &mut String,
    line: LineState,
    line_root: bool,
    url: &str,
    _title: &str,
) -> StackRes {
    StackRes::Push(StackInline::new(state, out, StackInlineArgs{
        start_bound: Some("[".into()),
        end_bound: Some(format!("]({})", url)),
        line_state: line,
        line_root: line_root,
    }))
}

trait StackEl {
    fn handle(&mut self, state: &mut State, out: &mut String, e: Event) -> StackRes;
}

struct StackInline {
    end_bound: Option<String>,
    line: LineState,
    line_root: bool,
}

struct StackInlineArgs {
    start_bound: Option<String>,
    end_bound: Option<String>,
    line_state: LineState,
    line_root: bool,
}

impl StackInline {
    fn new(state: &mut State, out: &mut String, args: StackInlineArgs) -> Box<dyn StackEl> {
        if let Some(b) = args.start_bound {
            args.line_state.write_unbreakable(state, out, &b);
        }
        Box::new(StackInline{
            end_bound: args.end_bound,
            line: args.line_state,
            line_root: args.line_root,
        })
    }
}

impl StackEl for StackInline {
    fn handle(&mut self, state: &mut State, out: &mut String, e: Event) -> StackRes {
        match e {
            Event::End(_) => {
                if let Some(b) = &self.end_bound {
                    self.line.write_unbreakable(state, out, &b);
                }
                if self.line_root {
                    self.line.flush(state, out);
                }
                StackRes::Pop
            },
            Event::Text(x) => {
                self.line.write_breakable(state, out, &x);
                StackRes::Keep
            },
            Event::Code(x) => {
                self.line.write_unbreakable(state, out, &format!("`{}`", x));
                StackRes::Keep
            },
            Event::Start(x) => match x {
                pulldown_cmark::Tag::Emphasis => push_emphasis(state, out, self.line.share(), false),
                pulldown_cmark::Tag::Strong => push_strong(state, out, self.line.share(), false),
                pulldown_cmark::Tag::Strikethrough => push_strikethrough(state, out, self.line.share(), false),
                pulldown_cmark::Tag::Link(_, url, title) => {
                    push_link(state, out, self.line.share(), false, &url, &title)
                },
                pulldown_cmark::Tag::Image(_, url, title) => {
                    write_image(state, out, &self.line, &url, &title);
                    StackRes::Keep
                },
                pulldown_cmark::Tag::Paragraph => unreachable!(),
                pulldown_cmark::Tag::Heading(_, _, _) => unreachable!(),
                pulldown_cmark::Tag::BlockQuote => unreachable!(),
                pulldown_cmark::Tag::CodeBlock(_) => unreachable!(),
                pulldown_cmark::Tag::List(_) => unreachable!(),
                pulldown_cmark::Tag::Item => unreachable!(),
                pulldown_cmark::Tag::FootnoteDefinition(_) => unreachable!(),
                pulldown_cmark::Tag::Table(_) => unreachable!(),
                pulldown_cmark::Tag::TableHead => unreachable!(),
                pulldown_cmark::Tag::TableRow => unreachable!(),
                pulldown_cmark::Tag::TableCell => unreachable!(),
            },
            Event::SoftBreak => {
                self.line.write_whitespace(state, out, " ");
                StackRes::Keep
            },
            Event::HardBreak => {
                self.line.write_whitespace(state, out, " ");
                StackRes::Keep
            },
            Event::Html(x) => {
                self.line.write_unbreakable(state, out, &x);
                StackRes::Keep
            },
            Event::FootnoteReference(_) => unreachable!(),
            Event::Rule => unreachable!(),
            Event::TaskListMarker(_) => unreachable!(),
        }
    }
}

struct StackBlock {
    line: LineState,
    first: bool,
    was_inline: bool,
    end_bound: Option<&'static str>,
}

struct StackBlockArgs {
    line: LineState,
    start_bound: Option<String>,
    end_bound: Option<&'static str>,
}

impl StackBlock {
    fn new(state: &mut State, out: &mut String, args: StackBlockArgs) -> Box<dyn StackEl> {
        if let Some(b) = args.start_bound {
            args.line.write_unbreakable(state, out, &b);
        }
        Box::new(StackBlock{
            line: args.line,
            end_bound: args.end_bound,
            first: true,
            was_inline: false,
        })
    }

    fn block_ev(&mut self, state: &mut State, out: &mut String) {
        if self.was_inline {
            self.line.flush(state, out);
            self.was_inline = false;
        }
        if !self.first {
            self.line.write_newline(state, out);
        }
        self.first = false;
    }

    fn inline_ev(&mut self) {
        self.was_inline = true;
        self.first = false;
    }
}

impl StackEl for StackBlock {
    fn handle(&mut self, state: &mut State, out: &mut String, e: Event) -> StackRes {
        match e {
            Event::Start(x) => match x {
                pulldown_cmark::Tag::Paragraph => {
                    self.block_ev(state, out);
                    StackRes::Push(StackInline::new(state, out, StackInlineArgs{
                        start_bound: None,
                        end_bound: None,
                        line_state: self.line.indent(None, "".into(), false),
                        line_root: true,
                    }))
                },
                pulldown_cmark::Tag::Heading(level, _, _) => {
                    self.block_ev(state, out);
                    StackRes::Push(StackInline::new(state, out, StackInlineArgs{
                        start_bound: None,
                        end_bound: None,
                        line_state: self
                            .line
                            .indent(Some(format!("{} ", "#".repeat(level as i32 as usize))), "  ".into(), true),
                        line_root: true,
                    }))
                },
                pulldown_cmark::Tag::BlockQuote => {
                    self.block_ev(state, out);
                    StackRes::Push(StackBlock::new(state, out, StackBlockArgs{
                        line: self.line.indent(None, "> ".into(), false),
                        start_bound: None,
                        end_bound: None,
                    }))
                },
                pulldown_cmark::Tag::CodeBlock(lang) => {
                    self.block_ev(state, out);
                    self.line.write_unbreakable(state, out, &format!("```{}", match &lang {
                        pulldown_cmark::CodeBlockKind::Indented => "",
                        pulldown_cmark::CodeBlockKind::Fenced(x) => x,
                    }));
                    self.line.flush(state, out);
                    StackRes::Push(Box::new(StackCodeBlock{ line: self.line.zero_indent() }))
                },
                pulldown_cmark::Tag::List(ordered) => {
                    self.block_ev(state, out);
                    match ordered {
                        Some(index) => StackRes::Push(Box::new(StackNumberList{
                            count: index,
                            line: self.line.zero_indent(),
                            first: true,
                        })),
                        None => StackRes::Push(Box::new(StackBulletList{
                            line: self.line.zero_indent(),
                            first: true,
                        })),
                    }
                },
                pulldown_cmark::Tag::Table(_) => {
                    todo!();
                    // StackRes::Push(StackTable::new(out, self.line.zero_indent()))
                },
                pulldown_cmark::Tag::Link(_, url, title) => {
                    self.was_inline = true;
                    push_link(state, out, self.line.share(), true, &url, &title);
                    StackRes::Keep
                },
                pulldown_cmark::Tag::Image(_, url, title) => {
                    self.inline_ev();
                    write_image(state, out, &self.line, &url, &title);
                    StackRes::Keep
                },
                pulldown_cmark::Tag::Emphasis => {
                    self.inline_ev();
                    push_emphasis(state, out, self.line.share(), true)
                },
                pulldown_cmark::Tag::Strong => {
                    self.inline_ev();
                    push_strong(state, out, self.line.share(), true)
                },
                pulldown_cmark::Tag::Strikethrough => {
                    self.inline_ev();
                    push_strikethrough(state, out, self.line.share(), true)
                },
                pulldown_cmark::Tag::Item => {
                    // What is this... this feels like a bug.
                    push_bullet_item(state, out, &self.line)
                },
                pulldown_cmark::Tag::TableHead => unreachable!(),
                pulldown_cmark::Tag::TableRow => unreachable!(),
                pulldown_cmark::Tag::TableCell => unreachable!(),
                pulldown_cmark::Tag::FootnoteDefinition(_) => unreachable!(),
            },
            Event::End(_) => {
                if self.was_inline {
                    self.line.flush(state, out);
                }
                if let Some(b) = self.end_bound {
                    self.line.write_unbreakable(state, out, b);
                    self.line.flush(state, out);
                }
                StackRes::Pop
            },
            Event::Text(t) => {
                self.inline_ev();
                self.line.write_breakable(state, out, &t);
                StackRes::Keep
            },
            Event::Code(x) => {
                self.inline_ev();
                self.line.write_unbreakable(state, out, &format!("`{}`", x));
                StackRes::Keep
            },
            Event::SoftBreak => {
                StackRes::Keep
            },
            Event::Html(x) => {
                self.inline_ev();
                self.line.write_unbreakable(state, out, &x);
                StackRes::Keep
            },
            Event::FootnoteReference(_) => unreachable!(),
            Event::HardBreak => unreachable!(),
            Event::Rule => unreachable!(),
            Event::TaskListMarker(_) => unreachable!(),
        }
    }
}

struct StackNumberList {
    count: u64,
    line: LineState,
    first: bool,
}

impl StackNumberList {
    fn indent(&mut self, use_count: u64) -> LineState {
        self.line.indent(Some(format!("{}. ", use_count)), "   ".into(), false)
    }
}

impl StackEl for StackNumberList {
    fn handle(&mut self, state: &mut State, out: &mut String, e: Event) -> StackRes {
        let use_count = self.count;
        self.count += 1;
        match e {
            Event::Start(e) => {
                match e {
                    pulldown_cmark::Tag::Item => {
                        if self.first {
                            self.first = false;
                        } else {
                            self.line.write_newline(state, out);
                        }
                        StackRes::Push(StackBlock::new(state, out, StackBlockArgs{
                            line: self.indent(use_count),
                            start_bound: None,
                            end_bound: None,
                        }))
                    },
                    pulldown_cmark::Tag::Link(_, url, title) => {
                        push_link(state, out, self.indent(use_count), true, &url, &title)
                    },
                    pulldown_cmark::Tag::Paragraph => unreachable!(),
                    pulldown_cmark::Tag::Heading(_, _, _) => unreachable!(),
                    pulldown_cmark::Tag::BlockQuote => unreachable!(),
                    pulldown_cmark::Tag::CodeBlock(_) => unreachable!(),
                    pulldown_cmark::Tag::List(_) => unreachable!(),
                    pulldown_cmark::Tag::FootnoteDefinition(_) => unreachable!(),
                    pulldown_cmark::Tag::Table(_) => unreachable!(),
                    pulldown_cmark::Tag::TableHead => unreachable!(),
                    pulldown_cmark::Tag::TableRow => unreachable!(),
                    pulldown_cmark::Tag::TableCell => unreachable!(),
                    pulldown_cmark::Tag::Emphasis => unreachable!(),
                    pulldown_cmark::Tag::Strong => unreachable!(),
                    pulldown_cmark::Tag::Strikethrough => unreachable!(),
                    pulldown_cmark::Tag::Image(_, _, _) => unreachable!(),
                }
            },
            Event::Text(t) => {
                if self.first {
                    self.first = false;
                } else {
                    self.line.write_newline(state, out);
                }
                let line = self.indent(use_count);
                line.write_breakable(state, out, &t);
                line.flush(state, out);
                StackRes::Keep
            },
            Event::Code(x) => {
                if self.first {
                    self.first = false;
                } else {
                    self.line.write_newline(state, out);
                }
                let line = self.indent(use_count);
                line.write_unbreakable(state, out, &format!("`{}`", x));
                line.flush(state, out);
                StackRes::Keep
            },
            Event::End(_) => StackRes::Pop,
            _ => unreachable!(),
        }
    }
}

struct StackBulletList {
    line: LineState,
    first: bool,
}

impl StackBulletList {
    fn indent(&mut self) -> LineState {
        bullet_indent(&self.line)
    }
}

fn bullet_indent(line: &LineState) -> LineState {
    line.indent(Some("* ".into()), "   ".into(), false)
}

fn push_bullet_item(state: &mut State, out: &mut String, line: &LineState) -> StackRes {
    StackRes::Push(StackBlock::new(state, out, StackBlockArgs{
        line: bullet_indent(line),
        start_bound: None,
        end_bound: None,
    }))
}

impl StackEl for StackBulletList {
    fn handle(&mut self, state: &mut State, out: &mut String, e: Event) -> StackRes {
        match e {
            Event::Start(e) => match e {
                pulldown_cmark::Tag::Item => {
                    if self.first {
                        self.first = false;
                    } else {
                        self.line.write_newline(state, out);
                    }
                    push_bullet_item(state, out, &self.line)
                },
                pulldown_cmark::Tag::Link(_, url, title) => {
                    push_link(state, out, self.indent(), true, &url, &title)
                },
                pulldown_cmark::Tag::Paragraph => unreachable!(),
                pulldown_cmark::Tag::Heading(_, _, _) => unreachable!(),
                pulldown_cmark::Tag::BlockQuote => unreachable!(),
                pulldown_cmark::Tag::CodeBlock(_) => unreachable!(),
                pulldown_cmark::Tag::List(_) => unreachable!(),
                pulldown_cmark::Tag::FootnoteDefinition(_) => unreachable!(),
                pulldown_cmark::Tag::Table(_) => unreachable!(),
                pulldown_cmark::Tag::TableHead => unreachable!(),
                pulldown_cmark::Tag::TableRow => unreachable!(),
                pulldown_cmark::Tag::TableCell => unreachable!(),
                pulldown_cmark::Tag::Emphasis => unreachable!(),
                pulldown_cmark::Tag::Strong => unreachable!(),
                pulldown_cmark::Tag::Strikethrough => unreachable!(),
                pulldown_cmark::Tag::Image(_, _, _) => unreachable!(),
            },
            Event::End(_) => StackRes::Pop,
            Event::Text(t) => {
                if self.first {
                    self.first = false;
                } else {
                    self.line.write_newline(state, out);
                }
                let line = self.indent();
                line.write_breakable(state, out, &t);
                line.flush(state, out);
                StackRes::Keep
            },
            Event::Code(x) => {
                if self.first {
                    self.first = false;
                } else {
                    self.line.write_newline(state, out);
                }
                let line = self.indent();
                line.write_unbreakable(state, out, &format!("`{}`", x));
                line.flush(state, out);
                StackRes::Keep
            },
            x => unreachable!("non-item in list: {:?}", x),
        }
    }
}

struct StackCodeBlock {
    line: LineState,
}

impl StackEl for StackCodeBlock {
    fn handle(&mut self, state: &mut State, out: &mut String, e: Event) -> StackRes {
        match e {
            Event::Text(t) => {
                for l in t.lines() {
                    self.line.write_unbreakable(state, out, l);
                    self.line.flush_always(state, out);
                }
                StackRes::Keep
            },
            Event::End(_) => {
                self.line.write_unbreakable(state, out, "```");
                self.line.flush(state, out);
                StackRes::Pop
            },
            Event::Start(_) |
            Event::Code(_) |
            Event::Html(_) |
            Event::FootnoteReference(_) |
            Event::SoftBreak |
            Event::HardBreak |
            Event::Rule |
            Event::TaskListMarker(_) => unreachable!(

            ),
        }
    }
}

pub(crate) fn format_md(
    out: &mut String,
    max_width: usize,
    rel_max_width: Option<usize>,
    prefix: &str,
    source: &str,
) {
    let mut state = State{
        stack: vec![],
        line_buffer: String::new(),
        need_nl: false,
    };
    let start_state = StackBlock::new(&mut state, out, StackBlockArgs{
        line: LineState::new(None, prefix.to_string(), max_width, rel_max_width, false),
        start_bound: None,
        end_bound: None,
    });
    state.stack.push(start_state);
    for e in pulldown_cmark::Parser::new(source) {
        let mut top = state.stack.pop().expect("markdown parsing blew up on comments");
        match top.handle(&mut state, out, e) {
            StackRes::Push(e) => {
                state.stack.push(top);
                state.stack.push(e);
            },
            StackRes::Keep => {
                state.stack.push(top);
            },
            StackRes::Pop => { },
        }
    }
}

use proc_macro2::LineColumn;
use pulldown_cmark::Event;
use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;
use structre::UnicodeRegex;

use crate::{Comment, CommentMode};

#[derive(PartialEq, Eq, Debug)]
pub(crate) struct HashLineColumn(pub LineColumn);

impl Hash for HashLineColumn {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.0.line, self.0.column).hash(state);
    }
}

pub(crate) struct CommentExtractor<'a> {
    pub(crate) source: &'a str,
    pub(crate) last_loc: LineColumn,
    pub(crate) last_offset: usize,
    pub(crate) comments: HashMap<HashLineColumn, Vec<Comment>>,
}

impl<'a> CommentExtractor<'a> {
    pub(crate) fn advance_to(&mut self, loc: LineColumn) -> &str {
        if loc.line < self.last_loc.line || (loc.line  == self.last_loc.line && loc.column < self.last_loc.column) {
            return "";
        }
        let mut offset = self.last_offset;
        let mut col = self.last_loc.column;
        for _ in self.last_loc.line..loc.line {
            offset += self.source[offset..].find('\n').unwrap() + 1;
            col = 0;
        }
        offset += self.source[offset..]
            .chars()
            .take(loc.column - col)
            .map(char::len_utf8)
            .sum::<usize>();
        let out = &self.source[self.last_offset..offset];
        self.last_offset = offset;
        self.last_loc = loc;
        out
    }

    pub(crate) fn extract_to(&mut self, loc: LineColumn) {
        let whole_text = self.advance_to(loc);

        let start_re = UnicodeRegex::new(r#"(?:(//)(/|!)?)|(?:(/\*)(\*|!)?)"#).unwrap();
        let block_event_re = UnicodeRegex::new(r#"((?:/\*)|(?:\*/))"#).unwrap();
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
                self.out.push(Comment {
                    mode: self.mode,
                    lines: self.lines.split_off(0).join("\n"),
                });
            }

            fn add(&mut self, mode: CommentMode, line: &str) {
                if self.mode != mode && !self.lines.is_empty() {
                    self.flush();
                }
                self.mode = mode;
                self.lines.push(line.trim().to_string());
            }
        }

        let mut buffer = CommentBuffer {
            out: vec![],
            mode: CommentMode::Normal,
            lines: vec![],
        };

        let mut text = whole_text;
        'comment_loop: loop {
            match start_re.captures(text) {
                Some(found_start) => {
                    let start_prefix_match = found_start.get(1).or(found_start.get(3)).unwrap();
                    match start_prefix_match.as_str() {
                        "//" => {
                            let mode = {
                                let start_suffix_match = found_start.get(2);
                                let (mode, match_end) = match start_suffix_match {
                                    Some(start_suffix_match) => (
                                        match start_suffix_match.as_str() {
                                            "/" => CommentMode::DocOuter,
                                            "!" => CommentMode::DocInner,
                                            _ => unreachable!(),
                                        },
                                        start_suffix_match.end(),
                                    ),
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
                        }
                        "/*" => {
                            let mode = {
                                let start_suffix_match = found_start.get(2);
                                let (mode, match_end) = match start_suffix_match {
                                    Some(start_suffix_match) => (
                                        match start_suffix_match.as_str() {
                                            "*" => CommentMode::DocOuter,
                                            "!" => CommentMode::DocInner,
                                            _ => unreachable!(),
                                        },
                                        start_suffix_match.end(),
                                    ),
                                    None => (CommentMode::Normal, start_prefix_match.end()),
                                };
                                text = &text[match_end..];
                                mode
                            };

                            let mut nesting = 1;
                            let mut search_end_at = 0usize;
                            let (lines, next_start) = loop {
                                let found_event = block_event_re
                                    .captures(&text[search_end_at..])
                                    .unwrap()
                                    .get(1)
                                    .unwrap();
                                let event_start = search_end_at + found_event.start();
                                search_end_at = search_end_at + found_event.end();
                                match found_event.as_str() {
                                    "/*" => {
                                        nesting += 1;
                                    }
                                    "*/" => {
                                        nesting -= 1;
                                        if nesting == 0 {
                                            break (&text[..event_start], search_end_at);
                                        }
                                    }
                                    _ => unreachable!(),
                                }
                            };

                            for line in lines.lines() {
                                buffer.add(mode, line.trim());
                            }

                            assert_ne!(next_start, 0);
                            text = &text[next_start..];
                        }
                        _ => unreachable!(),
                    }
                }
                None => {
                    break 'comment_loop;
                }
            }
        }

        buffer.flush();

        if !buffer.out.is_empty() {
            self.comments.insert(HashLineColumn(loc), buffer.out);
        }
    }
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

struct LineState_ {
    first_prefix: Option<String>,
    prefix: String,
    explicit_wrap: bool,
    max_width: usize,
}

impl LineState_ {
    fn flush(&mut self, state: &mut State, out: &mut String) {
        if !state.line_buffer.is_empty() {
            out.push_str(&format!(
                "{}{}{}{}",
                if state.need_nl { "\n" } else { "" },
                match &self.first_prefix.take() {
                    Some(t) => t,
                    None => &*self.prefix,
                },
                &state.line_buffer,
                if self.explicit_wrap { " \\" } else { "" },
            ));
            state.line_buffer.clear();
            state.need_nl = true;
        }
    }
}

struct LineState(Rc<RefCell<LineState_>>);

impl LineState {
    fn new(
        first_prefix: Option<String>,
        prefix: String,
        max_width: usize,
        explicit_wrap: bool,
    ) -> LineState {
        LineState(Rc::new(RefCell::new(LineState_ {
            first_prefix: first_prefix,
            prefix: prefix,
            explicit_wrap: explicit_wrap,
            max_width: max_width,
        })))
    }

    fn share(&self) -> LineState {
        LineState(self.0.clone())
    }

    fn zero_indent(&self) -> LineState {
        let mut s = self.0.as_ref().borrow_mut();
        LineState(Rc::new(RefCell::new(LineState_ {
            first_prefix: s.first_prefix.take(),
            prefix: s.prefix.clone(),
            explicit_wrap: s.explicit_wrap,
            max_width: s.max_width,
        })))
    }

    fn indent(
        &self,
        first_prefix: Option<String>,
        prefix: String,
        explicit_wrap: bool,
    ) -> LineState {
        let mut s = self.0.as_ref().borrow_mut();
        LineState(Rc::new(RefCell::new(LineState_ {
            first_prefix: match (s.first_prefix.take(), first_prefix) {
                (None, None) => None,
                (None, Some(p)) => Some(p),
                (Some(p), None) => Some(p.clone()),
                (Some(p1), Some(p2)) => Some(format!("{}{}", p1, p2)),
            },
            prefix: format!("{}{}", &s.prefix, prefix),
            explicit_wrap: s.explicit_wrap || explicit_wrap,
            max_width: s.max_width,
        })))
    }

    fn write_breakable(&self, state: &mut State, out: &mut String, text: &str) {
        let mut s = self.0.as_ref().borrow_mut();
        let max_width = s.max_width - if s.explicit_wrap { 2 } else { 0 };
        //let segmenter = LineBreakSegmenter::try_new_unstable(&icu_testdata::unstable()).unwrap();
        let mut text = text;
        while !text.is_empty() {
            if state.line_buffer.len() + text.len() > max_width {
                //match segmenter
                //   .segment_str(&text)
                match text
                    .char_indices()
                    .filter(|i| i.1 == ' ')
                    .map(|i| i.0 + 1)
                    .take_while(|b| state.line_buffer.len() + *b < max_width)
                    .last()
                {
                    Some(b) => {
                        state.line_buffer.push_str(&text[..b]);
                        s.flush(state, out);
                        text = &text[b..];
                    }
                    None => {
                        if !state.line_buffer.is_empty() {
                            s.flush(state, out);
                        } else {
                            state.line_buffer.push_str(text);
                            s.flush(state, out);
                            text = &text[text.len()..];
                        }
                    }
                }
            } else {
                state.line_buffer.push_str(text);
                text = &text[text.len()..];
            }
        }
    }

    fn flush(&self, state: &mut State, out: &mut String) {
        self.0.as_ref().borrow_mut().flush(state, out);
    }

    fn write_unbreakable(&self, state: &mut State, out: &mut String, text: &str) {
        let mut s = self.0.as_ref().borrow_mut();
        let max_width = s.max_width - if s.explicit_wrap { 2 } else { 0 };
        if state.line_buffer.len() + text.len() > max_width {
            s.flush(state, out);
        }
        state.line_buffer.push_str(text);
    }

    fn write_whitespace(&self, state: &mut State, out: &mut String, text: &str) {
        let mut s = self.0.as_ref().borrow_mut();
        let max_width = s.max_width - if s.explicit_wrap { 2 } else { 0 };
        if state.line_buffer.len() + text.len() >= max_width {
            s.flush(state, out);
        } else {
            state.line_buffer.push_str(text);
        }
    }
}

trait StackEl {
    fn handle(&mut self, state: &mut State, out: &mut String, e: Event) -> StackRes;
}

struct StackInline {
    bound: Option<&'static str>,
    line_state: LineState,
    line_root: bool,
}

struct StackInlineArgs {
    bound: Option<&'static str>,
    line_state: LineState,
    line_root: bool,
}

impl StackInline {
    fn new(state: &mut State, out: &mut String, args: StackInlineArgs) -> Box<dyn StackEl> {
        if let Some(b) = args.bound {
            args.line_state.write_unbreakable(state, out, b);
        }
        Box::new(StackInline {
            bound: args.bound,
            line_state: args.line_state,
            line_root: args.line_root,
        })
    }
}

impl StackEl for StackInline {
    fn handle(&mut self, state: &mut State, out: &mut String, e: Event) -> StackRes {
        match e {
            Event::End(_) => {
                if let Some(b) = self.bound {
                    self.line_state.write_unbreakable(state, out, &b);
                }
                if self.line_root {
                    self.line_state.flush(state, out);
                }
                StackRes::Pop
            }
            Event::Text(x) => {
                self.line_state.write_breakable(state, out, &x);
                StackRes::Keep
            }
            Event::Code(x) => {
                self.line_state
                    .write_unbreakable(state, out, &format!("`{}`", x));
                StackRes::Keep
            }
            Event::Start(x) => match x {
                pulldown_cmark::Tag::Emphasis => StackRes::Push(StackInline::new(
                    state,
                    out,
                    StackInlineArgs {
                        bound: Some("_".into()),
                        line_state: self.line_state.share(),
                        line_root: true,
                    },
                )),
                pulldown_cmark::Tag::Strong => StackRes::Push(StackInline::new(
                    state,
                    out,
                    StackInlineArgs {
                        bound: Some("**".into()),
                        line_state: self.line_state.share(),
                        line_root: true,
                    },
                )),
                pulldown_cmark::Tag::Strikethrough => StackRes::Push(StackInline::new(
                    state,
                    out,
                    StackInlineArgs {
                        bound: Some("~".into()),
                        line_state: self.line_state.share(),
                        line_root: true,
                    },
                )),
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
                pulldown_cmark::Tag::Link(_, _, _) => unreachable!(),
                pulldown_cmark::Tag::Image(_, _, _) => unreachable!(),
            },
            Event::Html(_) => unreachable!(),
            Event::FootnoteReference(_) => unreachable!(),
            Event::SoftBreak => {
                self.line_state.write_whitespace(state, out, " ");
                StackRes::Keep
            }
            Event::HardBreak => StackRes::Keep,
            Event::Rule => unreachable!(),
            Event::TaskListMarker(_) => unreachable!(),
        }
    }
}

struct StackBlock {
    line: LineState,
    need_line: bool,
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
        Box::new(StackBlock {
            line: args.line,
            end_bound: args.end_bound,
            need_line: false,
        })
    }
}

impl StackEl for StackBlock {
    fn handle(&mut self, state: &mut State, out: &mut String, e: Event) -> StackRes {
        if self.need_line {
            out.push_str("\n");
        }
        let out = match e {
            Event::Start(x) => match x {
                pulldown_cmark::Tag::Paragraph => StackRes::Push(StackInline::new(
                    state,
                    out,
                    StackInlineArgs {
                        bound: None,
                        line_state: self.line.indent(None, "".into(), false),
                        line_root: true,
                    },
                )),
                pulldown_cmark::Tag::Heading(level, _, _) => StackRes::Push(StackInline::new(
                    state,
                    out,
                    StackInlineArgs {
                        bound: None,
                        line_state: self.line.indent(
                            Some("#".repeat(level as i32 as usize).into()),
                            "  ".into(),
                            true,
                        ),
                        line_root: true,
                    },
                )),
                pulldown_cmark::Tag::BlockQuote => StackRes::Push(StackBlock::new(
                    state,
                    out,
                    StackBlockArgs {
                        line: self.line.indent(None, ">".into(), false),
                        start_bound: None,
                        end_bound: None,
                    },
                )),
                pulldown_cmark::Tag::CodeBlock(lang) => {
                    self.line.write_unbreakable(
                        state,
                        out,
                        &format!(
                            "```{}",
                            match &lang {
                                pulldown_cmark::CodeBlockKind::Indented => "",
                                pulldown_cmark::CodeBlockKind::Fenced(x) => x,
                            }
                        ),
                    );
                    StackRes::Push(Box::new(StackCodeBlock {
                        line: self.line.zero_indent(),
                    }))
                }
                pulldown_cmark::Tag::List(ordered) => match ordered {
                    Some(index) => StackRes::Push(Box::new(StackNumberList {
                        count: index,
                        line: self.line.zero_indent(),
                    })),
                    None => StackRes::Push(Box::new(StackBulletList {
                        line: self.line.zero_indent(),
                    })),
                },
                pulldown_cmark::Tag::Table(_) => {
                    todo!();
                    //StackRes::Push(StackTable::new(out, self.line.zero_indent()))
                }
                pulldown_cmark::Tag::Link(_, url, title) => {
                    self.line.write_unbreakable(state, out, "[");
                    self.line.write_breakable(state, out, &title);
                    self.line
                        .write_unbreakable(state, out, &format!("]({})", url));
                    self.line.flush(state, out);
                    StackRes::Keep
                }
                pulldown_cmark::Tag::Image(_, url, title) => {
                    self.line
                        .write_unbreakable(state, out, &format!("![]({}", url));
                    if title.is_empty() {
                        self.line.write_unbreakable(state, out, ")");
                    } else {
                        self.line.write_unbreakable(state, out, " \"");
                        self.line.write_breakable(state, out, &title);
                        self.line.write_unbreakable(state, out, "\")");
                    }
                    self.line.flush(state, out);
                    StackRes::Keep
                }
                pulldown_cmark::Tag::Item
                | pulldown_cmark::Tag::TableHead
                | pulldown_cmark::Tag::TableRow
                | pulldown_cmark::Tag::TableCell
                | pulldown_cmark::Tag::Emphasis
                | pulldown_cmark::Tag::Strong
                | pulldown_cmark::Tag::Strikethrough
                | pulldown_cmark::Tag::FootnoteDefinition(_) => unreachable!(),
            },
            Event::End(_) => {
                if let Some(b) = self.end_bound {
                    self.line.write_unbreakable(state, out, b);
                    self.line.flush(state, out);
                }
                StackRes::Pop
            }
            Event::Text(_)
            | Event::Code(_)
            | Event::Html(_)
            | Event::FootnoteReference(_)
            | Event::SoftBreak
            | Event::HardBreak
            | Event::Rule
            | Event::TaskListMarker(_) => unreachable!(),
        };
        self.need_line = true;
        out
    }
}

struct StackNumberList {
    count: u64,
    line: LineState,
}

impl StackEl for StackNumberList {
    fn handle(&mut self, state: &mut State, out: &mut String, e: Event) -> StackRes {
        let use_count = self.count;
        self.count += 1;
        match e {
            Event::Start(pulldown_cmark::Tag::Item) => StackRes::Push(StackBlock::new(
                state,
                out,
                StackBlockArgs {
                    line: self
                        .line
                        .indent(Some(format!("{}. ", use_count)), "   ".into(), false),
                    start_bound: None,
                    end_bound: None,
                },
            )),
            _ => unreachable!(),
        }
    }
}

struct StackBulletList {
    line: LineState,
}

impl StackEl for StackBulletList {
    fn handle(&mut self, state: &mut State, out: &mut String, e: Event) -> StackRes {
        match e {
            Event::Start(pulldown_cmark::Tag::Item) => StackRes::Push(StackBlock::new(
                state,
                out,
                StackBlockArgs {
                    line: self.line.indent(Some("* ".into()), "   ".into(), false),
                    start_bound: None,
                    end_bound: None,
                },
            )),
            _ => unreachable!(),
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
                    self.line.flush(state, out);
                }
                StackRes::Keep
            }
            Event::End(_) => {
                self.line.write_unbreakable(state, out, "```");
                self.line.flush(state, out);
                StackRes::Pop
            }
            Event::Start(_)
            | Event::Code(_)
            | Event::Html(_)
            | Event::FootnoteReference(_)
            | Event::SoftBreak
            | Event::HardBreak
            | Event::Rule
            | Event::TaskListMarker(_) => unreachable!(),
        }
    }
}

pub(crate) fn format_md(out: &mut String, max_width: usize, prefix: &str, source: &str) {
    let mut state = State {
        stack: vec![],
        line_buffer: String::new(),
        need_nl: false,
    };
    let start_state = StackBlock::new(
        &mut state,
        out,
        StackBlockArgs {
            line: LineState::new(None, prefix.to_string(), max_width, false),
            start_bound: None,
            end_bound: None,
        },
    );
    state.stack.push(start_state);
    for e in pulldown_cmark::Parser::new(source) {
        let mut top = state.stack.pop().unwrap();
        match top.handle(&mut state, out, e) {
            StackRes::Push(e) => {
                state.stack.push(top);
                state.stack.push(e);
            }
            StackRes::Keep => {
                state.stack.push(top);
            }
            StackRes::Pop => {}
        }
    }
}

use {
    crate::{
        Comment,
        CommentMode,
        Whitespace,
        WhitespaceMode,
    },
    loga::ea,
    markdown::mdast::Node,
    proc_macro2::{
        Group,
        LineColumn,
        TokenStream,
    },
    regex::Regex,
    std::{
        cell::RefCell,
        collections::HashMap,
        hash::Hash,
        rc::Rc,
        str::FromStr,
    },
};

#[derive(PartialEq, Eq, Debug)]
pub struct HashLineColumn(pub LineColumn);

impl Hash for HashLineColumn {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (self.0.line, self.0.column).hash(state);
    }
}

#[derive(Debug, derive_more::Add, PartialEq, Eq, PartialOrd, Ord, derive_more::Sub, Clone, Copy)]
struct VisualLen(usize);

fn unicode_len(text: &str) -> VisualLen {
    VisualLen(text.chars().count())
}

/// Identifies the start/stop locations of whitespace in a chunk of source.
/// Whitespace is grouped runs, but the `keep_max_blank_lines` parameter allows
/// splitting the groups.
pub fn extract_whitespaces(
    keep_max_blank_lines: usize,
    source: &str,
) -> Result<(HashMap<HashLineColumn, Vec<Whitespace>>, TokenStream), loga::Error> {
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
        keep_max_blank_lines: usize,
        // starting offset of each line
        line_lookup: Vec<usize>,
        whitespaces: HashMap<HashLineColumn, Vec<Whitespace>>,
        // records the beginning of the last line extracted - this is the destination for
        // transposed comments
        line_start: Option<LineColumn>,
        last_offset: usize,
        start_re: Option<Regex>,
        block_event_re: Option<Regex>,
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

        fn add_comments(&mut self, end: LineColumn, between_ast_nodes: &str) {
            let start_re = &self.start_re.get_or_insert_with(|| Regex::new(
                // `//` maybe followed by `[/!.?]`, `/**/`, or `/*` maybe followed by `[*!]`
                r#"(?:(//)(/|!|\.|\?)?)|(/\*\*/)|(?:(/\*)(\*|!)?)"#,
            ).unwrap());
            let block_event_re =
                &self.block_event_re.get_or_insert_with(|| Regex::new(r#"((?:/\*)|(?:\*/))"#).unwrap());

            struct CommentBuffer {
                keep_max_blank_lines: usize,
                blank_lines: usize,
                out: Vec<Whitespace>,
                mode: CommentMode,
                lines: Vec<String>,
                loc: LineColumn,
            }

            impl CommentBuffer {
                fn flush(&mut self) {
                    if self.lines.is_empty() {
                        return;
                    }
                    self.out.push(Whitespace {
                        loc: self.loc,
                        mode: crate::WhitespaceMode::Comment(Comment {
                            mode: self.mode,
                            lines: self.lines.split_off(0).join("\n"),
                        }),
                    });
                    self.blank_lines = 0;
                }

                fn add(&mut self, mode: CommentMode, line: &str) {
                    if self.mode != mode && !self.lines.is_empty() {
                        self.flush();
                    }
                    self.mode = mode;
                    self.lines.push(line.to_string());
                }

                fn add_blank_lines(&mut self, text: &str) {
                    let blank_lines = text.as_bytes().iter().filter(|x| **x == b'\n').count();
                    if blank_lines > 1 && self.keep_max_blank_lines > 0 {
                        self.flush();
                        self.out.push(Whitespace {
                            loc: self.loc,
                            mode: crate::WhitespaceMode::BlankLines(
                                (blank_lines - 1).min(self.keep_max_blank_lines),
                            ),
                        });
                    }
                }
            }

            let mut buffer = CommentBuffer {
                keep_max_blank_lines: self.keep_max_blank_lines,
                blank_lines: 0,
                out: vec![],
                mode: CommentMode::Normal,
                lines: vec![],
                loc: end,
            };
            let mut text = between_ast_nodes;
            'comment_loop : loop {
                match start_re.captures(text) {
                    Some(found_start) => {
                        let start_prefix_match =
                            found_start
                                .get(1)
                                .or_else(|| found_start.get(3))
                                .or_else(|| found_start.get(4))
                                .unwrap();
                        if buffer.out.is_empty() && buffer.lines.is_empty() {
                            buffer.add_blank_lines(&text[..start_prefix_match.start()]);
                        }
                        match start_prefix_match.as_str() {
                            "//" => {
                                let mode = {
                                    let start_suffix_match = found_start.get(2);
                                    let (mut mode, mut match_end) = match start_suffix_match {
                                        Some(start_suffix_match) => (match start_suffix_match.as_str() {
                                            "/" => CommentMode::DocOuter,
                                            "!" => CommentMode::DocInner,
                                            "." => CommentMode::Verbatim,
                                            "?" => CommentMode::ExplicitNormal,
                                            _ => unreachable!(),
                                        }, start_suffix_match.end()),
                                        None => (CommentMode::Normal, start_prefix_match.end()),
                                    };
                                    if mode == CommentMode::DocOuter && text[match_end..].starts_with("/") {
                                        // > 3 slashes, so actually not a doc comment
                                        mode = CommentMode::Normal;
                                        match_end = start_prefix_match.end();
                                    }
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
                            "/**/" => {
                                buffer.add(CommentMode::Normal, "".into());
                                text = &text[start_prefix_match.end()..];
                            },
                            "/*" => {
                                let mode = {
                                    let start_suffix_match = found_start.get(5);
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
                        if buffer.out.is_empty() && buffer.lines.is_empty() {
                            buffer.add_blank_lines(text);
                        }
                        break 'comment_loop;
                    },
                }
            }
            buffer.flush();
            if !buffer.out.is_empty() {
                let whitespaces = self.whitespaces.entry(HashLineColumn(end)).or_insert(vec![]);

                // Merge with existing comments (basically only if comments come before and after
                // at the end of the line)
                'merge : loop {
                    let Some(previous_whitespace) = whitespaces.last_mut() else {
                        break;
                    };
                    let WhitespaceMode::Comment(previous_comment) = &mut previous_whitespace.mode else {
                        break;
                    };
                    let start = buffer.out.remove(0);
                    loop {
                        let WhitespaceMode::Comment(start_comment) = &start.mode else {
                            break;
                        };
                        if previous_comment.mode != start_comment.mode {
                            break;
                        }
                        previous_comment.lines.push_str("\n");
                        previous_comment.lines.push_str(&start_comment.lines);
                        break 'merge;
                    }
                    buffer.out.insert(0, start);
                    break;
                }

                // Add rest without merging
                whitespaces.extend(buffer.out);
            }
        }

        fn extract(&mut self, mut start: usize, end: LineColumn) {
            // Transpose line-end comments to line-start
            if loop {
                let previous_start = match &self.line_start {
                    // No line start recorded, must be within first line
                    None => break true,
                    Some(s) => s,
                };
                if end.line <= previous_start.line {
                    // Not at end of line yet, or at bad element (moved backwards); don't do anything
                    break false;
                }
                let eol = match self.source[start..].find('\n') {
                    Some(n) => start + n,
                    None => self.source.len(),
                };
                let text = &self.source[start .. eol];
                if text.trim_start().starts_with("//") {
                    self.add_comments(*previous_start, text);
                }
                start = eol;
                break true;
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
        source: source,
        keep_max_blank_lines: keep_max_blank_lines,
        line_lookup: line_lookup,
        whitespaces: HashMap::new(),
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
                        // Syn converts doc comments into doc attrs, work around that here by detecting a
                        // mismatch between the token and the source (written /, token is #) and skipping
                        // all tokens within the fake doc attr range
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
                |e| loga::err_with(
                    "Error undoing syn parse transformations",
                    ea!(
                        line = e.span().start().line,
                        column = e.span().start().column,
                        error = e.to_string(),
                        source = source.lines().skip(e.span().start().line - 1).next().unwrap()
                    ),
                ),
            )?,
        );
    state.add_comments(LineColumn {
        line: 0,
        column: 1,
    }, &source[state.last_offset..]);
    Ok((state.whitespaces, tokens))
}

struct State {
    line_buffer: String,
    need_nl: bool,
}

#[derive(Debug)]
struct LineState_ {
    base_prefix_len: VisualLen,
    first_prefix: Option<String>,
    prefix: String,
    explicit_wrap: bool,
    max_width: VisualLen,
    rel_max_width: Option<VisualLen>,
    backward_break: Option<(usize, bool)>,
}

impl LineState_ {
    fn flush_always(&mut self, state: &mut State, out: &mut String, explicit_wrap: bool, wrapping: bool) {
        out.push_str(format!("{}{}{}{}", if state.need_nl {
            "\n"
        } else {
            ""
        }, match &self.first_prefix.take() {
            Some(t) => t,
            None => &*self.prefix,
        }, &state.line_buffer, if wrapping && explicit_wrap {
            " \\"
        } else {
            ""
        }).trim_end());
        state.line_buffer.clear();
        state.need_nl = true;
        self.backward_break = None;
    }

    fn flush(&mut self, state: &mut State, out: &mut String, explicit_wrap: bool, wrapping: bool) {
        if !state.line_buffer.trim().is_empty() {
            self.flush_always(state, out, explicit_wrap, wrapping);
        }
    }

    fn calc_max_width(&self) -> VisualLen {
        match self.rel_max_width {
            Some(w) => unicode_len(&self.prefix) + w,
            None => self.max_width - if self.explicit_wrap {
                VisualLen(2)
            } else {
                VisualLen(0)
            },
        }
    }

    fn calc_current_len(&self, state: &State) -> VisualLen {
        self.base_prefix_len + unicode_len(&state.line_buffer)
    }
}

/// Line split points, must be interior indexes (no 0 and no text.len())
fn get_splits(text: &str) -> Vec<usize> {
    // let segmenter =
    // LineBreakSegmenter::try_new_unstable(&icu_testdata::unstable()).unwrap(); match
    // segmenter .segment_str(&text)
    text.char_indices().filter(|i| i.1 == ' ').map(|i| i.0 + 1).collect()
}

struct LineState(Rc<RefCell<LineState_>>);

impl LineState {
    fn new(
        base_prefix_len: VisualLen,
        first_prefix: Option<String>,
        prefix: String,
        max_width: VisualLen,
        rel_max_width: Option<VisualLen>,
        explicit_wrap: bool,
    ) -> LineState {
        LineState(Rc::new(RefCell::new(LineState_ {
            base_prefix_len: base_prefix_len,
            first_prefix,
            prefix,
            explicit_wrap,
            max_width,
            rel_max_width,
            backward_break: None,
        })))
    }

    fn clone_inline(&self) -> LineState {
        LineState(self.0.clone())
    }

    fn clone_zero_indent(&self) -> LineState {
        let mut s = self.0.as_ref().borrow_mut();
        LineState(Rc::new(RefCell::new(LineState_ {
            base_prefix_len: s.base_prefix_len,
            first_prefix: s.first_prefix.take(),
            prefix: s.prefix.clone(),
            explicit_wrap: s.explicit_wrap,
            max_width: s.max_width,
            rel_max_width: s.rel_max_width,
            backward_break: None,
        })))
    }

    fn clone_indent(&self, first_prefix: Option<String>, prefix: String, explicit_wrap: bool) -> LineState {
        let mut s = self.0.as_ref().borrow_mut();
        LineState(Rc::new(RefCell::new(LineState_ {
            base_prefix_len: s.base_prefix_len,
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
            backward_break: None,
        })))
    }

    fn write(&self, state: &mut State, out: &mut String, text: &str, breaks: &[usize]) {
        let mut s = self.0.as_ref().borrow_mut();
        let max_len = s.calc_max_width();

        struct FoundWritableLen<'a> {
            /// How much of text can be written to the current line. If a break is used, will
            /// be equal to the break point, but if the whole string is written may be longer
            writable: usize,
            /// If a break is used, the break and the remaining unused breaks
            previous_break: Option<(usize, &'a [usize])>,
            /// The next break after the last break before the max length, 2nd fallback (after
            /// retro-break)
            next_break: Option<(usize, &'a [usize])>,
        }

        fn find_writable_len<
            'a,
        >(
            width: VisualLen,
            max_len: VisualLen,
            text: &str,
            breaks_offset: usize,
            breaks: &'a [usize],
        ) -> FoundWritableLen<'a> {
            let mut previous_break = None;
            let mut writable = 0;
            for (i, b) in breaks.iter().enumerate() {
                let b = *b - breaks_offset;
                let next_break = Some((b, &breaks[i + 1..]));
                if width + unicode_len(&text[..b]) > max_len {
                    return FoundWritableLen {
                        writable: writable,
                        previous_break: previous_break,
                        next_break: next_break,
                    };
                }
                previous_break = next_break;
                writable = b;
            }
            return FoundWritableLen {
                writable: if width + unicode_len(&text) > max_len {
                    writable
                } else {
                    text.len()
                },
                previous_break: previous_break,
                next_break: None,
            };
        }

        /// Write new text following a break, storing the new break point with it
        fn write_forward(state: &mut State, s: &mut LineState_, text: &str, b: Option<usize>) {
            if let Some(b) = b {
                s.backward_break = Some((state.line_buffer.len() + b, s.explicit_wrap));
            }
            state.line_buffer.push_str(&text);
        }

        fn write_forward_breaks(
            state: &mut State,
            s: &mut LineState_,
            out: &mut String,
            max_len: VisualLen,
            mut first: bool,
            mut text: String,
            mut breaks_offset: usize,
            breaks: &[usize],
        ) {
            let mut breaks = breaks;
            while !text.is_empty() {
                if first {
                    first = false;
                } else {
                    s.flush(state, out, s.explicit_wrap, true);
                }
                let found = find_writable_len(s.calc_current_len(state), max_len, &text, breaks_offset, breaks);
                if found.writable > 0 {
                    write_forward(state, s, &text[..found.writable], found.previous_break.map(|b| b.0));
                    breaks = found.previous_break.map(|b| b.1).unwrap_or(breaks);
                    text = text.split_off(found.writable);
                    breaks_offset += found.writable;
                } else if let Some((b, breaks0)) = found.next_break {
                    write_forward(state, s, &text[..b], Some(b));
                    breaks = breaks0;
                    text = text.split_off(b);
                    breaks_offset += b;
                } else {
                    state.line_buffer.push_str(&text);
                    return;
                }
            }
        }

        let found = find_writable_len(s.calc_current_len(state), max_len, text, 0, breaks);
        if found.writable > 0 {
            write_forward(state, &mut s, &text[..found.writable], found.previous_break.map(|b| b.0));
            write_forward_breaks(
                state,
                &mut s,
                out,
                max_len,
                false,
                (&text[found.writable..]).to_string(),
                found.writable,
                found.previous_break.map(|b| b.1).unwrap_or(breaks),
            );
        } else if let Some((at, explicit_wrap)) = s.backward_break.take() {
            // Couldn't split forward but there's a retroactive split point in previously
            // written segments
            let prefix = state.line_buffer.split_off(at);
            s.flush(state, out, explicit_wrap, true);
            state.line_buffer.push_str(&prefix);
            write_forward_breaks(state, &mut s, out, max_len, true, text.to_string(), 0, breaks);
        } else if let Some((b, breaks)) = found.next_break {
            // No retroactive split, try first split after max len (overflow, but better than
            // nothing)
            write_forward(state, &mut s, &text[..b], Some(b));
            write_forward_breaks(state, &mut s, out, max_len, false, (&text[b..]).to_string(), b, breaks);
        } else {
            state.line_buffer.push_str(text);
            return;
        }
    }

    fn write_breakable(&self, state: &mut State, out: &mut String, text: &str) {
        self.write(state, out, text, &get_splits(text));
    }

    fn write_unbreakable(&self, state: &mut State, out: &mut String, text: &str) {
        self.write(state, out, text, &[]);
    }

    fn flush_always(&self, state: &mut State, out: &mut String) {
        self.0.as_ref().borrow_mut().flush_always(state, out, false, false);
    }

    fn write_newline(&self, state: &mut State, out: &mut String) {
        let mut s = self.0.as_ref().borrow_mut();
        if !state.line_buffer.is_empty() {
            panic!();
        }
        s.flush_always(state, out, false, false);
    }
}

fn recurse_write(state: &mut State, out: &mut String, line: LineState, node: &Node, inline: bool) {
    fn join_lines(text: &str) -> String {
        let lines = Regex::new("\r?\n").unwrap().split(text).collect::<Vec<&str>>();
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
        Node::Blockquote(x) => {
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
                            line.clone_indent(Some("* ".into()), "  ".into(), false),
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
            let alt = join_lines(&x.alt);
            match (get_splits(&join_lines(&alt)).first().is_some(), &x.title) {
                (false, None) => {
                    line.write_unbreakable(state, out, &format!("![{}]({})", alt, x.url));
                },
                (false, Some(t)) => {
                    line.write_unbreakable(state, out, &format!("![{}]({}", alt, x.url));
                    line.write_unbreakable(state, out, " \"");
                    line.write_breakable(state, out, &join_lines(t));
                    line.write_unbreakable(state, out, "\")");
                },
                (true, None) => {
                    line.write_unbreakable(state, out, "![");
                    line.write_breakable(state, out, &alt);
                    line.write_unbreakable(state, out, &format!("]({})", x.url));
                },
                (true, Some(t)) => {
                    line.write_unbreakable(state, out, "![");
                    line.write_breakable(state, out, &alt);
                    line.write_unbreakable(state, out, &format!("]({}", x.url));
                    line.write_unbreakable(state, out, " \"");
                    line.write_breakable(state, out, &join_lines(t));
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
                Node::Text(t) => {
                    let t = join_lines(&t.value);
                    if get_splits(&t).first().is_some() {
                        None
                    } else {
                        Some(t)
                    }
                },
                Node::InlineCode(t) => {
                    let t = join_lines(&t.value);
                    if get_splits(&t).first().is_some() {
                        None
                    } else {
                        Some(format!("`{}`", t))
                    }
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
                Node::Text(t) => if get_splits(&t.value).first().is_some() {
                    None
                } else {
                    Some(t.value.clone())
                },
                Node::InlineCode(t) => if get_splits(&t.value).first().is_some() {
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

pub fn format_md(
    true_out: &mut String,
    max_width: usize,
    rel_max_width: Option<usize>,
    prefix: &str,
    source: &str,
) -> Result<(), loga::Error> {
    // TODO, due to a bug a bunch of unreachable branches might have had code added.
    // I'd like to go back and see if some block-level starts can be removed in
    // contexts they shouldn't appear.
    match || -> Result<String, loga::Error> {
        let mut out = String::new();
        let mut state = State {
            line_buffer: String::new(),
            need_nl: false,
        };
        let ast = markdown::to_mdast(source, &markdown::ParseOptions {
            constructs: markdown::Constructs { ..Default::default() },
            ..Default::default()
        }).map_err(|e| loga::err_with("Error parsing markdown", ea!(err = e)))?;
        recurse_write(
            &mut state,
            &mut out,
            LineState::new(
                unicode_len(&prefix),
                None,
                prefix.to_string(),
                VisualLen(max_width),
                rel_max_width.map(VisualLen),
                false,
            ),
            &ast,
            false,
        );
        Ok(out)
    }() {
        Ok(o) => {
            true_out.push_str(&o);
            Ok(())
        },
        Err(e) => {
            Err(e)
        },
    }
}

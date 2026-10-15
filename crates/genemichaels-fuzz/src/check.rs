use {
    genemichaels_lib::{
        DeclarationNormalizationMode,
        FormatConfig,
        ImportNormalizationMode,
        FormatRes,
        format_str,
    },
    proc_macro2::{
        Delimiter,
        LineColumn,
        Spacing,
        TokenStream,
        TokenTree,
    },
    serde::{
        Deserialize,
        Serialize,
    },
    std::{
        cell::Cell,
        collections::{
            BTreeMap,
            BTreeSet,
        },
        ops::Range,
        panic::{
            AssertUnwindSafe,
            catch_unwind,
        },
        str::FromStr,
        sync::{
            Arc,
            Mutex,
        },
    },
};

const KEYWORDS: &[&str] =
    &[
        "as",
        "async",
        "await",
        "break",
        "const",
        "continue",
        "crate",
        "dyn",
        "else",
        "enum",
        "extern",
        "fn",
        "for",
        "if",
        "impl",
        "in",
        "let",
        "loop",
        "macro_rules",
        "match",
        "mod",
        "move",
        "mut",
        "pub",
        "ref",
        "return",
        "self",
        "Self",
        "static",
        "struct",
        "super",
        "trait",
        "type",
        "union",
        "unsafe",
        "use",
        "where",
        "while",
        "yield",
        "default",
        "auto",
    ];

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum FailureKind {
    Panic,
    Error,
    Crash,
    Hang,
    LostComments,
    CommentsChanged,
    TrailingWhitespace,
    CommentMoved,
    TokensChanged,
    OutputUnparseable,
    NotIdempotent,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Failure {
    pub kind: FailureKind,
    pub signature: String,
    pub detail: String,
    pub rendered: Option<String>,
}

pub enum Outcome {
    Skip,
    Pass,
    Fail(Failure),
}

pub struct Scan {
    pub tokens: Vec<String>,
    pub token_starts: Vec<usize>,
    pub comment_words: BTreeMap<String, usize>,
    pub ranges: Vec<Range<usize>>,
    pub leaves: Vec<(Range<usize>, String)>,
}

impl Scan {
    pub fn new(source: &str) -> Option<Scan> {
        let mut source = source.to_string();
        let is_shebang =
            source.starts_with("#!") &&
                TokenStream::from_str(&source[2..])
                    .ok()
                    .and_then(|ts| ts.into_iter().next())
                    .map(|t| !matches!(t, TokenTree:: Group(g) if g.delimiter() == Delimiter:: Bracket))
                    .unwrap_or(true);
        if is_shebang {
            let end = source.find('\n').unwrap_or(source.len());
            source.replace_range(..end, &" ".repeat(end));
        }
        let mut line_starts = vec![0usize];
        for (i, c) in source.char_indices() {
            if c == '\n' {
                line_starts.push(i + 1);
            }
        }

        struct State<'a> {
            source: &'a str,
            line_starts: Vec<usize>,
            last_lookup: Cell<(usize, usize, usize)>,
            tokens: Vec<(String, bool, usize)>,
            in_macro: bool,
            comment_text: String,
            doc_comment_text: String,
            ranges: Vec<Range<usize>>,
            leaves: Vec<(Range<usize>, String)>,
            last_offset: usize,
        }

        impl<'a> State<'a> {
            fn to_offset(&self, loc: LineColumn) -> usize {
                let (mut column, mut offset) = match self.last_lookup.get() {
                    (line, column, offset) if line == loc.line && column <= loc.column => (column, offset),
                    _ => (0, self.line_starts[loc.line - 1]),
                };
                for c in self.source[offset..].chars() {
                    if column == loc.column {
                        break;
                    }
                    offset += c.len_utf8();
                    column += 1;
                }
                self.last_lookup.set((loc.line, column, offset));
                return offset;
            }

            fn gap(&mut self, end: usize) {
                self.comment_text.push_str(&self.source[self.last_offset .. end]);
                self.comment_text.push(' ');
            }

            fn recurse(&mut self, ts: TokenStream) {
                let mut ts = ts.into_iter().peekable();
                let mut recent = Vec::<String>::new();
                while let Some(t) = ts.next() {
                    match t {
                        TokenTree::Group(g) => {
                            let n = recent.len();
                            let is_name = |s: &str| {
                                return s.chars().next().map(|c| c.is_alphabetic() || c == '_').unwrap_or(false) &&
                                    !KEYWORDS.contains(&s);
                            };
                            let macro_body =
                                (n >= 2 && recent[n - 1] == "!" && is_name(&recent[n - 2]) &&
                                    recent[n - 2] != "vec") ||
                                    (n >= 3 && recent[n - 2] == "!" && recent[n - 3] == "macro_rules") ||
                                    (n >= 2 && recent[n - 2] == "macro") ||
                                    (n >= 3 && recent[n - 3] == "macro");
                            let (open, close) = match g.delimiter() {
                                Delimiter::Parenthesis => ("(", ")"),
                                Delimiter::Brace => ("{", "}"),
                                Delimiter::Bracket => ("[", "]"),
                                Delimiter::None => ("", ""),
                            };
                            let open_start = self.to_offset(g.span_open().start());
                            let open_end = self.to_offset(g.span_open().end());
                            let close_start = self.to_offset(g.span_close().start());
                            let close_end = self.to_offset(g.span_close().end());
                            self.gap(open_start);
                            self.tokens.push((open.to_string(), self.in_macro, open_start));
                            self.last_offset = open_end;
                            let outer_in_macro = self.in_macro;
                            self.in_macro |= macro_body;
                            self.recurse(g.stream());
                            self.in_macro = outer_in_macro;
                            self.gap(close_start);
                            self.tokens.push((close.to_string(), self.in_macro, close_start));
                            recent.push(open.to_string());
                            self.last_offset = close_end;
                            self.leaves.push((open_start .. open_end, open.to_string()));
                            self.leaves.push((close_start .. close_end, close.to_string()));
                            self.ranges.push(open_start .. close_end);
                            self.ranges.push(open_end .. close_start);
                        },
                        t => {
                            let start = self.to_offset(t.span().start());
                            let end = self.to_offset(t.span().end());
                            if let TokenTree::Punct(p) = &t {
                                if p.as_char() == '#' && self.source[start..].starts_with('/') {
                                    while ts.peek().map(|n| n.span().start() < t.span().end()).unwrap_or(false) {
                                        ts.next();
                                    }
                                    self.gap(start);
                                    self.doc_comment_text.push_str(&self.source[start .. end]);
                                    self.doc_comment_text.push(' ');
                                    self.last_offset = end;
                                    continue;
                                }
                            }
                            self.gap(start);
                            self.last_offset = end;
                            self.ranges.push(start .. end);
                            self.leaves.push((start .. end, match &t {
                                TokenTree::Ident(i) if KEYWORDS.contains(&i.to_string().as_str()) => i.to_string(),
                                TokenTree::Ident(_) => "ident".to_string(),
                                TokenTree::Literal(_) => "lit".to_string(),
                                t => t.to_string(),
                            }));
                            if let TokenTree::Punct(p) = &t {
                                if p.as_char() == '+' && p.spacing() == Spacing::Joint {
                                    if let Some(TokenTree::Punct(next)) = ts.peek() {
                                        if next.as_char() == '=' {
                                            let next_end = self.to_offset(next.span().end());
                                            self.ranges.push(end .. next_end);
                                            self.last_offset = next_end;
                                            self.tokens.push(("+=".to_string(), self.in_macro, start));
                                            recent.push("+=".to_string());
                                            ts.next();
                                            continue;
                                        }
                                    }
                                }
                            }
                            self.tokens.push((t.to_string(), self.in_macro, start));
                            recent.push(t.to_string());
                        },
                    }
                }
            }
        }

        let ts = TokenStream::from_str(&source).ok()?;
        let mut state = State {
            source: &source,
            line_starts: line_starts,
            last_lookup: Cell::new((0, 0, 0)),
            tokens: vec![],
            in_macro: false,
            comment_text: String::new(),
            doc_comment_text: String::new(),
            ranges: vec![],
            leaves: vec![],
            last_offset: 0,
        };
        state.recurse(ts);
        state.gap(source.len());
        state.leaves.sort_by_key(|(r, _)| r.start);
        let mut tokens = Vec::<(String, bool, usize)>::new();
        for (t, in_macro, start) in state.tokens {
            if tokens.last().is_some_and(|(l, _, _)| l == ".") {
                if t.len() > 1 && t.ends_with('.') && t[..t.len() - 1].chars().all(|c| c.is_ascii_digit()) {
                    tokens.push((t[..t.len() - 1].to_string(), in_macro, start));
                    tokens.push((".".to_string(), in_macro, start));
                    continue;
                }
                if let Some((a, b)) = t.split_once('.') {
                    if !a.is_empty() && !b.is_empty() && a.chars().all(|c| c.is_ascii_digit()) &&
                        b.chars().all(|c| c.is_ascii_digit()) {
                        tokens.push((a.to_string(), in_macro, start));
                        tokens.push((".".to_string(), in_macro, start));
                        tokens.push((b.to_string(), in_macro, start));
                        continue;
                    }
                }
            }
            if !in_macro {
                let normalizable = |tokens: &Vec<(String, bool, usize)>, i: usize, want: &[&str]| {
                    return tokens.len() >= i && !tokens[tokens.len() - i].1 &&
                        want.contains(&tokens[tokens.len() - i].0.as_str());
                };
                if t == "dyn" {
                    continue;
                }
                if t == "." && normalizable(&tokens, 1, &["."]) && normalizable(&tokens, 2, &["."]) {
                    tokens.push(("=".to_string(), false, start));
                    continue;
                }
                if t == "," && normalizable(&tokens, 1, &["}"]) {
                    continue;
                }
                if t == ">" && normalizable(&tokens, 1, &["<"]) {
                    tokens.pop();
                    continue;
                }
                if [",", ">", "{", ";"].contains(&t.as_str()) && normalizable(&tokens, 1, &[":"]) &&
                    !normalizable(&tokens, 2, &[":"]) {
                    tokens.pop();
                }
                if [")", "}", "]", ">", "{", ";", "=", ",", "|"].contains(&t.as_str()) {
                    while normalizable(&tokens, 1, &[",", "+"]) {
                        tokens.pop();
                    }
                }
            }
            tokens.push((t, in_macro, start));
        }
        let token_starts = tokens.iter().map(|(_, _, s)| *s).collect::<Vec<_>>();
        let tokens = tokens.into_iter().map(|(t, _, _)| t).collect::<Vec<_>>();
        let mut comment_words = BTreeMap::new();
        for (prefix, text) in [("", &state.comment_text), ("doc:", &state.doc_comment_text)] {
            for word in text.split(|c: char| !c.is_alphanumeric()).filter(|w| !w.is_empty()) {
                *comment_words.entry(format!("{}{}", prefix, word)).or_insert(0usize) += 1;
            }
        }
        return Some(Scan {
            tokens: tokens,
            token_starts: token_starts,
            comment_words: comment_words,
            ranges: state.ranges,
            leaves: state.leaves,
        });
    }
}

pub const MARKER: &str = "injected";

fn find_marker(text: &str) -> Option<usize> {
    let mut found = text.match_indices(MARKER);
    let (first, _) = found.next()?;
    if found.next().is_some() {
        return None;
    }
    return Some(first);
}

pub struct Checker {
    pub config: FormatConfig,
    last_panic: Arc<Mutex<Option<(String, String)>>>,
}

impl Checker {
    pub fn new(config: FormatConfig) -> Checker {
        let last_panic = Arc::new(Mutex::new(None));
        std::panic::set_hook({
            let last_panic = last_panic.clone();
            Box::new(move |info| {
                let message = if let Some(m) = info.payload().downcast_ref::<&str>() {
                    m.to_string()
                } else if let Some(m) = info.payload().downcast_ref::<String>() {
                    m.clone()
                } else {
                    "(non-string panic payload)".to_string()
                };
                let location = info.location().map(|l| format!("{}:{}", l.file(), l.line())).unwrap_or_default();
                *last_panic.lock().unwrap() = Some((location, message));
            })
        });
        return Checker {
            config: config,
            last_panic: last_panic,
        };
    }

    pub fn check(&self, source: &str) -> Outcome {
        let out = (|| -> Outcome {
            if syn::parse_file(source).is_err() {
                return Outcome::Skip;
            }
            let Some(before) = Scan::new(source) else {
                return Outcome::Skip;
            };
            let res = match self.format("", source) {
                Ok(r) => r,
                Err(f) => return Outcome::Fail(f),
            };
            let rendered = res.rendered;
            let fail = |kind: FailureKind, signature: String, detail: String| Outcome::Fail(Failure {
                kind: kind,
                signature: signature,
                detail: detail,
                rendered: Some(rendered.clone()),
            });
            if !res.lost_comments.is_empty() {
                return fail(FailureKind::LostComments, "".to_string(), format!("{:#?}", res.lost_comments));
            }
            if let Err(e) = syn::parse_file(&rendered) {
                return fail(
                    FailureKind::OutputUnparseable,
                    e.to_string(),
                    format!("line {} column {}: {}", e.span().start().line, e.span().start().column, e),
                );
            }
            let Some(after) = Scan::new(&rendered) else {
                return fail(FailureKind::OutputUnparseable, "lex".to_string(), "".to_string());
            };
            let reorders =
                !matches!(self.config.import_normalization, ImportNormalizationMode::None) ||
                    !matches!(self.config.declaration_normalization, DeclarationNormalizationMode::None);
            let tokens_match = if reorders {
                let words = |tokens: &Vec<String>| {
                    return tokens
                        .iter()
                        .filter(
                            |t| t.starts_with(|c: char| c.is_alphanumeric() || c == '_' || c == '"' || c == '\''),
                        )
                        .cloned()
                        .collect::<BTreeSet<_>>();
                };
                words(&before.tokens) == words(&after.tokens)
            } else {
                before.tokens.len() == after.tokens.len() &&
                    before.tokens.iter().zip(after.tokens.iter()).all(|(a, b)| a == b || (a == "+=" && b == "="))
            };
            if !tokens_match {
                let i =
                    before
                        .tokens
                        .iter()
                        .zip(after.tokens.iter())
                        .position(|(a, b)| a != b)
                        .unwrap_or(before.tokens.len().min(after.tokens.len()));
                let at = |tokens: &Vec<String>, i: usize| tokens.get(i).cloned().unwrap_or("<eof>".to_string());
                let context =
                    |tokens: &Vec<String>| tokens[i.saturating_sub(8) .. (i + 8).min(tokens.len())].join(" ");
                return fail(
                    FailureKind::TokensChanged,
                    format!("{} -> {}", at(&before.tokens, i), at(&after.tokens, i)),
                    format!("before: {}\nafter:  {}", context(&before.tokens), context(&after.tokens)),
                );
            }
            if before.comment_words != after.comment_words {
                let mut missing = vec![];
                let mut extra = vec![];
                for (word, count) in &before.comment_words {
                    let after_count = after.comment_words.get(word).cloned().unwrap_or(0);
                    if after_count < *count {
                        missing.push(word.clone());
                    }
                }
                for (word, count) in &after.comment_words {
                    let before_count = before.comment_words.get(word).cloned().unwrap_or(0);
                    if before_count < *count {
                        extra.push(word.clone());
                    }
                }
                let copies_comments = matches!(self.config.import_normalization, ImportNormalizationMode::Split);
                if missing.is_empty() && copies_comments {
                    extra.clear();
                }
                let signature = match (missing.is_empty(), extra.is_empty()) {
                    (true, true) => None,
                    (false, true) => Some("missing"),
                    (true, false) => Some("extra"),
                    (false, false) => Some("missing and extra"),
                };
                if let Some(signature) = signature {
                    return fail(
                        FailureKind::CommentsChanged,
                        signature.to_string(),
                        format!("missing: {:?}\nextra: {:?}", missing, extra),
                    );
                }
            }
            let skips = source.contains("rustfmt::skip") || source.contains("genemichaels-skip");
            if !reorders && !skips {
                if let (Some(in_at), Some(out_at)) = (find_marker(source), find_marker(&rendered)) {
                    let p_in = before.token_starts.partition_point(|s| *s < in_at);
                    let p_out = after.token_starts.partition_point(|s| *s < out_at);
                    let line_start = source[..in_at].rfind('\n').map(|i| i + 1).unwrap_or(0);
                    let comment_start =
                        source[line_start .. in_at]
                            .rfind("//")
                            .max(source[line_start .. in_at].rfind("/*"))
                            .map(|i| line_start + i);
                    let mut lower = p_in;
                    if let Some(comment_start) = comment_start {
                        let comment = &source[comment_start..];
                        let is_doc =
                            (comment.starts_with("///") && !comment.starts_with("////")) ||
                                (comment.starts_with("/**") && !comment.starts_with("/**/"));
                        let comment_end = if comment.starts_with("//") {
                            comment_start + comment.find('\n').unwrap_or(comment.len())
                        } else {
                            source[in_at..].find("*/").map(|i| in_at + i + 2).unwrap_or(source.len())
                        };
                        let rest_of_line = source[comment_end..].split('\n').next().unwrap().trim();
                        let line_end =
                            !is_doc && !source[line_start .. comment_start].trim().is_empty() &&
                                (rest_of_line.is_empty() || rest_of_line.starts_with("//") ||
                                    rest_of_line.starts_with("/*"));
                        if line_end {
                            lower = before.token_starts.partition_point(|s| *s < line_start);
                        }
                    }
                    let context =
                        |tokens: &Vec<String>, at: usize| format!(
                            "{} ⟨comment⟩ {}",
                            tokens[at.saturating_sub(6) .. at].join(" "),
                            tokens[at .. (at + 6).min(tokens.len())].join(" ")
                        );
                    let moved = if p_out > p_in {
                        before.tokens[p_in .. p_out]
                            .iter()
                            .find(|t| ![",", "+", "|"].contains(&t.as_str()))
                            .map(|t| format!("forward over {}", t))
                    } else if p_out < lower {
                        let mut enclosing = vec![];
                        for t in &before.tokens[..lower] {
                            match t.as_str() {
                                "[" | "(" | "{" => enclosing.push(t.as_str()),
                                ")" | "}" | "]" => {
                                    enclosing.pop();
                                },
                                _ => { },
                            }
                        }
                        let mut depth = 0usize;
                        let mut crossed = None;
                        for t in before.tokens[p_out .. lower].iter().rev() {
                            let boundary = match t.as_str() {
                                "]" | ")" | "}" => {
                                    depth += 1;
                                    false
                                },
                                "[" | "(" | "{" if depth > 0 => {
                                    depth -= 1;
                                    false
                                },
                                "{" => true,
                                "[" | "(" => {
                                    enclosing.pop();
                                    false
                                },
                                ";" => depth == 0 && enclosing.last().is_none_or(|e| *e == "{"),
                                _ => false,
                            };
                            if boundary {
                                crossed = Some(t);
                                break;
                            }
                        }
                        crossed.map(|t| format!("backward over {}", t))
                    } else {
                        None
                    };
                    if let Some(signature) = moved {
                        return fail(
                            FailureKind::CommentMoved,
                            signature,
                            format!(
                                "before: {}\nafter:  {}",
                                context(&before.tokens, p_in),
                                context(&after.tokens, p_out)
                            ),
                        );
                    }
                }
            }
            if !source.lines().any(|l| l.ends_with([' ', '\t'])) {
                if let Some((i, line)) = rendered.lines().enumerate().find(|(_, l)| l.ends_with([' ', '\t'])) {
                    return fail(
                        FailureKind::TrailingWhitespace,
                        "".to_string(),
                        format!("line {}: {:?}", i + 1, line),
                    );
                }
            }
            let res2 = match self.format("second pass: ", &rendered) {
                Ok(r) => r,
                Err(mut f) => {
                    f.rendered = Some(rendered.clone());
                    return Outcome::Fail(f);
                },
            };
            if res2.rendered != rendered {
                let i =
                    rendered
                        .lines()
                        .zip(res2.rendered.lines())
                        .position(|(a, b)| a != b)
                        .unwrap_or(rendered.lines().count().min(res2.rendered.lines().count()));
                let context =
                    |text: &str| text.lines().skip(i.saturating_sub(2)).take(5).collect::<Vec<_>>().join("\n");
                return fail(
                    FailureKind::NotIdempotent,
                    "".to_string(),
                    format!(
                        "first differing line {}\npass 1:\n{}\npass 2:\n{}",
                        i + 1,
                        context(&rendered),
                        context(&res2.rendered)
                    ),
                );
            }
            return Outcome::Pass;
        })();
        proc_macro2::extra::invalidate_current_thread_spans();
        return out;
    }

    fn format(&self, pass: &str, source: &str) -> Result<FormatRes, Failure> {
        match catch_unwind(AssertUnwindSafe(|| format_str(source, &self.config))) {
            Ok(Ok(r)) => return Ok(r),
            Ok(Err(e)) => {
                let detail = e.to_string();
                let first_line = detail.lines().next().unwrap_or("");
                let (message, attrs) = first_line.split_once(" [").unwrap_or((first_line, ""));
                let mut attrs =
                    attrs
                        .trim_end_matches(']')
                        .split(", ")
                        .map(|a| a.trim())
                        .filter(|a| !a.starts_with("line =") && !a.starts_with("column ="))
                        .collect::<Vec<_>>();
                attrs.sort();
                return Err(Failure {
                    kind: FailureKind::Error,
                    signature: format!("{}{} [{}]", pass, message, attrs.join(", ")),
                    detail: detail,
                    rendered: None,
                });
            },
            Err(_) => {
                let (location, message) =
                    self.last_panic.lock().unwrap().take().unwrap_or(("?".to_string(), "?".to_string()));
                return Err(Failure {
                    kind: FailureKind::Panic,
                    signature: format!("{}{}", pass, location),
                    detail: message,
                    rendered: None,
                });
            },
        }
    }
}

impl Checker {
    pub fn minimize(&self, source: &str, max_attempts: usize) -> Option<(String, Failure)> {
        let Outcome::Fail(target) = self.check(source) else {
            return None;
        };
        let attempts = Cell::new(0usize);
        let out_of_attempts = || attempts.get() >= max_attempts;
        let still_fails = |candidate: &str| -> bool {
            attempts.set(attempts.get() + 1);
            return match self.check(candidate) {
                Outcome::Fail(f) => f.kind == target.kind && f.signature == target.signature,
                _ => false,
            };
        };
        let mut current = source.to_string();
        loop {
            let start_len = current.len();
            let mut lines = current.split_inclusive('\n').map(String::from).collect::<Vec<_>>();
            let mut chunk = (lines.len() / 2).max(1);
            loop {
                let mut i = 0;
                while i < lines.len() && !out_of_attempts() {
                    let end = (i + chunk).min(lines.len());
                    let candidate = format!("{}{}", lines[..i].concat(), lines[end..].concat());
                    if still_fails(&candidate) {
                        lines.drain(i .. end);
                    } else {
                        i += chunk;
                    }
                }
                if chunk == 1 {
                    break;
                }
                chunk /= 2;
            }
            current = lines.concat();
            'ranges: while !out_of_attempts() {
                let Some(scan) = Scan::new(&current) else {
                    break;
                };
                let mut ranges = scan.ranges;
                ranges.sort_by_key(|r| std::cmp::Reverse(r.len()));
                for r in ranges {
                    if r.is_empty() || out_of_attempts() {
                        continue;
                    }
                    let candidate = format!("{}{}", &current[..r.start], &current[r.end..]);
                    if still_fails(&candidate) {
                        current = candidate;
                        continue 'ranges;
                    }
                }
                break;
            }
            if current.len() == start_len || out_of_attempts() {
                break;
            }
        }
        let Outcome::Fail(failure) = self.check(&current) else {
            unreachable!();
        };
        return Some((current, failure));
    }
}

use {
    comfy_table::Cell,
    std::{
        any::TypeId,
        cell::RefCell,
        collections::{
            HashMap,
            HashSet,
        },
        rc::Rc,
    },
};

fn style_usage(s: impl AsRef<str>) -> String {
    return s.as_ref().to_string();
}

fn style_description(s: impl AsRef<str>) -> String {
    return s.as_ref().to_string();
}

fn style_id(s: impl AsRef<str>) -> String {
    return console::Style::new().blue().dim().apply_to(s.as_ref()).to_string();
}

fn style_type(s: impl AsRef<str>) -> String {
    return console::Style::new().magenta().apply_to(s.as_ref()).to_string();
}

fn style_logical(s: impl AsRef<str>) -> String {
    return console::Style::new().dim().apply_to(s.as_ref()).to_string();
}

fn style_literal(s: impl AsRef<str>) -> String {
    return console::Style::new().bold().apply_to(s.as_ref()).to_string();
}

#[doc(hidden)]
#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
pub struct HelpProductionKey {
    type_id: TypeId,
    variant: usize,
}

#[doc(hidden)]
pub struct HelpProduction {
    id: String,
    description: String,
    content: HelpProductionType,
}

#[doc(hidden)]
pub enum HelpPartialContent {
    Pattern(HelpPattern),
    Production(HelpProductionType),
}

impl HelpPartialContent {
    pub fn struct_(fields: Vec<HelpField>, optional_fields: Vec<HelpFlagField>) -> Self {
        return HelpPartialContent::Production(
            HelpProductionType::Struct(Rc::new(RefCell::new(HelpProductionTypeStruct {
                fields: fields,
                flag_fields: optional_fields,
            }))),
        );
    }

    pub fn enum_(variants: Vec<HelpVariant>) -> Self {
        return HelpPartialContent::Production(HelpProductionType::Enum(Rc::new(RefCell::new(variants))));
    }
}

/// State for a partially-parsed field for rendering help.
pub struct HelpPartialProduction {
    pub description: String,
    pub content: HelpPartialContent,
}

pub enum HelpProductionType {
    Struct(Rc<RefCell<HelpProductionTypeStruct>>),
    Enum(Rc<RefCell<Vec<HelpVariant>>>),
}

pub struct HelpProductionTypeStruct {
    pub fields: Vec<HelpField>,
    pub flag_fields: Vec<HelpFlagField>,
}

pub struct HelpField {
    pub id: String,
    pub pattern: HelpPattern,
    pub description: String,
}

pub struct HelpFlagField {
    pub option: bool,
    pub flags: Vec<String>,
    pub pattern: HelpPattern,
    pub description: String,
}

pub struct HelpVariant {
    pub literal: String,
    pub pattern: HelpPattern,
    pub description: String,
}

/// Structured help information - this list of pattern elements that is styled and
/// joined with spaces on output.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct HelpPattern(pub Vec<HelpPatternElement>);

impl HelpPattern {
    pub fn render(&self, stack: &mut Vec<(HelpProductionKey, Rc<HelpProduction>)>, state: &HelpState) -> String {
        let mut out = String::new();
        for (i, e) in self.0.iter().enumerate() {
            if i > 0 {
                out.push_str(" ");
            }
            out.push_str(&e.render(stack, state));
        }
        return out;
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum HelpPatternElement {
    // xyz - denotes literal text user must type
    Literal(String),
    // `<XYZ>` - denotes type of data for user
    Type(String),
    // XYZ - refers to another section of help output
    Reference(HelpProductionKey),
    // Like reference, but the other section might not exist.  Used for `...` when
    // using `help_break` to limit output.
    PseudoReference(String),
    // `[XYZ]` - indicates a pattern is optional
    Option(HelpPattern),
    // `XYZ[ ...]` - indicates a pattern can be repeated, space separated
    Array(HelpPattern),
    // `xyz | abc` - indicates one of multiple patterns can be selected
    Variant(Vec<HelpPattern>),
}

impl HelpPatternElement {
    fn render(&self, stack: &mut Vec<(HelpProductionKey, Rc<HelpProduction>)>, state: &HelpState) -> String {
        match self {
            HelpPatternElement::Literal(l) => return style_literal(l),
            HelpPatternElement::Type(i) => return style_type(format!("<{}>", i)),
            HelpPatternElement::Reference(i) => {
                let production = state.productions.get(i).unwrap();
                stack.push((*i, production.clone()));
                return style_id(production.id.as_str())
            },
            HelpPatternElement::PseudoReference(key) => {
                return style_id(key.as_str())
            },
            HelpPatternElement::Option(i) => return format!(
                "{}{}{}",
                style_logical("["),
                i.render(stack, state),
                style_logical("]")
            ),
            HelpPatternElement::Array(i) => return format!("{}{}", i.render(stack, state), style_logical("[ ...]")),
            HelpPatternElement::Variant(i) => return i
                .iter()
                .map(|x| x.render(stack, state))
                .collect::<Vec<_>>()
                .join(&style_logical(" | ")),
        }
    }
}

#[doc(hidden)]
#[derive(Default)]
pub struct HelpState {
    // Write during building
    name_counter: HashMap<String, usize>,
    // Write during building, read during rendering
    productions: HashMap<HelpProductionKey, Rc<HelpProduction>>,
}

impl HelpState {
    fn add(
        &mut self,
        type_id: TypeId,
        type_id_variant: usize,
        id: impl ToString,
        description: impl ToString,
        content: HelpProductionType,
    ) -> HelpProductionKey {
        let mut id = id.to_string();
        let count = *self.name_counter.entry(id.clone()).and_modify(|x| *x += 1).or_insert(1);
        if count > 1 {
            id = format!("{} ({})", id, count);
        }
        let key = HelpProductionKey {
            type_id: type_id,
            variant: type_id_variant,
        };
        self.productions.insert(key, Rc::new(HelpProduction {
            id: id,
            description: description.to_string(),
            content: content,
        }));
        return key;
    }

    pub fn add_struct(
        &mut self,
        type_id: TypeId,
        type_id_variant: usize,
        id: impl ToString,
        description: impl ToString,
    ) -> (HelpProductionKey, Rc<RefCell<HelpProductionTypeStruct>>) {
        let out = Rc::new(RefCell::new(HelpProductionTypeStruct {
            fields: vec![],
            flag_fields: vec![],
        }));
        let key = self.add(type_id, type_id_variant, id, description, HelpProductionType::Struct(out.clone()));
        return (key, out);
    }

    pub fn add_enum(
        &mut self,
        type_id: TypeId,
        type_id_variant: usize,
        id: impl ToString,
        description: impl ToString,
    ) -> (HelpProductionKey, Rc<RefCell<Vec<HelpVariant>>>) {
        let out = Rc::new(RefCell::new(vec![]));
        let key = self.add(type_id, type_id_variant, id, description, HelpProductionType::Enum(out.clone()));
        return (key, out);
    }
}

/// State required for building a help string.
pub struct VarkRetHelp {
    pub(crate) command: Option<String>,
    pub(crate) args: Vec<String>,
    pub(crate) consumed_args: usize,
    pub(crate) builder: Box<dyn FnOnce(&mut HelpState) -> HelpPartialProduction>,
}

impl VarkRetHelp {
    /// Build a help string using current operating environment line widths.
    pub fn render(self) -> String {
        fn format_desc(out: &mut String, desc: &str) {
            if !desc.is_empty() {
                out.push_str(
                    &style_description(
                        textwrap::wrap(
                            desc,
                            &textwrap::Options::with_termwidth().initial_indent("    ").subsequent_indent("    "),
                        ).join("\n"),
                    ),
                );
                out.push_str("\n\n");
            }
        }

        fn format_pattern(out: &mut String, content: &HelpProductionType) {
            match content {
                HelpProductionType::Struct(struct_) => {
                    let struct_ = struct_.borrow();
                    for f in &struct_.fields {
                        out.push_str(" ");
                        out.push_str(&style_id(&f.id));
                    }
                    if !struct_.flag_fields.is_empty() {
                        let all_opt = struct_.flag_fields.iter().all(|x| x.option);
                        out.push_str(" ");
                        if all_opt {
                            out.push_str(&style_logical("[ ...FLAGS]"));
                        } else {
                            out.push_str(&style_logical("...FLAGS"));
                        }
                    }
                },
                HelpProductionType::Enum(fields) => {
                    for (i, f) in fields.borrow().iter().enumerate() {
                        if i > 0 {
                            out.push_str(" |");
                        }
                        out.push_str(" ");
                        out.push_str(&style_literal(&f.literal));
                    }
                },
            }
        }

        fn format_content(
            out: &mut String,
            stack: &mut Vec<(HelpProductionKey, Rc<HelpProduction>)>,
            help_state: &HelpState,
            content: &HelpProductionType,
        ) {
            let mut table = comfy_table::Table::new();
            table.load_preset(comfy_table::presets::NOTHING);
            table.set_content_arrangement(comfy_table::ContentArrangement::Dynamic);
            match content {
                HelpProductionType::Struct(struct_) => {
                    let struct_ = struct_.borrow();
                    for f in &struct_.fields {
                        table.add_row(
                            vec![
                                comfy_table::Cell::new(
                                    format!("   {}: {}", style_id(&f.id), f.pattern.render(stack, help_state)),
                                ),
                                Cell::new(style_description(&f.description))
                            ],
                        );
                    }
                    for f in &struct_.flag_fields {
                        let first_flag = f.flags.first().unwrap();
                        for (i, flag) in f.flags.iter().enumerate() {
                            let mut elems = vec![HelpPatternElement::Literal(flag.clone())];
                            elems.extend(f.pattern.0.clone());
                            let left_col = format!("   {}", if f.option {
                                HelpPattern(vec![HelpPatternElement::Option(HelpPattern(elems))])
                            } else {
                                HelpPattern(elems)
                            }.render(stack, help_state));
                            if i == 0 {
                                table.add_row(
                                    vec![
                                        comfy_table::Cell::new(left_col),
                                        Cell::new(style_description(&f.description))
                                    ],
                                );
                            } else {
                                table.add_row(
                                    vec![
                                        comfy_table::Cell::new(left_col),
                                        Cell::new(style_description(&format!("(synonym for `{}`)", first_flag)))
                                    ],
                                );
                            }
                        }
                    }
                },
                HelpProductionType::Enum(fields) => {
                    for f in &*fields.borrow() {
                        table.add_row(
                            vec![
                                comfy_table::Cell::new(
                                    format!(
                                        "   {} {}",
                                        style_literal(&f.literal),
                                        f.pattern.render(stack, help_state)
                                    ),
                                ),
                                Cell::new(style_description(&f.description))
                            ],
                        );
                    }
                },
            }
            table.set_constraints(vec![comfy_table::ColumnConstraint::Boundaries {
                lower: comfy_table::Width::Percentage(20),
                upper: comfy_table::Width::Percentage(60),
            }]);
            out.push_str(&table.to_string());
            out.push_str("\n\n");
        }

        let mut help_state = HelpState::default();
        let mut stack = Vec::<(HelpProductionKey, Rc<HelpProduction>)>::new();
        let mut seen_productions = HashSet::<HelpProductionKey>::new();
        let partial = (self.builder)(&mut help_state);

        // Write initial partial production
        let mut out = style_usage("Usage:");
        if let Some(s) = &self.command {
            out.push_str(" ");
            out.push_str(&style_usage(s));
        }
        for s in self.args.iter().take(self.consumed_args) {
            out.push_str(" ");
            out.push_str(&style_usage(s));
        }
        let mut temp_stack = vec![];
        match &partial.content {
            HelpPartialContent::Pattern(p) => {
                if !p.0.is_empty() {
                    out.push_str(" ");
                    out.push_str(&p.render(&mut temp_stack, &help_state));
                }
            },
            HelpPartialContent::Production(content) => {
                format_pattern(&mut out, &content);
            },
        }
        out.push_str("\n\n");
        format_desc(&mut out, &partial.description);
        match &partial.content {
            HelpPartialContent::Pattern(_) => {
                out.push_str("\n\n");
            },
            HelpPartialContent::Production(content) => {
                format_content(&mut out, &mut temp_stack, &mut help_state, content);
            },
        }
        temp_stack.reverse();
        stack.extend(temp_stack);

        // Recurse productions
        while let Some((key, top)) = stack.pop() {
            if !seen_productions.insert(key) {
                continue;
            }
            out.push_str(&style_id(&top.id));
            out.push_str(":");
            format_pattern(&mut out, &top.content);
            out.push_str("\n\n");
            format_desc(&mut out, &top.description);
            let mut temp_stack = vec![];
            format_content(&mut out, &mut temp_stack, &mut help_state, &top.content);
            temp_stack.reverse();
            stack.extend(temp_stack);
        }
        return out;
    }
}

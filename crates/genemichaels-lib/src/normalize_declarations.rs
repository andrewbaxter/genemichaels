use {
    crate::{
        CommentMode,
        DeclarationNormalizationCategory,
        DeclarationNormalizationMode,
        FormatConfig,
        Whitespace,
        WhitespaceMode,
        sg_general::has_directive_comment,
        whitespace::HashLineColumn,
    },
    proc_macro2::LineColumn,
    quote::ToTokens,
    std::collections::BTreeMap,
    syn::{
        Field,
        File,
        ImplItem,
        Item,
        ItemEnum,
        ItemImpl,
        ItemMod,
        ItemStruct,
        ItemTrait,
        TraitItem,
        Variant,
        spanned::Spanned,
        visit_mut::VisitMut,
    },
};

const AUTO_CATEGORY_ORDER: &[DeclarationNormalizationCategory] =
    &[
        DeclarationNormalizationCategory::Mod,
        DeclarationNormalizationCategory::Use,
        DeclarationNormalizationCategory::Macro,
        DeclarationNormalizationCategory::MacroCall,
        DeclarationNormalizationCategory::Const,
        DeclarationNormalizationCategory::Trait,
        DeclarationNormalizationCategory::Concrete,
    ];
const WRAPPER_TYPES: &[&str] = &["Arc", "Box", "Rc"];

fn category_rank(
    category: &DeclarationNormalizationCategory,
    ranks: &BTreeMap<DeclarationNormalizationCategory, usize>,
) -> usize {
    if let Some(rank) = ranks.get(category) {
        return *rank;
    }

    // `Concrete` is always inserted by `resolve_category_order` — leaves fall through
    // here.
    ranks[&DeclarationNormalizationCategory::Concrete]
}

fn classify_impls(
    impl_items: Vec<Item>,
    local_type_names: &[String],
    local_trait_names: &[String],
) -> (BTreeMap<String, Vec<Item>>, BTreeMap<String, Vec<Item>>, Vec<Item>) {
    let mut type_impls: BTreeMap<String, Vec<Item>> = BTreeMap::new();
    let mut trait_impls: BTreeMap<String, Vec<Item>> = BTreeMap::new();
    let mut leftover_impls: Vec<Item> = Vec::new();
    for item in impl_items {
        if let Item::Impl(ref impl_item) = item {
            let base_type = extract_base_type_name(&impl_item.self_ty);
            if local_type_names.contains(&base_type) {
                type_impls.entry(base_type).or_default().push(item);
            } else if let Some((_, trait_path, _)) = &impl_item.trait_ {
                let trait_name =
                    trait_path.segments.last().map(|s| s.ident.to_string().to_lowercase()).unwrap_or_default();
                if local_trait_names.contains(&trait_name) {
                    trait_impls.entry(trait_name).or_default().push(item);
                } else {
                    leftover_impls.push(item);
                }
            } else {
                leftover_impls.push(item);
            }
        }
    }
    let self_ty_rank = |impl_item: &ItemImpl| -> usize {
        if impl_item.trait_.is_none() {
            return 0;
        }
        let ty = &*impl_item.self_ty;
        if matches!(ty, syn::Type::Reference(_)) {
            return 2;
        }
        if let syn::Type::Path(p) = ty {
            if let Some(seg) = p.path.segments.last() {
                if WRAPPER_TYPES.iter().any(|w| w.eq_ignore_ascii_case(&seg.ident.to_string())) {
                    return 3;
                }
            }
        }
        1
    };
    let generics_sort_key = |impl_item: &ItemImpl| -> (usize, String) {
        if impl_item.generics.params.is_empty() {
            return (0, item_sort_name(&Item::Impl(impl_item.clone())));
        }
        (1, impl_item.generics.params.to_token_stream().to_string().to_lowercase())
    };
    for impls in type_impls.values_mut() {
        impls.sort_by(|a, b| {
            if let (Item::Impl(a_impl), Item::Impl(b_impl)) = (a, b) {
                let rank_a = self_ty_rank(a_impl);
                let rank_b = self_ty_rank(b_impl);
                rank_a.cmp(&rank_b).then_with(|| generics_sort_key(a_impl).cmp(&generics_sort_key(b_impl)))
            } else {
                std::cmp::Ordering::Equal
            }
        });
    }
    for impls in trait_impls.values_mut() {
        impls.sort_by(|a, b| item_sort_name(a).cmp(&item_sort_name(b)));
    }
    leftover_impls.sort_by(|a, b| item_sort_name(a).cmp(&item_sort_name(b)));
    (type_impls, trait_impls, leftover_impls)
}

fn collect_local_names<'a>(items: impl IntoIterator<Item = &'a Item>) -> (Vec<String>, Vec<String>) {
    let mut local_type_names: Vec<String> = Vec::new();
    let mut local_trait_names: Vec<String> = Vec::new();
    for item in items {
        let kind = item_kind(item);
        if kind.is_data_type() {
            local_type_names.push(item_sort_name(item));
        } else if kind.is_trait_def() {
            local_trait_names.push(item_sort_name(item));
        }
    }
    (local_type_names, local_trait_names)
}

pub(crate) struct DeclarationNormalizer<'a> {
    pub(crate) config: &'a FormatConfig,
    pub(crate) whitespaces: &'a mut BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)>,
}

impl<'a> DeclarationNormalizer<'a> {
    fn emit_with_trait_impls(
        &self,
        item: &Item,
        trait_impls: &mut BTreeMap<String, Vec<Item>>,
        out: &mut Vec<Item>,
    ) {
        out.push(item.clone());
        if item_kind(item).is_trait_def() {
            let trait_name = item_sort_name(item);
            if let Some(impls) = trait_impls.remove(&trait_name) {
                out.extend(impls);
            }
        }
    }

    fn emit_with_type_impls(&self, item: &Item, type_impls: &mut BTreeMap<String, Vec<Item>>, out: &mut Vec<Item>) {
        out.push(item.clone());
        if item_kind(item).is_data_type() {
            let type_name = item_sort_name(item);
            if let Some(impls) = type_impls.remove(&type_name) {
                out.extend(impls);
            }
        }
    }

    fn process_fields(
        &mut self,
        loc: LineColumn,
        keyword_loc: LineColumn,
        attrs: &[syn::Attribute],
        fields: &mut syn::Fields,
    ) {
        if let syn::Fields::Named(named) = fields {
            if named.named.is_empty() {
                return;
            }
            if matches!(self.config.declaration_normalization, DeclarationNormalizationMode::None) {
                return;
            }
            if is_decl_layout_significant(self.whitespaces, loc, keyword_loc, attrs) {
                return;
            }
            let mut sorted: Vec<Field> = named.named.iter().cloned().collect();
            sorted.sort_by(|a, b| field_sort_name(a).cmp(&field_sort_name(b)));
            let trailing = named.named.trailing_punct();
            named.named = sorted.into_iter().collect();
            if trailing && !named.named.trailing_punct() {
                named.named.push_punct(syn::token::Comma::default());
            }
        }
    }

    fn process_impl_items(&mut self, items: &mut Vec<ImplItem>) {
        if items.is_empty() {
            return;
        }
        match &self.config.declaration_normalization {
            DeclarationNormalizationMode::None => return,
            DeclarationNormalizationMode::ByName => {
                items.sort_by(|a, b| impl_item_sort_name(a).cmp(&impl_item_sort_name(b)));
            },
            DeclarationNormalizationMode::Auto | DeclarationNormalizationMode::ByCategory(_) => {
                items.sort_by(|a, b| {
                    let cat_a = sub_item_category_rank(&impl_item_category(a));
                    let cat_b = sub_item_category_rank(&impl_item_category(b));
                    cat_a.cmp(&cat_b).then_with(|| {
                        impl_item_sort_name(a).cmp(&impl_item_sort_name(b))
                    })
                });
            },
        }
    }

    fn process_items(&mut self, items: &mut Vec<Item>) {
        if items.is_empty() {
            return;
        }

        // Remove //! inner doc comments from the first item's whitespace before sorting
        let first_key = HashLineColumn(items[0].span().start());
        let mut inner_docs = Vec::new();
        if let Some((_, ws_list)) = self.whitespaces.get_mut(&first_key) {
            let mut rest = Vec::new();
            for ws in ws_list.drain(..) {
                match &ws.mode {
                    WhitespaceMode::Comment(c) if c.mode == CommentMode::DocInner => {
                        inner_docs.push(ws);
                    },
                    _ => {
                        rest.push(ws);
                    },
                }
            }
            *ws_list = rest;
        }
        match &self.config.declaration_normalization {
            DeclarationNormalizationMode::None => { },
            DeclarationNormalizationMode::ByName => {
                self.sort_by_name(items);
            },
            DeclarationNormalizationMode::Auto | DeclarationNormalizationMode::ByCategory(_) => {
                self.sort_by_category(items);
            },
        }

        // Re-add //! inner doc comments to the (possibly new) first item
        if !inner_docs.is_empty() {
            let new_first_key = HashLineColumn(items[0].span().start());
            let entry = self.whitespaces.entry(new_first_key).or_insert_with(|| (1, Vec::new()));
            inner_docs.extend(entry.1.drain(..));
            entry.1 = inner_docs;
        }
    }

    fn process_trait_items(&mut self, items: &mut Vec<TraitItem>) {
        if items.is_empty() {
            return;
        }
        match &self.config.declaration_normalization {
            DeclarationNormalizationMode::None => return,
            DeclarationNormalizationMode::ByName => {
                items.sort_by(|a, b| trait_item_sort_name(a).cmp(&trait_item_sort_name(b)));
            },
            DeclarationNormalizationMode::Auto | DeclarationNormalizationMode::ByCategory(_) => {
                items.sort_by(|a, b| {
                    let cat_a = sub_item_category_rank(&trait_item_category(a));
                    let cat_b = sub_item_category_rank(&trait_item_category(b));
                    cat_a.cmp(&cat_b).then_with(|| {
                        trait_item_sort_name(a).cmp(&trait_item_sort_name(b))
                    })
                });
            },
        }
    }

    fn process_variants(
        &mut self,
        loc: LineColumn,
        keyword_loc: LineColumn,
        attrs: &[syn::Attribute],
        variants: &mut syn::punctuated::Punctuated<Variant, syn::token::Comma>,
    ) {
        if variants.is_empty() {
            return;
        }
        if matches!(self.config.declaration_normalization, DeclarationNormalizationMode::None) {
            return;
        }
        if is_decl_layout_significant(self.whitespaces, loc, keyword_loc, attrs) {
            return;
        }
        let mut sorted: Vec<Variant> = variants.iter().cloned().collect();
        sorted.sort_by(|a, b| variant_sort_name(a).cmp(&variant_sort_name(b)));
        let trailing = variants.trailing_punct();
        *variants = sorted.into_iter().collect();
        if trailing && !variants.trailing_punct() {
            variants.push_punct(syn::token::Comma::default());
        }
    }

    fn sort_by_category(&mut self, items: &mut Vec<Item>) {
        let category_order = resolve_category_order(self.config);
        let mut macro_use_items: Vec<Item> = Vec::new();
        let mut trailing_items: Vec<Item> = Vec::new();
        let mut categorized: BTreeMap<usize, Vec<Item>> = BTreeMap::new();
        let mut all_impls: Vec<Item> = Vec::new();
        for item in items.drain(..) {
            match item_kind(&item) {
                ItemKind::Trailing => trailing_items.push(item),
                ItemKind::Impl => all_impls.push(item),
                ItemKind::Category(cat) => {
                    if has_macro_use(&item) {
                        macro_use_items.push(item);
                    } else {
                        let rank = category_rank(&cat, &category_order);
                        categorized.entry(rank).or_default().push(item);
                    }
                },
            }
        }
        let (local_type_names, local_trait_names) =
            collect_local_names(categorized.values().flat_map(|g| g.iter()));
        for group in categorized.values_mut() {
            group.sort_by(|a, b| item_sort_name(a).cmp(&item_sort_name(b)));
        }
        let (mut type_impls, mut trait_impls, leftover_impls) =
            classify_impls(all_impls, &local_type_names, &local_trait_names);
        items.extend(macro_use_items);
        for (_, group) in &categorized {
            for item in group {
                let kind = item_kind(item);
                if kind.is_data_type() {
                    self.emit_with_type_impls(item, &mut type_impls, items);
                } else if kind.is_trait_def() {
                    self.emit_with_trait_impls(item, &mut trait_impls, items);
                } else {
                    items.push(item.clone());
                }
            }
        }
        let mut remaining_impls: Vec<Item> = Vec::new();
        for (_, impls) in type_impls {
            remaining_impls.extend(impls);
        }
        for (_, impls) in trait_impls {
            remaining_impls.extend(impls);
        }
        remaining_impls.extend(leftover_impls);
        if !remaining_impls.is_empty() {
            remaining_impls.sort_by(|a, b| item_sort_name(a).cmp(&item_sort_name(b)));
            items.extend(remaining_impls);
        }
        items.extend(trailing_items);
    }

    fn sort_by_name(&mut self, items: &mut Vec<Item>) {
        let mut macro_use_items: Vec<Item> = Vec::new();
        let mut uses: Vec<Item> = Vec::new();
        let mut rest: Vec<Item> = Vec::new();
        for item in items.drain(..) {
            if has_macro_use(&item) {
                macro_use_items.push(item);
            } else if matches!(item_kind(&item), ItemKind::Category(DeclarationNormalizationCategory::Use)) {
                uses.push(item);
            } else {
                rest.push(item);
            }
        }
        uses.sort_by(|a, b| item_sort_name(a).cmp(&item_sort_name(b)));
        let (local_type_names, local_trait_names) = collect_local_names(rest.iter());
        let mut non_impls: Vec<Item> = Vec::new();
        let mut all_impls: Vec<Item> = Vec::new();
        for item in rest.drain(..) {
            if matches!(item_kind(&item), ItemKind::Impl) {
                all_impls.push(item);
            } else {
                non_impls.push(item);
            }
        }
        non_impls.sort_by(|a, b| item_sort_name(a).cmp(&item_sort_name(b)));
        let (mut type_impls, mut trait_impls, leftover_impls) =
            classify_impls(all_impls, &local_type_names, &local_trait_names);
        items.extend(macro_use_items);
        items.extend(uses);
        for item in &non_impls {
            let kind = item_kind(item);
            if kind.is_data_type() {
                self.emit_with_type_impls(item, &mut type_impls, items);
            } else if kind.is_trait_def() {
                self.emit_with_trait_impls(item, &mut trait_impls, items);
            } else {
                items.push(item.clone());
            }
        }

        // Append any remaining impls
        let mut remaining_impls: Vec<Item> = Vec::new();
        for (_, impls) in type_impls {
            remaining_impls.extend(impls);
        }
        for (_, impls) in trait_impls {
            remaining_impls.extend(impls);
        }
        remaining_impls.extend(leftover_impls);
        remaining_impls.sort_by(|a, b| item_sort_name(a).cmp(&item_sort_name(b)));
        items.extend(remaining_impls);
    }
}

impl<'a> VisitMut for DeclarationNormalizer<'a> {
    fn visit_file_mut(&mut self, i: &mut File) {
        self.process_items(&mut i.items);
        syn::visit_mut::visit_file_mut(self, i);
    }

    fn visit_item_enum_mut(&mut self, i: &mut ItemEnum) {
        self.process_variants(i.span().start(), i.enum_token.span.start(), &i.attrs, &mut i.variants);
        syn::visit_mut::visit_item_enum_mut(self, i);
    }

    fn visit_item_impl_mut(&mut self, i: &mut ItemImpl) {
        self.process_impl_items(&mut i.items);
        syn::visit_mut::visit_item_impl_mut(self, i);
    }

    fn visit_item_mod_mut(&mut self, i: &mut ItemMod) {
        if let Some((_, items)) = &mut i.content {
            self.process_items(items);
        }
        syn::visit_mut::visit_item_mod_mut(self, i);
    }

    fn visit_item_struct_mut(&mut self, i: &mut ItemStruct) {
        self.process_fields(i.span().start(), i.struct_token.span.start(), &i.attrs, &mut i.fields);
        syn::visit_mut::visit_item_struct_mut(self, i);
    }

    fn visit_item_trait_mut(&mut self, i: &mut ItemTrait) {
        self.process_trait_items(&mut i.items);
        syn::visit_mut::visit_item_trait_mut(self, i);
    }
}

fn extract_base_type_name(ty: &syn::Type) -> String {
    match ty {
        syn::Type::Reference(r) => extract_base_type_name(&r.elem),
        syn::Type::Paren(p) => extract_base_type_name(&p.elem),
        syn::Type::Path(p) => {
            if let Some(seg) = p.path.segments.last() {
                let ident_str = seg.ident.to_string();
                if WRAPPER_TYPES.iter().any(|w| w.eq_ignore_ascii_case(&ident_str)) {
                    if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                        if let Some(syn::GenericArgument::Type(inner)) = args.args.first() {
                            return extract_base_type_name(inner);
                        }
                    }
                }
                ident_str.to_lowercase()
            } else {
                ty.to_token_stream().to_string().to_lowercase()
            }
        },
        _ => ty.to_token_stream().to_string().to_lowercase(),
    }
}

fn field_sort_name(f: &Field) -> String {
    f.ident.as_ref().map(|i| i.to_string().to_lowercase()).unwrap_or_default()
}

fn has_macro_use(item: &Item) -> bool {
    let attrs = match item {
        Item::ExternCrate(i) => &i.attrs,
        Item::Use(i) => &i.attrs,
        Item::Mod(i) => &i.attrs,
        Item::Macro(i) => &i.attrs,
        _ => return false,
    };
    attrs.iter().any(|a| a.path().is_ident("macro_use"))
}

fn impl_item_category(item: &ImplItem) -> SubItemCategory {
    match item {
        ImplItem::Const(_) => SubItemCategory::Const,
        ImplItem::Type(_) => SubItemCategory::Type,
        ImplItem::Fn(_) => SubItemCategory::Fn,
        ImplItem::Macro(_) => SubItemCategory::Macro,
        _ => SubItemCategory::Other,
    }
}

fn impl_item_sort_name(item: &ImplItem) -> String {
    match item {
        ImplItem::Const(c) => c.ident.to_string().to_lowercase(),
        ImplItem::Type(t) => t.ident.to_string().to_lowercase(),
        ImplItem::Fn(f) => f.sig.ident.to_string().to_lowercase(),
        ImplItem::Macro(m) => m.mac.path.to_token_stream().to_string().to_lowercase(),
        ImplItem::Verbatim(v) => v.to_string().to_lowercase(),
        _ => String::new(),
    }
}

fn is_decl_layout_significant(
    whitespaces: &BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)>,
    loc: LineColumn,
    keyword_loc: LineColumn,
    attrs: &[syn::Attribute],
) -> bool {
    const DIRECTIVE: &str = "genemichaels-norm-skip";
    has_directive_comment(whitespaces, loc, DIRECTIVE) ||
        has_directive_comment(whitespaces, keyword_loc, DIRECTIVE) ||
        attrs.iter().any(|attr| {
            if !attr.path().is_ident("repr") {
                return false;
            }
            let list = match &attr.meta {
                syn::Meta::List(list) => list,
                _ => return false,
            };
            list.tokens.clone().into_iter().any(|tt| match tt {
                proc_macro2::TokenTree::Ident(ident) => ident != "align",
                _ => false,
            })
        })
}

#[deny(clippy::wildcard_enum_match_arm)]
fn item_kind(item: &Item) -> ItemKind {
    match item {
        Item::Const(_) | Item::Static(_) => ItemKind::Category(DeclarationNormalizationCategory::Const),
        Item::Enum(_) => ItemKind::Category(DeclarationNormalizationCategory::Enum),
        Item::ExternCrate(_) | Item::Use(_) => ItemKind::Category(DeclarationNormalizationCategory::Use),
        Item::Fn(_) => ItemKind::Category(DeclarationNormalizationCategory::Fn),
        Item::ForeignMod(_) => ItemKind::Category(DeclarationNormalizationCategory::ForeignMod),
        Item::Impl(_) => ItemKind::Impl,
        Item::Macro(m) if m.ident.is_none() => ItemKind::Category(DeclarationNormalizationCategory::MacroCall),
        Item::Macro(_) => ItemKind::Category(DeclarationNormalizationCategory::Macro),
        Item::Mod(_) => ItemKind::Category(DeclarationNormalizationCategory::Mod),
        Item::Struct(_) => ItemKind::Category(DeclarationNormalizationCategory::Struct),
        Item::Trait(_) | Item::TraitAlias(_) => ItemKind::Category(DeclarationNormalizationCategory::Trait),
        Item::Type(_) => ItemKind::Category(DeclarationNormalizationCategory::TypeAlias),
        Item::Union(_) => ItemKind::Category(DeclarationNormalizationCategory::Union),
        Item::Verbatim(_) => ItemKind::Trailing,
        _ => ItemKind::Trailing,
    }
}

fn item_sort_name(item: &Item) -> String {
    match item {
        Item::Mod(m) => m.ident.to_string().to_lowercase(),
        Item::Use(u) => u.tree.to_token_stream().to_string().to_lowercase(),
        Item::Macro(m) => {
            m.ident.as_ref().map(|i| i.to_string().to_lowercase()).unwrap_or_else(|| {
                m.mac.path.to_token_stream().to_string().to_lowercase()
            })
        },
        Item::Const(c) => c.ident.to_string().to_lowercase(),
        Item::Static(s) => s.ident.to_string().to_lowercase(),
        Item::Trait(t) => t.ident.to_string().to_lowercase(),
        Item::TraitAlias(t) => t.ident.to_string().to_lowercase(),
        Item::Fn(f) => f.sig.ident.to_string().to_lowercase(),
        Item::Struct(s) => s.ident.to_string().to_lowercase(),
        Item::Enum(e) => e.ident.to_string().to_lowercase(),
        Item::Union(u) => u.ident.to_string().to_lowercase(),
        Item::Type(t) => t.ident.to_string().to_lowercase(),
        Item::Impl(i) => {
            let type_name = i.self_ty.to_token_stream().to_string().to_lowercase();
            if let Some((_, path, _)) = &i.trait_ {
                format!("{} {}", type_name, path.to_token_stream().to_string().to_lowercase())
            } else {
                type_name
            }
        },
        Item::ExternCrate(e) => e.ident.to_string().to_lowercase(),
        Item::ForeignMod(_) => String::new(),
        Item::Verbatim(v) => v.to_string().to_lowercase(),
        _ => String::new(),
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum ItemKind {
    Category(DeclarationNormalizationCategory),
    Impl,
    Trailing,
}

impl ItemKind {
    fn is_data_type(self) -> bool {
        matches!(
            self,
            ItemKind::Category(
                DeclarationNormalizationCategory::Struct | DeclarationNormalizationCategory::Enum |
                    DeclarationNormalizationCategory::Union |
                    DeclarationNormalizationCategory::TypeAlias
            )
        )
    }

    fn is_trait_def(self) -> bool {
        matches!(self, ItemKind::Category(DeclarationNormalizationCategory::Trait))
    }
}

fn resolve_category_order(config: &FormatConfig) -> BTreeMap<DeclarationNormalizationCategory, usize> {
    let user_order: Vec<DeclarationNormalizationCategory> = match &config.declaration_normalization {
        DeclarationNormalizationMode::ByCategory(order) => order.clone(),
        _ => AUTO_CATEGORY_ORDER.to_vec(),
    };
    let mut ranks: BTreeMap<DeclarationNormalizationCategory, usize> = BTreeMap::new();
    let mut next_rank: usize = 0;
    for cat in user_order.iter().chain(AUTO_CATEGORY_ORDER.iter()) {
        if !ranks.contains_key(cat) {
            ranks.insert(*cat, next_rank);
            next_rank += 1;
        }
    }
    ranks
}

fn sub_item_category_rank(cat: &SubItemCategory) -> usize {
    match cat {
        SubItemCategory::Const => 0,
        SubItemCategory::Type => 1,
        SubItemCategory::Fn => 2,
        SubItemCategory::Macro => 3,
        SubItemCategory::Other => 4,
    }
}

enum SubItemCategory {
    Const,
    Fn,
    Macro,
    Other,
    Type,
}

fn trait_item_category(item: &TraitItem) -> SubItemCategory {
    match item {
        TraitItem::Const(_) => SubItemCategory::Const,
        TraitItem::Type(_) => SubItemCategory::Type,
        TraitItem::Fn(_) => SubItemCategory::Fn,
        TraitItem::Macro(_) => SubItemCategory::Macro,
        _ => SubItemCategory::Other,
    }
}

fn trait_item_sort_name(item: &TraitItem) -> String {
    match item {
        TraitItem::Const(c) => c.ident.to_string().to_lowercase(),
        TraitItem::Type(t) => t.ident.to_string().to_lowercase(),
        TraitItem::Fn(f) => f.sig.ident.to_string().to_lowercase(),
        TraitItem::Macro(m) => m.mac.path.to_token_stream().to_string().to_lowercase(),
        TraitItem::Verbatim(v) => v.to_string().to_lowercase(),
        _ => String::new(),
    }
}

pub(crate) fn validate_declaration_normalization(config: &FormatConfig) -> Result<(), loga::Error> {
    let DeclarationNormalizationMode::ByCategory(order) = &config.declaration_normalization else {
        return Ok(());
    };
    let mut seen: std::collections::HashSet<DeclarationNormalizationCategory> = std::collections::HashSet::new();
    for cat in order {
        if *cat == DeclarationNormalizationCategory::Concrete {
            continue;
        }
        if !seen.insert(*cat) {
            return Err(
                loga::err_with(
                    "Duplicate leaf category in `declaration_normalization.by_category`",
                    loga::ea!(category = format!("{:?}", cat)),
                ),
            );
        }
    }
    Ok(())
}

fn variant_sort_name(v: &Variant) -> String {
    v.ident.to_string().to_lowercase()
}

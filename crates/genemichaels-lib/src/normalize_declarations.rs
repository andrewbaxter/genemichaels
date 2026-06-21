use {
    crate::{
        CommentMode,
        DeclarationNormalizationCategory,
        DeclarationNormalizationMode,
        FormatConfig,
        Whitespace,
        WhitespaceMode,
        whitespace::HashLineColumn,
    },
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

fn category_rank(category: &DeclarationNormalizationCategory, order: &[DeclarationNormalizationCategory]) -> usize {
    order.iter().position(|c| c == category).unwrap_or(order.len())
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

fn collect_local_names(items: &[Item]) -> (Vec<String>, Vec<String>) {
    let local_type_names: Vec<String> = items.iter().filter_map(|item| match item {
        Item::Struct(s) => Some(s.ident.to_string().to_lowercase()),
        Item::Enum(e) => Some(e.ident.to_string().to_lowercase()),
        Item::Union(u) => Some(u.ident.to_string().to_lowercase()),
        Item::Type(t) => Some(t.ident.to_string().to_lowercase()),
        _ => None,
    }).collect();
    let local_trait_names: Vec<String> = items.iter().filter_map(|item| match item {
        Item::Trait(t) => Some(t.ident.to_string().to_lowercase()),
        Item::TraitAlias(t) => Some(t.ident.to_string().to_lowercase()),
        _ => None,
    }).collect();
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
        if is_trait_def(item) {
            let trait_name = item_sort_name(item);
            if let Some(impls) = trait_impls.remove(&trait_name) {
                out.extend(impls);
            }
        }
    }

    fn emit_with_type_impls(&self, item: &Item, type_impls: &mut BTreeMap<String, Vec<Item>>, out: &mut Vec<Item>) {
        out.push(item.clone());
        if is_data_type(item) {
            let type_name = item_sort_name(item);
            if let Some(impls) = type_impls.remove(&type_name) {
                out.extend(impls);
            }
        }
    }

    fn process_fields(&mut self, fields: &mut syn::Fields) {
        if let syn::Fields::Named(named) = fields {
            if named.named.is_empty() {
                return;
            }
            if matches!(self.config.declaration_normalization, DeclarationNormalizationMode::None) {
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

    fn process_variants(&mut self, variants: &mut syn::punctuated::Punctuated<Variant, syn::token::Comma>) {
        if variants.is_empty() {
            return;
        }
        if matches!(self.config.declaration_normalization, DeclarationNormalizationMode::None) {
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

        // Extract #[macro_use] items first, preserving their original order
        let mut macro_use_items: Vec<Item> = Vec::new();
        let mut remaining: Vec<Item> = Vec::new();
        for item in items.drain(..) {
            if has_macro_use(&item) {
                macro_use_items.push(item);
            } else {
                remaining.push(item);
            }
        }

        // Collect local type and trait names, then separate impls
        let (local_type_names, local_trait_names) = collect_local_names(&remaining);
        let mut categorized: BTreeMap<usize, Vec<Item>> = BTreeMap::new();
        let mut all_impls: Vec<Item> = Vec::new();
        for item in remaining.drain(..) {
            if let Item::Impl(_) = &item {
                all_impls.push(item);
            } else {
                let cat = item_category_with_macro_call(&item);
                let rank = category_rank(&cat, &category_order);
                categorized.entry(rank).or_default().push(item);
            }
        }

        // Sort within each category by name
        for group in categorized.values_mut() {
            group.sort_by(|a, b| item_sort_name(a).cmp(&item_sort_name(b)));
        }

        // Classify impls into type-attached, trait-attached, and leftover
        let (mut type_impls, mut trait_impls, leftover_impls) =
            classify_impls(all_impls, &local_type_names, &local_trait_names);

        // Build final list
        items.extend(macro_use_items);
        let concrete_rank = category_rank(&DeclarationNormalizationCategory::Concrete, &category_order);
        let trait_rank = category_rank(&DeclarationNormalizationCategory::Trait, &category_order);
        for (rank, group) in &categorized {
            if *rank == concrete_rank {
                for item in group {
                    self.emit_with_type_impls(item, &mut type_impls, items);
                }
            } else if *rank == trait_rank {
                for item in group {
                    self.emit_with_trait_impls(item, &mut trait_impls, items);
                }
            } else {
                items.extend(group.iter().cloned());
            }
        }

        // Collect any remaining impls (unmatched type/trait impls + leftovers)
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
            if !categorized.contains_key(&concrete_rank) {
                // Insert at the proper position for the concrete category
                let mut pos = 0;
                for (rank, group) in &categorized {
                    if *rank > concrete_rank {
                        break;
                    }
                    pos += group.len();
                }

                // Account for macro_use items at the start
                pos += items.len() - categorized.values().map(|g| g.len()).sum::<usize>();
                for imp in remaining_impls.into_iter().rev() {
                    items.insert(pos, imp);
                }
            } else {
                items.extend(remaining_impls);
            }
        }
    }

    fn sort_by_name(&mut self, items: &mut Vec<Item>) {
        // Extract #[macro_use] items first, preserving their original order
        let mut macro_use_items: Vec<Item> = Vec::new();
        let mut uses: Vec<Item> = Vec::new();
        let mut rest: Vec<Item> = Vec::new();
        for item in items.drain(..) {
            if has_macro_use(&item) {
                macro_use_items.push(item);
            } else {
                match &item {
                    Item::Use(_) | Item::ExternCrate(_) => uses.push(item),
                    _ => rest.push(item),
                }
            }
        }
        uses.sort_by(|a, b| item_sort_name(a).cmp(&item_sort_name(b)));

        // Separate impls from non-impl items
        let (local_type_names, local_trait_names) = collect_local_names(&rest);
        let mut non_impls: Vec<Item> = Vec::new();
        let mut all_impls: Vec<Item> = Vec::new();
        for item in rest.drain(..) {
            if let Item::Impl(_) = &item {
                all_impls.push(item);
            } else {
                non_impls.push(item);
            }
        }
        non_impls.sort_by(|a, b| item_sort_name(a).cmp(&item_sort_name(b)));

        // Classify impls
        let (mut type_impls, mut trait_impls, leftover_impls) =
            classify_impls(all_impls, &local_type_names, &local_trait_names);

        // Build final list: macro_use, uses, then non-impl items with impls interleaved
        items.extend(macro_use_items);
        items.extend(uses);
        for item in &non_impls {
            if is_data_type(item) {
                self.emit_with_type_impls(item, &mut type_impls, items);
            } else if is_trait_def(item) {
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
        self.process_variants(&mut i.variants);
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
        self.process_fields(&mut i.fields);
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

fn is_data_type(item: &Item) -> bool {
    matches!(item, Item::Struct(_) | Item::Enum(_) | Item::Union(_) | Item::Type(_))
}

fn is_macro_call(item: &Item) -> bool {
    match item {
        Item::Macro(m) => m.ident.is_none(),
        _ => false,
    }
}

fn is_trait_def(item: &Item) -> bool {
    matches!(item, Item::Trait(_) | Item::TraitAlias(_))
}

fn item_category(item: &Item) -> DeclarationNormalizationCategory {
    match item {
        Item::Mod(_) => DeclarationNormalizationCategory::Mod,
        Item::Use(_) => DeclarationNormalizationCategory::Use,
        Item::Macro(_) => DeclarationNormalizationCategory::Macro,
        Item::Const(_) | Item::Static(_) => DeclarationNormalizationCategory::Const,
        Item::Trait(_) | Item::TraitAlias(_) => DeclarationNormalizationCategory::Trait,
        Item::Impl(_) => DeclarationNormalizationCategory::Concrete,
        Item::Fn(_) | Item::Struct(_) | Item::Enum(_) | Item::Union(_) | Item::Type(_) => {
            DeclarationNormalizationCategory::Concrete
        },
        Item::ExternCrate(_) => DeclarationNormalizationCategory::Use,
        Item::ForeignMod(_) => DeclarationNormalizationCategory::Concrete,
        Item::Verbatim(_) => DeclarationNormalizationCategory::Concrete,
        _ => DeclarationNormalizationCategory::Concrete,
    }
}

fn item_category_with_macro_call(item: &Item) -> DeclarationNormalizationCategory {
    if is_macro_call(item) {
        return DeclarationNormalizationCategory::MacroCall;
    }
    item_category(item)
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

fn resolve_category_order(config: &FormatConfig) -> Vec<DeclarationNormalizationCategory> {
    match &config.declaration_normalization {
        DeclarationNormalizationMode::Auto => AUTO_CATEGORY_ORDER.to_vec(),
        DeclarationNormalizationMode::ByCategory(user_order) => {
            let mut order = user_order.clone();
            for cat in AUTO_CATEGORY_ORDER {
                if !order.contains(cat) {
                    order.push(*cat);
                }
            }
            order
        },
        _ => AUTO_CATEGORY_ORDER.to_vec(),
    }
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

fn variant_sort_name(v: &Variant) -> String {
    v.ident.to_string().to_lowercase()
}

use {
    crate::{
        DeclarationNormalizationCategory,
        DeclarationNormalizationMode,
        FormatConfig,
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

fn category_rank(category: &DeclarationNormalizationCategory, order: &[DeclarationNormalizationCategory]) -> usize {
    order.iter().position(|c| c == category).unwrap_or(order.len())
}

pub(crate) struct DeclarationNormalizer<'a> {
    pub(crate) config: &'a FormatConfig,
}

fn field_sort_name(f: &Field) -> String {
    f.ident.as_ref().map(|i| i.to_string().to_lowercase()).unwrap_or_default()
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

fn is_impl_for_type(impl_item: &syn::ItemImpl, type_name: &str) -> bool {
    impl_item.self_ty.to_token_stream().to_string().to_lowercase() == type_name
}

fn is_macro_call(item: &Item) -> bool {
    match item {
        Item::Macro(m) => m.ident.is_none(),
        _ => false,
    }
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

impl<'a> DeclarationNormalizer<'a> {
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
        match &self.config.declaration_normalization {
            DeclarationNormalizationMode::None => return,
            DeclarationNormalizationMode::ByName => {
                self.sort_by_name(items);
            },
            DeclarationNormalizationMode::Auto | DeclarationNormalizationMode::ByCategory(_) => {
                self.sort_by_category(items);
            },
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

        // Group items by category, keeping impls attached to their concrete types
        let mut categorized: BTreeMap<usize, Vec<Item>> = BTreeMap::new();
        let mut impl_items: Vec<Item> = Vec::new();
        for item in items.drain(..) {
            if let Item::Impl(_) = &item {
                impl_items.push(item);
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

        // Sort impls by their type name
        impl_items.sort_by(|a, b| item_sort_name(a).cmp(&item_sort_name(b)));

        // Build final list. For "concrete" category, interleave impls after their types.
        let concrete_rank = category_rank(&DeclarationNormalizationCategory::Concrete, &category_order);
        for (rank, group) in &categorized {
            if *rank == concrete_rank {
                // Interleave: emit each concrete item followed by its impls
                for item in group {
                    items.push(item.clone());
                    let type_name = item_sort_name(item);
                    let mut i = 0;
                    while i < impl_items.len() {
                        if let Item::Impl(ref impl_item) = impl_items[i] {
                            if is_impl_for_type(impl_item, &type_name) {
                                items.push(impl_items.remove(i));
                                continue;
                            }
                        }
                        i += 1;
                    }
                }
            } else {
                items.extend(group.iter().cloned());
            }
        }

        // If there were no concrete items in the categorized list, or leftover impls
        if !impl_items.is_empty() {
            if !categorized.contains_key(&concrete_rank) {
                // Insert concrete category at proper position
                let insert_pos = items.len();
                let mut pos = 0;
                for (rank, group) in &categorized {
                    if *rank > concrete_rank {
                        break;
                    }
                    pos += group.len();
                }
                let _ = insert_pos;

                // Just append remaining impls sorted
                for imp in impl_items {
                    items.insert(pos, imp);
                    pos += 1;
                }
            } else {
                // Append any remaining impls at the end of the concrete section Find where
                // concrete items end
                items.extend(impl_items);
            }
        }
    }

    fn sort_by_name(&mut self, items: &mut Vec<Item>) {
        // Partition: use items first, then everything else sorted by name
        let mut uses: Vec<Item> = Vec::new();
        let mut rest: Vec<Item> = Vec::new();
        for item in items.drain(..) {
            match &item {
                Item::Use(_) | Item::ExternCrate(_) => uses.push(item),
                _ => rest.push(item),
            }
        }
        uses.sort_by(|a, b| item_sort_name(a).cmp(&item_sort_name(b)));
        rest.sort_by(|a, b| item_sort_name(a).cmp(&item_sort_name(b)));
        items.extend(uses);
        items.extend(rest);
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

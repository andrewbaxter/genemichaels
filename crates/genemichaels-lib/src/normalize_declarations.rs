use {
    crate::{
        DeclarationSortCategory,
        DeclarationSortMode,
        FormatConfig,
    },
    std::collections::BTreeMap,
    syn::{
        File,
        Item,
        ItemMod,
        visit_mut::VisitMut,
    },
};

const AUTO_CATEGORY_ORDER: &[DeclarationSortCategory] =
    &[
        DeclarationSortCategory::Mod,
        DeclarationSortCategory::Use,
        DeclarationSortCategory::Macro,
        DeclarationSortCategory::MacroCall,
        DeclarationSortCategory::Const,
        DeclarationSortCategory::Trait,
        DeclarationSortCategory::Concrete,
    ];

fn item_category(item: &Item) -> DeclarationSortCategory {
    match item {
        Item::Mod(_) => DeclarationSortCategory::Mod,
        Item::Use(_) => DeclarationSortCategory::Use,
        Item::Macro(_) => DeclarationSortCategory::Macro,
        Item::Const(_) | Item::Static(_) => DeclarationSortCategory::Const,
        Item::Trait(_) | Item::TraitAlias(_) => DeclarationSortCategory::Trait,
        Item::Impl(_) => DeclarationSortCategory::Concrete,
        Item::Fn(_) | Item::Struct(_) | Item::Enum(_) | Item::Union(_) | Item::Type(_) => {
            DeclarationSortCategory::Concrete
        },
        Item::ExternCrate(_) => DeclarationSortCategory::Use,
        Item::ForeignMod(_) => DeclarationSortCategory::Concrete,
        Item::Verbatim(_) => DeclarationSortCategory::Concrete,
        _ => DeclarationSortCategory::Concrete,
    }
}

fn is_macro_call(item: &Item) -> bool {
    match item {
        Item::Macro(m) => m.ident.is_none(),
        _ => false,
    }
}

fn item_category_with_macro_call(item: &Item) -> DeclarationSortCategory {
    if is_macro_call(item) {
        return DeclarationSortCategory::MacroCall;
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

use quote::ToTokens;

fn category_rank(category: &DeclarationSortCategory, order: &[DeclarationSortCategory]) -> usize {
    order.iter().position(|c| c == category).unwrap_or(order.len())
}

fn resolve_category_order(config: &FormatConfig) -> Vec<DeclarationSortCategory> {
    match &config.declaration_sort {
        DeclarationSortMode::Auto => AUTO_CATEGORY_ORDER.to_vec(),
        DeclarationSortMode::ByCategory(user_order) => {
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

fn is_impl_for_type(impl_item: &syn::ItemImpl, type_name: &str) -> bool {
    impl_item.self_ty.to_token_stream().to_string().to_lowercase() == type_name
}

pub(crate) struct DeclarationNormalizer<'a> {
    pub(crate) config: &'a FormatConfig,
}

impl<'a> DeclarationNormalizer<'a> {
    fn process_items(&mut self, items: &mut Vec<Item>) {
        if items.is_empty() {
            return;
        }
        match &self.config.declaration_sort {
            DeclarationSortMode::None => return,
            DeclarationSortMode::ByName => {
                self.sort_by_name(items);
            },
            DeclarationSortMode::Auto | DeclarationSortMode::ByCategory(_) => {
                self.sort_by_category(items);
            },
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
        let concrete_rank = category_rank(&DeclarationSortCategory::Concrete, &category_order);
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
}

impl<'a> VisitMut for DeclarationNormalizer<'a> {
    fn visit_file_mut(&mut self, i: &mut File) {
        self.process_items(&mut i.items);
        syn::visit_mut::visit_file_mut(self, i);
    }

    fn visit_item_mod_mut(&mut self, i: &mut ItemMod) {
        if let Some((_, items)) = &mut i.content {
            self.process_items(items);
        }
        syn::visit_mut::visit_item_mod_mut(self, i);
    }
}

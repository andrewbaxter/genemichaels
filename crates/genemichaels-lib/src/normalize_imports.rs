use std::collections::BTreeMap;
use crate::{
    FormatConfig,
    ImportNormalizationMode,
};
use crate::{
    Whitespace,
    WhitespaceMode,
};
use crate::whitespace::HashLineColumn;
use syn::{
    Block,
    File,
    Ident,
    Item,
    ItemMod,
    ItemUse,
    Stmt,
    UseName,
    UsePath,
    UseTree,
};
use syn::spanned::Spanned;
use syn::visit_mut::VisitMut;
use quote::ToTokens;

fn cmp_use_tree(a: &syn::UseTree, b: &syn::UseTree) -> std::cmp::Ordering {
    fn get_rank(tree: &syn::UseTree) -> u8 {
        match tree {
            syn::UseTree::Name(_) | syn::UseTree::Path(_) | syn::UseTree::Rename(_) => 0,
            syn::UseTree::Group(_) => 1,
            syn::UseTree::Glob(_) => 2,
        }
    }

    fn get_sub_primary_ident(tree: &syn::UseTree) -> &Ident {
        match tree {
            UseTree::Path(t) => &t.ident,
            UseTree::Name(t) => &t.ident,
            UseTree::Rename(t) => &t.ident,
            UseTree::Glob(_) => unreachable!(),
            UseTree::Group(_) => unreachable!(),
        }
    }

    fn get_subrank(tree: &syn::UseTree) -> u8 {
        match tree {
            syn::UseTree::Name(_) => 0,
            syn::UseTree::Path(_) => 1,
            syn::UseTree::Rename(_) => 2,
            syn::UseTree::Group(_) => unreachable!(),
            syn::UseTree::Glob(_) => unreachable!(),
        }
    }

    let rank_a = get_rank(a);
    let rank_b = get_rank(b);
    if rank_a != rank_b {
        return rank_a.cmp(&rank_b);
    }
    match (a, b) {
        (syn::UseTree::Name(n_a), b) => match n_a.ident.cmp(get_sub_primary_ident(b)) {
            std::cmp::Ordering::Equal => get_subrank(a).cmp(&get_subrank(b)),
            notequal => notequal,
        },
        (syn::UseTree::Path(p_a), b) => match p_a.ident.cmp(get_sub_primary_ident(b)) {
            std::cmp::Ordering::Equal => get_subrank(a).cmp(&get_subrank(b)),
            notequal => notequal,
        },
        (syn::UseTree::Rename(r_a), b) => match r_a.ident.cmp(get_sub_primary_ident(b)) {
            std::cmp::Ordering::Equal => if let syn::UseTree::Rename(r_b) = b {
                r_a.rename.cmp(&r_b.rename)
            } else {
                get_subrank(a).cmp(&get_subrank(b))
            },
            notequal => notequal,
        },
        (syn::UseTree::Group(_), syn::UseTree::Group(_)) => std::cmp::Ordering::Equal,
        (syn::UseTree::Glob(_), syn::UseTree::Glob(_)) => std::cmp::Ordering::Equal,
        _ => unreachable!(),
    }
}

fn cmp_key_for_import_node(
    item: &ItemUse,
    whitespaces: &BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)>,
) -> (String, String, String) {
    let get_ws = |span: proc_macro2::Span| {
        let mut s = String::new();
        if let Some((_, ws)) = whitespaces.get(&HashLineColumn(span.start())) {
            for w in ws.iter() {
                match &w.mode {
                    WhitespaceMode::Comment(c) => {
                        s.push_str(&c.lines);
                    },
                    WhitespaceMode::BlankLines(l) => {
                        s.push_str(&l.to_string());
                    },
                }
            }
        }
        s
    };

    fn get_ts_ws(
        ts: proc_macro2::TokenStream,
        whitespaces: &BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)>,
        get_ws: &impl Fn(proc_macro2::Span) -> String,
    ) -> String {
        let mut s = String::new();
        for tt in ts {
            match tt {
                proc_macro2::TokenTree::Group(g) => {
                    s.push_str(&get_ws(g.span_open()));
                    s.push_str(&get_ts_ws(g.stream(), whitespaces, get_ws));
                    s.push_str(&get_ws(g.span_close()));
                },
                _ => {
                    s.push_str(&get_ws(tt.span()));
                },
            }
        }
        s
    }

    let mut key = String::new();
    key.push_str(&item.vis.to_token_stream().to_string());
    for attr in &item.attrs {
        key.push_str(&attr.to_token_stream().to_string());
        key.push_str(&get_ts_ws(attr.to_token_stream(), whitespaces, &get_ws));
    }
    return (key, get_ws(item.use_token.span).to_string(), get_ws(item.semi_token.span).to_string());
}

fn process_item_uses(
    imports: &mut Vec<ItemUse>,
    new_items: &mut Vec<ItemUse>,
    config: &FormatConfig,
    whitespaces: &mut BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)>,
) {
    match config.import_normalization {
        ImportNormalizationMode::None => {
            new_items.extend(imports.drain(..));
        },
        ImportNormalizationMode::Combine => {
            // Group, separated by attrs like `#[cfg]` (so that items with different cfg
            // aren't merged)
            let mut groups: Vec<Vec<ItemUse>> = Vec::new();
            for item in imports.drain(..) {
                if let Some(last_group) = groups.last_mut() {
                    if cmp_key_for_import_node(&last_group[0], whitespaces) ==
                        cmp_key_for_import_node(&item, whitespaces) {
                        // Same attrs, combine
                        last_group.push(item);
                    } else {
                        // Different attrs, start new group
                        groups.push(vec![item]);
                    }
                } else {
                    groups.push(vec![item]);
                }
            }

            // Create a `use` tree per group
            for mut subgroup in groups {
                if subgroup.len() == 1 {
                    new_items.push(subgroup.pop().unwrap());
                    continue;
                }

                // Move the `use`/`;` whitespace onto the first child to avoid losing it
                for item in subgroup.iter() {
                    let tree_start = HashLineColumn(item.tree.span().start());
                    for span in [item.use_token.span, item.semi_token.span] {
                        let hl = HashLineColumn(span.start());
                        if hl == tree_start {
                            continue;
                        }
                        if let Some(ws) = whitespaces.remove(&hl) {
                            whitespaces.entry(tree_start).or_insert((0, Vec::new())).1.extend(ws.1);
                        }
                    }
                }
                let template = subgroup[0].clone();

                // Identify/split apart all leaf imports
                #[derive(Clone)]
                struct ImportLeaf {
                    path: Vec<syn::UsePath>,
                    leaf: syn::UseTree,
                }

                fn collect_leaves(tree: UseTree, current_path: &mut Vec<syn::UsePath>, out: &mut Vec<ImportLeaf>) {
                    match tree {
                        UseTree::Path(p) => {
                            let inner = *p.tree.clone();
                            let mut p_shallow = p.clone();
                            p_shallow.tree =
                                Box::new(
                                    UseTree::Name(
                                        UseName { ident: syn::Ident::new("dummy", proc_macro2::Span::call_site()) },
                                    ),
                                );
                            current_path.push(p_shallow);
                            collect_leaves(inner, current_path, out);
                            current_path.pop();
                        },
                        UseTree::Group(g) => {
                            for item in g.items {
                                collect_leaves(item, current_path, out);
                            }
                        },
                        UseTree::Name(_) | UseTree::Rename(_) | UseTree::Glob(_) => {
                            out.push(ImportLeaf {
                                path: current_path.clone(),
                                leaf: tree,
                            });
                        },
                    }
                }

                let mut all_leaves = Vec::new();
                for item in &subgroup {
                    collect_leaves(item.tree.clone(), &mut Vec::new(), &mut all_leaves);
                }

                // Recombine into trees
                fn group_leaves(
                    source_leaves: Vec<ImportLeaf>,
                    depth: usize,
                    whitespaces: &BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)>,
                ) -> Vec<syn::UseTree> {
                    // Split leaves into: leaves at this level, or re-group by either ident or comment
                    // location for ones with comments
                    let mut items = Vec::new();
                    let mut nested: BTreeMap<String, (syn::UsePath, Vec<ImportLeaf>)> = BTreeMap::new();
                    let mut nested_unmergeable: BTreeMap<HashLineColumn, (syn::UsePath, Vec<ImportLeaf>)> =
                        BTreeMap::new();
                    for source_leaf in source_leaves {
                        if depth == source_leaf.path.len() {
                            items.push(source_leaf.leaf);
                        } else {
                            fn has_whitespace(
                                span: proc_macro2::Span,
                                whitespaces: &BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)>,
                            ) -> bool {
                                if let Some((_, ws)) = whitespaces.get(&HashLineColumn(span.start())) {
                                    return !ws.is_empty();
                                }
                                false
                            }

                            let p = &source_leaf.path[depth];
                            if has_whitespace(p.ident.span(), whitespaces) ||
                                has_whitespace(p.colon2_token.spans[0], whitespaces) {
                                nested_unmergeable
                                    .entry(HashLineColumn(p.ident.span().start()))
                                    .or_insert_with(|| (p.clone(), Vec::new()))
                                    .1
                                    .push(source_leaf);
                            } else {
                                nested
                                    .entry(p.ident.to_string())
                                    .or_insert_with(|| (p.clone(), Vec::new()))
                                    .1
                                    .push(source_leaf);
                            }
                        }
                    }

                    // Recurse the groups to build subtrees
                    for (mut path_node, group_flats) in Iterator::chain(nested_unmergeable.into_values(), nested.into_values()) {
                        let mut child_trees = group_leaves(group_flats, depth + 1, whitespaces);
                        if child_trees.len() == 1 {
                            path_node.tree = Box::new(child_trees.pop().unwrap());
                            items.push(syn::UseTree::Path(path_node));
                        } else {
                            let group = syn::UseGroup {
                                brace_token: Default::default(),
                                items: child_trees.into_iter().collect(),
                            };
                            path_node.tree = Box::new(syn::UseTree::Group(group));
                            items.push(syn::UseTree::Path(path_node));
                        }
                    }

                    // Sort and done
                    items.sort_by(|a, b| cmp_use_tree(a, b));
                    return items;
                }

                let new_trees = group_leaves(all_leaves, 0, whitespaces);

                // If one tree, place directly, otherwise group
                if new_trees.len() == 1 {
                    let mut new_item = template.clone();
                    new_item.tree = new_trees.into_iter().next().unwrap();
                    new_items.push(new_item);
                } else if !new_trees.is_empty() {
                    let mut new_item = template.clone();
                    new_item.tree = syn::UseTree::Group(syn::UseGroup {
                        brace_token: Default::default(),
                        items: new_trees.into_iter().collect(),
                    });
                    new_items.push(new_item);
                }
            }
        },
        ImportNormalizationMode::Split => {
            // Recursively gather all leaf imports
            #[derive(Clone)]
            struct ImportLeaf {
                path: Vec<syn::Ident>,
                leaf: syn::UseTree,
            }

            impl PartialEq for ImportLeaf {
                fn eq(&self, other: &Self) -> bool {
                    self.cmp(other) == std::cmp::Ordering::Equal
                }
            }

            impl Eq for ImportLeaf { }

            impl PartialOrd for ImportLeaf {
                fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                    Some(self.cmp(other))
                }
            }

            impl Ord for ImportLeaf {
                fn cmp(&self, other: &Self) -> std::cmp::Ordering {
                    match self.path.cmp(&other.path) {
                        std::cmp::Ordering::Equal => cmp_use_tree(&self.leaf, &other.leaf),
                        o => o,
                    }
                }
            }

            fn collect_leaves(tree: UseTree, current_path: &mut Vec<syn::Ident>, out: &mut Vec<ImportLeaf>) {
                match tree {
                    UseTree::Path(p) => {
                        current_path.push(p.ident);
                        collect_leaves(*p.tree, current_path, out);
                        current_path.pop();
                    },
                    UseTree::Name(n) => {
                        out.push(ImportLeaf {
                            path: current_path.clone(),
                            leaf: UseTree::Name(n),
                        });
                    },
                    UseTree::Rename(r) => {
                        out.push(ImportLeaf {
                            path: current_path.clone(),
                            leaf: UseTree::Rename(r),
                        });
                    },
                    UseTree::Glob(g) => {
                        out.push(ImportLeaf {
                            path: current_path.clone(),
                            leaf: UseTree::Glob(g),
                        });
                    },
                    UseTree::Group(g) => {
                        for item in g.items {
                            collect_leaves(item, current_path, out);
                        }
                    },
                }
            }

            let mut all_leaves = Vec::new();
            for item in imports.drain(..) {
                let mut leaves = Vec::new();
                collect_leaves(item.tree.clone(), &mut Vec::new(), &mut leaves);
                for l in leaves {
                    all_leaves.push((l, item.clone()));
                }
                whitespaces.entry(HashLineColumn(item.use_token.span.start())).and_modify(|e| e.0 -= 1);
                whitespaces.entry(HashLineColumn(item.semi_token.span.start())).and_modify(|e| e.0 -= 1);
                for token in item.tree.to_token_stream() {
                    whitespaces.remove(&HashLineColumn(token.span().start()));
                }
            }

            // Sort
            all_leaves.sort_by(|(f_a, _), (f_b, _)| f_a.cmp(f_b));

            // Re-constitute the split items into actual syn ast nodes
            for (f, root_item) in all_leaves {
                let mut tree = f.leaf;
                for ident in f.path.into_iter().rev() {
                    tree = UseTree::Path(UsePath {
                        ident: ident,
                        colon2_token: Default::default(),
                        tree: Box::new(tree),
                    });
                }
                let mut new_item = root_item;
                whitespaces.entry(HashLineColumn(new_item.use_token.span.start())).and_modify(|e| e.0 += 1);
                whitespaces.entry(HashLineColumn(new_item.semi_token.span.start())).and_modify(|e| e.0 += 1);
                new_item.tree = tree;
                new_items.push(new_item);
            }
        },
    }
}

pub(crate) struct ImportNormalizer<'a> {
    pub(crate) config: &'a FormatConfig,
    pub(crate) whitespaces: &'a mut BTreeMap<HashLineColumn, (usize, Vec<Whitespace>)>,
}

impl<'a> ImportNormalizer<'a> {
    fn process_items(&mut self, items: &mut Vec<Item>) {
        let mut new_items = Vec::new();
        let mut consecutive_imports = Vec::new();
        for item in items.drain(..) {
            if let Item::Use(u) = item {
                consecutive_imports.push(u);
            } else {
                // Process and flush the current use group
                let mut uses = Vec::new();
                process_item_uses(&mut consecutive_imports, &mut uses, self.config, self.whitespaces);
                for u in uses {
                    new_items.push(Item::Use(u));
                }

                // Now handle the non-import item itself
                new_items.push(item);
            }
        }

        // Flush the final group
        let mut uses = Vec::new();
        process_item_uses(&mut consecutive_imports, &mut uses, self.config, self.whitespaces);
        for u in uses {
            new_items.push(Item::Use(u));
        }
        *items = new_items;
    }

    fn process_stmts(&mut self, stmts: &mut Vec<Stmt>) {
        let mut new_stmts = Vec::new();
        let mut consecutive_imports = Vec::new();
        for stmt in stmts.drain(..) {
            if let Stmt::Item(Item::Use(u)) = stmt {
                consecutive_imports.push(u);
            } else {
                // Process and flush the current use group
                let mut uses = Vec::new();
                process_item_uses(&mut consecutive_imports, &mut uses, self.config, self.whitespaces);
                for u in uses {
                    new_stmts.push(Stmt::Item(Item::Use(u)));
                }

                // Now handle the non-import item itself
                new_stmts.push(stmt);
            }
        }

        // Flush the final group
        let mut uses = Vec::new();
        process_item_uses(&mut consecutive_imports, &mut uses, self.config, self.whitespaces);
        for u in uses {
            new_stmts.push(Stmt::Item(Item::Use(u)));
        }
        *stmts = new_stmts;
    }
}

impl<'a> VisitMut for ImportNormalizer<'a> {
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

    fn visit_block_mut(&mut self, i: &mut Block) {
        self.process_stmts(&mut i.stmts);
        syn::visit_mut::visit_block_mut(self, i);
    }
}

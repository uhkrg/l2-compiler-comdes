use std::collections::{BinaryHeap, HashMap, HashSet};

use crate::{ir::block_ir::BlockIR, parser::Ident, reg_alloc::liveness::liveness_after};

mod liveness;

#[derive(Debug)]
pub enum Location {
    Register(String),
    Stack(usize),
}

pub fn reg_alloc(ir: &BlockIR) -> HashMap<Ident, Location> {
    let graph = gen_interference_graph(ir);
    let coloring = calc_greedy_coloring(&graph);
    coloring
        .into_iter()
        .map(|(ident, c)| (ident, color_to_location(c)))
        .collect()
}

struct InterferenceGraph {
    mapping: HashMap<Ident, usize>,
    rev_mapping: Vec<Ident>,
    edges: Vec<HashSet<usize>>,
}

impl InterferenceGraph {
    fn new() -> Self {
        Self {
            mapping: HashMap::new(),
            rev_mapping: vec![],
            edges: vec![],
        }
    }

    fn get_idx_or_new(&mut self, x: String) -> usize {
        let idx = *self.mapping.entry(x.clone()).or_insert(self.edges.len());
        if self.edges.len() == idx {
            self.edges.push(HashSet::new());
            self.rev_mapping.push(x);
        }
        idx
    }

    fn push_edge(&mut self, p: usize, q: usize) {
        self.edges[p].insert(q);
        self.edges[q].insert(p);
    }
}

fn gen_interference_graph(ir: &BlockIR) -> InterferenceGraph {
    let liveness = liveness_after(ir);
    let mut graph = InterferenceGraph::new();
    for (x, y) in liveness
        .into_iter()
        .flatten()
        .flat_map(liveness_to_interferences)
    {
        let x_idx = graph.get_idx_or_new(x);
        let y_idx = graph.get_idx_or_new(y);
        graph.push_edge(x_idx, y_idx);
    }
    graph
}

fn liveness_to_interferences(live: HashSet<String>) -> Vec<(String, String)> {
    live.iter()
        .flat_map(|v| {
            let mut rest = live.clone();
            rest.remove(v);
            rest.into_iter().map(|w| (v.clone(), w))
        })
        .collect()
}

fn calc_greedy_coloring(graph: &InterferenceGraph) -> HashMap<Ident, usize> {
    let mut coloring = vec![0; graph.edges.len()];
    for v in calc_simplicial_elimination_ordering(graph) {
        let neighbouring_colors: HashSet<_> = graph.edges[v]
            .iter()
            .filter_map(|n| coloring.get(*n))
            .collect();
        coloring[v] = (1..)
            .filter(|c| !neighbouring_colors.contains(c))
            .next()
            .unwrap();
    }
    coloring
        .into_iter()
        .enumerate()
        .map(|(v, c)| (graph.rev_mapping[v].clone(), c))
        .collect()
}

fn calc_simplicial_elimination_ordering(graph: &InterferenceGraph) -> Vec<usize> {
    #[derive(PartialEq, Eq)]
    struct Node {
        v: usize,
        wt: usize,
    }
    impl Ord for Node {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            self.wt.cmp(&other.wt)
        }
    }
    impl PartialOrd for Node {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }
    let mut ordering = vec![];
    let mut weight = vec![0; graph.edges.len()];
    let mut done = HashSet::new();
    let mut pq = BinaryHeap::new();
    for i in 0..graph.edges.len() {
        pq.push(Node { v: i, wt: 0 });
    }
    while let Some(Node { v, .. }) = pq.pop() {
        if done.contains(&v) {
            continue;
        }
        ordering.push(v);
        done.insert(v);
        graph.edges[v]
            .iter()
            .filter(|n| !done.contains(n))
            .for_each(|n| {
                weight[*n] += 1;
                pq.push(Node {
                    v: *n,
                    wt: weight[*n],
                })
            });
    }
    ordering
}

fn color_to_location(color: usize) -> Location {
    match color {
        0 => unreachable!(),
        1 => Location::Register("%ecx".to_string()),
        2 => Location::Register("%esi".to_string()),
        3 => Location::Register("%edi".to_string()),
        4 => Location::Register("%r8d".to_string()),
        5 => Location::Register("%r9d".to_string()),
        6 => Location::Register("%r10d".to_string()),
        7 => Location::Register("%r11d".to_string()),
        8 => Location::Register("%r12d".to_string()),
        9 => Location::Register("%r13d".to_string()),
        10 => Location::Register("%r14d".to_string()),
        11 => Location::Register("%r15d".to_string()),
        n => Location::Stack(4 * (n - 10)),
    }
}

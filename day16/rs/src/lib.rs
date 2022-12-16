use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};
use std::iter;

use std::str;

use lazy_static::lazy_static;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

#[derive(Debug)]
struct Node<'a> {
    from: &'a str,
    to: Vec<&'a str>,
    rate: u32,
}

#[derive(Debug, Clone)]
struct State<'a> {
    node: &'a str,
    time: usize,
    sum: u32,
    h_sum: u32,
    valves: u32,
}

struct Solve<'a> {
    distances: HashMap<&'a str, Vec<(&'a str, usize)>>,
    node2id: HashMap<&'a str, usize>,
    rates: Vec<u32>,
    all_valves: u32,
}

impl<'a> State<'a> {
    fn new(node: &'a str, time: usize, rates: &[u32], valves: u32) -> Self {
        let sum = 0;
        let h_sum = rates
            .iter()
            .enumerate()
            .map(|(i, v)| {
                if valves & (1 << i) == 0 {
                    v * time as u32
                } else {
                    0
                }
            })
            .sum::<u32>()
            + sum;
        
        Self {
            node,
            time,
            sum,
            h_sum,
            valves,
        }
    }

    fn key(&self) -> (&'a str, u32) {
        (self.node, self.valves)
    }

    fn next(&self, idx: usize, node: &'a str, d: usize, rates: &[u32]) -> Self {
        let time = self.time - d - 1;
        let sum = self.sum + time as u32 * rates[idx];
        let valves = self.valves | (1 << idx);

        let h_sum = rates
            .iter()
            .enumerate()
            .map(|(i, v)| {
                if valves & (1 << i) == 0 {
                    v * time as u32
                } else {
                    0
                }
            })
            .sum::<u32>()
            + sum;

        Self {
            node,
            time,
            sum,
            h_sum,
            valves,
        }
    }
}

impl<'a> PartialEq for State<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.node == other.node && self.valves == other.valves && self.sum == other.sum
    }
}

impl<'a> PartialOrd for State<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.h_sum.cmp(&other.h_sum))
    }
}

impl<'a> Eq for State<'a> {}

impl<'a> Ord for State<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl<'a> Solve<'a> {
    fn new(input: &'a str) -> Self {
        let nodes = input
            .lines()
            .map(|line| {
                let mut i = line.split_whitespace();

                Node {
                    from: i.nth(1).unwrap(),
                    rate: {
                        let rate = i.nth(2).unwrap();
                        rate["rate=".len()..rate.len() - 1].parse().unwrap()
                    },
                    to: i.skip(4).map(|xx| &xx[..2]).collect(),
                }
            })
            .collect::<Vec<Node>>();

        let rates = nodes
            .iter()
            .filter_map(|Node { from, rate, .. }| if *rate > 0 { Some((from, rate)) } else { None })
            .collect::<HashMap<_, _>>();

        let distances = rates
            .keys()
            .map(|node| (**node, distances(&nodes, node)))
            .chain(iter::once(("AA", distances(&nodes, "AA"))))
            .collect::<HashMap<_, _>>();

        let (node2id, rates, all_valves) = rates.iter().enumerate().fold(
            (HashMap::new(), Vec::new(), 0),
            |(mut n2i, mut rs, v), (i, (node, r))| {
                n2i.insert(**node, i);
                rs.push(**r);
                (n2i, rs, (v << 1) | 1)
            },
        );

        Self {
            distances,
            node2id,
            rates,
            all_valves,
        }
    }

    fn solve(&self, time: usize, init_valves: u32) -> u32 {
        let mut visited = HashMap::new();
        let mut heap = BinaryHeap::new();

        let current = State::new("AA", time, &self.rates, init_valves);
        let current_key = current.key();
        visited.insert(current_key, current.sum);
        heap.push(current);

        let mut current_max = 0;
        while let Some(state) = heap.pop() {
            if state.valves == self.all_valves {
                let max = visited.values().max().unwrap();
                if *max <= state.sum {
                    return state.sum;
                } else {
                    current_max = current_max.max(state.sum);
                }
            }

            for (node, d) in &self.distances[state.node] {
                let node_id = self.node2id[node];

                if state.time <= *d + 1 || state.valves & (1 << node_id) != 0 {
                    continue;
                }

                let new_state = state.next(node_id, node, *d, &self.rates);
                let new_state_key = new_state.key();

                let old_sum = visited.entry(new_state_key).or_default();
                if new_state.sum > *old_sum {
                    *old_sum = new_state.sum;
                    heap.push(new_state);
                }
            }
        }

        if current_max == 0 {
            *visited.values().max().unwrap()
        } else {
            current_max
        }
    }
}

fn distances<'a>(nodes: &[Node<'a>], from: &'a str) -> Vec<(&'a str, usize)> {
    let mut r = Vec::new();

    let mut visited = HashSet::new();
    let mut queue = VecDeque::new();

    visited.insert(from);
    queue.push_back((from, 0));

    while let Some((current, steps)) = queue.pop_front() {
        let Node { from, to, rate } = nodes
            .iter()
            .find(|Node { from, .. }| from == &current)
            .unwrap();
        if *rate > 0 {
            r.push((*from, steps));
        }

        for target in to {
            if visited.contains(target) {
                continue;
            }

            visited.insert(target);
            queue.push_back((target, steps + 1));
        }
    }

    r
}

pub fn solve_1(input: &str) -> u32 {
    Solve::new(input).solve(30, 0)
}

pub fn solve_2(input: &str) -> u32 {
    let solve = Solve::new(input);

    let r = (0..(1 << solve.rates.len()))
        .map(|i| (i, solve.solve(26, i)))
        .collect::<HashMap<_, _>>();

    let r = (0..(1 << solve.rates.len()) / 2)
        .map(|i| r[&i] + r[&(solve.all_valves & !i)])
        .collect::<Vec<_>>();

    *r.iter().max().unwrap()
}

pub fn part_1() -> u32 {
    solve_1(&INPUT)
}

pub fn part_2() -> u32 {
    solve_2(&INPUT)
}

#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref EXAMPLE1: &'static str = include_str!("../../example1");
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&EXAMPLE1), 1651);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&EXAMPLE1), 1707);
    }
}

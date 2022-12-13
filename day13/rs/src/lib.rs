use std::cmp::Ordering;
use std::iter;

use serde::Deserialize;

use lazy_static::lazy_static;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

#[derive(Deserialize, Debug, Clone)]
#[serde(untagged)]
enum List {
    Int(u32),
    List(Vec<List>),
}

fn is_valid(packets: &str) -> bool {
    let mut packets = packets.lines();
    let packet1: List =
        serde_json::from_str(packets.next().expect("packet 1 not found")).expect("invalid list");
    let packet2: List =
        serde_json::from_str(packets.next().expect("packet 2 not found")).expect("invalid list");

    cmp(packet1, packet2) != Ordering::Greater
}

fn cmp(packet1: List, packet2: List) -> Ordering {
    match (packet1, packet2) {
        (List::Int(a), List::Int(b)) => a.cmp(&b),
        (a @ List::Int(_), b @ List::List(_)) => cmp(List::List(vec![a]), b),
        (a @ List::List(_), b @ List::Int(_)) => cmp(a, List::List(vec![b])),
        (List::List(a), List::List(b)) => {
            let (l1, l2) = (a.len(), b.len());
            a
                .into_iter()
                .zip(b.into_iter())
                .fold(Ordering::Equal, |acc, (a, b)| acc.then_with(|| cmp(a, b)))
                .then_with(|| l1.cmp(&l2))
        }
    }
}

pub fn solve_1(input: &str) -> usize {
    input
        .split("\n\n")
        .enumerate()
        .filter_map(|(i, packets)| if is_valid(packets) { Some(i + 1) } else { None })
        .sum()
}

pub fn solve_2(input: &str) -> usize {
    let decoder_key_2 = serde_json::from_str("[[2]]").unwrap();
    let decoder_key_6 = serde_json::from_str("[[6]]").unwrap();

    let mut packets = input
        .lines()
        .filter_map(|line| {
            if line.is_empty() {
                None
            } else {
                Some((false, serde_json::from_str(line).expect("invalid list")))
            }
        })
        .chain(iter::once((true, decoder_key_2)))
        .chain(iter::once((true, decoder_key_6)))
        .collect::<Vec<(bool, List)>>();

    packets.sort_by(|(_, a), (_, b)| cmp(a.clone(), b.clone()));

    packets
        .into_iter()
        .enumerate()
        .filter_map(|(i, (k, _))| if k { Some(i + 1) } else { None })
        .product()
}

pub fn part_1() -> usize {
    solve_1(&INPUT)
}

pub fn part_2() -> usize {
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
        assert_eq!(solve_1(&EXAMPLE1), 13);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&EXAMPLE1), 140);
    }
}

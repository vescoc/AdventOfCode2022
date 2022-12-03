use std::collections::HashSet;

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

fn solve_1(input: &str) -> u32 {
    input
        .lines()
        .map(|line| {
            let len = line.len();
            let common = line[0..len / 2]
                .chars()
                .collect::<HashSet<_>>()
                .intersection(&line[len / 2..].chars().collect::<HashSet<_>>())
                .into_iter()
                .next()
                .copied()
                .unwrap();
            if ('a'..='z').contains(&common) {
                common as u32 - 'a' as u32 + 1
            } else {
                common as u32 - 'A' as u32 + 27
            }
        })
        .sum()
}

fn solve_2(input: &str) -> u32 {
    input
        .lines()
        .collect::<Vec<_>>()
        .chunks(3)
        .map(|chunk| {
            let common = chunk
                .iter()
                .map(|a| a.chars().collect::<HashSet<_>>())
                .reduce(|a, b| a.intersection(&b).copied().collect::<HashSet<_>>())
                .unwrap()
                .into_iter()
                .next()
                .unwrap();
            if ('a'..='z').contains(&common) {
                common as u32 - 'a' as u32 + 1
            } else {
                common as u32 - 'A' as u32 + 27
            }
        })
        .sum()
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
        static ref INPUT: &'static str = include_str!("../../example1");
    }

    #[test]
    fn line_1() {
        assert_eq!(solve_1("vJrwpWtwJgWrhcsFMMfFFhFp"), 16);
    }

    #[test]
    fn line_2() {
        assert_eq!(solve_1("jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"), 38);
    }

    #[test]
    fn line_3() {
        assert_eq!(solve_1("PmmdzqPrVvPwwTWBwg"), 42);
    }

    #[test]
    fn line_4() {
        assert_eq!(solve_1("wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"), 22);
    }

    #[test]
    fn line_5() {
        assert_eq!(solve_1("ttgJtRGJQctTZtZT"), 20);
    }

    #[test]
    fn line_6() {
        assert_eq!(solve_1("CrZsJsPPZsGzwwsLwLmpwMDw"), 19);
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 157);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 70);
    }
}

use std::str;

use lazy_static::lazy_static;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

struct Section(u32, u32);

impl Section {
    fn contains(&self, Section(bl, bh): &Self) -> bool {
        let Section(al, ah) = *self;
        (al..=ah).contains(bl) && (al..=ah).contains(bh)
    }

    fn overlap(&self, Section(bl, bh): &Self) -> bool {
        let Section(al, ah) = *self;
        (al..=ah).contains(bl) || (al..=ah).contains(bh)
    }
}

impl str::FromStr for Section {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Section, Self::Err> {
        let invalid_number = |_| "invalid number";
        s.split_once('-').ok_or("- not found").and_then(|(l, h)| {
            Ok(Section(
                l.parse().map_err(invalid_number)?,
                h.parse().map_err(invalid_number)?,
            ))
        })
    }
}

fn parse(line: &str) -> (Section, Section) {
    let (a, b) = line.split_once(',').unwrap();
    (a.parse().unwrap(), b.parse().unwrap())
}

fn solve<F: Fn(&Section, &Section) -> bool>(input: &str, test: F) -> usize {
    input.lines().map(parse).filter(|(a, b)| test(a, b)).count()
}

pub fn solve_1(input: &str) -> usize {
    solve(input, |a, b| a.contains(b) || b.contains(a))
}

pub fn solve_2(input: &str) -> usize {
    solve(input, |a, b| a.overlap(b) || b.overlap(a))
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
        assert_eq!(solve_1(&EXAMPLE1), 2);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&EXAMPLE1), 4);
    }
}

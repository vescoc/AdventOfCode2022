use std::collections::HashSet;

use lazy_static::lazy_static;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

fn follow((hx, hy): &(i32, i32), &(tx, ty): &(i32, i32)) -> (i32, i32) {
    let (dx, dy) = (hx - tx, hy - ty);

    match (dx.abs(), dy.abs()) {
        (a, 0) if a <= 1 => (tx, ty),
        (_, 0) => (tx + dx.signum(), ty),
        (0, a) if a <= 1 => (tx, ty),
        (0, _) => (tx, ty + dy.signum()),
        (a, b) if a + b <= 2 => (tx, ty),
        _ => (tx + dx.signum(), ty + dy.signum()),
    }
}

pub fn solve<const SIZE: usize>(input: &str) -> usize {
    let mut positions = HashSet::new();

    let mut rope = [(0, 0); SIZE];

    for line in input.lines() {
        let (direction, moves) = line.split_once(' ').expect("invalid move format");
        let (dx, dy) = match direction {
            "U" => (0, -1),
            "D" => (0, 1),
            "L" => (-1, 0),
            "R" => (1, 0),
            _ => panic!("invalid direction"),
        };

        let moves = moves.parse::<i32>().expect("invalid moves");
        for _ in 0..moves {
            rope[0].0 += dx;
            rope[0].1 += dy;

            for i in 1..rope.len() {
                rope[i] = follow(&rope[i - 1], &rope[i]);
            }

            positions.insert(rope[SIZE - 1]);
        }
    }

    positions.len()
}

pub fn solve_1(input: &str) -> usize {
    solve::<2>(input)
}

pub fn solve_2(input: &str) -> usize {
    solve::<10>(input)
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
        static ref EXAMPLE2: &'static str = include_str!("../../example2");
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&EXAMPLE1), 13);
    }

    #[test]
    fn same_results_2_1() {
        assert_eq!(solve_2(&EXAMPLE1), 1);
    }

    #[test]
    fn same_results_2_2() {
        assert_eq!(solve_2(&EXAMPLE2), 36);
    }
}

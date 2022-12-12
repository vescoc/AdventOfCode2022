use std::collections::{HashSet, VecDeque};

use lazy_static::lazy_static;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

pub fn solve_1(input: &str) -> usize {
    let mut start = None;
    let mut end = None;

    let area = input
        .lines()
        .enumerate()
        .map(|(r, line)| {
            line.as_bytes()
                .iter()
                .enumerate()
                .map(|(c, b)| match b {
                    b'S' => {
                        start = Some((c as isize, r as isize));
                        b'a'
                    }
                    b'E' => {
                        end = Some((c as isize, r as isize));
                        b'z'
                    }
                    _ => *b,
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let start = start.expect("cannot find start");
    let end = end.expect("cannot find end");

    let mut visited = HashSet::with_capacity(area.len() * area[0].len());
    visited.insert(start);

    let mut queue = VecDeque::new();
    queue.push_back((start, 0));

    while let Some((current, steps)) = queue.pop_front() {
        if current == end {
            return steps;
        }

        let (x, y) = current;
        let c = area[y as usize][x as usize] as i32;
        for (dx, dy) in [(0, 1), (0, -1), (1, 0), (-1, 0)] {
            let (nx, ny) = (x + dx, y + dy);
            if nx >= 0
                && ny >= 0
                && nx < area[0].len() as isize
                && ny < area.len() as isize
                && !visited.contains(&(nx, ny))
                && area[ny as usize][nx as usize] as i32 - c <= 1
            {
                visited.insert((nx, ny));
                queue.push_back(((nx, ny), steps + 1));
            }
        }
    }

    panic!("not found")
}

pub fn solve_2(input: &str) -> usize {
    let mut start = None;
    let mut ends = HashSet::new();

    let area = input
        .lines()
        .enumerate()
        .map(|(r, line)| {
            line.as_bytes()
                .iter()
                .enumerate()
                .map(|(c, b)| match b {
                    b'S' | b'a' => {
                        ends.insert((c as isize, r as isize));
                        b'a'
                    }
                    b'E' => {
                        start = Some((c as isize, r as isize));
                        b'z'
                    }
                    _ => *b,
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let start = start.expect("cannot find start");

    let mut visited = HashSet::with_capacity(area.len() * area[0].len());
    visited.insert(start);

    let mut queue = VecDeque::new();
    queue.push_back((start, 0));

    while let Some((current, steps)) = queue.pop_front() {
        if ends.contains(&current) {
            return steps;
        }

        let (x, y) = current;
        let c = area[y as usize][x as usize] as i32;
        for (dx, dy) in [(0, 1), (0, -1), (1, 0), (-1, 0)] {
            let (nx, ny) = (x + dx, y + dy);
            if nx >= 0
                && ny >= 0
                && nx < area[0].len() as isize
                && ny < area.len() as isize
                && !visited.contains(&(nx, ny))
                && c - area[ny as usize][nx as usize] as i32 <= 1
            {
                visited.insert((nx, ny));
                queue.push_back(((nx, ny), steps + 1));
            }
        }
    }

    panic!("not found")
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
        assert_eq!(solve_1(&EXAMPLE1), 31);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&EXAMPLE1), 29);
    }
}

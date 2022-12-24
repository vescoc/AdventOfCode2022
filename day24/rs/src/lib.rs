use lazy_static::lazy_static;

use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashSet};

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

enum Blizzard {
    Up,
    Down,
    Left,
    Right,
}

type Coord = (isize, isize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct State {
    steps: usize,
    position: Coord,
    h: isize,
}

struct BlizzardsMap {
    blizzards: Vec<(Coord, Blizzard)>,
    end_x: isize,
    end_y: isize,
    width: isize,
    height: isize,
}

impl BlizzardsMap {
    fn new(input: &str) -> Self {
        let (mut end_x, mut end_y) = (isize::MIN, isize::MIN);
        let mut blizzards = Vec::with_capacity(40 * 40);
        for (y, row) in input.lines().enumerate() {
            for (x, c) in row.chars().enumerate() {
                let (x, y) = (x as isize, y as isize);
                match c {
                    '>' => blizzards.push(((x, y), Blizzard::Right)),
                    '<' => blizzards.push(((x, y), Blizzard::Left)),
                    '^' => blizzards.push(((x, y), Blizzard::Up)),
                    'v' => blizzards.push(((x, y), Blizzard::Down)),
                    _ => {}
                }

                end_x = end_x.max(x);
            }

            end_y = end_y.max(y as isize);
        }

        let width = end_x - 1;
        let height = end_y - 1;

        Self {
            blizzards,
            end_x,
            end_y,
            width,
            height,
        }
    }

    fn solve(&self, start_position: Coord, end_position: Coord, start_time: usize) -> usize {
        let mut heap = BinaryHeap::new();
        let mut visited = HashSet::with_capacity(1_024 * 1_024);

        let current_h = manhattan(start_position, end_position);
        let current = State::new(0, start_position, current_h);
        heap.push(current);
        visited.insert(current);

        while let Some(State {
            steps,
            position: position @ (x, y),
            h: _,
        }) = heap.pop()
        {
            if position == end_position {
                return steps;
            }

            let steps = steps + 1;

            let next_positions = [(-1, 0), (1, 0), (0, 1), (0, -1), (0, 0)]
                .iter()
                .filter_map(|(dx, dy)| {
                    let (x, y) = (x + dx, y + dy);

                    if ((1..self.end_x).contains(&x) && (1..self.end_y).contains(&y))
                        || (x, y) == start_position
                        || (x, y) == end_position
                    {
                        Some((x, y))
                    } else {
                        None
                    }
                })
                .collect::<HashSet<_>>();

            let next_blizzard_positions = self
                .blizzards
                .iter()
                .map(|((x, y), blizzard)| {
                    let steps = (steps + start_time) as isize;

                    match blizzard {
                        Blizzard::Up => (*x, (y - 1 - steps).rem_euclid(self.height) + 1),
                        Blizzard::Down => (*x, (y - 1 + steps).rem_euclid(self.height) + 1),
                        Blizzard::Left => ((x - 1 - steps).rem_euclid(self.width) + 1, *y),
                        Blizzard::Right => ((x - 1 + steps).rem_euclid(self.width) + 1, *y),
                    }
                })
                .collect::<HashSet<_>>();

            for p in next_positions.difference(&next_blizzard_positions) {
                let h = steps as isize + manhattan(*p, end_position);
                let next_state = State::new(steps, *p, h);

                if !visited.contains(&next_state) {
                    visited.insert(next_state);
                    heap.push(next_state);
                }
            }
        }

        unreachable!()
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(other.h.cmp(&self.h))
    }
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other.h.cmp(&self.h)
    }
}

impl State {
    fn new(steps: usize, position: Coord, h: isize) -> Self {
        State { steps, position, h }
    }
}

fn manhattan((ax, ay): Coord, (bx, by): Coord) -> isize {
    (ax - bx).abs() + (ay - by).abs()
}

pub fn solve_1(input: &str) -> usize {
    let blizzards_map = BlizzardsMap::new(input);

    let start_position = (1, 0);
    let end_position = (blizzards_map.end_x - 1, blizzards_map.end_y);

    blizzards_map.solve(start_position, end_position, 0)
}

pub fn solve_2(input: &str) -> usize {
    let blizzards_map = BlizzardsMap::new(input);

    let start_position = (1, 0);
    let end_position = (blizzards_map.end_x - 1, blizzards_map.end_y);

    let t_start2end = blizzards_map.solve(start_position, end_position, 0);
    #[cfg(debug_assertions)]
    println!("start -> end: {t_start2end}");

    let t_end2start = blizzards_map.solve(end_position, start_position, t_start2end);
    #[cfg(debug_assertions)]
    println!("end -> start: {t_end2start}");

    let t_start2end_2 =
        blizzards_map.solve(start_position, end_position, t_end2start + t_start2end);
    #[cfg(debug_assertions)]
    println!("start -> end: {t_start2end_2}");

    t_start2end + t_end2start + t_start2end_2
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
        assert_eq!(solve_1(&EXAMPLE1), 18);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&EXAMPLE1), 54);
    }
}

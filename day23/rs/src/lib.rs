use lazy_static::lazy_static;

use std::collections::{HashMap, HashSet};

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

type Move = ((isize, isize), [(isize, isize); 3]);

const MOVES: [Move; 4] = [
    ((0, -1), [(0, -1), (1, -1), (-1, -1)]),
    ((0, 1), [(0, 1), (1, 1), (-1, 1)]),
    ((-1, 0), [(-1, 0), (-1, 1), (-1, -1)]),
    ((1, 0), [(1, 0), (1, 1), (1, -1)]),
];

struct Elves {
    elves: HashSet<(isize, isize)>,
    direction_index: usize,
}

impl Elves {
    fn new(input: &str) -> Self {
        let elves = input
            .lines()
            .enumerate()
            .flat_map(|(y, line)| {
                line.chars().enumerate().filter_map(move |(x, c)| {
                    if c == '#' {
                        Some((x as isize, y as isize))
                    } else {
                        None
                    }
                })
            })
            .collect::<HashSet<_>>();

        Self {
            elves,
            direction_index: 0,
        }
    }

    fn step(&mut self) -> bool {
        let mut proposals = Vec::new();
        let mut rest = Vec::new();
        let mut dest: HashMap<(isize, isize), u32> = HashMap::new();

        // first half turn
        for (x, y) in &self.elves {
            // 1. check if elf can move
            if [
                (-1, -1),
                (0, -1),
                (1, -1),
                (-1, 0),
                (1, 0),
                (-1, 1),
                (0, 1),
                (1, 1),
            ]
            .iter()
            .all(|(dx, dy)| !self.elves.contains(&(x + dx, y + dy)))
            {
                rest.push((*x, *y));

                continue;
            }

            // 2. propose move
            if let Some((dx, dy)) = MOVES
                .iter()
                .cycle()
                .skip(self.direction_index)
                .take(4)
                .find_map(|(d, dirs)| {
                    if dirs
                        .iter()
                        .all(|(dx, dy)| !self.elves.contains(&(x + dx, y + dy)))
                    {
                        Some(d)
                    } else {
                        None
                    }
                })
            {
                proposals.push(((*x, *y), (x + dx, y + dy)));
                *dest.entry((x + dx, y + dy)).or_default() += 1;
            } else {
                rest.push((*x, *y));
            }
        }

        // increment direction
        self.direction_index += 1;

        if rest.len() != self.elves.len() {
            // second turn half
            self.elves = rest
                .into_iter()
                .chain(
                    proposals
                        .into_iter()
                        .map(|(o, n)| if dest[&n] == 1 { n } else { o }),
                )
                .collect();

            true
        } else {
            false
        }
    }

    fn bounds(&self) -> ((isize, isize), (isize, isize)) {
        self.elves.iter().fold(
            ((isize::MAX, isize::MAX), (isize::MIN, isize::MIN)),
            |((min_x, min_y), (max_x, max_y)), (x, y)| {
                (
                    (min_x.min(*x), min_y.min(*y)),
                    (max_x.max(*x), max_y.max(*y)),
                )
            },
        )
    }

    fn size(&self) -> usize {
        self.elves.len()
    }
}

pub fn solve_1(input: &str) -> usize {
    let mut elves = Elves::new(input);

    (0..10).for_each(|_| {
        elves.step();
    });

    let ((min_x, min_y), (max_x, max_y)) = elves.bounds();

    ((max_x - min_x + 1) * (max_y - min_y + 1)) as usize - elves.size()
}

pub fn solve_2(input: &str) -> usize {
    let mut elves = Elves::new(input);

    (0..).position(|_| !elves.step()).unwrap() + 1
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
        assert_eq!(solve_1(&EXAMPLE1), 110);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&EXAMPLE1), 20);
    }
}

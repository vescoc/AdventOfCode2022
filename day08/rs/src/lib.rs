use std::collections::HashSet;

use lazy_static::lazy_static;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

pub fn solve_1(input: &str) -> usize {
    let forest = input
        .lines()
        .map(|line| line.as_bytes().to_vec())
        .collect::<Vec<_>>();

    let set = forest // left to right
        .iter()
        .enumerate()
        .flat_map(|(y, row)| {
            let mut max = 0;
            row.iter().enumerate().filter_map(move |(x, &v)| {
                if v > max {
                    max = v;
                    Some((x, y))
                } else {
                    None
                }
            })
        })
        .chain(
            forest // right to left
                .iter()
                .enumerate()
                .flat_map(|(y, row)| {
                    let mut max = 0;
                    row.iter().enumerate().rev().filter_map(move |(x, &v)| {
                        if v > max {
                            max = v;
                            Some((x, y))
                        } else {
                            None
                        }
                    })
                }),
        )
        .chain(
            (0..forest[0].len()) // top to bottom
                .flat_map(|x| {
                    let mut max = 0;
                    forest.iter().enumerate().filter_map(move |(y, row)| {
                        let v = row[x];
                        if v > max {
                            max = v;
                            Some((x, y))
                        } else {
                            None
                        }
                    })
                }),
        )
        .chain(
            (0..forest[0].len()) // bottom to top
                .flat_map(|x| {
                    let mut max = 0;
                    forest.iter().enumerate().rev().filter_map(move |(y, row)| {
                        let v = row[x];
                        if v > max {
                            max = v;
                            Some((x, y))
                        } else {
                            None
                        }
                    })
                }),
        )
        .collect::<HashSet<_>>();

    set.len()
}

pub fn solve_2(input: &str) -> usize {
    let forest: Vec<Vec<u8>> = input.lines().map(|line| line.as_bytes().to_vec()).collect();

    let mx = forest[0].len() - 1;
    let my = forest.len() - 1;

    let top = |v, x, y| {
        let mut count = 0;
        for yy in (0..y).rev() {
            count += 1;
            if v <= forest[yy][x] {
                break;
            }
        }
        count
    };

    let bottom = |v, x, y| {
        let mut count = 0;
        for row in forest.iter().skip(y + 1) {
            count += 1;
            if v <= row[x] {
                break;
            }
        }
        count
    };

    let left = |v, x, y| {
        let mut count = 0;
        for xx in (0..x).rev() {
            count += 1;
            let row: &Vec<u8> = &forest[y];
            if v <= row[xx] {
                break;
            }
        }
        count
    };

    let right = |v, x, y| {
        let mut count = 0;
        for xx in x + 1..mx {
            count += 1;
            let row: &Vec<u8> = &forest[y];
            if v <= row[xx] {
                break;
            }
        }
        count
    };

    forest
        .iter()
        .enumerate()
        .flat_map(|(y, row)| {
            row.iter().enumerate().map(move |(x, &v)| {
                if x == 0 || y == 0 || x == mx || y == my {
                    0
                } else {
                    top(v, x, y) * bottom(v, x, y) * left(v, x, y) * right(v, x, y)
                }
            })
        })
        .max()
        .unwrap()
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
        assert_eq!(solve_1(&EXAMPLE1), 21);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&EXAMPLE1), 8);
    }
}

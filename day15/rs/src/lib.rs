use std::collections::HashSet;

use lazy_static::lazy_static;
use regex::Regex;

lazy_static! {
    static ref REGEX: Regex =
        Regex::new(r"Sensor at x=(-?\d+), y=(-?\d+): closest beacon is at x=(-?\d+), y=(-?\d+)")
            .expect("invalid regex");
    pub static ref INPUT: &'static str = include_str!("../../input");
}

fn manhattan((sx, sy): (i32, i32), (bx, by): (i32, i32)) -> i32 {
    (sx - bx).abs() + (sy - by).abs()
}

fn diff((al, ah): (i32, i32), (bl, bh): (i32, i32)) -> Vec<(i32, i32)> {
    let mut r = Vec::new();

    if ah < bl || al > bh {
        r.push((al, ah))
    } else if (bl..=bh).contains(&ah) {
        if !(bl..=bh).contains(&al) {
            if al < bl {
                r.push((al, bl - 1));
            }
        } else {
            // full included
        }
    } else if (bl..=bh).contains(&al) {
        if !(bl..=bh).contains(&ah) {
            if ah > bh {
                r.push((bh + 1, ah));
            }
        } else {
            // full included
        }
    } else {
        if al < bl {
            r.push((al, bl - 1));
        }
        if ah > bh {
            r.push((bh + 1, ah));
        }
    }

    r
}

fn solve_y(input: &str, y: i32) -> i32 {
    let sensors = input
        .lines()
        .enumerate()
        .map(|(i, line)| {
            let captures = REGEX.captures(line).unwrap();
            (
                i,
                (captures[1].parse().unwrap(), captures[2].parse().unwrap()),
                (captures[3].parse().unwrap(), captures[4].parse().unwrap()),
            )
        })
        .map(|(i, s, b)| (i, s, b, manhattan(s, b)))
        .collect::<Vec<(usize, (i32, i32), (i32, i32), i32)>>();

    let total = sensors
        .iter()
        .flat_map(|(i, (sx, sy), _, sd)| {
            let d = (sy - y).abs();
            if d > *sd {
                None
            } else {
                Some((i, (sx - (sd - d), sx + (sd - d))))
            }
        })
        .fold(Vec::new(), |acc, (_, i)| {
            let mut r = Vec::new();
            for c in acc {
                r.append(&mut diff(c, i));
            }
            r.push(i);
            r
        })
        .into_iter()
        .map(|(l, h)| h - l + 1)
        .sum::<i32>();

    let beacons = sensors
        .iter()
        .filter_map(|(_, _, (bx, by), _)| if *by == y { Some((bx, by)) } else { None })
        .collect::<HashSet<_>>()
        .len() as i32;

    total - beacons
}

fn solve_d(input: &str, d: i32) -> i64 {
    let sensors = input
        .lines()
        .enumerate()
        .map(|(i, line)| {
            let captures = REGEX.captures(line).unwrap();
            (
                i,
                (captures[1].parse().unwrap(), captures[2].parse().unwrap()),
                (captures[3].parse().unwrap(), captures[4].parse().unwrap()),
            )
        })
        .map(|(i, s, b)| (i, s, b, manhattan(s, b)))
        .collect::<Vec<(usize, (i32, i32), (i32, i32), i32)>>();

    for (_, (sx, sy), _, sd) in sensors.iter() {
        for x in 0.max(sx - (sd + 1))..=d.min(sx + sd + 1) {
            let dy = ((x - sx).abs() - (sd + 1)).abs();
            let y = sy + dy;
            if y >= 0
                && y <= d
                && sensors
                    .iter()
                    .all(|(_, ts, _, tsd)| manhattan(*ts, (x, y)) > *tsd)
            {
                return x as i64 * 4_000_000 + y as i64;
            } else if dy != 0 {
                let y = sy - dy;
                if y >= 0
                    && y <= d
                    && sensors
                        .iter()
                        .all(|(_, ts, _, tsd)| manhattan(*ts, (x, y)) > *tsd)
                {
                    return x as i64 * 4_000_000 + y as i64;
                }
            }
        }
    }

    panic!("not found")
}

pub fn solve_1(input: &str) -> i32 {
    solve_y(input, 2_000_000)
}

pub fn solve_2(input: &str) -> i64 {
    solve_d(input, 4_000_000)
}

pub fn part_1() -> i32 {
    solve_1(&INPUT)
}

pub fn part_2() -> i64 {
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
        assert_eq!(solve_y(&EXAMPLE1, 10), 26);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_d(&EXAMPLE1, 20), 56000011);
    }
}

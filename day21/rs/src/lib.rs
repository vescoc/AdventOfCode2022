use lazy_static::lazy_static;

use std::collections::HashMap;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

#[derive(Debug, PartialEq)]
enum Op<'a> {
    Add(&'a str, &'a str),
    Min(&'a str, &'a str),
    Div(&'a str, &'a str),
    Mul(&'a str, &'a str),
    Number(i64),
}

impl<'a> Op<'a> {
    fn parse<I: Iterator<Item = &'a str>>(mut parts: I) -> Result<Op<'a>, &'static str> {
        let a = parts.next().ok_or("cannot find number of first operand")?;
        match (parts.next(), parts.next()) {
            (Some("+"), Some(v)) => Ok(Op::Add(a, v)),
            (Some("-"), Some(v)) => Ok(Op::Min(a, v)),
            (Some("/"), Some(v)) => Ok(Op::Div(a, v)),
            (Some("*"), Some(v)) => Ok(Op::Mul(a, v)),
            (None, None) => Ok(Op::Number(a.parse().map_err(|_| "invalid number")?)),
            _ => Err("invalid format"),
        }
    }

    fn solve(&self, mem: &HashMap<&'a str, Op<'a>>) -> Option<i64> {
        Some(match self {
            Op::Number(v) => *v,
            Op::Add(a, b) => {
                mem.get(a).and_then(|a| a.solve(mem))? + mem.get(b).and_then(|b| b.solve(mem))?
            }
            Op::Min(a, b) => {
                mem.get(a).and_then(|a| a.solve(mem))? - mem.get(b).and_then(|b| b.solve(mem))?
            }
            Op::Div(a, b) => {
                mem.get(a).and_then(|a| a.solve(mem))? / mem.get(b).and_then(|b| b.solve(mem))?
            }
            Op::Mul(a, b) => {
                mem.get(a).and_then(|a| a.solve(mem))? * mem.get(b).and_then(|b| b.solve(mem))?
            }
        })
    }

    fn inv(&self, mem: &HashMap<&'a str, Op<'a>>, v: i64) -> i64 {
        match self {
            // +
            Op::Add(a, "humn") => v - mem[a].solve(mem).unwrap(),
            Op::Add("humn", b) => v - mem[b].solve(mem).unwrap(),
            Op::Add(a, b) => match (mem[a].solve(mem), mem[b].solve(mem)) {
                (Some(a), None) => mem[b].inv(mem, v - a),
                (None, Some(b)) => mem[a].inv(mem, v - b),
                _ => panic!("arghhh"),
            },

            // -
            Op::Min(a, "humn") => mem[a].solve(mem).unwrap() - v,
            Op::Min("humn", b) => v + mem[b].solve(mem).unwrap(),
            Op::Min(a, b) => match (mem[a].solve(mem), mem[b].solve(mem)) {
                (Some(a), None) => mem[b].inv(mem, a - v),
                (None, Some(b)) => mem[a].inv(mem, v + b),
                _ => panic!("arghhh"),
            },

            // /
            Op::Div(a, "humn") => mem[a].solve(mem).unwrap() / v,
            Op::Div("humn", b) => mem[b].solve(mem).unwrap() * v,
            Op::Div(a, b) => match (mem[a].solve(mem), mem[b].solve(mem)) {
                (Some(a), None) => mem[b].inv(mem, a / v),
                (None, Some(b)) => mem[a].inv(mem, b * v),
                _ => panic!("arghhh"),
            },

            // *
            Op::Mul(a, "humn") => v / mem[a].solve(mem).unwrap(),
            Op::Mul("humn", b) => v / mem[b].solve(mem).unwrap(),
            Op::Mul(a, b) => match (mem[a].solve(mem), mem[b].solve(mem)) {
                (Some(a), None) => mem[b].inv(mem, v / a),
                (None, Some(b)) => mem[a].inv(mem, v / b),
                _ => panic!("arghhh"),
            },

            // Invalid
            _ => panic!("number?"),
        }
    }
}

pub fn solve_1(input: &str) -> i64 {
    let mem = input
        .lines()
        .map(|line| {
            let mut parts = line.split_whitespace();
            Ok((
                parts
                    .next()
                    .map(|part| &part[0..part.len() - 1])
                    .ok_or("cannot find def")?,
                Op::parse(parts)?,
            ))
        })
        .collect::<Result<HashMap<&str, Op>, &'static str>>()
        .expect("invalid input");

    mem["root"].solve(&mem).expect("cannot find root value")
}

pub fn solve_2(input: &str) -> i64 {
    let mut root = None;

    let mem = input
        .lines()
        .filter_map(|line| {
            let mut parts = line.split_whitespace();
            let parse = || {
                Ok::<(&str, Op<'_>), &'static str>((
                    parts
                        .next()
                        .map(|part| &part[0..part.len() - 1])
                        .ok_or("cannot find def")?,
                    Op::parse(parts)?,
                ))
            };
            match parse() {
                Ok(("root", Op::Number(_))) => Some(Err("invalid root")),
                Ok(("root", Op::Add(a, b))) => {
                    root = Some((a, b));
                    None
                }
                Ok(("root", Op::Min(a, b))) => {
                    root = Some((a, b));
                    None
                }
                Ok(("root", Op::Div(a, b))) => {
                    root = Some((a, b));
                    None
                }
                Ok(("root", Op::Mul(a, b))) => {
                    root = Some((a, b));
                    None
                }
                Ok(("humn", _)) => None,
                Ok((d, op)) => Some(Ok((d, op))),
                Err(e) => Some(Err(e)),
            }
        })
        .collect::<Result<HashMap<&str, Op>, &'static str>>()
        .expect("invalid input");

    let (l, r) = root.unwrap();

    match (l, r) {
        ("humn", r) => mem[r].solve(&mem).unwrap(),
        (l, "humn") => mem[l].solve(&mem).unwrap(),
        _ => match (mem[l].solve(&mem), mem[r].solve(&mem)) {
            (Some(v), None) => mem[r].inv(&mem, v),
            (None, Some(v)) => mem[l].inv(&mem, v),
            _ => panic!("arghhh!"),
        },
    }
}

pub fn part_1() -> i64 {
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
        assert_eq!(solve_1(&EXAMPLE1), 152);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&EXAMPLE1), 301);
    }
}

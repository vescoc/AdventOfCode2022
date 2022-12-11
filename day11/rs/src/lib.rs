use std::collections::VecDeque;
use std::str;

use lazy_static::lazy_static;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

#[derive(Debug)]
enum Operation {
    Add(u64),
    Multiply(u64),
    Square,
}

impl Operation {
    fn eval(&self, value: u64) -> u64 {
        match &self {
            Operation::Add(other) => value + other,
            Operation::Multiply(other) => value * other,
            Operation::Square => value * value,
        }
    }
}

#[derive(Debug)]
struct Monkey {
    #[allow(dead_code)]
    id: usize,
    items: VecDeque<u64>,
    operation: Operation,
    test: u64,
    true_target: usize,
    false_target: usize,
    inspections: u64,
}

impl str::FromStr for Monkey {
    type Err = &'static str;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let invalid_number = |_| "invalid number";

        let mut lines = input.lines();

        let id = lines.next().ok_or("no monkey id")?;
        let id = id["Monkey ".len()..id.len() - 1]
            .parse()
            .map_err(invalid_number)?;

        let items = lines.next().ok_or("no items")?;
        let items = items["  Starting items: ".len()..]
            .split(", ")
            .map(|item| item.parse())
            .collect::<Result<VecDeque<_>, _>>()
            .map_err(invalid_number)?;

        let operation = lines.next().ok_or("no operation")?;
        let operation = match &operation[0.."  Operation: new = old * ".len()] {
            "  Operation: new = old + " => Operation::Add(
                operation["  Operation: new = old * ".len()..]
                    .parse()
                    .map_err(invalid_number)?,
            ),
            "  Operation: new = old * "
                if &operation["  Operation: new = old * ".len()..] == "old" =>
            {
                Operation::Square
            }
            "  Operation: new = old * " => Operation::Multiply(
                operation["  Operation: new = old * ".len()..]
                    .parse()
                    .map_err(invalid_number)?,
            ),
            _ => Err("invalid operation")?,
        };

        let test = lines.next().ok_or("no test")?;
        let test = test["  Test: divisible by ".len()..]
            .parse()
            .map_err(invalid_number)?;

        let true_target = lines.next().ok_or("no true target")?;
        let true_target = true_target["    If true: throw to monkey ".len()..]
            .parse()
            .map_err(invalid_number)?;

        let false_target = lines.next().ok_or("no false target")?;
        let false_target = false_target["    If false: throw to monkey ".len()..]
            .parse()
            .map_err(invalid_number)?;

        Ok(Monkey {
            id,
            items,
            operation,
            test,
            true_target,
            false_target,
            inspections: 0,
        })
    }
}

fn solve<FD: Fn(u64) -> u64, F: FnOnce(&[Monkey]) -> FD>(input: &str, n: usize, f: F) -> u64 {
    let mut monkeys = input
        .split("\n\n")
        .map(|monkey| monkey.parse::<Monkey>().expect("invalid monkey"))
        .collect::<Vec<_>>();

    let divisor = f(&monkeys);

    for _r in 0..n {
        for i in 0..monkeys.len() {
            while let Some(item) = monkeys[i].items.pop_front() {
                monkeys[i].inspections += 1;

                let item = monkeys[i].operation.eval(item);
                let item = divisor(item);
                let target = if item % monkeys[i].test == 0 {
                    monkeys[i].true_target
                } else {
                    monkeys[i].false_target
                };
                monkeys[target].items.push_back(item);
            }
        }
    }

    let mut inspections = monkeys
        .iter()
        .map(|monkey| monkey.inspections)
        .collect::<Vec<_>>();

    inspections.sort_by(|a, b| b.cmp(a));

    inspections[0] * inspections[1]
}

pub fn solve_1(input: &str) -> u64 {
    solve(input, 20, |_| |item| item / 3)
}

pub fn solve_2(input: &str) -> u64 {
    solve(input, 10_000, |monkeys| {
        let divisor: u64 = monkeys.iter().map(|monkey| monkey.test).product();
        move |item| item % divisor
    })
}

pub fn part_1() -> u64 {
    solve_1(&INPUT)
}

pub fn part_2() -> u64 {
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
        assert_eq!(solve_1(&EXAMPLE1), 10605);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&EXAMPLE1), 2713310158);
    }
}

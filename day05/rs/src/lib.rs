use lazy_static::lazy_static;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

fn solve<F: Fn(&mut Vec<Vec<char>>, usize, usize, usize)>(input: &str, f: F) -> String {
    let (stacks, moves) = input.split_once("\n\n").unwrap();

    let mut cargo: Vec<Vec<char>> = Vec::new();
    for _ in 0..9 {
        cargo.push(Vec::new());
    }

    for line in stacks.lines().rev().skip(1) {
        for (stack, value) in line
            .as_bytes()
            .chunks(4)
            .map(|arr| arr[1] as char)
            .enumerate()
        {
            if value != ' ' {
                cargo[stack].push(value);
            }
        }
    }

    for m in moves.lines() {
        let mut i = m.split_whitespace();
        i.next(); // move
        let quantity = i.next().unwrap().parse().unwrap();
        i.next(); // from
        let from = i.next().unwrap().parse().unwrap();
        i.next(); // to
        let to = i.next().unwrap().parse().unwrap();

        f(&mut cargo, quantity, from, to);
    }

    cargo
        .into_iter()
        .map(|stack| *stack.last().unwrap_or(&' '))
        .filter(|c| *c != ' ')
        .collect::<String>()
}

pub fn solve_1(input: &str) -> String {
    solve(input, |cargo, quantity, from, to| {
        for _ in 0..quantity {
            let value = cargo[from - 1].pop().unwrap();
            cargo[to - 1].push(value);
        }
    })
}

pub fn solve_2(input: &str) -> String {
    solve(input, |cargo, quantity, from, to| {
        let cut = cargo[from - 1].len() - quantity;
        let mut v = cargo[from - 1].split_off(cut);
        cargo[to - 1].append(&mut v);
    })
}

pub fn part_1() -> String {
    solve_1(&INPUT)
}

pub fn part_2() -> String {
    solve_2(&INPUT)
}

#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref INPUT: &'static str = include_str!("../../example1");
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), "CMZ");
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), "MCD");
    }
}

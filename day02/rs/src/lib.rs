use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

// ROCK: A, X 1
// PAPER: B, Y 2
// SHISSOR: C, Z 3
fn play1(p1: &str, p2: &str) -> u32 {
    match (p1, p2) {
        ("A", "X") => 4,
        ("A", "Y") => 8,
        ("A", "Z") => 3,
        ("B", "X") => 1,
        ("B", "Y") => 5,
        ("B", "Z") => 9,
        ("C", "X") => 7,
        ("C", "Y") => 2,
        ("C", "Z") => 6,
        _ => unreachable!(),
    }
}

fn play2(p1: &str, p2: &str) -> u32 {
    match (p1, p2) {
        ("A", "X") => 3,
        ("A", "Y") => 4,
        ("A", "Z") => 8,
        ("B", "X") => 1,
        ("B", "Y") => 5,
        ("B", "Z") => 9,
        ("C", "X") => 2,
        ("C", "Y") => 6,
        ("C", "Z") => 7,
        _ => unreachable!(),
    }
}

fn solve<F: Fn(&str, &str) -> u32>(input: &str, f: F) -> u32 {
    input
        .lines()
        .map(|line| {
            let mut i = line.split_whitespace();
            let p1 = i.next().unwrap();
            let p2 = i.next().unwrap();
            f(p1, p2)
        })
        .sum()
}

fn solve_1(input: &str) -> u32 {
    solve(input, play1)
}

fn solve_2(input: &str) -> u32 {
    solve(input, play2)
}

pub fn part_1() -> u32 {
    solve_1(&INPUT)
}

pub fn part_2() -> u32 {
    solve_2(&INPUT)
}

#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref INPUT: &'static str = r#"A Y
B X
C Z"#;
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 15);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 12);
    }
}

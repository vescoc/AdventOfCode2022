use lazy_static::lazy_static;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

pub fn solve_1(input: &str) -> u32 {
    input
        .split("\n\n")
        .map(|part| part.lines().map(|line| line.parse::<u32>().unwrap()).sum())
        .max()
        .unwrap()
}

pub fn solve_2(input: &str) -> u32 {
    let mut calories = input
        .split("\n\n")
        .map(|part| part.lines().map(|line| line.parse::<u32>().unwrap()).sum())
        .collect::<Vec<u32>>();
    calories.sort_by(|a, b| b.cmp(a));
    calories.into_iter().take(3).sum()
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
        static ref INPUT: &'static str = r#"1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"#;
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&INPUT), 24000);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&INPUT), 45000);
    }
}

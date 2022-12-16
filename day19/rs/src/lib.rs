use lazy_static::lazy_static;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

pub fn solve_1(_input: &str) -> usize {
    todo!()
}

pub fn solve_2(_input: &str) -> usize {
    todo!()
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
        assert_eq!(solve_1(&EXAMPLE1), 666);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&EXAMPLE1), 666);
    }
}

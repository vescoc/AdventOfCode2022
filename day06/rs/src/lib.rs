use lazy_static::lazy_static;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

fn solve<const LEN: usize>(input: &str) -> usize {
    input
        .as_bytes()
        .windows(LEN)
        .enumerate()
        .find_map(|(i, data)| {
            for (ii, c) in data.iter().take(data.len() - 1).enumerate() {
                for o in &data[ii + 1..] {
                    if c == o {
                        return None;
                    }
                }
            }
            Some(i)
        })
        .unwrap()
        + LEN
}

pub fn solve_1(input: &str) -> usize {
    solve::<4>(input)
}

pub fn solve_2(input: &str) -> usize {
    solve::<14>(input)
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
        assert_eq!(solve_1(&EXAMPLE1), 7);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&EXAMPLE1), 19);
    }
}

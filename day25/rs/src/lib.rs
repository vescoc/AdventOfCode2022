use lazy_static::lazy_static;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

trait ToSnafu {
    fn to_snafu(self) -> String;
}

impl ToSnafu for i64 {
    fn to_snafu(mut self) -> String {
        let convert_digit = |d| match d {
            -2 => '=',
            -1 => '-',
            0 => '0',
            1 => '1',
            2 => '2',
            _ => unreachable!(),
        };

        let mut r = Vec::new();
        loop {
            if self.abs() <= 2 {
                r.push(convert_digit(self));
                break;
            } else {
                let (mut da, mut u) = (self / 5, self % 5);
                if u >= 3 {
                    da += 1;
                    u -= 5;
                }
                r.push(convert_digit(u));
                self = da;
            }
        }

        r.iter().rev().collect()
    }
}

pub fn solve_1(input: &str) -> String {
    let sum = input
        .lines()
        .map(|line| {
            line.chars()
                .map(|c| match c {
                    '=' => -2,
                    '-' => -1,
                    _ => c.to_digit(3).expect("invalid digit") as i64,
                })
                .fold(0, |acc, d| acc * 5 + d)
        })
        .sum::<i64>();

    sum.to_snafu()
}

pub fn part_1() -> String {
    solve_1(&INPUT)
}

#[cfg(test)]
mod tests {
    use super::*;

    lazy_static! {
        static ref EXAMPLE1: &'static str = include_str!("../../example1");
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&EXAMPLE1), "2=-1=0".to_string());
    }
}

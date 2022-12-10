use lazy_static::lazy_static;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

fn parse(input: &str) -> Vec<i32> {
    let mut cycle = vec![1];
    for line in input.lines() {
        let last = *cycle.last().unwrap();
        if &line[0..4] == "noop" {
            cycle.push(last);
        } else if &line[0..4] == "addx" {
            cycle.push(last);

            let value = &line[5..].parse().unwrap();
            cycle.push(last + value);
        }
    }

    cycle
}

pub fn solve_1(input: &str) -> i32 {
    let cycle = parse(input);
    
    [20, 60, 100, 140, 180, 220]
        .into_iter()
        .map(|i| cycle[i - 1] * i as i32)
        .sum()
}

pub fn solve_2(input: &str) -> String {
    let cycle = parse(input);

    let mut display = [['.'; 40]; 6];
    for (r, row) in display.iter_mut().enumerate() {
        for (c, v) in row.iter_mut().enumerate() {
            let i = r * 40 + c;
            let value = cycle[i];
            if (value - 1..value + 2).contains(&((i - r * 40) as i32)) {
                *v = '#';
            }
        }
    }

    let mut result = String::new();
    for (r, row) in display.iter().enumerate() {
        result += &row.iter().collect::<String>();
        if r != 5 {
            result += "\n";
        }
    }

    result
}

pub fn part_1() -> i32 {
    solve_1(&INPUT)
}

pub fn part_2() -> String {
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
        assert_eq!(solve_1(&EXAMPLE1), 13140);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(
            solve_2(&EXAMPLE1),
            r"##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######....."
        );
    }
}

use lazy_static::lazy_static;

lazy_static! {
    static ref INPUT: &'static str = include_str!("../../input");
}

fn solve_1(input: &str) -> u32 {
    input
        .lines()
        .fold((0, u32::MIN),
              |(acc, max), line| {
                  if let Ok(value) = line.parse::<u32>() {
                      (acc + value, max)
                  } else {
                      (0, max.max(acc))
                  }
              }).1
}

fn solve_2(input: &str) -> u32 {
    let mut calories = input
        .lines()
        .fold((0, vec![]),
              |(acc, mut calories), line| {
                  if let Ok(value) = line.parse::<u32>() {
                      (acc + value, calories)
                  } else {
                      (0,
                       {
                           calories.push(acc);
                           calories
                       })
                  }
              }).1;
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
        static ref INPUT: &'static str = r#"XX"#;
    }

    #[test]
    fn same_results_1() {
        // assert_eq!(solve_1(&INPUT), 666);
        todo!();
    }

    #[test]
    fn same_results_2() {
        // assert_eq!(solve_2(&INPUT), 666);
        todo!();
    }
}

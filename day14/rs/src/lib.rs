use lazy_static::lazy_static;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

#[derive(Debug)]
struct Waterfall {
    area: Vec<Vec<u8>>,
    start: (usize, usize),
}

impl Waterfall {
    fn new(input: &str, baseline: bool) -> Self {
        let (mut min_x, mut max_x, mut max_y) = (usize::MAX, 0, 0);
        let mut lines = input
            .lines()
            .map(|line| {
                line.split(" -> ")
                    .map(|point| {
                        let (x, y) = point.split_once(',').unwrap();
                        let (x, y) = (x.parse().unwrap(), y.parse().unwrap());
                        if x < min_x {
                            min_x = x;
                        }
                        if x > max_x {
                            max_x = x;
                        }
                        if y > max_y {
                            max_y = y;
                        }
                        (x, y)
                    })
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        if baseline {
            max_y += 2;
            min_x = min_x.min(500 - max_y - 1);
            max_x = max_x.max(500 + max_y + 1);

            lines.push(vec![(min_x, max_y), (max_x, max_y)]);
        }

        let mut area = vec![vec![0; max_x - min_x + 1]; max_y + 1];
        for line in lines {
            for points in line.windows(2) {
                let (start_x, start_y) =
                    (points[0].0.min(points[1].0), points[0].1.min(points[1].1));
                let (end_x, end_y) = (points[0].0.max(points[1].0), points[0].1.max(points[1].1));
                match (end_x - start_x, end_y - start_y) {
                    (0, d) => {
                        for i in 0..=d {
                            area[start_y + i][start_x - min_x] = 1;
                        }
                    }

                    (d, 0) => {
                        for i in 0..=d {
                            area[start_y][start_x - min_x + i] = 1;
                        }
                    }

                    _ => panic!("invalid line"),
                }
            }
        }

        Self {
            area,
            start: (500 - min_x, 0),
        }
    }

    fn pour(&mut self) -> Option<usize> {
        let (mut x, mut y) = self.start;

        let mut moves = 0;
        loop {
            let cell = self.area.get(y).and_then(|row| row.get(x))?;
            if *cell == 1 {
                if (x, y) == self.start {
                    break Some(0);
                }

                let left_cell =
                    self.area
                        .get(y)
                        .and_then(|row| if x > 0 { row.get(x - 1) } else { None })?;
                if *left_cell == 1 {
                    let right_cell = self.area.get(y).and_then(|row| row.get(x + 1))?;
                    if *right_cell == 1 {
                        self.area[y - 1][x] = 1;
                        break Some(moves);
                    } else {
                        x += 1;
                    }
                } else {
                    x -= 1;
                }
            } else {
                y += 1;
            }

            moves += 1;
        }
    }
}

pub fn solve_1(input: &str) -> usize {
    let mut area = Waterfall::new(input, false);

    let mut count = 0;
    while area.pour().is_some() {
        count += 1;
    }

    count
}

pub fn solve_2(input: &str) -> usize {
    let mut area = Waterfall::new(input, true);

    let mut count = 0;
    loop {
        let moves = area.pour().unwrap();
        if moves > 0 {
            count += 1;
        } else {
            break;
        }
    }

    count
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
        assert_eq!(solve_1(&EXAMPLE1), 24);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&EXAMPLE1), 93);
    }
}

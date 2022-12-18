use std::collections::{HashSet, VecDeque};

use lazy_static::lazy_static;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

type Cube = (i32, i32, i32);

fn distance(a: &Cube, b: &Cube) -> i32 {
    (a.0 - b.0).abs() + (a.1 - b.1).abs() + (a.2 - b.2).abs()
}

pub fn solve_1(input: &str) -> usize {
    let cubes = input
        .lines()
        .map(|line| {
            let mut line = line.split(',');
            (
                line.next()
                    .expect("x not found")
                    .parse()
                    .expect("invalid x"),
                line.next()
                    .expect("y not found")
                    .parse()
                    .expect("invalid y"),
                line.next()
                    .expect("y not found")
                    .parse()
                    .expect("invalid y"),
            )
        })
        .collect::<Vec<Cube>>();

    let mut faces = 6 * cubes.len();
    for (ia, a) in cubes.iter().enumerate() {
        for (ib, b) in cubes.iter().enumerate() {
            if ia != ib && distance(a, b) <= 1 {
                faces -= 1;
            }
        }
    }

    faces
}

pub fn solve_2(input: &str) -> usize {
    let ((mut min_x, mut max_x), (mut min_y, mut max_y), (mut min_z, mut max_z)) = (
        (i32::MAX, i32::MIN),
        (i32::MAX, i32::MIN),
        (i32::MAX, i32::MIN),
    );

    let cubes = input
        .lines()
        .map(|line| {
            let mut line = line.split(',');

            let x = line
                .next()
                .expect("x not found")
                .parse()
                .expect("invalid x");
            let y = line
                .next()
                .expect("y not found")
                .parse()
                .expect("invalid y");
            let z = line
                .next()
                .expect("z not found")
                .parse()
                .expect("invalid z");

            min_x = min_x.min(x);
            max_x = max_x.max(x);

            min_y = min_y.min(y);
            max_y = max_y.max(y);

            min_z = min_z.min(z);
            max_z = max_z.max(z);

            (x, y, z)
        })
        .collect::<HashSet<Cube>>();

    min_x -= 1;
    max_x += 1;

    min_y -= 1;
    max_y += 1;

    min_z -= 1;
    max_z += 1;

    let mut reachable_faces = 0;

    let mut visited = HashSet::with_capacity(
        ((min_x - max_x).abs() * (min_y - max_y).abs() * (min_z - max_z).abs()) as usize,
    );
    let mut queue = VecDeque::new();

    let current = (min_x, min_y, min_z);
    visited.insert(current);
    queue.push_back(current);

    while let Some((x, y, z)) = queue.pop_front() {
        for (dx, dy, dz) in [
            (1, 0, 0),
            (-1, 0, 0),
            (0, 1, 0),
            (0, -1, 0),
            (0, 0, 1),
            (0, 0, -1),
        ] {
            let (x, y, z) = (x + dx, y + dy, z + dz);
            if (min_x..=max_x).contains(&x)
                && (min_y..=max_y).contains(&y)
                && (min_z..=max_z).contains(&z)
                && !visited.contains(&(x, y, z))
            {
                if cubes.contains(&(x, y, z)) {
                    reachable_faces += 1;
                } else {
                    visited.insert((x, y, z));
                    queue.push_back((x, y, z));
                }
            }
        }
    }

    reachable_faces
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
        assert_eq!(solve_1(&EXAMPLE1), 64);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&EXAMPLE1), 58);
    }
}

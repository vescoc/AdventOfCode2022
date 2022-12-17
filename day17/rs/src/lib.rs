use lazy_static::lazy_static;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

type Coord = (usize, usize);

#[derive(Copy, Clone, Debug)]
enum Shape {
    Line,
    Cross,
    L,
    I,
    Square,
}

#[derive(PartialEq, Hash, Eq, Clone)]
struct Display(Vec<u8>);

impl Shape {
    fn shape(&self) -> &'static [Coord] {
        use Shape::*;
        match self {
            Line => &[(0, 0), (1, 0), (2, 0), (3, 0)],
            Cross => &[(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
            L => &[(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
            I => &[(0, 0), (0, 1), (0, 2), (0, 3)],
            Square => &[(0, 0), (1, 0), (0, 1), (1, 1)],
        }
    }

    fn draw_at(&self, (x, y): Coord) -> Vec<Coord> {
        self.shape()
            .iter()
            .map(|(dx, dy)| (x + dx, y + dy))
            .collect()
    }
}

impl Display {
    fn new() -> Self {
        Display(Vec::new())
    }

    fn height(&self) -> usize {
        self.0
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, v)| if *v != 0 { Some(i + 1) } else { None })
            .unwrap_or(0)
    }

    fn check(&self, coords: &[Coord]) -> bool {
        coords.iter().all(|p| self.is_air(p))
    }

    fn is_air(&self, (x, y): &Coord) -> bool {
        (0..7).contains(x) && (*y >= self.0.len() || self.0[*y] & (1 << (*x)) == 0)
    }

    fn draw(&mut self, (x, y): &Coord) {
        while self.0.len() <= *y {
            self.0.push(0);
        }
        self.0[*y] |= 1 << *x;
    }

    fn draw_coords(&mut self, coords: &[Coord]) {
        for c in coords {
            self.draw(c);
        }
    }

    fn left(&self, shape: Shape, (x, y): Coord) -> Coord {
        if x > 0 {
            let (nx, ny) = (x - 1, y);
            let points = shape.draw_at((nx, ny));
            if self.check(&points) {
                (nx, ny)
            } else {
                (x, y)
            }
        } else {
            (x, y)
        }
    }

    fn right(&self, shape: Shape, (x, y): Coord) -> Coord {
        let (nx, ny) = (x + 1, y);
        let points = shape.draw_at((nx, ny));
        if self.check(&points) {
            (nx, ny)
        } else {
            (x, y)
        }
    }

    fn down(&mut self, shape: Shape, (x, y): Coord) -> Option<Coord> {
        if y > 0 {
            let (nx, ny) = (x, y - 1);
            let points = shape.draw_at((nx, ny));
            if self.check(&points) {
                Some((nx, ny))
            } else {
                self.draw_coords(&shape.draw_at((x, y)));
                None
            }
        } else {
            self.draw_coords(&shape.draw_at((x, y)));
            None
        }
    }
}

pub fn solve_1(input: &str) -> usize {
    let mut display = Display::new();

    let mut jets = input.chars().cycle();

    for (_i, shape) in [Shape::Line, Shape::Cross, Shape::L, Shape::I, Shape::Square]
        .iter()
        .cycle()
        .enumerate()
        .take(2022)
    {
        let (mut x, mut y) = (2, display.height() + 3);

        loop {
            match jets.next() {
                Some('<') => {
                    (x, y) = display.left(*shape, (x, y));
                }
                Some('>') => {
                    (x, y) = display.right(*shape, (x, y));
                }
                j => panic!("invalid jet: {:?}", j),
            }

            (x, y) = {
                if let Some((x, y)) = display.down(*shape, (x, y)) {
                    (x, y)
                } else {
                    break;
                }
            };
        }
    }

    display.height()
}

pub fn solve_2(input: &str) -> usize {
    let mut display = Display::new();

    let mut jets = input.chars().cycle();

    let mut deltas = Vec::new();

    for (_i, shape) in [Shape::Line, Shape::Cross, Shape::L, Shape::I, Shape::Square]
        .iter()
        .cycle()
        .enumerate()
        .take(5_000)
    {
        let start_height = display.height();

        let (mut x, mut y) = (2, start_height + 3);

        loop {
            match jets.next() {
                Some('<') => {
                    (x, y) = display.left(*shape, (x, y));
                }
                Some('>') => {
                    (x, y) = display.right(*shape, (x, y));
                }
                j => panic!("invalid jet: {:?}", j),
            }

            (x, y) = {
                if let Some((x, y)) = display.down(*shape, (x, y)) {
                    (x, y)
                } else {
                    deltas.push(display.height() - start_height);

                    break;
                }
            };
        }
    }

    let (offset, size) = (10..500)
        .find_map(|offset| {
            (2..=2_500)
                .find(|size| {
                    let candidate = &deltas[offset..offset + size];
                    deltas[offset..]
                        .chunks(*size)
                        .all(|chunk| &candidate[..chunk.len()] == chunk)
                })
                .map(|size| (offset, size))
        })
        .unwrap();

    let mut count = 1_000_000_000_000;
    count -= offset;

    let offset_delta = deltas[0..offset].iter().sum::<usize>();

    let cycle_delta = deltas[offset..offset + size].iter().sum::<usize>();

    let cycle_count = count / size;

    count %= size;

    let remaining_height = deltas[offset..offset + count].iter().sum::<usize>();

    offset_delta + cycle_count * cycle_delta + remaining_height
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
        assert_eq!(solve_1(&EXAMPLE1), 3068);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&EXAMPLE1), 1514285714288);
    }
}

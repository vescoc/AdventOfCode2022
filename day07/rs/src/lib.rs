use std::collections::HashMap;

use lazy_static::lazy_static;

lazy_static! {
    pub static ref INPUT: &'static str = include_str!("../../input");
}

#[derive(Debug)]
enum Node<'a> {
    Dir(&'a str),
    File(&'a str, u32),
}

fn parse_filesystem(input: &str) -> HashMap<Vec<&str>, u32> {
    let mut filesystem = HashMap::new();
    let mut cwd = Vec::new();

    for line in input.lines() {
        let mut parts = line.split_whitespace();
        match parts.next() {
            Some("$") => match parts.next() {
                Some("cd") => {
                    match parts.next() {
                        Some("/") => cwd.clear(),
                        Some("..") => {
                            cwd.pop();
                        }
                        Some(dir) => cwd.push(dir),
                        _ => panic!("invalid cd"),
                    }

                    filesystem
                        .entry(cwd.clone())
                        .or_insert_with(Vec::new);
                },
                Some("ls") => {}
                _ => panic!("invalid command"),
            },
            Some("dir") => filesystem.get_mut(&cwd)
                .unwrap()
                .push(Node::Dir(parts.next().unwrap())),
            Some(size) => filesystem.get_mut(&cwd)
                .unwrap()
                .push(Node::File(
                    parts.next().unwrap(),
                    size.parse().expect("invalid size"),
                )),
            _ => panic!("invalid node"),
        }
    }

    visit(HashMap::new(), &filesystem, vec![])
}

fn visit<'a>(
    mut current: HashMap<Vec<&'a str>, u32>,
    filesystem: &HashMap<Vec<&'a str>, Vec<Node<'a>>>,
    cwd: Vec<&'a str>,
) -> HashMap<Vec<&'a str>, u32> {
    let mut sum = 0;
    for entry in filesystem.get(&cwd).unwrap() {
        match entry {
            Node::File(_, size) => sum += size,
            Node::Dir(dir) => {
                let wd = {
                    let mut wd = cwd.clone();
                    wd.push(dir);
                    wd
                };
                current = visit(current, filesystem, wd.clone());
                sum += current[&wd];
            }
        }
    }

    current.insert(cwd, sum);

    current
}

pub fn solve_1(input: &str) -> u32 {
    parse_filesystem(input)
        .values()
        .filter(|size| **size <= 100_000)
        .sum()
}

pub fn solve_2(input: &str) -> u32 {
    let filesystem = parse_filesystem(input);

    let used_space = filesystem[&vec![]];
    let unused_space = 70_000_000 - used_space;
    let needed_space = 30_000_000 - unused_space;

    *filesystem
        .values()
        .filter(|size| **size >= needed_space)
        .min()
        .unwrap()
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
        static ref EXAMPLE1: &'static str = include_str!("../../example1");
    }

    #[test]
    fn same_results_1() {
        assert_eq!(solve_1(&EXAMPLE1), 95437);
    }

    #[test]
    fn same_results_2() {
        assert_eq!(solve_2(&EXAMPLE1), 24933642);
    }
}

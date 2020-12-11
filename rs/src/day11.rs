use itertools::Itertools;
use std::cmp::{max, min};
use std::collections::HashMap;
use std::iter;

const DIRECTIONS: [(isize, isize); 8] = [
    (-1, -1),
    (-1, 0),
    (-1, 1),
    (0, -1),
    (0, 1),
    (1, -1),
    (1, 0),
    (1, 1),
];

fn parse<'a, I, S>(
    lines: I,
) -> (
    ((isize, isize), (isize, isize)),
    HashMap<(isize, isize), bool>,
)
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (mut x0, mut y0, mut x1, mut y1) = (isize::MAX, isize::MAX, isize::MIN, isize::MIN);
    let mut m = HashMap::new();
    for (y, line) in lines.into_iter().enumerate() {
        let y = y as isize;
        for (x, c) in line.as_ref().chars().enumerate() {
            let x = x as isize;
            if c == '#' {
                m.insert((x, y), true);
            } else if c == 'L' {
                m.insert((x, y), false);
            } else {
                continue;
            }
            x0 = min(x0, x);
            y0 = min(y0, y);
            x1 = max(x1, x);
            y1 = max(y1, y);
        }
    }
    (((x0, y0), (x1, y1)), m)
}

fn step<F>(m: &HashMap<(isize, isize), bool>, d: usize, adj: F) -> HashMap<(isize, isize), bool>
where
    F: Fn((isize, isize), (isize, isize)) -> bool,
{
    let mut next = HashMap::new();
    for (pos, b) in m.iter() {
        let n = DIRECTIONS.iter().filter(|dir| adj(*pos, **dir)).count();
        next.insert(*pos, if *b { n < d } else { n == 0 });
    }
    next
}

fn in_range<A, B>(bounds: &((A, B), (A, B)), value: &(A, B)) -> bool
where
    A: Ord,
    B: Ord,
{
    let ((x0, y0), (x1, y1)) = bounds;
    let (x, y) = value;
    (x0..=x1).contains(&x) && (y0..=y1).contains(&y)
}

pub fn part1<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (_, m0) = parse(lines);
    iter::successors(Some(m0), |m| {
        Some(step(m, 4, |(x, y), (dx, dy)| {
            m.get(&(x + dx, y + dy)).copied().unwrap_or(false)
        }))
    })
    .map(|m| m.values().filter(|b| **b).count())
    .tuple_windows()
    .filter(|(l, r)| *l == *r)
    .map(|(l, _)| l)
    .next()
}

pub fn part2<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (bounds, m0) = parse(lines);
    iter::successors(Some(m0), |m| {
        Some(step(m, 5, |(x, y), (dx, dy)| {
            for i in 1.. {
                let k = (x + i * dx, y + i * dy);
                if !in_range(&bounds, &k) {
                    break;
                }
                if let Some(b) = m.get(&k) {
                    return *b;
                }
            }
            false
        }))
    })
    .map(|m| m.values().filter(|b| **b).count())
    .tuple_windows()
    .filter(|(l, r)| *l == *r)
    .map(|(l, _)| l)
    .next()
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "L.LL.LL.LL",
        "LLLLLLL.LL",
        "L.L.L..L..",
        "LLLL.LL.LL",
        "L.LL.LL.LL",
        "L.LLLLL.LL",
        "..L.L.....",
        "LLLLLLLLLL",
        "L.LLLLLL.L",
        "L.LLLLL.LL",
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(Some(37), part1(EXAMPLE));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(26), part2(EXAMPLE));
    }
}

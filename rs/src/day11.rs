use itertools::Itertools;
use std::convert::TryInto;
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

fn parse<'a, I, S>(lines: I, is_far: bool) -> (Vec<Vec<usize>>, Vec<bool>)
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (positions, values): (Vec<_>, _) = lines
        .into_iter()
        .enumerate()
        .flat_map(|(y, line)| {
            line.as_ref()
                .chars()
                .enumerate()
                .filter_map(move |(x, c)| match c {
                    '#' => Some(((y, x), true)),
                    'L' => Some(((y, x), false)),
                    _ => None,
                })
        })
        .unzip();
    let max_y = positions.iter().map(|(y, _)| *y).max().unwrap_or(0);
    let max_x = positions.iter().map(|(_, x)| *x).max().unwrap_or(0);
    (
        positions
            .iter()
            .map(|(y, x)| {
                DIRECTIONS
                    .iter()
                    .filter_map(|(dy, dx)| {
                        let mut ty = TryInto::<isize>::try_into(*y).ok()?.checked_add(*dy)?;
                        let mut tx = TryInto::<isize>::try_into(*x).ok()?.checked_add(*dx)?;
                        while let (Some(y), Some(x)) = (
                            ty.try_into().ok().filter(|ty| *ty <= max_y),
                            tx.try_into().ok().filter(|tx| *tx <= max_x),
                        ) {
                            if let Ok(i) = positions.binary_search(&(y, x)) {
                                return Some(i);
                            }
                            if !is_far {
                                break;
                            }
                            ty = ty.checked_add(*dy)?;
                            tx = tx.checked_add(*dx)?;
                        }
                        None
                    })
                    .collect()
            })
            .collect(),
        values,
    )
}

fn step<I>(d: usize, adjs: &[I], m: &[bool]) -> Vec<bool>
where
    I: IntoIterator<Item = usize> + Clone,
{
    m.iter()
        .zip(adjs)
        .map(|(b, ixs)| {
            if *b {
                ixs.clone().into_iter().filter(|i| m[*i]).nth(d).is_none()
            } else {
                ixs.clone().into_iter().find(|i| m[*i]).is_none()
            }
        })
        .collect()
}

pub fn part1<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (adjs, m) = parse(lines, false);
    iter::successors(Some(m), |m| Some(step(3, &adjs, &m)))
        .map(|m| m.iter().filter(|b| **b).count())
        .tuple_windows()
        .find(|(l, r)| *l == *r)
        .map(|(n, _)| n)
}

pub fn part2<'a, I, S>(lines: I) -> Option<usize>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (adjs, m) = parse(lines, true);
    iter::successors(Some(m), |m| Some(step(4, &adjs, &m)))
        .map(|m| m.iter().filter(|b| **b).count())
        .tuple_windows()
        .find(|(l, r)| *l == *r)
        .map(|(n, _)| n)
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

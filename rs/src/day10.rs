use super::util;
use itertools::Itertools;
use std::collections::VecDeque;
use std::num::ParseIntError;

pub fn part1<'a, I, S>(lines: I) -> Result<usize, ParseIntError>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut nums = util::parse_many(lines)?;
    nums.sort_unstable();
    let (mut x, mut y) = (0, 0);
    for (a, b) in [0].iter().chain(&nums).tuple_windows() {
        match b - a {
            1 => x += 1,
            3 => y += 1,
            _ => {}
        }
    }
    Ok(x * (y + 1))
}

pub fn part2<'a, I, S>(lines: I) -> Result<usize, ParseIntError>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut nums = util::parse_many(lines)?;
    nums.sort_unstable();
    let mut k = VecDeque::with_capacity(0);
    k.push_back(1);
    k.push_back(0);
    k.push_back(0);
    k.push_back(0);
    for (a, b) in [0].iter().chain(&nums).tuple_windows() {
        for _ in 0..b - a {
            k.pop_back();
            k.push_front(0);
        }
        k[0] += k.iter().sum::<usize>();
    }
    Ok(k[0])
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE_1: &[&str] = &["16", "10", "15", "5", "1", "11", "7", "19", "6", "12", "4"];
    static EXAMPLE_2: &[&str] = &[
        "28", "33", "18", "42", "31", "14", "46", "20", "48", "47", "24", "23", "49", "45", "19",
        "38", "39", "11", "1", "32", "25", "35", "8", "17", "7", "9", "4", "2", "34", "10", "3",
    ];

    #[test]
    fn part1_examples() -> Result<(), ParseIntError> {
        assert_eq!(35, part1(EXAMPLE_1)?);
        assert_eq!(220, part1(EXAMPLE_2)?);
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), ParseIntError> {
        assert_eq!(8, part2(EXAMPLE_1)?);
        assert_eq!(19208, part2(EXAMPLE_2)?);
        Ok(())
    }
}

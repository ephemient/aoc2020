use std::collections::HashMap;
use std::num::ParseIntError;

pub fn part1<'a, I, S>(lines: I) -> Result<Option<usize>, ParseIntError>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    run(2020, lines)
}

pub fn part2<'a, I, S>(lines: I) -> Result<Option<usize>, ParseIntError>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    run(30000000, lines)
}

fn run<'a, I, S>(n: usize, lines: I) -> Result<Option<usize>, ParseIntError>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let nums = match lines.into_iter().next() {
        Some(line) => line
            .as_ref()
            .split(',')
            .map(|s| s.parse())
            .collect::<Result<Vec<usize>, _>>()?,
        None => return Ok(None),
    };
    if n < nums.len() {
        return Ok(Some(nums[n - 1]));
    }
    let mut last = match nums.last() {
        Some(last) => *last,
        None => return Ok(None),
    };
    let mut seen = nums[0..nums.len() - 1]
        .into_iter()
        .copied()
        .enumerate()
        .map(|(i, x)| (x, i))
        .collect::<HashMap<_, _>>();
    for i in seen.len()..n - 1 {
        let next = seen.get(&last).map_or(0, |x| i - *x);
        seen.insert(last, i);
        last = next;
    }
    Ok(Some(last))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn part1_examples() -> Result<(), ParseIntError> {
        assert_eq!(Some(0), run(1, &["0,3,6"])?);
        assert_eq!(Some(3), run(2, &["0,3,6"])?);
        assert_eq!(Some(6), run(3, &["0,3,6"])?);
        assert_eq!(Some(0), run(4, &["0,3,6"])?);
        assert_eq!(Some(3), run(5, &["0,3,6"])?);
        assert_eq!(Some(3), run(6, &["0,3,6"])?);
        assert_eq!(Some(1), run(7, &["0,3,6"])?);
        assert_eq!(Some(0), run(8, &["0,3,6"])?);
        assert_eq!(Some(4), run(9, &["0,3,6"])?);
        assert_eq!(Some(0), run(10, &["0,3,6"])?);
        assert_eq!(Some(436), part1(&["0,3,6"])?);
        assert_eq!(Some(1), part1(&["1,3,2"])?);
        assert_eq!(Some(10), part1(&["2,1,3"])?);
        assert_eq!(Some(27), part1(&["1,2,3"])?);
        assert_eq!(Some(78), part1(&["2,3,1"])?);
        assert_eq!(Some(438), part1(&["3,2,1"])?);
        assert_eq!(Some(1836), part1(&["3,1,2"])?);
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), ParseIntError> {
        assert_eq!(Some(175594), part2(&["0,3,6"])?);
        assert_eq!(Some(2578), part2(&["1,3,2"])?);
        assert_eq!(Some(3544142), part2(&["2,1,3"])?);
        assert_eq!(Some(261214), part2(&["1,2,3"])?);
        assert_eq!(Some(6895259), part2(&["2,3,1"])?);
        assert_eq!(Some(18), part2(&["3,2,1"])?);
        assert_eq!(Some(362), part2(&["3,1,2"])?);
        Ok(())
    }
}

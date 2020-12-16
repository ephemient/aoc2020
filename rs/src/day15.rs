use std::cmp::max;
use std::num::ParseIntError;

pub fn part1<'a, I, S>(lines: I) -> Result<Option<u32>, ParseIntError>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    run(2020, lines)
}

pub fn part2<'a, I, S>(lines: I) -> Result<Option<u32>, ParseIntError>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    run(30000000, lines)
}

fn run<'a, I, S>(n: u32, lines: I) -> Result<Option<u32>, ParseIntError>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let nums = match lines.into_iter().next() {
        Some(line) => line
            .as_ref()
            .split(',')
            .map(|s| s.parse())
            .collect::<Result<Vec<u32>, _>>()?,
        None => return Ok(None),
    };
    if n <= nums.len() as u32 {
        return Ok(Some(nums[n as usize - 1]));
    }
    let mut last = match nums.last() {
        Some(last) => *last,
        None => return Ok(None),
    };
    let top = max(n, nums.iter().map(|x| x + 1).max().unwrap_or(0));
    let mut seen = vec![0; top as usize];
    for (i, x) in nums[0..nums.len() - 1].iter().enumerate() {
        seen[*x as usize] = i as u32 + 1
    }
    for i in nums.len() as u32..n {
        let j = seen[last as usize];
        let next = if j == 0 { 0 } else { i - j };
        seen[last as usize] = i;
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

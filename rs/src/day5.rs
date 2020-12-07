use super::util::Error;
use itertools::Itertools;
use std::cmp::max;

fn parse(line: &str) -> Result<u32, Error> {
    line.chars().try_fold(0, |acc, c| match c {
        'F' | 'L' => Ok(acc << 1),
        'B' | 'R' => Ok(acc << 1 | 1),
        _ => Err(Error),
    })
}

pub fn part1<'a, I, S>(lines: I) -> Result<Option<u32>, Error>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Ok(lines
        .into_iter()
        .try_fold(None, |acc, line| -> Result<_, Error> {
            let num = parse(line.as_ref())?;
            Ok(Some(acc.map_or(num, |acc| max(acc, num))))
        })?)
}

pub fn part2<'a, I, S>(lines: I) -> Result<Option<u32>, Error>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut nums = lines
        .into_iter()
        .map(|line| parse(line.as_ref()))
        .collect::<Result<Vec<_>, _>>()?;
    nums.sort_unstable();
    Ok(nums
        .iter()
        .tuple_windows()
        .filter_map(|(l, r)| if *l + 1 < *r { Some(*l + 1) } else { None })
        .next())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn part1_examples() -> Result<(), Error> {
        assert_eq!(Some(357), part1(&["FBFBBFFRLR"])?);
        assert_eq!(Some(567), part1(&["BFFFBBFRRR"])?);
        assert_eq!(Some(119), part1(&["FFFBBBFRRR"])?);
        assert_eq!(Some(820), part1(&["BBFFBBFRLL"])?);
        assert_eq!(
            Some(820),
            part1(&["FBFBBFFRLR", "BFFFBBFRRR", "FFFBBBFRRR", "BBFFBBFRLL"])?
        );
        Ok(())
    }
}

use super::util;
use std::error::Error;

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let nums = util::parse_many::<'a, i32, _, _>(lines)?;
    util::choose::<_, 2>(&nums[..])
        .filter(|choice| choice.iter().copied().sum::<i32>() == 2020)
        .map(|choice| choice.iter().copied().product::<i32>())
        .next()
        .ok_or_else(|| util::Error.into())
}

pub fn part2<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let nums = util::parse_many::<'a, i32, _, _>(lines)?;
    util::choose::<_, 3>(&nums[..])
        .filter(|choice| choice.iter().copied().sum::<i32>() == 2020)
        .map(|choice| choice.iter().copied().product::<i32>())
        .next()
        .ok_or_else(|| util::Error.into())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &["1721", "979", "366", "299", "675", "1456"];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(514579, part1(EXAMPLE)?);
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(241861950, part2(EXAMPLE)?);
        Ok(())
    }
}

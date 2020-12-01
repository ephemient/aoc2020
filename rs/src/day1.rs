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

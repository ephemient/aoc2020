use super::util;
use std::error::Error;

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let nums = util::parse_many::<'a, i32, _, _>(lines)?;
    util::choose(&nums[..])
        .filter_map(|choice: [_; 2]| {
            if choice.iter().copied().sum::<i32>() == 2020 {
                Some(choice.iter().copied().product::<i32>())
            } else {
                None
            }
        })
        .next()
        .ok_or_else(|| util::Error.into())
}

pub fn part2<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let nums = util::parse_many::<'a, i32, _, _>(lines)?;
    util::choose(&nums[..])
        .filter_map(|choice: [_; 3]| {
            if choice.iter().copied().sum::<i32>() == 2020 {
                Some(choice.iter().copied().product::<i32>())
            } else {
                None
            }
        })
        .next()
        .ok_or_else(|| util::Error.into())
}

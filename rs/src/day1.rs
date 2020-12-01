use super::util;
use std::error::Error;

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let nums = util::parse_many::<'a, i32, _, _>(lines)?;
    for (i, x) in nums.iter().enumerate() {
        for y in nums[..i].iter() {
            if x + y == 2020 {
                return Ok(x * y);
            }
        }
    }
    Err(util::Error.into())
}

pub fn part2<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let nums = util::parse_many::<'a, i32, _, _>(lines)?;
    for (i, x) in nums.iter().enumerate() {
        for (j, y) in nums[..i].iter().enumerate() {
            for z in nums[..j].iter() {
                if x + y + z == 2020 {
                    return Ok(x * y * z);
                }
            }
        }
    }
    Err(util::Error.into())
}

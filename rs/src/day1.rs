use super::util;
use std::error::Error;

fn find_pair(nums: &[i32], target: i32) -> Option<(i32, i32)> {
    let mut lo = 0;
    let mut hi = nums.len();
    while lo + 1 < hi {
        match nums[lo + 1..hi].binary_search(&(target - nums[lo])) {
            Ok(pos) => return Some((nums[lo], nums[lo + 1 + pos])),
            Err(pos) => hi = lo + 1 + pos,
        }
        if lo + 1 >= hi {
            break;
        }
        match nums[lo..hi - 1].binary_search(&(target - nums[hi - 1])) {
            Ok(pos) => return Some((nums[lo + pos], nums[hi - 1])),
            Err(pos) => lo = lo + pos,
        }
    }
    None
}

pub fn part1<'a, I, S>(lines: I) -> Result<Option<i32>, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut nums = util::parse_many(lines)?;
    nums.sort_unstable();
    Ok(find_pair(&nums[..], 2020).map(|(x, y)| x * y))
}

pub fn part2<'a, I, S>(lines: I) -> Result<Option<i32>, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut nums = util::parse_many::<'a, i32, _, _>(lines)?;
    nums.sort_unstable();
    Ok(nums
        .iter()
        .enumerate()
        .filter_map(|(i, x)| find_pair(&nums[i + 1..], 2020 - x).map(|(y, z)| x * y * z))
        .next())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &["1721", "979", "366", "299", "675", "1456"];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(514579), part1(EXAMPLE)?);
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(241861950), part2(EXAMPLE)?);
        Ok(())
    }
}

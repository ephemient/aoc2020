use super::util;
use std::error::Error;

pub fn part1<'a, I, S>(lines: I) -> Result<Option<usize>, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Ok(part1_internal(&util::parse_many(lines)?, 25))
}

fn part1_internal(nums: &[usize], n: usize) -> Option<usize> {
    for i in n..nums.len() {
        if !(i - n..i - 1).any(|j| (j + 1..i).any(|k| nums[i] == nums[j] + nums[k])) {
            return Some(nums[i]);
        }
    }
    None
}

pub fn part2<'a, I, S>(lines: I) -> Result<Option<usize>, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let nums = util::parse_many(lines)?;
    Ok(part1_internal(&nums, 25).and_then(|target| part2_internal(&nums, target)))
}

fn part2_internal(nums: &[usize], target: usize) -> Option<usize> {
    let mut sum = 0;
    let mut i = 0;
    let mut j = 0;
    while i < nums.len() {
        if sum == target && i + 1 < j {
            return Some(nums[i..j].iter().min()? + nums[i..j].iter().max()?);
        } else if sum < target && j < nums.len() {
            sum += nums[j];
            j += 1;
        } else {
            sum -= nums[i];
            i += 1;
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[usize] = &[
        35, 20, 15, 25, 47, 40, 62, 55, 65, 95, 102, 117, 150, 182, 127, 219, 299, 277, 309, 576,
    ];

    #[test]
    fn part1_examples() {
        assert_eq!(Some(127), part1_internal(EXAMPLE, 5));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(62), part2_internal(EXAMPLE, 127));
    }
}

use std::iter;

fn parse<'a, I, S>(lines: I) -> Option<Vec<u32>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Some(
        lines
            .into_iter()
            .next()?
            .as_ref()
            .chars()
            .filter_map(|c| c.to_digit(10))
            .collect(),
    )
}

fn step<const N: u32>(arr: &mut [u32], x: u32) -> Option<u32> {
    let a = arr[x as usize];
    let b = arr[a as usize];
    let c = arr[b as usize];
    let y = arr[c as usize];
    let t = iter::successors(Some(x), |t| Some(t.checked_sub(1).unwrap_or(N - 1)))
        .skip(1)
        .find(|t| *t != a && *t != b && *t != c)?;
    let u = *arr.get(t as usize)?;
    arr[x as usize] = y;
    arr[t as usize] = a;
    arr[c as usize] = u;
    Some(y)
}

pub fn part1<'a, I, S>(lines: I) -> Option<u32>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let nums = parse(lines)?;
    let mut arr = [0; 9];
    for (i, x) in nums.iter().enumerate() {
        arr[x.checked_sub(1)? as usize] = *nums.get(i + 1).or_else(|| nums.first())? - 1;
    }
    let mut x = nums[0] - 1;
    for _ in 0..100 {
        x = step::<9>(&mut arr, x)?;
    }
    Some(
        iter::successors(arr.get(0), |x| arr.get(**x as usize))
            .take_while(|x| **x != 0)
            .fold(0, |acc, x| 10 * acc + x + 1),
    )
}

pub fn part2<'a, I, S>(lines: I) -> Option<u64>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let nums = parse(lines)?;
    let mut arr = Vec::with_capacity(1000000);
    arr.extend(1..=1000000);
    for (i, x) in nums.iter().enumerate() {
        arr[x.checked_sub(1)? as usize] = *nums.get(i + 1).unwrap_or(&10) - 1;
    }
    let mut x = nums[0] - 1;
    *arr.last_mut()? = x;
    for _ in 0..10000000 {
        x = step::<1000000>(&mut arr[..], x)?;
    }
    Some((arr[0] as u64 + 1) * (arr[arr[0] as usize] as u64 + 1))
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn part1_examples() {
        assert_eq!(Some(67384529), part1(&["389125467"]));
    }

    #[test]
    fn part2_examples() {
        assert_eq!(Some(149245887792), part2(&["389125467"]));
    }
}

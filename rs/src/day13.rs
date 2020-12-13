use super::util;
use std::cmp::Ordering;
use std::error::Error;

pub fn part1<'a, I, S>(lines: I) -> Result<Option<usize>, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut iter = lines.into_iter();
    let n: usize = match iter.next() {
        Some(s) => s.as_ref().parse()?,
        None => return Err(util::Error.into()),
    };
    let line = match iter.next() {
        Some(s) => s.as_ref(),
        None => return Ok(None),
    };
    Ok(line
        .split(',')
        .filter_map(|s| s.parse().ok())
        .map(|x: usize| (x - (n + x - 1) % x - 1, x))
        .min_by_key(|(t, _)| *t)
        .map(|(t, x)| t * x))
}

pub fn part2<'a, I, S>(lines: I) -> usize
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let line = match lines.into_iter().nth(1) {
        Some(s) => s.as_ref(),
        None => return 0,
    };
    line.split(',')
        .enumerate()
        .filter_map(|(i, s)| s.parse().ok().map(|x: usize| (x - (i + x - 1) % x - 1, x)))
        .fold((0, 1), |(r1, q1), (r2, q2)| crt(r1, q1, r2, q2))
        .0
}

fn crt(r1: usize, q1: usize, r2: usize, q2: usize) -> (usize, usize) {
    let mut a = r1;
    let mut b = r2;
    let q = q1 * q2 / gcd(q1, q2);
    loop {
        match a.cmp(&b) {
            Ordering::Less => a += ((b - a + q1 - 1) / q1) * q1,
            Ordering::Equal => return (a, q),
            Ordering::Greater => b += ((a - b + q2 - 1) / q2) * q2,
        }
    }
}

fn gcd(a: usize, b: usize) -> usize {
    let (mut x, mut y) = if a < b { (a, b) } else { (b, a) };
    while x != 0 {
        let tmp = x;
        x = y % x;
        y = tmp;
    }
    y
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(295), part1(&["939", "7,13,x,x,59,x,31,19"])?);
        Ok(())
    }

    #[test]
    fn part2_examples() {
        assert_eq!(1068781, part2(&["", "7,13,x,x,59,x,31,19"]));
        assert_eq!(3417, part2(&["", "17,x,13,19"]));
        assert_eq!(754018, part2(&["", "67,7,59,61"]));
        assert_eq!(779210, part2(&["", "67,x,7,59,61"]));
        assert_eq!(1261476, part2(&["", "67,7,x,59,61"]));
        assert_eq!(1202161486, part2(&["", "1789,37,47,1889"]));
    }
}

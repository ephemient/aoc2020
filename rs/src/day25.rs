use super::util;
use std::error::Error;
use std::iter;

pub fn part1<'a, I, S>(lines: I) -> Result<u32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut lines = lines.into_iter();
    let pub1: u32 = lines.next().ok_or(util::Error)?.as_ref().parse()?;
    let pub2: u32 = lines.next().ok_or(util::Error)?.as_ref().parse()?;
    drop(lines);
    let mut r = 1u64;
    let mut b = pub1 as u64;
    let mut e = iter::successors(Some(1), |n| Some(7 * n % 20201227).filter(|m| *m != 1))
        .enumerate()
        .find(|(_, n)| *n == pub2)
        .ok_or(util::Error)?
        .0;
    while e != 0 {
        if e % 2 != 0 {
            r = r * b % 20201227;
        }
        b = b * b % 20201227;
        e /= 2;
    }
    Ok(r as u32)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(14897079, part1(&["5764801", "17807724"])?);
        Ok(())
    }
}

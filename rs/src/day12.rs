use super::util;
use std::error::Error;
use std::str::FromStr;

enum Instruction {
    Relative(i32, i32),
    Left,
    Right,
    UTurn,
    Forward(i32),
}

impl FromStr for Instruction {
    type Err = Box<dyn Error + Send + Sync>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(n) = s.strip_prefix("N") {
            Ok(Instruction::Relative(0, n.parse()?))
        } else if let Some(n) = s.strip_prefix("E") {
            Ok(Instruction::Relative(n.parse()?, 0))
        } else if let Some(n) = s.strip_prefix("S") {
            Ok(Instruction::Relative(0, -n.parse()?))
        } else if let Some(n) = s.strip_prefix("W") {
            Ok(Instruction::Relative(-n.parse()?, 0))
        } else if s == "L90" || s == "R270" {
            Ok(Instruction::Left)
        } else if s == "R90" || s == "L270" {
            Ok(Instruction::Right)
        } else if s == "L180" || s == "R180" {
            Ok(Instruction::UTurn)
        } else if let Some(n) = s.strip_prefix("F") {
            Ok(Instruction::Forward(n.parse()?))
        } else {
            Err(util::Error.into())
        }
    }
}

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (mut x, mut y, mut dx, mut dy) = (0, 0, 1, 0);
    for line in lines.into_iter() {
        match line.as_ref().parse()? {
            Instruction::Relative(dx, dy) => {
                x += dx;
                y += dy;
            }
            Instruction::Left => {
                let tmp = dx;
                dx = -dy;
                dy = tmp;
            }
            Instruction::Right => {
                let tmp = dx;
                dx = dy;
                dy = -tmp;
            }
            Instruction::UTurn => {
                dx = -dx;
                dy = -dy;
            }
            Instruction::Forward(n) => {
                x += n * dx;
                y += n * dy;
            }
        }
    }
    Ok(x.abs() + y.abs())
}

pub fn part2<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (mut x, mut y, mut wx, mut wy) = (0, 0, 10, 1);
    for line in lines.into_iter() {
        match line.as_ref().parse()? {
            Instruction::Relative(dx, dy) => {
                wx += dx;
                wy += dy;
            }
            Instruction::Left => {
                let tmp = wx;
                wx = -wy;
                wy = tmp;
            }
            Instruction::Right => {
                let tmp = wx;
                wx = wy;
                wy = -tmp;
            }
            Instruction::UTurn => {
                wx = -wx;
                wy = -wy;
            }
            Instruction::Forward(n) => {
                x += n * wx;
                y += n * wy;
            }
        }
    }
    Ok(x.abs() + y.abs())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &["F10", "N3", "F7", "R90", "F11"];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(25, part1(EXAMPLE)?);
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(286, part2(EXAMPLE)?);
        Ok(())
    }
}

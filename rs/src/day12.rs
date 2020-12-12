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

pub fn solve<'a, I, S>(lines: I, waypoints: bool) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (mut x, mut y) = (0, 0);
    let (mut dx, mut dy) = if waypoints { (10, 1) } else { (1, 0) };
    for line in lines.into_iter() {
        match line.as_ref().parse()? {
            Instruction::Relative(rx, ry) => {
                if waypoints {
                    dx += rx;
                    dy += ry;
                } else {
                    x += rx;
                    y += ry;
                }
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

pub fn part1<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve(lines, false)
}

pub fn part2<'a, I, S>(lines: I) -> Result<i32, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    solve(lines, true)
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

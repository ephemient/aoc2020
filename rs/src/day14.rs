use std::collections::HashMap;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::num::ParseIntError;
use std::str::FromStr;

#[derive(Clone, Debug)]
enum ParseInstructionErr {
    Format,
    Value(ParseIntError),
}

impl Display for ParseInstructionErr {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Format => write!(f, "ParseInstructionErr::Format"),
            Self::Value(err) => write!(f, "ParseInstructionErr::Value({})", err),
        }
    }
}

impl Error for ParseInstructionErr {}

enum Instruction {
    Mask { off: u64, on: u64 },
    Write { addr: u64, value: u64 },
}

impl FromStr for Instruction {
    type Err = ParseInstructionErr;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(mask) = s.strip_prefix("mask = ") {
            Ok(Instruction::Mask {
                off: u64::from_str_radix(&mask.replace('X', "0"), 2)
                    .map_err(ParseInstructionErr::Value)?,
                on: u64::from_str_radix(&mask.replace('X', "1"), 2)
                    .map_err(ParseInstructionErr::Value)?,
            })
        } else if let Some((addr, value)) = s.strip_prefix("mem[").and_then(|s| {
            let mut iter = s.splitn(2, "] = ");
            Some((iter.next()?, iter.next()?))
        }) {
            Ok(Instruction::Write {
                addr: addr.parse().map_err(ParseInstructionErr::Value)?,
                value: value.parse().map_err(ParseInstructionErr::Value)?,
            })
        } else {
            Err(ParseInstructionErr::Format)
        }
    }
}

pub fn part1<'a, I, S>(lines: I) -> Result<u64, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut mem = HashMap::new();
    let mut mask = (0, 0);
    for line in lines.into_iter() {
        match line.as_ref().parse()? {
            Instruction::Mask { off, on } => mask = (off, on),
            Instruction::Write { addr, value } => {
                mem.insert(addr, (value | mask.0) & mask.1);
            }
        }
    }
    Ok(mem.values().sum())
}

pub fn part2<'a, I, S>(lines: I) -> Result<u64, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut mem = HashMap::new();
    let mut mask = (0, 0);
    for line in lines.into_iter() {
        match line.as_ref().parse()? {
            Instruction::Mask { off, on } => mask = (off, on),
            Instruction::Write { addr, value } => {
                let diff = mask.0 ^ mask.1;
                for i in 0..1 << diff.count_ones() {
                    mem.insert(
                        (0..diff.count_ones())
                            .fold((addr | mask.0, diff), |(addr2, diff2), j| {
                                (
                                    addr2 ^ diff & (diff2 ^ (diff2 - (i >> j & 1))),
                                    diff2 & (diff2 - 1),
                                )
                            })
                            .0,
                        value,
                    );
                }
            }
        }
    }
    Ok(mem.values().sum())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(
            165,
            part1(&[
                "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
                "mem[8] = 11",
                "mem[7] = 101",
                "mem[8] = 0"
            ])?
        );
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(
            208,
            part2(&[
                "mask = 000000000000000000000000000000X1001X",
                "mem[42] = 100",
                "mask = 00000000000000000000000000000000X0XX",
                "mem[26] = 1"
            ])?
        );
        Ok(())
    }
}

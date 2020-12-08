use super::machine::{Instruction, Machine};
use super::util;
use std::collections::HashSet;
use std::error::Error;

pub fn part1<'a, I, S>(lines: I) -> Result<Option<isize>, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let instructions = util::parse_many(lines)?;
    let mut seen = HashSet::new();
    seen.insert(0);
    let mut machine = Machine::new(&instructions[..]);
    loop {
        match machine.next() {
            Some((acc, Some(ip))) => {
                if !seen.insert(ip) {
                    break Ok(Some(acc));
                }
            }
            _ => break Ok(None),
        }
    }
}

pub fn part2<'a, I, S>(lines: I) -> Result<Option<isize>, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut instructions = util::parse_many(lines)?;
    for i in 0..instructions.len() {
        let instruction = instructions[i];
        let flipped = match instruction {
            Instruction::Jmp(value) => Instruction::Nop(value),
            Instruction::Nop(value) => Instruction::Jmp(value),
            _ => continue,
        };
        instructions[i] = flipped;
        let mut seen = HashSet::new();
        seen.insert(0);
        let mut machine = Machine::new(&instructions[..]);
        loop {
            match machine.next() {
                Some((acc, None)) => return Ok(Some(acc)),
                Some((_, Some(ip))) => {
                    if !seen.insert(ip) {
                        break;
                    }
                }
                None => return Err(util::Error.into()),
            }
        }
        instructions[i] = instruction;
    }
    Ok(None)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "nop +0", "acc +1", "jmp +4", "acc +3", "jmp -3", "acc -99", "acc +1", "jmp -4", "acc +6",
    ];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(5), part1(EXAMPLE)?);
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(Some(8), part2(EXAMPLE)?);
        Ok(())
    }
}

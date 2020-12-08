use super::machine::{Instruction, Machine};
use super::util;
use std::collections::HashSet;
use std::convert::TryInto;
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
    let instructions = util::parse_many(lines)?;
    let mut stack = vec![(0, 0, HashSet::new(), false)];
    while let Some((mut acc, mut ip, mut seen, mutated)) = stack.pop() {
        while seen.insert(ip) {
            match instructions.get(ip) {
                None => return Ok(Some(acc)),
                Some(Instruction::Acc(x)) => {
                    acc += x;
                    ip += 1;
                }
                Some(Instruction::Jmp(x)) => {
                    if !mutated {
                        stack.push((acc, ip + 1, seen.clone(), true));
                    }
                    match ip
                        .try_into()
                        .ok()
                        .and_then(|ip: isize| ip.checked_add(*x)?.try_into().ok())
                    {
                        Some(ip_plus) => ip = ip_plus,
                        None => return Ok(Some(acc)),
                    }
                }
                Some(Instruction::Nop(x)) => {
                    if !mutated {
                        match ip
                            .try_into()
                            .ok()
                            .and_then(|ip: isize| ip.checked_add(*x)?.try_into().ok())
                        {
                            Some(ip_plus) => stack.push((acc, ip_plus, seen.clone(), true)),
                            None => return Ok(Some(acc)),
                        }
                    }
                    ip += 1;
                }
            }
        }
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

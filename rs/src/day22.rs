use super::util;
use std::collections::{HashSet, VecDeque};
use std::error::Error;

fn parse<'a, I, S>(
    lines: I,
) -> Result<(VecDeque<usize>, VecDeque<usize>), Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut iter = lines.into_iter();
    if iter.next().map(|s| s.as_ref()) != Some("Player 1:") {
        return Err(util::Error.into());
    }
    let mut deck = VecDeque::new();
    while let Some(line) = iter.next() {
        let line = line.as_ref();
        if line.is_empty() {
            break;
        }
        deck.push_back(line.parse()?);
    }
    if iter.next().map(|s| s.as_ref()) != Some("Player 2:") {
        return Err(util::Error.into());
    }
    Ok((
        deck,
        iter.map(|s| s.as_ref())
            .map(|s| s.parse())
            .collect::<Result<_, _>>()?,
    ))
}

pub fn part1<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (mut deck1, mut deck2) = parse(lines)?;
    while !deck1.is_empty() && !deck2.is_empty() {
        let card1 = deck1.pop_front().ok_or(util::Error)?;
        let card2 = deck2.pop_front().ok_or(util::Error)?;
        if card1 < card2 {
            deck2.push_back(card2);
            deck2.push_back(card1);
        } else {
            deck1.push_back(card1);
            deck1.push_back(card2);
        }
    }
    let winner = Some(deck1).filter(|deck| !deck.is_empty()).unwrap_or(deck2);
    let n = winner.len();
    Ok(winner
        .into_iter()
        .enumerate()
        .map(|(i, x)| (n - i) * x)
        .sum())
}

pub fn part2<'a, I, S>(lines: I) -> Result<usize, Box<dyn Error + Send + Sync>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let (mut deck1, mut deck2) = parse(lines)?;
    go(&mut deck1, &mut deck2).ok_or(util::Error)?;
    let winner = Some(deck1).filter(|deck| !deck.is_empty()).unwrap_or(deck2);
    let n = winner.len();
    Ok(winner
        .into_iter()
        .enumerate()
        .map(|(i, x)| (n - i) * x)
        .sum())
}

fn go(deck1: &mut VecDeque<usize>, deck2: &mut VecDeque<usize>) -> Option<bool> {
    let mut seen = HashSet::<(Vec<_>, Vec<_>)>::new();
    while !deck1.is_empty() && !deck2.is_empty() {
        if !seen.insert((
            deck1.iter().copied().collect(),
            deck2.iter().copied().collect(),
        )) {
            return Some(false);
        }
        let card1 = deck1.pop_front()?;
        let card2 = deck2.pop_front()?;
        let is_lt = if card1 <= deck1.len() && card2 <= deck2.len() {
            go(
                &mut deck1.iter().take(card1).copied().collect(),
                &mut deck2.iter().take(card2).copied().collect(),
            )?
        } else {
            card1 < card2
        };
        if is_lt {
            deck2.push_back(card2);
            deck2.push_back(card1);
        } else {
            deck1.push_back(card1);
            deck1.push_back(card2);
        }
    }
    Some(deck1.is_empty())
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    static EXAMPLE: &[&str] = &[
        "Player 1:",
        "9",
        "2",
        "6",
        "3",
        "1",
        "",
        "Player 2:",
        "5",
        "8",
        "4",
        "7",
        "10",
    ];

    #[test]
    fn part1_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(306, part1(EXAMPLE)?);
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Box<dyn Error + Send + Sync>> {
        assert_eq!(291, part2(EXAMPLE)?);
        Ok(())
    }
}

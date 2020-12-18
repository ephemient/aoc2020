use super::util::Unpeekable;
use std::error::Error as StdError;
use std::fmt::{self, Debug, Display, Formatter};
use std::marker::PhantomData;
use std::ops::{AddAssign, MulAssign};
use std::str::FromStr;

#[derive(Debug)]
pub enum Error<T>
where
    T: FromStr,
    <T as FromStr>::Err: Debug,
{
    FormatError(<T as FromStr>::Err),
    LexError { byte: u8, pos: usize },
    EvalError { token: Option<Token<T>> },
}

impl<T> Display for Error<T>
where
    T: Debug + FromStr,
    <T as FromStr>::Err: Debug + Display,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::FormatError(err) => write!(f, "FormatError({})", err),
            Self::LexError { byte, pos } => {
                write!(f, "LexError {{ byte: {}, pos: {} }}", byte, pos)
            }
            Self::EvalError { token } => write!(f, "EvalError {{ token: {:?} }}", token),
        }
    }
}

impl<T> StdError for Error<T>
where
    T: Debug + FromStr,
    <T as FromStr>::Err: StdError,
{
}

#[derive(Clone, Copy, Debug)]
pub enum Token<T> {
    Literal(T),
    Add,
    Mul,
    LParen,
    RParen,
}

struct Tokens<'a, T> {
    source: &'a str,
    pos: usize,
    __phantom: PhantomData<T>,
}

impl<'a, T> Tokens<'a, T> {
    fn from_str(source: &'a str) -> Tokens<'a, T> {
        Tokens {
            source,
            pos: 0,
            __phantom: PhantomData,
        }
    }
}

impl<'a, T> Iterator for Tokens<'a, T>
where
    T: FromStr,
    <T as FromStr>::Err: Debug,
{
    type Item = Result<Token<T>, Error<T>>;
    fn next(&mut self) -> Option<Self::Item> {
        let mut i = self.pos;
        let bytes = self.source.as_bytes();
        while i < bytes.len() {
            match bytes[i] {
                b'+' => {
                    self.pos = i + 1;
                    return Some(Ok(Token::Add));
                }
                b'*' => {
                    self.pos = i + 1;
                    return Some(Ok(Token::Mul));
                }
                b'(' => {
                    self.pos = i + 1;
                    return Some(Ok(Token::LParen));
                }
                b')' => {
                    self.pos = i + 1;
                    return Some(Ok(Token::RParen));
                }
                b' ' => {}
                _ => {
                    let digits = bytes[i..]
                        .iter()
                        .take_while(|b| (b'0'..=b'9').contains(b))
                        .count();
                    if digits == 0 {
                        return Some(Err(Error::LexError {
                            byte: bytes[i],
                            pos: i,
                        }));
                    }
                    self.pos = i + digits;
                    return Some(
                        self.source[i..self.pos]
                            .parse()
                            .map_or_else(|e| Err(Error::FormatError(e)), |n| Ok(Token::Literal(n))),
                    );
                }
            }
            i += 1;
        }
        None
    }
}

fn eval1<I, T>(tokens: &mut I, is_subexpression: bool) -> Result<T, Error<T>>
where
    I: Iterator<Item = Result<Token<T>, Error<T>>>,
    T: Debug + FromStr + AddAssign + MulAssign,
    <T as FromStr>::Err: Debug,
{
    let mut value = atom1(tokens)?;
    loop {
        match tokens.next().transpose()? {
            Some(Token::Add) => value += atom1(tokens)?,
            Some(Token::Mul) => value *= atom1(tokens)?,
            Some(Token::RParen) if is_subexpression => break,
            None if !is_subexpression => break,
            token => return Err(Error::EvalError { token }),
        }
    }
    Ok(value)
}

fn atom1<I, T>(tokens: &mut I) -> Result<T, Error<T>>
where
    I: Iterator<Item = Result<Token<T>, Error<T>>>,
    T: Debug + FromStr + AddAssign + MulAssign,
    <T as FromStr>::Err: Debug,
{
    Ok(match tokens.next().unwrap()? {
        Token::Literal(n) => n,
        Token::LParen => eval1(tokens, true)?,
        token => return Err(Error::EvalError { token: Some(token) }),
    })
}

fn eval2<I, T>(tokens: &mut Unpeekable<I>) -> Result<T, Error<T>>
where
    I: Iterator<Item = Result<Token<T>, Error<T>>>,
    T: Debug + FromStr + AddAssign + MulAssign,
    <T as FromStr>::Err: Debug,
{
    let value = mul_chain(tokens)?;
    if let Some(token) = tokens.next().transpose()? {
        return Err(Error::EvalError { token: Some(token) });
    }
    Ok(value)
}

fn mul_chain<I, T>(tokens: &mut Unpeekable<I>) -> Result<T, Error<T>>
where
    I: Iterator<Item = Result<Token<T>, Error<T>>>,
    T: Debug + FromStr + AddAssign + MulAssign,
    <T as FromStr>::Err: Debug,
{
    let mut value = add_chain(tokens)?;
    while let Some(token) = tokens.next().transpose()? {
        match token {
            Token::Mul => value *= add_chain(tokens)?,
            _ => {
                tokens.unpeek(Ok(token));
                break;
            }
        }
    }
    Ok(value)
}

fn add_chain<I, T>(tokens: &mut Unpeekable<I>) -> Result<T, Error<T>>
where
    I: Iterator<Item = Result<Token<T>, Error<T>>>,
    T: Debug + FromStr + AddAssign + MulAssign,
    <T as FromStr>::Err: Debug,
{
    let mut value = atom2(tokens)?;
    while let Some(token) = tokens.next().transpose()? {
        match token {
            Token::Add => value += atom2(tokens)?,
            token => {
                tokens.unpeek(Ok(token));
                break;
            }
        }
    }
    Ok(value)
}

fn atom2<I, T>(tokens: &mut Unpeekable<I>) -> Result<T, Error<T>>
where
    I: Iterator<Item = Result<Token<T>, Error<T>>>,
    T: Debug + FromStr + AddAssign + MulAssign,
    <T as FromStr>::Err: Debug,
{
    Ok(match tokens.next().unwrap()? {
        Token::Literal(n) => n,
        Token::LParen => {
            let value = mul_chain(tokens)?;
            match tokens.next().transpose()? {
                Some(Token::RParen) => value,
                token => return Err(Error::EvalError { token }),
            }
        }
        token => return Err(Error::EvalError { token: Some(token) }),
    })
}

pub fn part1<'a, I, S>(lines: I) -> Result<i64, Error<i64>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut sum = 0;
    for line in lines.into_iter() {
        sum += eval1::<_, i64>(&mut Tokens::from_str(line.as_ref()), false)?;
    }
    Ok(sum)
}

pub fn part2<'a, I, S>(lines: I) -> Result<i64, Error<i64>>
where
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    let mut sum = 0;
    for line in lines.into_iter() {
        sum += eval2::<_, i64>(&mut Unpeekable::new(Tokens::from_str(line.as_ref())))?;
    }
    Ok(sum)
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    #[test]
    fn part1_examples() -> Result<(), Error<i64>> {
        assert_eq!(71, part1(&["1 + 2 * 3 + 4 * 5 + 6"])?);
        assert_eq!(51, part1(&["1 + (2 * 3) + (4 * (5 + 6))"])?);
        assert_eq!(26, part1(&["2 * 3 + (4 * 5)"])?);
        assert_eq!(437, part1(&["5 + (8 * 3 + 9 + 3 * 4 * 3)"])?);
        assert_eq!(
            12240,
            part1(&["5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"])?
        );
        assert_eq!(
            13632,
            part1(&["((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"])?
        );
        Ok(())
    }

    #[test]
    fn part2_examples() -> Result<(), Error<i64>> {
        assert_eq!(231, part2(&["1 + 2 * 3 + 4 * 5 + 6"])?);
        assert_eq!(51, part2(&["1 + (2 * 3) + (4 * (5 + 6))"])?);
        assert_eq!(46, part2(&["2 * 3 + (4 * 5)"])?);
        assert_eq!(1445, part2(&["5 + (8 * 3 + 9 + 3 * 4 * 3)"])?);
        assert_eq!(
            669060,
            part2(&["5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"])?
        );
        assert_eq!(
            23340,
            part2(&["((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"])?
        );
        Ok(())
    }
}

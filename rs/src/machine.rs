use std::convert::TryInto;
use std::error::Error;
use std::fmt::{self, Display, Formatter};
use std::ops::AddAssign;
use std::str::FromStr;

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ParseInstructionErr<E> {
    Format,
    Operation(String),
    Value(E),
}

impl<E> Display for ParseInstructionErr<E>
where
    E: Display,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            Self::Format => write!(f, "ParseInstructionErr::Format"),
            Self::Operation(op) => write!(f, "ParseInstructionErr::Operation({})", op),
            Self::Value(err) => write!(f, "ParseInstructionErr::Value({})", err),
        }
    }
}

impl<E> Error for ParseInstructionErr<E> where E: Error {}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Instruction<T> {
    Acc(T),
    Jmp(T),
    Nop(T),
}

impl<T> FromStr for Instruction<T>
where
    T: FromStr,
{
    type Err = ParseInstructionErr<<T as FromStr>::Err>;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if let Some(value) = s.strip_prefix("acc ") {
            Ok(Self::Acc(value.parse().map_err(Self::Err::Value)?))
        } else if let Some(value) = s.strip_prefix("jmp ") {
            Ok(Self::Jmp(value.parse().map_err(Self::Err::Value)?))
        } else if let Some(value) = s.strip_prefix("nop ") {
            Ok(Self::Nop(value.parse().map_err(Self::Err::Value)?))
        } else {
            Err(s
                .split(' ')
                .next()
                .map_or(Self::Err::Format, |op| Self::Err::Operation(op.to_string())))
        }
    }
}

#[derive(Debug)]
pub struct Machine<'a, T> {
    instructions: &'a [Instruction<T>],
    acc: T,
    ip: Option<usize>,
}

impl<'a, T> Machine<'a, T>
where
    T: Default,
{
    pub fn new(instructions: &'a [Instruction<T>]) -> Machine<'a, T> {
        Machine {
            instructions,
            acc: T::default(),
            ip: if instructions.is_empty() {
                None
            } else {
                Some(0)
            },
        }
    }
}

impl<'a, T> Iterator for Machine<'a, T>
where
    T: AddAssign,
    T: Copy,
    T: Into<isize>,
{
    type Item = (T, Option<usize>);
    fn next(&mut self) -> Option<Self::Item> {
        let ip = self.ip?;
        let mut jmp = 1;
        match &self.instructions[ip] {
            Instruction::Acc(x) => self.acc += *x,
            Instruction::Jmp(x) => jmp = (*x).into(),
            Instruction::Nop(_) => {}
        }
        self.ip = ip
            .try_into()
            .ok()
            .and_then(|ip: isize| ip.checked_add(jmp)?.try_into().ok())
            .filter(|ip| *ip < self.instructions.len());
        Some((self.acc, self.ip))
    }
}

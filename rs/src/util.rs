use std::error;
use std::fmt;
use std::io;
use std::str::FromStr;

#[derive(Clone, Debug)]
pub struct Error;

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "error")
    }
}

impl error::Error for Error {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        None
    }
}

pub fn to_ioerror<E>(error: E) -> io::Error
where
    E: Into<Box<dyn error::Error + Send + Sync>>,
{
    io::Error::new(io::ErrorKind::Other, error)
}

pub fn parse_many<'a, F, I, S>(lines: I) -> Result<Vec<F>, <F as FromStr>::Err>
where
    F: FromStr,
    I: IntoIterator<Item = &'a S>,
    S: AsRef<str> + 'a,
{
    Ok(lines
        .into_iter()
        .map(|s| s.as_ref().parse::<F>())
        .collect::<Result<_, _>>()?)
}

use std::{
    mem::{forget, MaybeUninit},
    num::ParseIntError,
    ops::{Range, RangeFrom, RangeInclusive, RangeTo, RangeToInclusive},
    panic::{catch_unwind, UnwindSafe},
    str::FromStr,
};

use itertools::Itertools;
use nom::{branch::Permutation, error::ParseError, Parser};

#[derive(Debug, Clone, PartialEq)]
pub enum GenericRange {
    DoubleBounded(Range<usize>),
    DoubleBoundedInclusive(RangeInclusive<usize>),
    From(RangeFrom<usize>),
    To(RangeTo<usize>),
    ToInclusive(RangeToInclusive<usize>),
    Full,
}

impl GenericRange {
    pub fn from(&self) -> Option<usize> {
        match self {
            GenericRange::DoubleBounded(range) => Some(range.start),
            GenericRange::DoubleBoundedInclusive(range) => Some(*range.start()),
            GenericRange::From(range) => Some(range.start),
            GenericRange::To(_) | GenericRange::ToInclusive(_) | GenericRange::Full => None,
        }
    }

    pub fn to(&self) -> Option<usize> {
        match self {
            GenericRange::DoubleBounded(range) => Some(range.end - 1),
            GenericRange::DoubleBoundedInclusive(range) => Some(*range.end()),
            GenericRange::From(_) | GenericRange::Full => None,
            GenericRange::ToInclusive(range) => Some(range.end),
            GenericRange::To(range) => Some(range.end - 1),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum GenericRangeParseError {
    Format,
    Object(ParseIntError),
}

impl FromStr for GenericRange {
    type Err = GenericRangeParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == ".." {
            return Ok(GenericRange::Full);
        }
        match (s.starts_with(".."), s.ends_with(".."), s.contains('=')) {
            (false, false, false) => {
                let Some((from, to)) = s.split_once("..") else {
                    return Err(GenericRangeParseError::Format);
                };
                let from: usize = from
                    .parse()
                    .map_err(|err| GenericRangeParseError::Object(err))?;
                let to: usize = to
                    .parse()
                    .map_err(|err| GenericRangeParseError::Object(err))?;
                Ok(GenericRange::DoubleBounded(from..to))
            }
            (false, false, true) => {
                let Some((from, to)) = s.split_once("..=") else {
                    return Err(GenericRangeParseError::Format);
                };
                let from: usize = from
                    .parse()
                    .map_err(|err| GenericRangeParseError::Object(err))?;
                let to: usize = to
                    .parse()
                    .map_err(|err| GenericRangeParseError::Object(err))?;
                Ok(GenericRange::DoubleBoundedInclusive(from..=to))
            }
            (false, true, false) => {
                let Some((from, "")) = s.split_once("..") else {
                    return Err(GenericRangeParseError::Format);
                };
                let from: usize = from
                    .parse()
                    .map_err(|err| GenericRangeParseError::Object(err))?;
                Ok(GenericRange::From(from..))
            }
            (true, false, false) => {
                let Some(("", to)) = s.split_once("..") else {
                    return Err(GenericRangeParseError::Format);
                };
                let to: usize = to
                    .parse()
                    .map_err(|err| GenericRangeParseError::Object(err))?;
                Ok(GenericRange::To(..to))
            }
            (true, false, true) => {
                let Some(("", to)) = s.split_once("..=") else {
                    return Err(GenericRangeParseError::Format);
                };
                let to: usize = to
                    .parse()
                    .map_err(|err| GenericRangeParseError::Object(err))?;
                Ok(GenericRange::ToInclusive(..=to))
            }
            _ => Err(GenericRangeParseError::Format),
        }
    }
}

#[cfg(test)]
mod generic_rage_tests {
    use super::*;
    macro_rules! test {
        {$name:ident, $input:literal, $output:expr} => {
            #[test]
            fn $name() {
                // arrange

                // act
                let output: Result<GenericRange, _> = $input.parse();

                // assert
                assert_eq!(output.expect("Should be able to parse"), $output);
            }
        };
    }

    test! {full, "..", GenericRange::Full}
    test! {from, "1..", GenericRange::From(1..)}
    test! {double_ended, "1..10", GenericRange::DoubleBounded(1..10)}
    test! {double_ended_inclusive, "10..=100", GenericRange::DoubleBoundedInclusive(10..=100)}
    test! {to, "..100", GenericRange::To(..100)}
    test! {to_inclusive, "..=100", GenericRange::ToInclusive(..=100)}
    test! {idk1, "5..=100", GenericRange::DoubleBoundedInclusive(5..=100)}
}

#[derive(derive_more::From)]
#[repr(transparent)]
#[derive(Debug, derive_more::Deref, derive_more::DerefMut)]
pub struct Equivalent<T>(pub T);

impl<T> PartialEq for Equivalent<T> {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl<T> Eq for Equivalent<T> {}

#[derive(derive_more::From)]
#[repr(transparent)]
#[derive(Debug, derive_more::Deref)]
// TODO REMOVE THAT, IT VERY-VERY BAD SEMANTICALLY!!!!
pub struct Immutable<T>(T);

#[derive(Debug)]
pub struct InlinedStack<'v, T> {
    inner: &'v mut Vec<T>,
}

impl<'v, T> InlinedStack<'v, T> {
    pub fn new(inner: &'v mut Vec<T>) -> Self {
        Self { inner }
    }

    pub fn accessor<'l>(&'l mut self) -> InlinedStackAccessor<'l, 'v, T> {
        InlinedStackAccessor {
            recorded_size: self.inner.len(),
            reference: self,
        }
    }
}

pub struct InlinedStackAccessor<'l, 'v: 'l, T> {
    reference: &'l mut InlinedStack<'v, T>,
    recorded_size: usize,
}

impl<'l, 'v: 'l, T> InlinedStackAccessor<'l, 'v, T> {
    pub fn push(&mut self, t: T) {
        self.reference.inner.push(t);
    }

    pub fn accessor<'l1>(&'l1 mut self) -> InlinedStackAccessor<'l1, 'v, T> {
        InlinedStackAccessor {
            recorded_size: self.reference.inner.len(),
            reference: self.reference,
        }
    }

    pub fn revert(&mut self) {
        self.reference.inner.truncate(self.recorded_size);
    }
}

#[cfg(test)]
mod inline_stack_tests {
    use super::InlinedStack;

    #[test]
    fn create() {
        // arrange
        let mut v = vec![1, 2, 3];

        // act
        let mut stack = InlinedStack::new(&mut v);
        let _accessor = stack.accessor();

        // assert
        assert_eq!(v, vec![1, 2, 3]);
    }

    #[test]
    fn push() {
        // arrange
        let mut v = vec![1, 2, 3];
        let mut stack = InlinedStack::new(&mut v);
        let mut accessor = stack.accessor();

        // act
        accessor.push(4);
        accessor.push(5);

        // assert
        assert_eq!(v, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn push_inner() {
        // arrange
        let mut v = vec![1, 2, 3];
        let mut stack = InlinedStack::new(&mut v);
        let mut accessor = stack.accessor();
        let mut accessor_inner = accessor.accessor();

        // act
        accessor_inner.push(4);
        accessor_inner.push(5);

        // assert
        assert_eq!(v, vec![1, 2, 3, 4, 5]);
    }

    #[test]
    fn revert() {
        // arrange
        let mut v = vec![1, 2, 3];
        let mut stack = InlinedStack::new(&mut v);
        let mut accessor = stack.accessor();
        accessor.push(4);
        accessor.push(5);

        // act
        accessor.revert();

        // assert
        assert_eq!(v, vec![1, 2, 3]);
    }

    #[test]
    fn revert_inner() {
        // arrange
        let mut v = vec![1, 2, 3];
        let mut stack = InlinedStack::new(&mut v);
        let mut accessor = stack.accessor();
        accessor.push(4);
        let mut accessor_inner = accessor.accessor();
        accessor_inner.push(5);

        // act
        accessor_inner.revert();

        // assert
        assert_eq!(v, vec![1, 2, 3, 4]);
    }

    #[test]
    fn revert_outer() {
        // arrange
        let mut v = vec![1, 2, 3];
        let mut stack = InlinedStack::new(&mut v);
        let mut accessor = stack.accessor();
        accessor.push(4);
        let mut accessor_inner = accessor.accessor();
        accessor_inner.push(5);

        // act
        accessor.revert();

        // assert
        assert_eq!(v, vec![1, 2, 3]);
    }
}

#[derive(Debug)]
pub enum ArrayCollectError {
    NotEnough { expeected: usize, got: usize },
    TooMuch(usize),
}
pub trait IteratorArrayCollectExt<T>: Sized {
    fn collect_array<const N: usize>(self) -> Result<[T; N], ArrayCollectError>;
}

// FIXME memory leaks in case of panic
impl<T, I: Sized + Iterator<Item = T>> IteratorArrayCollectExt<T> for I {
    // TODO ADD TESTS FOR THIS, ESPECIALLY FOR THE CASE OF VALUE DROPPING
    fn collect_array<const N: usize>(mut self) -> Result<[T; N], ArrayCollectError> {
        {
            // Ok, so unsafe is used here, and I am really scared of it. But I couldn't find any alternative, really

            // Create an array of uninitialized values.
            let mut array: [MaybeUninit<T>; N] = unsafe { MaybeUninit::uninit().assume_init() };

            for i in 0..N {
                match self.next() {
                    None => {
                        return Err(ArrayCollectError::NotEnough {
                            expeected: N,
                            got: i,
                        });
                    }
                    Some(next) => {
                        array[i] = MaybeUninit::new(next);
                    }
                }
            }
            if self.next().is_some() {
                // there are even more elements to get
                return Err(ArrayCollectError::TooMuch(N));
            }

            Ok(unsafe { std::mem::transmute_copy::<_, [T; N]>(&array) })
        }
    }
}

pub struct PermutatedArray<'l, T, const N: usize>(pub &'l mut [T; N]);

impl<'l, O, E: ParseError<&'l str>, P: Parser<&'l str, O, E>, const N: usize>
    Permutation<&'l str, [O; N], E> for PermutatedArray<'_, P, N>
{
    fn permutation(&mut self, input: &'l str) -> nom::IResult<&'l str, [O; N], E> {
        // I'm so done.
        // TODO implement permutations myself, so that there are no vectors inside
        (0..N)
            .into_iter()
            .permutations(N)
            .map(|perm| {
                let mut input = input;
                perm.into_iter()
                    .map(|i| {
                        let (rest, m) = self.0[i].parse(input).ok()?;
                        input = rest;
                        Some(m)
                    })
                    .filter_map(std::convert::identity)
                    .collect_array()
                    .map(|arr| (input, arr))
            })
            .find_map(Result::ok)
            .ok_or(nom::Err::Error(E::from_error_kind(
                input,
                nom::error::ErrorKind::Permutation,
            )))
    }
}
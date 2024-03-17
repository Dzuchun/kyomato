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

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum GenericRangeParseError {
    #[error("Generic range format is wrong")]
    Format,
    #[error("{}", .0)]
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

pub trait VecTryExtendExt<T, E> {
    fn try_extend(&mut self, iter: impl Iterator<Item = Result<T, E>>) -> Result<(), E>;
}

impl<T, E> VecTryExtendExt<T, E> for Vec<T> {
    fn try_extend(&mut self, iter: impl Iterator<Item = Result<T, E>>) -> Result<(), E> {
        for el in iter {
            self.push(el?);
        }
        Ok(())
    }
}

macro_rules! fn_ref {
    ($input:expr) => {{
        fn focus_pokus_amogus<I, O>(input: fn(I) -> O) -> fn(I) -> O {
            input
        }
        focus_pokus_amogus(|i| $input(&i))
    }};
}
macro_rules! fn_mut {
    ($input:expr) => {{
        fn focus_pokus_amogus<I, O>(input: fn(I) -> O) -> fn(I) -> O {
            input
        }
        focus_pokus_amogus(|mut i| $input(&mut i))
    }};
}

macro_rules! fn_chain {
    ($fn1:expr, $fn2:expr) => {{
        fn focus_pokus_amogus<I, O>(input: fn(I) -> O) -> fn(I) -> O {
            input
        }
        focus_pokus_amogus(|input| $fn2($fn1(input)))
    }};
}

#[cfg(test)]
mod fn_ref_tests {
    #[test]
    fn works() {
        // act
        let _ref_pointer = fn_ref!(String::len);
        let _mut_pointer = fn_mut!(Vec::<i32>::pop);
        let _chain_pointer = fn_chain!(fn_mut!(Vec::<i32>::pop), Option::unwrap);
    }
}

struct MergeCorr<I1, I2, It1, It2, Corr> {
    i1: I1,
    i2: I2,
    buf1: Vec<It1>,
    buf2: Vec<It2>,
    correlation: Corr,
}

fn merge_corr<I1, I2, It1, It2, Corr>(
    i1: I1,
    i2: I2,
    correlation: Corr,
) -> impl Iterator<Item = (It1, It2)>
where
    I1: Iterator<Item = It1>,
    I2: Iterator<Item = It2>,
    Corr: FnMut(&It1, &It2) -> bool,
{
    MergeCorr {
        i1,
        i2,
        buf1: Vec::new(),
        buf2: Vec::new(),
        correlation,
    }
}

impl<I1, I2, It1, It2, Corr> MergeCorr<I1, I2, It1, It2, Corr>
where
    I1: Iterator<Item = It1>,
    I2: Iterator<Item = It2>,
    Corr: FnMut(&It1, &It2) -> bool,
{
    fn next12(&mut self) -> Option<(It1, It2)> {
        for i in 0..self.buf1.len() {
            for j in 0..self.buf2.len() {
                if (&mut self.correlation)(&self.buf1[i], &self.buf2[j]) {
                    return Some((self.buf1.swap_remove(i), self.buf2.swap_remove(j)));
                }
            }
        }
        None
    }

    fn next1(&mut self) -> Result<(It1, It2), bool> {
        // First, try getting next element from the iterator
        if let Some(e1) = self.i1.next() {
            // If succeeded, try finding correspondence among already present `buf2` elements
            if let Some(ind2) = self
                .buf2
                .iter()
                .position(|e2| (&mut self.correlation)(&e1, e2))
            {
                // If succeeded, return the result
                let e2 = self.buf2.swap_remove(ind2);
                return Ok((e1, e2));
            }
            // There's no correspondence already present. Add this element to buf1
            self.buf1.push(e1);
        }
        self.next12().ok_or(true)
    }
    fn next2(&mut self) -> Result<(It1, It2), bool> {
        // First, try getting next element from the iterator
        if let Some(e2) = self.i2.next() {
            // If succeeded, try finding correspondence among already present `buf2` elements
            if let Some(ind1) = self
                .buf1
                .iter()
                .position(|e1| (&mut self.correlation)(e1, &e2))
            {
                // If succeeded, return the result
                let e1 = self.buf1.swap_remove(ind1);
                return Ok((e1, e2));
            }
            // There's no correspondence already present. Add this element to buf1
            self.buf2.push(e2);
        }
        self.next12().ok_or(true)
    }
}

impl<I1, I2, It1, It2, Corr> Iterator for MergeCorr<I1, I2, It1, It2, Corr>
where
    I1: Iterator<Item = It1>,
    I2: Iterator<Item = It2>,
    Corr: FnMut(&It1, &It2) -> bool,
{
    type Item = (It1, It2);

    fn next(&mut self) -> Option<Self::Item> {
        let mut try1 = true;
        let mut try2 = true;
        while try1 || try2 {
            match self.next1() {
                Ok(res) => return Some(res),
                Err(e) => try1 = e,
            }
            match self.next2() {
                Ok(res) => return Some(res),
                Err(e) => try2 = e,
            }
        }
        None
    }
}

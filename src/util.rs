use std::{
    collections::VecDeque,
    mem::{forget, MaybeUninit},
    num::ParseIntError,
    ops::{Range, RangeFrom, RangeInclusive, RangeTo, RangeToInclusive},
    panic::{catch_unwind, UnwindSafe},
    str::FromStr,
};

use itertools::Itertools;
use nom::{branch::Permutation, error::ParseError, IResult, Parser};

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

#[repr(transparent)]
#[derive(Debug, derive_more::From, derive_more::Deref, derive_more::DerefMut)]
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
// XXX REMOVE THAT, IT VERY-VERY BAD SEMANTICALLY!!!!
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

// [ ] memory leaks in case of panic
impl<T, I: Sized + Iterator<Item = T>> IteratorArrayCollectExt<T> for I {
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

pub trait OptionalPermutation<In, Out, E> {
    fn optional_permutation(&mut self, input: In) -> IResult<In, Out, E>;
}

macro_rules! impl_optional_permutation {
        {$Count:expr, $type_ident:ident: $out_ident:ident} => {
            // base case
            #[allow(non_snake_case)]
            impl<'source, Err, $out_ident, $type_ident> OptionalPermutation<&'source str, (Option<$out_ident>,), Err> for ($type_ident,)
            where
                Err: ParseError<&'source str>,
                $type_ident: Parser<&'source str, $out_ident, Err>,
            {
                fn optional_permutation(&mut self, input: &'source str) -> IResult<&'source str, (Option<$out_ident>,), Err>
                {
                    // in a debug build, let's assert that we've passed a correct type count
                    debug_assert!($Count == 1, "Should parse number equal to number of arguments");
                    // for a single variant, just try parsing it, and propagate any sort of failure
                    match self.0.parse(input) {
                        Ok((rest, result)) => Ok((rest, (Some(result), ))),
                        Err(nom::Err::Failure(err)) => Err(nom::Err::Failure(err)),
                        Err(_) => Ok((input, (None, )))
                    }
                }
            }
        };
        {$Count:expr, $type_ident0:ident: $out_ident0:ident, $($type_ident:ident: $out_ident:ident), *} => {
            #[allow(non_snake_case)]
            impl<'source, Err, $out_ident0, $($out_ident), *, $type_ident0, $($type_ident), *> OptionalPermutation<&'source str, (Option<$out_ident0>, $(Option<$out_ident>, ) *), Err>
                for ($type_ident0, $($type_ident, ) *)
            where
                Err: ParseError<&'source str>,
                $type_ident0: Parser<&'source str, $out_ident0, Err>,
                $(
                    $type_ident: Parser<&'source str, $out_ident, Err>
                ), *
            {
                fn optional_permutation(&mut self, mut input: &'source str) -> IResult<&'source str, (Option<$out_ident0>, $(Option<$out_ident>), *), Err> {
                    // for multiple variants, here comes chaos:
                    // we can't create any sort of array or vector to hold the results -
                    // they have different types, that's the point!
                    // So we are bound to creating a BUNCH of variables
                    let mut $out_ident0: Option<$out_ident0> = None;
                    $(
                        let mut $out_ident: Option<$out_ident> = None;
                    ) *
                    // Also, since we can't index tuples, we'll need to "deconstruct self"
                    // I don't feel like creating separate identifiers for that, so I guess compiler IS to complain :idk:
                    let ($type_ident0, $($type_ident, ) *) = self;
                    // Now we can use `$type_ident` as corresponding parser, and `$out_ident` as a output variable!
                    // Amazing! (not really; I imagine that would be a nightmare to read)

                    // This is a max number of iterations we're gonna need
                    const COUNT: usize = $Count;
                    for _ in 0..COUNT {
                        if $out_ident0.is_none() {
                            match $type_ident0.parse(input) {
                                Ok((rest, result)) => {
                                    input = rest;
                                    $out_ident0 = Some(result);
                                    continue; // continues parsing attempts from the beginning
                                },
                                Err(nom::Err::Failure(err)) => return Err(nom::Err::Failure(err)),
                                Err(_) => { }
                            }
                        }
                        $(
                            if $out_ident.is_none() {
                                match $type_ident.parse(input) {
                                    Ok((rest, result)) => {
                                        input = rest;
                                        $out_ident = Some(result);
                                        continue; // continues parsing attempts from the beginning
                                    },
                                    Err(nom::Err::Failure(err)) => return Err(nom::Err::Failure(err)),
                                    Err(_) => { }
                                }
                            }
                        ) *
                        // All parsing attempts had failed?
                        // Break out, we're done here.
                        break;
                    }
                    let _ = 2; // <-- his name is Joey
                    Ok((input, ($out_ident0, $($out_ident, ) *)))
                }
            }

            // forward to N-1
            impl_optional_permutation!{$Count - 1, $($type_ident: $out_ident), *}
        };
    }

impl_optional_permutation! {16, A: AOut, B: BOut, C: COut, D: DOut, E: EOut, F: FOut, G: GOut, H: HOut, I: IOut, J: JOut, K: KOut, L: LOut, M: MOut, N: NOut, O: OOut, P: POut}

pub fn optional_permutation<'parsers, 'source: 'parsers, In, Out, E: ParseError<&'source str>>(
    mut parsers: impl OptionalPermutation<In, Out, E> + 'parsers,
) -> impl Parser<In, Out, E> + 'parsers {
    move |input| parsers.optional_permutation(input)
}

// FIXME this is SURELY not idiomatic. I should move to ident system
#[derive(Debug, derive_more::Deref, derive_more::DerefMut, PartialEq, Clone)]
pub struct HashIgnored<T>(pub T);
impl<T> std::hash::Hash for HashIgnored<T> {
    fn hash<H: std::hash::Hasher>(&self, _state: &mut H) {}
}

#[derive(Debug)]
pub struct FmtToIo<W>(W, VecDeque<std::io::Error>);
impl<W> FmtToIo<W> {
    pub fn new(io: W) -> Self
    where
        W: std::io::Write,
    {
        Self(io, VecDeque::new())
    }

    pub fn get_error(&mut self) -> Option<std::io::Error> {
        self.1.pop_front()
    }

    pub fn get_all_errors(&mut self) -> impl Iterator<Item = std::io::Error> + '_ {
        self.1.drain(..)
    }
}
impl<W: std::io::Write> std::fmt::Write for FmtToIo<W> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        if let Err(err) = self.0.write_all(s.as_bytes()) {
            self.1.push_back(err);
            return Err(std::fmt::Error);
        }
        Ok(())
    }
}

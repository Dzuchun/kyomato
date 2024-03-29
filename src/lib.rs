//! *Kyoko is being a tomato. Why? I don't know :idk:*
//!
//! This crate allows [obsidian]'s markdown transformation into a (la)tex document of the format I personally like.
//! One should note, that markdown is generally less descriptive than (la)tex, so it should code to no surprise,
//! that I introduced some custom markdown syntax to account for that.
//!
//! # Ideology
//! Although it's annoying to admit, there is no  way to do a fully-streamed zero-copy-type of transformation. There are two reasons for that:
//! - Markdown allows to decipher footnotes at an point up until the end of the file.
//! On the other hand, I could not find a way to make (la)tex "insert" footnote definition after first marker to it -
//! it seems to always insert them at the bottom of the page they were encountered.
//! - Static [ayano] blocks are supposed to be available for the entire document.
//! Thus, code must be fully collected first, that executed of the second, separate path.
//!
//! Due to these two factors, I decided to store the input in it's entirety during program's execution.
//! To (sorta) compensate for that, crate follows a strict zero-copy rule through the entire codebase.
//!
//! To further decrease allocations count, it was decided that any output would be generated through an object, that can be written to by parsed tokens, in order.
//! That object would ideally have some methods to append the output with a string, or th like that... Well, [`Write`] trait matches that perfectly!
//! So all tokens can be appended to an abstract, writer-sort-of-thing.
//! [`Display`] trait implementation was considered as an alternative, but I ultimately decided that was not idiomatic.
//!
//! Actual Token representation might vary, so there can be different actual ZST formatters, defined in their separate crates.
//!
//! ### Async
//! As much as I'd like all of the processes be nice and async, I see no option for that in case of continuous output generation.
//! Well, unless there's a way to efficiently write to a single output from multiple threads, while preserving output order.
//! Any sort of "collect" function would've destroyed the purpose anyway.

use data::{TitleInfo, Token};
use gen::OutputGenerator;
use lexer::KyomatoLexError;

/// This module defines types that are used to represent parsed data
mod data;
mod gen;
pub use gen::lab::LabaLatex;
mod lexer;
pub mod path_engine;

/// Reexports
pub fn lex<'source>(
    input: &'source str,
) -> Result<(data::TitleInfo<'source>, data::Token<'source>), KyomatoLexError> {
    let (input, title_info) =
        lexer::title_info::<'source, KyomatoLexError>(input).map_err(|err| match err {
            nom::Err::Error(err) | nom::Err::Failure(err) => err,
            nom::Err::Incomplete(_) => unreachable!("This is a complete input"),
        })?;
    let (_, tokens) = lexer::lex::<'source, KyomatoLexError>(input).map_err(|err| match err {
        nom::Err::Error(err) | nom::Err::Failure(err) => err,
        nom::Err::Incomplete(_) => unreachable!("This is a complete input"),
    })?;
    Ok((title_info, tokens))
}
pub fn gen<'token, 'source: 'token>(
    token: &'token data::Token<'source>,
    title_info: impl Into<Option<TitleInfo<'source>>>,
    mut output: impl std::io::Write,
) -> Result<(), Box<dyn std::error::Error + 'token>> {
    let buf = gen_to_string(token, title_info)?;
    // TODO make a transformer struct
    output.write_all(buf.as_bytes())?;
    Ok(())
}
pub fn gen_to_string<'token, 'source: 'token>(
    token: &'token data::Token<'source>,
    title_info: impl Into<Option<TitleInfo<'source>>>,
) -> Result<String, Box<dyn std::error::Error + 'token>> {
    let path_engine = path_engine::primitive();
    let mut meta = gen::lab::SourceMeta::collect(&token, &path_engine)?.init_ayano()?;
    let generator = gen::lab::LabaLatex::new(path_engine);
    if let Some(title_info) = title_info.into() {
        meta.title_info = title_info;
    }
    let mut buf = String::new();
    generator.write_preamble(&mut buf, &meta)?;
    let mut context = gen::lab::Context::default();
    generator.write_to(&mut buf, &meta, &mut context, token)?;
    generator.write_postamble(&mut buf, &meta, &mut context)?;
    Ok(buf)
}

pub fn parse_title_info<'source>(
    input: &mut &'source str,
) -> Result<TitleInfo<'source>, KyomatoLexError> {
    let (rest, title_info) =
        lexer::title_info::<'source, KyomatoLexError>(input).map_err(|err| match err {
            nom::Err::Error(err) | nom::Err::Failure(err) => err,
            nom::Err::Incomplete(_) => unreachable!("This input is complete"),
        })?;
    *input = rest;
    Ok(title_info)
}

pub fn parse_tokens<'source>(input: &mut &'source str) -> Result<Token<'source>, KyomatoLexError> {
    let (rest, tokens) =
        lexer::lex::<'source, KyomatoLexError>(input).map_err(|err| match err {
            nom::Err::Error(err) | nom::Err::Failure(err) => err,
            nom::Err::Incomplete(_) => unreachable!("This input is complete"),
        })?;
    *input = rest;
    Ok(tokens)
}

pub fn parse_all<'source>(
    mut input: &'source str,
) -> Result<(TitleInfo<'source>, Token<'source>), KyomatoLexError> {
    let title_info = parse_title_info(&mut input)?;
    let tokens = parse_tokens(&mut input)?;
    Ok((title_info, tokens))
}

/// This module houses "utility-like" structs and functions.
///
/// Since this is generally a bad practice, I'd like to know suggestions on std library/other crates items I could use instead
mod util;
pub use util::FmtToIo;

use std::{borrow::Cow, path::Path};

use nom::{
    branch::{alt, permutation},
    bytes::complete::{is_not, tag, take_till1, take_until1},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0, newline, space0},
    combinator::{all_consuming, map_res, opt, recognize},
    error::{context, ContextError, ErrorKind, FromExternalError, ParseError},
    multi::{fold_many1, many1_count},
    number::{self, complete::float},
    sequence::{delimited, preceded, terminated, tuple},
    FindToken, IResult, Parser,
};
use url::Url;

use crate::{
    data::{AyanoBlock, ListType, Token, Tokens},
    util::{IteratorArrayCollectExt, PermutatedArray},
};

pub enum LexingError {}

type LexingResult<'source, O = Token<'source>> = Result<(&'source str, O), LexingError>;

/// Lexes input into tokens
pub fn lex<'source, E: ParseError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Tokens<'source>, E> {
    todo!()
}

/// Lexes input into tokens, but forbids some types of tokens, like figures, tables, etc.
///
/// Intended to be used to lex captions
pub fn limited_lex<'source, E: ParseError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Tokens<'source>, E> {
    todo!()
}

// TODO come up with some sort of clever trait implementation to optimize this out
fn parse_args<'parser, 'source, E: ParseError<&'source str>, const ARGS: usize>(
    args: [&'parser mut dyn FnMut(&'source str) -> IResult<&'source str, &'source str, E>; ARGS],
) -> impl FnMut(&'source str) -> IResult<&'source str, [Option<&'source str>; ARGS], E> + 'parser {
    let mut parsers: [_; ARGS] = args.map(|p| {
        move |input| {
            let p = &mut *p;
            let (rest, value) = opt(tuple((
                space0,
                p,
                space0,
                alt((char(','), nom::combinator::peek(char('}')))),
            )))(input)?;
            Ok::<(&str, Option<&str>), nom::Err<E>>((rest, value.map(|(_, value, _, _)| value)))
        }
    });
    move |input| {
        preceded(
            opt(tuple((space0, char('\n')))),
            tuple((
                opt(char('\n')),
                char('{'),
                permutation(PermutatedArray(&mut parsers)),
                char('}'),
            )),
        )(input)
        .map(|(rest, (_, _, out, _))| (rest, out))
    }
}

// ARGUMENT KERNELS
fn caption_kernel<'source, E: ParseError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, &'source str, E> {
    delimited(
        char('"'),
        recognize(fold_many1(
            alt((tag("\\\""), is_not("\""))),
            || (),
            |_, _| (),
        ))
        // input slice between quotes must be possible to consume fully, but not actually parsed yet, due to
        // FIXME limitation of argument parsers -- they must return &'source str for now.
        .and_then(recognize(all_consuming(limited_lex::<'source, E>))),
        char('"'),
    )(input)
}

fn ref_kernel<'source, E: ParseError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, &'source str, E> {
    alphanumeric1(input)
}

// META ARG PARSERS
fn _parse_arg<'source, E: ParseError<&'source str>>(
    name: &'static str,
    kernel: fn(&'source str) -> IResult<&'source str, &'source str, E>,
) -> impl FnMut(&'source str) -> IResult<&'source str, &'source str, E> {
    move |input| {
        let (rest, (_, _, _, _, ident)) =
            tuple((tag(name), space0, char('='), space0, kernel))(input)?;
        Ok((rest, ident))
    }
}

fn parse_ref<'source, E: ParseError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, &'source str, E> {
    _parse_arg("ref", ref_kernel)(input)
}

fn parse_caption<'source, E: ParseError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, &'source str, E> {
    _parse_arg("caption", caption_kernel)(input)
}

fn __recognize_float<'source, E: ParseError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, &'source str, E> {
    recognize::<&str, f32, E, _>(float::<&str, E>)(input)
}

fn parse_width<'source, E: ParseError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, &'source str, E> {
    _parse_arg("width", __recognize_float)(input)
}

// TOKEN PARSERSu
// CONVENTION: each token should excessively match space after it

fn div_page<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, _) = context(
        "page divider",
        tuple((multispace0, tag("||||||"), space0, char('\n'))),
    )(input)?;
    Ok((rest, Token::PageDiv))
}

fn header<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, &'static str>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, (_, _, order, _, content)) = context(
        "header",
        tuple((
            multispace0,
            newline,
            map_res(many1_count(char('#')), |count| {
                if count <= 6 {
                    Ok(count)
                } else {
                    Err("headers can't be more than 6-fold")
                }
            }),
            space0,
            take_until1("\n"),
        )),
    )(input)?;
    Ok((
        rest,
        Token::Header {
            order,
            content: content.into(),
        },
    ))
}

fn equation<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, (_, _, content, _, [ident])) = context(
        "display mathmode",
        tuple((
            multispace0,
            tag("$$"),
            take_until1("\n$$"),
            tag("\n$$"),
            parse_args([&mut parse_ref]),
        )),
    )(input)?;
    Ok((
        rest,
        Token::Equation {
            content: content.into(),
            ident: ident.map(Cow::from),
        },
    ))
}

fn table<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    todo!()
}

fn figure<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    // TODO add width parameter to figure, I guess
    let (rest, (_, _, path, _, [ident, caption, _width])) = context(
        "figure",
        tuple((
            multispace0,
            tag("![["),
            take_until1("]]"),
            tag("]]"),
            parse_args([&mut parse_ref, &mut parse_caption, &mut parse_width]),
        )),
    )(input)?;
    let caption = match caption {
        Some(caption) =>
        // All caption must be consumed for successful lex
        // any error occurring during caption parsing, must be propagated
        {
            Some(all_consuming(limited_lex)(caption)?.1)
        }
        None => None,
    };
    let token = Token::Figure {
        src_name: Path::new(path).into(),
        caption,
        ident: ident.map(Cow::from),
    };
    Ok((rest, token))
}

fn href<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, (display, url)) = context(
        "href",
        tuple((
            delimited(char('['), take_till1(|c| c != ']'), char(']')),
            delimited(char('('), take_till1(|c| c != ')'), char(')')).and_then(|url: &str| {
                Ok((
                    "",
                    Url::parse(url).map_err(|_err| {
                        // TODO make a proper error
                        nom::Err::Error(E::from_error_kind(
                            "failed to parse url",
                            nom::error::ErrorKind::Verify,
                        ))
                    })?,
                ))
            }),
        )),
    )(input)?;
    Ok((
        rest,
        Token::Href {
            url,
            display: display.into(),
        },
    ))
}

fn code_block<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, (_, (lang, _, _, code))) = context(
        "code block",
        tuple((
            char('\n'),
            delimited(
                tag("```\n"),
                tuple((opt(alpha1), space0, char('\n'), take_until1("\n```"))),
                tag("\n```"),
            ),
        )),
    )(input)?;
    Ok((
        rest,
        Token::CodeBlock {
            code: code.into(),
            language: lang.map(Cow::from),
        },
    ))
}

fn ayano<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    // python, Ayano * <description> ~ <path> # <id>
    let (rest, (_, _, _, _, _, _, _, (display, insert, ident, is_static), _, code, _)) =
        context(
            "ayano",
            tuple((
                char('\n'),
                tag("```"),
                space0,
                tag("python,"),
                space0,
                tag("Ayano"),
                space0,
                permutation((
                    opt(tuple((
                        char('*'),
                        space0,
                        opt(terminated(caption_kernel, space0)),
                    ))),
                    opt(tuple((char('~'), space0, alphanumeric1, space0))),
                    opt(tuple((
                        char('#'),
                        space0,
                        digit1.and_then(|number: &str| {
                            Ok::<(&str, u16), nom::Err<E>>((
                                "",
                                number.parse().map_err(|_| {
                                    nom::Err::Error(E::from_error_kind(
                                        "Identifier must be parseable into u16",
                                        ErrorKind::Digit,
                                    ))
                                })?,
                            ))
                        }),
                        space0,
                    ))),
                    opt(terminated(char('!'), space0)),
                )),
                char('\n'),
                take_until1("\n```"),
                tag("\n```"),
            )),
        )(input)?;
    // type might be a bit confusing, but basically:
    // - outer None means this block is not meant to be displayed
    // - outer Some means this block is to be displayed
    // - inner None means this block has no display caption
    // - inner Some means this block has display caption
    let display: Option<Option<&'source str>> = display.map(|(_, _, caption)| caption); // TODO add code captions support
    let insert: Option<&'source Path> = insert.map(|(_, _, path, _)| Path::new(path));
    let _ident: Option<u16> = ident.map(|(_, _, ident, _)| ident); // TODO add support for codeblock identifiers
    let code: Cow<'_, str> = Cow::from(code);
    Ok((
        rest,
        Token::Ayano(AyanoBlock {
            insert_path: insert.map(Cow::from),
            code,
            is_display: display.is_some(),
            is_static: is_static.is_some(),
        }),
    ))
}

fn list<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, _) = context(
        "list",
        preceded(char('\n'), |mut input: &str| {
            let mut newline = 1;
            let mut non_whitespace = 0;
            while newline > non_whitespace {
                newline = input.find('\n').unwrap_or(input.len());
                non_whitespace = input
                    .find(|c: char| !c.is_whitespace())
                    .unwrap_or(input.len());
                input = &input[(newline + 1)..];
            }

            let indent = &input[..non_whitespace]; // how whitespace indent at the start of each line looks like
            let (list_type, item_start: dyn &mut FnMut(usize) -> String) = match indent[non_whitespace..].chars().next() {
                None => {
                    // there's not a single non-whitespace character -- it's impossible to properly parse a list here.
                    return Err(nom::Err::Error(E::from_error_kind("can't parse list at the empty location", ErrorKind::Eof)));
                },
                Some('1') => {
                    // that's a numeric list!
                    (ListType::Num, |i| Some(format!("{}.", i+1)))
                },
                Some('a') => {
                    // that's a latin list!
                    const SYMBOLS: [char; 26] = ('a'..='z').into_iter().collect_array().expect("There are exactly 2 characters in the alphabet");
                    (ListType::Latin, move |i| Some(if i >= 26 {
                        Err(nom::Err::Error(E::from_error_kind("can't have for than 26 elemenets in a regular list", ErrorKind::Char)))
                    } else {
                        Ok(SYMBOLS[i])
                    }))
                },
                Some('а') => {
                    // that's a cyrillic list!
                    const SYMBOLS: [char; 26] = ('а'..='я').into_iter().collect_array().expect("");
                    (ListType::Latin, move |i| Some(if i >= 26 {
                        Err(nom::Err::Error(E::from_error_kind("can't have for than 26 elemenets in a regular list", ErrorKind::Char)))
                    } else {
                        Ok(SYMBOLS[i])
                    }))
                }
                Some('-') => {
                    // that's a bullet list!
                    (ListType::Num,())
                }
                Some('I') => {
                    // that's a roman list!
                    unimplemented!()
                },
                Some(c) => {
                    // that's an unknown list type
                    // FIXME not sure how to use these errors properly, I think I need to study more examples
                    return Err(nom::Err::Error(E::from_error_kind("unknown list type", ErrorKind::Char)));
                },
            };
            input.lines().enumerate().take_while(|line: &&str| line.starts_with(indent)).map(|line| {
                let line = line.fin_
            })
        }),
    )(input)?;
    todo!()
}

fn formatting<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    todo!()
}
fn paragraph<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    todo!()
}
fn inline_math<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    todo!()
}
fn reference<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    todo!()
}
fn footnote_reference<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    todo!()
}
fn footnote_content<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    todo!()
}

fn tokens<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    todo!()
}

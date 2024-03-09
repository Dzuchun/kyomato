use std::{
    borrow::{Borrow, Cow},
    path::Path,
};

use itertools::Itertools;
use nom::{
    branch::{alt, permutation},
    bytes::complete::{escaped, is_not, tag, take_till1, take_until1, take_while},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0, newline, space0},
    combinator::{all_consuming, iterator, map_res, not, opt, recognize, success},
    error::{context, ContextError, ErrorKind, FromExternalError, ParseError},
    multi::{fold_many1, many1_count, separated_list1},
    number::complete::float,
    sequence::{delimited, preceded, terminated, tuple},
    IResult, Parser,
};
use url::Url;

use crate::{
    data::{AyanoBlock, ListType, Token, Tokens},
    util::IteratorArrayCollectExt,
};

// Conventions:
// - parsers should only parse empty characters with `empty`
// - parsers should only parse empty space at the beginning, not the end
// - newline parsing should be carefully managed

// TODO probably move that to util or th
// I won't include the entire tuple operation library, while I can perfectly implement this myself.
mod remove_last_unit {

    pub trait Trait {
        type Output;
        fn remove_last_unit(self) -> Self::Output;
    }

    macro_rules! impl_tuple_remove_last_unit {
        {$A0:ident} => {
            impl<$A0> Trait for ($A0, ()) { type Output = ($A0, );

                #[allow(non_snake_case)]
                #[inline]
                fn remove_last_unit(self) -> Self::Output {
                    let ($A0, _) = self;
                    ($A0, )
                }
            }
        };
        {$A0:ident $($Ai:ident) +} => {
            impl<$A0, $($Ai), +> Trait for ($A0, $($Ai), +, ()) {
                type Output = ($A0, $($Ai),+);

                #[allow(non_snake_case)]
                #[inline]
                fn remove_last_unit(self) -> Self::Output {
                    let ($A0, $($Ai), *, ()) = self;
                    ($A0, $($Ai), +)
                }
            }

            impl_tuple_remove_last_unit!{$($Ai) +}
        };
    }

    impl_tuple_remove_last_unit! {A B C D E F G H I J K L M N O P Q R S T U V W}
}

/// Generates argument parser that accounts for all necessary syntax
macro_rules! parse_args {
    [$($argument_name:literal = $argument_kernel:expr), +] => {
        tuple((
            space0,
            tag("\n{"), // NOTE: arguments must start from an empty line
            permutation((
                $(
                    opt(tuple((
                        space0,
                        tuple((tag($argument_name), space0, char('='), space0, context("arg", context($argument_name, $argument_kernel)))),
                        space0,
                        alt((char(','), nom::combinator::peek(char('}')))), // this can have unexpected consequences, but whatever :idk:
                    ))).map(|value| value.map(|(_, (_, _, _, _, v), _, _)| v))
                ), +,
                success(())
            )),
            char('}'),
        )).map(|(_, _, out, _)| <_ as remove_last_unit::Trait>::remove_last_unit(out))
    };
}

/// Parses caption (just the literal) part
///
/// Quite tricky task, as there can be double quotes -- but escaped.
/// Apart from single, unescaped quotes, caption can be ANYTHING.
/// There seems to be just the tool for a job -- `escaped` parser
/// TODO FIXME add a mention about caption actually TERMINATING after first unescaped double-quote, and where exactly escaping backslashes are removed
fn caption_kernel<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    // FIXME add `terminated(_, space0)` to all `all_consuming`
    escaped(not(char('"')), '\\', char('"'))
        .and_then(all_consuming(inner_lex::<'source, E>))
        .parse(input)
}

/// Parses page divider
/// TODO update it's definition in the manual
fn div_page<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    context("page divider", tuple((multispace0, tag("------"))))
        .map(|_| Token::PageDiv)
        .parse(input)
}

/// Parses header
///
/// - Always starts from a blank line
/// - Comprised of 1-6 '#'-symbols, and includes everything up until last empty space before line break
fn header<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    context(
        "header",
        tuple((
            multispace0::<&'source str, E>,
            newline,
            map_res(many1_count(char('#')), |count| {
                if count <= 6 {
                    Ok(count)
                } else {
                    Err(KyomatoLexErrorKind::TooDeepHeader { fold: count })
                }
            }),
            space0,
            take_until1("\n"),
        )),
    )
    .map(|(_, _, order, _, content)| Token::Header {
        order,
        content: content.trim_end().into(), // kinda sad I'm forced to do this here, but whatever
    })
    .parse(input)
}

/// Parses display math (equation)
///
/// - Always starts from a blank line with "$$" tag
/// - Continues up until another unescaped "$$" on a beginning of the line is encountered
/// - Possible "ref" argument (to refer to this equation) is matched afterwards
fn equation<
    'source,
    E: ParseError<&'source str> + ContextError<&'source str> + FromExternalError<&'source str, E>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let Some(ind) = input
        .char_indices()
        .filter_map(|(ind, c)| (c == '\n').then_some(ind))
        .find(|ind| input[*ind..].starts_with("\n$$"))
    else {
        return Err(nom::Err::Error(E::from_external_error(
            input,
            ErrorKind::CrLf,
            KyomatoLexErrorKind::BadSyntax {
                reason: "Display mathmode should start with a new line",
            },
        )));
    };
    
    let trimmed_input = &input[ind];
    let (rest, (_, _, content, _, (ident,))) = context(
        "display mathmode",
        tuple((
            multispace0,
            tag("$$"),
            take_until1("\n$$"),
            tag("\n$$"),
            parse_args!["ref" = alphanumeric1],
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

fn table<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, (header, _, cells, (ident, caption))) = context(
        "table",
        tuple((
            // first, parse the header
            preceded(
                char('\n'),
                delimited(
                    char('|'),
                    separated_list1(char('|'), delimited(space0, inner_lex, space0)),
                    char('|'),
                ),
            ),
            tuple((space0, char('\n'), take_until1("\n"))),
            // next, parse the cells
            preceded(
                char('\n'),
                delimited(
                    char('|'),
                    separated_list1(char('|'), delimited(space0, inner_lex, space0)),
                    char('|'),
                ),
            ),
            // finally, here come arguments
            parse_args!["ref" = alphanumeric1, "caption" = caption_kernel],
        )),
    )(input)?;
    Ok((
        rest,
        Token::Table {
            header: Tokens::new(header),
            cells: Tokens::new(cells),
            caption: caption.map(Box::new),
            ident: ident.map(Cow::from),
        },
    ))
}

fn figure<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    // TODO add width parameter to figure, I guess
    let (rest, (_, _, path, _, (ident, caption, _width))) = context(
        "figure",
        tuple((
            multispace0,
            tag("![["),
            take_until1("]]"),
            tag("]]"),
            parse_args![
                "ref" = alphanumeric1,
                "caption" = caption_kernel,
                "width" = float
            ],
        )),
    )(input)?;
    let token = Token::Figure {
        src_name: Path::new(path).into(),
        caption: caption.map(Box::new),
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

fn ayano<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    // python, Ayano * <description> ~ <path> # <id>
    // TODO add tests for escaping backslashes removal (generation-side)
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
    let display: Option<Option<Token<'source>>> = display.map(|(_, _, caption)| caption); // TODO add code captions support
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

macro_rules! char_array {
    ($from:literal .. $to:literal, $length:literal, $alphabet:literal) => {{
        static CHARS: ::once_cell::sync::Lazy<[char; $length]> =
            ::once_cell::sync::Lazy::new(|| {
                ($from..=$to).into_iter().collect_array().expect(&format!(
                    "There are exactly {} characters in {} alphabet",
                    $length, $alphabet
                ))
            });
        &*CHARS
    }};
}

fn _list_inner<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    mut input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
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
    let (list_type, item_start): (ListType, fn(usize) -> Option<Cow<'static, str>>) =
        match indent[non_whitespace..].chars().next() {
            None => {
                // there's not a single non-whitespace character -- it's impossible to properly parse a list here.
                return Err(nom::Err::Error(E::from_error_kind(
                    "can't parse list at the empty location",
                    ErrorKind::Eof,
                )));
            }
            Some('1') => {
                // that's a numeric list!
                (ListType::Num, |i| Some(format!("{}.", i + 1).into()))
            }
            Some('a') => {
                // that's a latin list!
                (ListType::Latin, move |i| {
                    char_array!('a'..'z', 26, "latin")
                        .get(i)
                        .map(char::to_string)
                        .map(Cow::from)
                })
            }
            Some('а') => {
                // that's a cyrillic list!
                (ListType::Latin, move |i| {
                    char_array!('а'..'я', 32, "cyrillic")
                        .get(i)
                        .map(char::to_string)
                        .map(Cow::from)
                })
            }
            Some('-') => {
                // that's a bullet list!
                (ListType::Num, |_| Some("-".into()))
            }
            Some('I') => {
                // that's a roman list!
                unimplemented!("Roman numerals list is not supported yet")
                // TODO add support, I guess
            }
            Some(_c) => {
                // that's an unknown list type
                // FIXME not sure how to use these errors properly, I think I need to study more examples
                return Err(nom::Err::Error(E::from_error_kind(
                    "unknown list type",
                    ErrorKind::Char,
                )));
            }
        };
    let items: Vec<_> = input
        .lines()
        .take_while(|line: &&str| line.starts_with(indent))
        .enumerate()
        .map(|(i, line): (usize, &str)| {
            let line = line.trim_start_matches::<&str>(
                item_start(i)
                    .ok_or(nom::Err::Error(E::from_error_kind(
                        "List has too much elements",
                        ErrorKind::Fix,
                    )))?
                    .borrow(),
            );
            let (_, item) = all_consuming(inner_lex::<'_, E>)(line)?;
            Ok::<_, nom::Err<E>>(item)
        })
        .try_collect()?;
    let content = Tokens::new(items);
    Ok(("", Token::List { list_type, content }))
}

fn list<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    context("list", preceded(char('\n'), _list_inner))(input)
}

fn _formatting_inner<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
    style: crate::data::Formatting,
) -> Result<Token<'source>, nom::Err<E>> {
    let (_, content) = all_consuming(inner_lex)(input)?;
    Ok(Token::Formatted(style, Box::new(content)))
}

fn formatting<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    use crate::data::Formatting;
    context(
        "formatting",
        alt((
            delimited(
                tag("**"),
                take_until1("**").and_then(all_consuming(inner_lex)),
                tag("**"),
            )
            .map(|input| (Box::new(input), Formatting::Bold)),
            delimited(
                tag("__"),
                take_until1("__").and_then(all_consuming(inner_lex)),
                tag("__"),
            )
            .map(|input| (Box::new(input), Formatting::Bold)),
            delimited(
                tag("*"),
                take_until1("*").and_then(all_consuming(inner_lex)),
                tag("*"),
            )
            .map(|input| (Box::new(input), Formatting::Italic)),
            delimited(
                tag("_"),
                take_until1("__").and_then(all_consuming(inner_lex)),
                tag("_"),
            )
            .map(|input| (Box::new(input), Formatting::Italic)),
            delimited(
                tag("~~"),
                take_until1("~~").and_then(all_consuming(inner_lex)),
                tag("~~"),
            )
            .map(|input| (Box::new(input), Formatting::StrikeThrough)),
        )),
    )
    .map(|(token, style)| Token::Formatted(style, token))
    .parse(input)
}

fn inline_math<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, formula) = context(
        "inline math",
        delimited(char('$'), take_until1("$"), char('$')),
    )(input)?;
    Ok((rest, Token::InlineMathmode(formula.into())))
}

fn reference<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, ident) = context(
        "reference",
        delimited(tag("[@"), take_until1("]"), char(']')),
    )(input)?;
    Ok((rest, Token::Reference(ident.into())))
}

fn footnote_reference<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, ident) = context(
        "footnote reference",
        delimited(tag("[^"), take_until1("]"), char(']')),
    )(input)?;
    Ok((rest, Token::FootNoteReference(ident.into())))
}

fn footnote_content<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, (ident, content)) = context(
        "footnote context",
        delimited(tag("[^"), take_until1("]:"), tag("]:")).and(preceded(
            space0,
            take_until1("\n").and_then(all_consuming(inner_lex)),
        )),
    )
    .parse(input)?;
    Ok((
        rest,
        Token::FootNoteContent {
            content: Box::new(content),
            ident: ident.into(),
        },
    ))
}

fn paragraph<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    // This is the most tricky component. It must stop matching AS SOON AS any sort-of syntax is encountered.
    // Should be noted, that this parsed is forward-blind, meaning it has no idea what element EXACTLY will be matched next.
    let bad_index = input
        .char_indices()
        .filter_map(|(ind, c)| matches!(c, '|' | '\n' | '$' | '`').then_some(ind))
        .find(|&ind| {
            ["|", "||||||", "\n#", "$", "$$", "`", "```"]
                .into_iter()
                .any(|p| input[ind..].starts_with(p))
        })
        .unwrap_or(input.len());
    let (this_text, rest) = input.split_at(bad_index);
    Ok((
        rest,
        Token::Paragraph(crate::data::Font::Normal, this_text.into()),
    ))
}

pub enum KyomatoLexErrorKind {
    /// Tokens were expected, but none were found.
    NoTokens,
    /// Headers only up to 6-fold are supported, but even more folded one was found
    TooDeepHeader {
        fold: usize,
    },
    /// Some token types might be forbidden in various contexts
    ForbiddenToken {
        token: &'static str,
        reason: &'static str,
    },
    BadSyntax {
        reason: &'static str,
    },
}

// TODO wrap outer-layer combinator into `complete`
/// Parses one or more tokens.
///
/// - If there's a single token to parse, it will be returned by itself, as a token
/// - If there's more than one token to parse, it will be returned as a `Token::Text` variant,
///     meaning it will allocate space on a heap for it's children
///
/// This parser accepts another parser it should use to parse tokens.
/// This point of customization allows for different possible token types to be permitted.
fn tokens_many1<
    'kernel,
    'source: 'kernel,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexErrorKind>
        + 'kernel,
>(
    mut tokens_kernel: impl FnMut(&'source str) -> IResult<&'source str, Token<'source>, E> + 'kernel,
) -> impl Parser<&'source str, Token<'source>, E> + 'kernel {
    enum InnerResult<'source> {
        None, // Will be converted into `KyomatoLexErrorKind::NoTokens`
        Single(Token<'source>),
        Multiple(Vec<Token<'source>>),
    }
    map_res(
        all_consuming(move |input| {
            // This iterator it kinda tricky to operate.
            // Basically, as far as I can see - it won't return error at each step.
            // Instead, error will be silently stored inside it, until it's `finish` function is called to extract it.
            // Basically, this means that each return from this function MUST INVOLVE `tokens.finish()?`!!!
            let mut iterator = iterator(input, &mut tokens_kernel);

            // First, let's try getting first token
            let Some(first_token) = (&mut iterator).next() else {
                // There is no first token!
                // Let's give iterator a chance to explain itself:
                let (rest, _) = iterator.finish()?;
                // Well, seems like there are no tokens and no error
                // (most likely input is just empty or th)
                // Return empty variant
                return Ok((rest, InnerResult::None));
            };

            // Now, let's try getting a second one
            let Some(second_token) = (&mut iterator).next() else {
                // Well, I suppose single token is enough
                // Still allow iterator to explain any problem
                let (rest, _) = iterator.finish()?;
                // Return single variant
                return Ok((rest, InnerResult::Single(first_token)));
            };

            // Ok so there are at least two tokens now.
            // Heap allocation is unavoidable
            let mut tokens = vec![first_token, second_token];
            tokens.extend(&mut iterator);
            // Iterator might have failed silently, so let them a chance to tell about it
            let (rest, _) = iterator.finish()?;
            // Return multiple variant
            Ok((rest, InnerResult::Multiple(tokens)))
        }),
        // Lastly, map this inner result into a single actual token.
        // Oh, and if there were no tokens - that's an error, so might return it as well.
        |inner_result| match inner_result {
            InnerResult::None => Err(KyomatoLexErrorKind::NoTokens),
            InnerResult::Single(token) => Ok(token),
            InnerResult::Multiple(tokens) => Ok(Token::Text(Tokens::new(tokens))),
        },
    )
}

/// Main entry point into the lexer
///
/// Basically, it attempts to parse as many tokens as possible,
/// and returns a single one, representing them all (a `Text` variant if there are more than one token)
pub fn lex<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    context(
        "unlimited lex",
        tokens_many1(alt((
            // Allow them all!
            div_page,
            header,
            equation,
            table,
            figure,
            href,
            code_block,
            ayano,
            list,
            formatting,
            inline_math,
            reference,
            footnote_content,
            footnote_reference,
            paragraph,
        ))),
    )
    .parse(input)
}

/// Transforms successful parse into *failure* indicating that it was not supposed to happen
///
/// If underlying parser gets parsed successfully, will return an error of `KyomatoLexErrorKind::ForbiddenToken` type
fn forbid<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    mut parser: impl Parser<&'source str, Token<'source>, E>,
    token: &'static str,
    reason: &'static str,
) -> impl Parser<&'source str, Token<'source>, E> {
    move |input| {
        // Attempt to parse the token
        // If parser fails here -- it's alright
        let _ = parser.parse(input)?;
        // Token was parsed. Return failure
        Err(nom::Err::Failure(E::from_external_error(
            input,
            ErrorKind::MapRes,
            KyomatoLexErrorKind::ForbiddenToken { token, reason },
        )))
    }
}

/// Auxiliary entry point into the lexer
///
/// Functions similarly to `lex`, but forbids certain token types
pub fn inner_lex<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    context(
        "inner lex",
        tokens_many1({
            alt((
                // Some tokens must be forbidden
                // I'd like parser to actually tell the user that token is forbidden in this context,
                // to avoid unnecessary confusion of "why wouldn't it see a figure here?!"
                forbid(
                    div_page,
                    "page divider",
                    "pages can only be divided in the outer layer",
                ),
                forbid(header, "header", "you are not supposed to do that"),
                forbid(
                    equation,
                    "display equation",
                    "LaTeX does not support display math inside captions/footnotes",
                ),
                forbid(table, "table", "table can only be outer element"),
                forbid(figure, "figure", "figure can only be outer element"),
                forbid(code_block, "code block", "you are not supposed to do that"),
                forbid(
                    list,
                    "list",
                    "LaTeX does not support lists inside captions and footnotes well",
                ),
                // Rest are ok to be parsed!
                href,
                ayano,
                formatting,
                inline_math,
                reference,
                footnote_content,
                footnote_reference,
                paragraph,
            ))
        }),
    )
    .parse(input)
}

/// Auxiliary entry point into the lexer, used to lex inner fields of Ayano-outputted tokens
///
/// Functions similarly to `lex`, but forbids certain token types
pub fn static_inner_lex<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    context(
        "static inner lex",
        tokens_many1({
            alt((
                // Some tokens must be forbidden
                // I'd like parser to actually tell the user that token is forbidden in this context,
                // to avoid unnecessary confusion of "why wouldn't it see a figure here?!"
                forbid(
                    div_page,
                    "page divider",
                    "pages can only be divided in the outer layer",
                ),
                forbid(header, "header", "you are not supposed to do that"),
                forbid(
                    equation,
                    "display equation",
                    "LaTeX does not support display math inside captions/footnotes",
                ),
                forbid(table, "table", "table can only be outer element"),
                forbid(figure, "figure", "figure can only be outer element"),
                forbid(code_block, "code block", "you are not supposed to do that"),
                forbid(
                    list,
                    "list",
                    "LaTeX does not support lists inside captions and footnotes well",
                ),
                forbid(
                    ayano,
                    "ayano",
                    "Ayano inside Ayano is not supported, you are not supposed to do that",
                ),
                forbid(
                    footnote_content,
                    "footnote content",
                    r"All footnote content must be static and available right away.
If you want to create a footnote with Ayano-defined content,
please include Ayano block *inside* you footnote definition.",
                ),
                // Rest are ok to be parsed!
                href,
                formatting,
                inline_math,
                reference,
                footnote_reference,
                paragraph,
            ))
        }),
    )
    .parse(input)
}

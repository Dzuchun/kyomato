use std::{
    borrow::Cow, error::Error, fmt::Debug, num::ParseIntError, path::Path, str::FromStr,
    sync::RwLock,
};

use itertools::Itertools;
use nom::{
    branch::alt,
    bytes::{
        complete::{tag, take_till1, take_until, take_until1, take_while},
        streaming::tag_no_case,
    },
    character::complete::{char, digit1, multispace0, space0},
    combinator::{all_consuming, cut, iterator, map_res, opt, recognize},
    error::{context, ContextError, ErrorKind, FromExternalError, ParseError},
    multi::{many0_count, many1},
    number::complete::float,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult, Parser,
};
use url::Url;

use crate::{
    data::{AyanoBlock, Formatting, ListType, Token, Tokens},
    util::{optional_permutation, Equivalent, IteratorArrayCollectExt, VecTryExtendExt},
};

/// Matches any sort of whitespace characters, *newline*, and a provided tag at the end
fn _multispace_line_break<'source, E: ParseError<&'source str>>(
    tag: &'static str,
) -> impl Parser<&'source str, (&'source str, &'source str), E> {
    move |input: &'source str| {
        // here's a chars iterator
        let chars = input.char_indices();
        // now, limit them to fist non-whitespace character
        let whitespace = chars.take_while(|(_, c)| c.is_whitespace());
        // then, filter out newline characters only
        let mut newline = whitespace.filter_map(|(ind, c)| (c == '\n').then_some(ind));
        // lastly, find first newline character that prepends provided tag
        let Some(res) = newline.find(|ind| input[(ind + 1)..].starts_with(tag)) else {
            // if there's no such idex, that's an error
            return Err(nom::Err::Error(E::from_error_kind(
                input,
                ErrorKind::MultiSpace,
            )));
        };

        // We may now construct the answer
        let space = &input[..=res]; // space is up to the newline included

        // will return a copy of provided tag, since it was already matched
        let rest = &input[(res + 1 + tag.len())..];

        Ok((rest, (space, tag)))
    }
}

fn _optional_permutation<'parsers, 'source, E: ParseError<&'source str>, const N: usize>(
    parsers: [&'parsers mut dyn Parser<&'source str, &'source str, E>; N],
) -> impl Parser<&'source str, [Option<&'source str>; N], E> + 'parsers {
    move |mut input| {
        // the end result
        let mut parsed = [None::<&'source str>; N];
        'outer: for _ in 0..N {
            for i in 0..N {
                if parsed[i].is_none() {
                    match parsers[i].parse(input) {
                        Ok((rest, res)) => {
                            // this parser succeeded, advance forward
                            parsed[i] = Some(res);
                            input = rest;
                            continue 'outer; // continue outer iteration
                        }
                        Err(nom::Err::Failure(err)) => {
                            // propagate a failure
                            return Err(nom::Err::Failure(err));
                        }
                        Err(_) => {
                            // well, do nothing
                        }
                    }
                }
            }
            // all parser had failed to parse (success would've continued outer cycle)
            // break out of outer cycle
            break 'outer;
        }
        // return the result (input variable was updated to be rest of the input, on successful parses)
        Ok((input, parsed))
    }
}

// Conventions:
// - parsers should only parse empty characters with `empty`
// - parsers should only parse empty space at the beginning, not the end
// - newline parsing should be carefully managed

/// Generates argument parser that accounts for all necessary syntax
macro_rules! parse_args {
    [$($argument_name:literal = $argument_kernel:expr), +] => {
        preceded(
            pair(
                space0,
                tag("\n{"), // NOTE: arguments must start from an empty line
            ),
            cut(terminated(
            optional_permutation((
                $(
                    tuple((
                        space0,
                        tuple((tag($argument_name), space0, char('='), space0, context("arg", context($argument_name, $argument_kernel)))),
                        space0,
                        alt((char(','), nom::combinator::peek(char('}')))), // this has unexpected consequences, but whatever :idk:
                    )).map(|(_, (_, _, _, _, v), _, _)| v)
                ), +,
            )),
            char('}'))),
        )
        // If parser above had failed - it's alright, arguments are optional anyway
        // Return `None` variant for everyone in this case
        .or(|input| Ok((input, ($({let _ = $argument_name; None},) +))))
    };
}

/// Parses caption (just the literal) part
///
/// Quite a tricky task, as there can be double quotes -- but escaped.
/// Apart from double unescaped quotes, caption can be ANYTHING.
fn caption_kernel<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexError>
        + 'source,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    // first, match opening quote
    let (input, _) = char('"')(input)?;
    // then, let's filter all double quotes
    let double_quotes = input.char_indices().filter(|(_, c)| c == &'"');
    // out of them, choose unescaped ones
    let unescaped_quotes = double_quotes.filter(|(i, _)| !&input[..*i].ends_with('\\'));
    // lastly, try parsing each of them
    let mut parsed = unescaped_quotes
        .map(|(i, _)| inner_lex::<'source, E>(&input[..i]).map(|(_, t)| (&input[i..], t)));
    let (input, token) =
        parsed
            .find_map(Result::ok)
            .ok_or(nom::Err::Error(E::from_external_error(
                input,
                ErrorKind::Escaped,
                KyomatoLexError::no_tokens(input),
            )))?;

    // lastly, match the actual closing quote
    let (input, _) = cut(char('"'))(input)?;
    Ok((input, token))
}

/// Parses page divider
#[inline]
fn div_page<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    context("page divider", _multispace_line_break("------"))
        .map(|_| Token::PageDiv)
        .parse(input)
}

fn ident_kernel<'source, E: ParseError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, &'source str, E> {
    take_till1(|c: char| !matches!(c, 'a'..='z' | 'A'..='Z' | '1'..='9' | '_' | ':')).parse(input)
}

/// Parses header
///
/// - Always starts from a blank line
/// - Comprised of 1-6 '#'-symbols, and includes everything up until last empty space before line break
#[inline]
fn header<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexError>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    context(
        "header",
        tuple((
            _multispace_line_break("#"),
            map_res(many0_count(char('#')), |mut count| {
                // one of the hashes was parsed already
                count += 1;
                if count <= 6 {
                    Ok(count)
                } else {
                    Err(KyomatoLexError::too_deep_header(input, count))
                }
            }),
            space0,
            take_until1("\n"),
        )),
    )
    .map(|(_, order, _, content)| Token::Header {
        order,
        content: content.trim_end().into(), // kinda sad I'm forced to do this here, but whatever
    })
    .parse(input)
}

/// Parses display math (equation)
///
/// - Always starts from a blank line with "$$" tag
/// - Continues up until another unescaped "$$" on a beginning of the line is encountered
/// - Possible "ref" argument (for referring to this equation) is matched afterwards
fn equation<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexError>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, (content, _, (ident,))) = context(
        "display mathmode",
        preceded(
            _multispace_line_break("$$\n"),
            // if mathmode opening found, lock into equation match
            cut(tuple((
                take_until1("\n$$"),
                tag("\n$$"),
                parse_args!["ref" = ident_kernel],
            ))),
        ),
    )(input)?;
    Ok((
        rest,
        Token::Equation {
            content: content.into(),
            ident: ident.map(Cow::from),
        },
    ))
}

/// Parses table row
///
/// Parses ALL of it's input
fn _parse_table_row<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexError>
        + 'source,
>(
    input: &'source str,
) -> IResult<
    &'source str,
    impl Iterator<Item = IResult<&'source str, Token<'source>, E>> + 'source,
    E,
> {
    let pipes = input.char_indices().filter(|(_, c)| c == &'|');
    let unescaped_pipes = pipes.filter(|(ind, _)| ind > &0 && !input[..*ind].ends_with('\\'));
    let mut dividers = unescaped_pipes.map(|(ind, _)| ind);
    let mut last_ind = dividers
        .next()
        .ok_or(nom::Err::Error(E::from_external_error(
            input,
            ErrorKind::Eof,
            KyomatoLexError::no_tokens(input),
        )))?;
    let slices = std::iter::once(&input[..last_ind]).chain(dividers.filter_map(move |ind| {
        if ind == last_ind {
            return None;
        }
        let slice = &input[(last_ind + 1)..ind];
        last_ind = ind;
        Some(slice)
    }));
    let tokens = slices.map(all_consuming(terminated(inner_lex, space0)));
    Ok(("", tokens))
}

fn try_fold_many1<I: Clone, Inner, O, E>(
    mut parser: impl Parser<I, Inner, E>,
    mut init: impl FnMut() -> O,
    mut combiner: impl FnMut(O, Inner) -> Result<O, nom::Err<E>>,
) -> impl Parser<I, O, E> {
    move |mut input: I| {
        let mut output = init();
        loop {
            // first, try parse the main token
            // any error should break the loop
            // failure should break with a failure
            let res = match parser.parse(input.clone()) {
                Ok(res) => res,
                Err(nom::Err::Failure(fail)) => return Err(nom::Err::Failure(fail)),
                Err(_) => break,
            };
            input = res.0;
            let next = res.1;
            // then, attempt to combine it
            output = combiner(output, next)?;
        }
        Ok((input, output))
    }
}

/// Parses table
///
/// - Always starts from a newline followed by '|' char
/// - First line is always interpreted as header
/// TODO - Next line is skipped for now; all tables are formatted with centered columns
/// - Following lines starting with '|' are interpreted as table cells
/// - Lastly, optional "ref" and "caption" arguments are parsed
fn table<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexError>
        + 'source,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, (_, headers, (_, _, cells, (ident, caption)))) = context(
        "table",
        tuple((
            // first, find the start
            _multispace_line_break("|"),
            take_until1("\n").and_then(|input| {
                let (_, headers) = _parse_table_row(input)?;
                let headers: Vec<_> = headers.map_ok(|(_, t)| t).try_collect()?;
                Ok(("", headers))
            }),
            // at this point this is guaranteed to be a table token, so we lock in
            cut(tuple((
                // first, match the end of this line
                char('\n'),
                // then, match format line (it's discarded right now)
                take_until("\n"),
                // from here, match table cells
                // while this is possible to do by just per-line collection of them into a vectors
                // (and merge them afterwards),
                // I can't stand unnecessary heap allocations, so here is my solution:
                try_fold_many1(
                    preceded(tag("\n|"), take_until("\n").and_then(_parse_table_row)),
                    Vec::<Token<'source>>::new,
                    |mut acc, next| {
                        acc.try_extend(next.map_ok(|(_, t)| t))?;
                        Ok(acc)
                    },
                ),
                // finally, here come the arguments
                parse_args!["ref" = ident_kernel, "caption" = caption_kernel],
            ))),
        )),
    )(input)?;
    Ok((
        rest,
        Token::Table {
            header: Tokens::new(headers),
            cells: Tokens::new(cells),
            caption: caption.map(Box::new),
            ident: ident.map(Cow::from),
        },
    ))
}

/// Matches figure
///
/// - Always starts from a newline followed by "![[" sequence (that's obsidian's image inclusion syntax or th)
/// - Everything up to "]]" sequence is interpreted as path to the image
/// - Lastly, optional "ref", "caption" and "width" arguments follow. Can supplied now, and will be recognized, but it's not supported yet
fn figure<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexError>
        + 'source,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    // TODO add width parameter to figure, I guess
    let (rest, (_, (path, _, (ident, caption, width)))) = context(
        "figure",
        pair(
            // first, match syntax start
            _multispace_line_break("![["),
            // at this point, it's supposed to be figure exclusively, so lock into this syntax
            cut(tuple((
                // that's gonna be a path to image
                take_until1("]]"),
                tag("]]"),
                // and here come the args
                parse_args![
                    "ref" = ident_kernel,
                    "caption" = caption_kernel,
                    "width" = float
                ],
            ))),
        ),
    )(input)?;
    let token = Token::Figure {
        src_name: Path::new(path).into(),
        caption: caption.map(Box::new),
        ident: ident.map(Cow::from),
        width,
    };
    Ok((rest, token))
}

/// Parses markdown's hyper reference syntax
///
/// Basic syntax is [DISPLAY](URL)
fn href<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexError>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    context(
        "href",
        preceded(
            multispace0,
            pair(
                delimited(char('['), take_until("]"), char(']')),
                // url should be parsed into proper format
                map_res(
                    delimited(char('('), take_until(")"), char(')')),
                    |url: &str| {
                        Url::from_str(url).map_err(|err| KyomatoLexError::bad_url(input, err))
                    },
                ),
            ),
        ),
    )
    .map(|(display, url)| Token::Href {
        url,
        display: display.trim().into(),
    })
    .parse(input)
}

/// Parses code block
///
/// *WARNING!* This parser CAN parse Ayano code blocks, which (obviously) wont execute them.
/// Consider attempting to parse for Ayano block BEFORE running this parser
///
/// - Always starts with a newline followed by "```" sequence
/// - Everything up to the end of this very line (stripped from whitespace) will be considered specified language
/// - All lines up to a newline followed by "```" sequence will be considered as code to include
fn code_block<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, (lang, code)) = context(
        "code block",
        pair(
            delimited(_multispace_line_break("```"), take_until("\n"), char('\n')),
            // At this point, this is a code block syntax for sure, so we lock in
            cut(terminated(take_until("\n```"), tag("\n```"))),
        ),
    )(input)?;
    // Trim the language name
    let lang = lang.trim();
    // If the language name turns out to be empty, it's as good as unspecified
    let language = if lang.is_empty() {
        None
    } else {
        Some(Cow::from(lang))
    };
    Ok((
        rest,
        Token::CodeBlock {
            code: code.into(),
            language,
        },
    ))
}

/// Parses an Ayano block
///
/// *because Ayano loves Kyoko*
///
/// There's quite a lot of syntax to cover:
/// - First, this token starts from a newline followed by "```" sequence.
/// - Next, "python, Ayano" is matched with various spacing options
/// - Next comes a permutation of various block parameters, like
///     - * "<CAPTION>" for display block with optional caption (can be omitted)
///     - ~ <PATH> for insert-path. WARN: PATH SHOULD NOT CONTAIN SPACES
///     - # <u16> ident of a block. Unused, as of now.
///         Was intended to make blocks more unique, but think about it -
///         same blocks can have shared representation in python code; this code is still executed twice).
///     - ! used to mark block as static, i.e. - it's code will be executed statically,
///         and nothing would be displayed in place of it in the output
/// - Everything following this line and up to a newline followed by "\n```" will be considered a Python code to execute
fn ayano<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexError>
        + 'source,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    // TODO add tests for escaping backslashes removal (generation-side)
    let (rest, ((display, insert, _ident, is_static), code)) = context(
        "ayano",
        preceded(
            tuple((
                _multispace_line_break("```"),
                space0,
                tag_no_case("python,"),
                space0,
                tag("Ayano"),
                space0,
            )),
            // This is guaranteed to be Ayano block at this point, so we lock in
            cut(pair(
                terminated(
                    optional_permutation((
                        preceded(
                            pair(char('*'), space0),
                            opt(terminated(caption_kernel, space0)),
                        ),
                        preceded(
                            pair(char('~'), space0),
                            terminated(take_till1(char::is_whitespace), space0),
                        ),
                        preceded(
                            pair(char('#'), space0),
                            terminated(
                                map_res(digit1, |n| {
                                    u16::from_str(n)
                                        .map_err(|err| KyomatoLexError::bad_indent(input, err))
                                }),
                                space0,
                            ),
                        ),
                        terminated(char('!'), space0),
                    )),
                    char('\n'),
                ),
                terminated(take_until("\n```"), tag("\n```")),
            )),
        ),
    )(input)?;
    // type might be a bit confusing, but basically:
    // - outer None means this block is not meant to be displayed
    // - outer Some means this block is to be displayed
    // - inner None means this block has no display caption
    // - inner Some means this block has display caption
    // TODO add code captions support
    let insert: Option<&'source Path> = insert.map(Path::new);
    // XXX add support for codeblock identifiers
    let code: Cow<'_, str> = Cow::from(code);
    Ok((
        rest,
        Token::Ayano {
            data: AyanoBlock {
                insert_path: insert.map(Cow::from),
                code,
                is_display: display.is_some(),
                is_static: is_static.is_some(),
            },
        },
    ))
}

/// Creates static character array from supplied character range.
///
/// Utilizes `once_cell` to do so.
///
/// ### Panics
/// if collection into array was not successful (i.e. wrong length was specified)
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

/// Yeah, there is some leaking happening here....
///
/// Don't worry about it, I guess (?)
/// (*why would I create and format these strings each time??!*)
/// TODO I guess, #[cached] macro would do better here, or th
fn _list_item_generator<'scope>(
    list_type: &'scope ListType,
) -> Box<dyn Iterator<Item = &'scope str> + 'scope> {
    match list_type {
        ListType::Bullet => Box::new(std::iter::repeat("-")),
        ListType::Num => {
            static STRS: RwLock<Vec<&'static str>> = RwLock::new(Vec::new());
            Box::new((1..).into_iter().map(|i| {
                // First, try get already generated string
                let read = STRS.read().unwrap();
                if let Some(res) = read.get(i - 1) {
                    return &**res;
                }
                // Hmmm, that didn't worked.
                // Give away read lock, and ask for modification
                drop(read);
                let mut write = STRS.write().unwrap();
                // Append all necessary strings for the array
                while write.len() < i {
                    let j = write.len() + 1;
                    write.push(format!("{}.", j).leak());
                }
                write
                    .get(i - 1)
                    .expect("Should be present now, we should've just pushed it")
            }))
        }
        ListType::Latin => {
            static STRS: once_cell::sync::Lazy<[&'static str; 26]> =
                once_cell::sync::Lazy::new(|| {
                    char_array!('a'..'z', 26, "latin")
                        .map(|l| format!("{l}.").leak() as &'static str)
                });
            Box::new((*STRS).into_iter().map(|b| &*b))
        }
        ListType::Cyrillic => {
            static STRS: once_cell::sync::Lazy<[&'static str; 32]> =
                once_cell::sync::Lazy::new(|| {
                    char_array!('а'..'я', 32, "cyrillic")
                        .map(|l| format!("{l}.").leak() as &'static str)
                });
            Box::new((*STRS).into_iter().map(|b| &*b))
        }
        ListType::Roman => unimplemented!("Roman numerated lists are not supported yet"),
    }
}

fn _list_inner<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexError>
        + 'source,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    // First, we need to actually detect list type
    let (input, _) = _multispace_line_break("").parse(input)?;

    let (_, first_discriminator) = recognize(take_till1(char::is_whitespace))(input)?;
    let list_type = match first_discriminator {
        "1." => ListType::Num,
        "a." => ListType::Latin,
        "а." => ListType::Cyrillic,
        "-" => ListType::Bullet,
        "I" => unimplemented!("Roman numerated lists are not supported yet"),
        _ => {
            // that's an unknown list type
            return Err(nom::Err::Error(E::from_external_error(
                input,
                ErrorKind::Char,
                KyomatoLexError::unknown_list_type(input),
            )));
        }
    };
    let mut start_generator = _list_item_generator(&list_type);

    let (rest, items) = many1(move |input| {
        let Some(this_start) = start_generator.next() else {
            // I'm not sure if this error is possible to happen under current implementation, but whatever :idk:
            return Err(nom::Err::Error(E::from_external_error(
                input,
                ErrorKind::IsA,
                KyomatoLexError::too_long_list(input),
            )));
        };
        // Match current item start and item itself
        preceded(
            tuple((opt(char('\n')), tag(this_start), space0)),
            take_until("\n").and_then(inner_lex),
        )(input)
    })(input)?;
    // Unfortunately, we can't use `map` function here, since it accepts `Fn` (not `FnOnce`),
    // and I'd like to avoid giving `Copy` to every single thing in existence
    Ok((
        rest,
        Token::List {
            list_type,
            content: Tokens::new(items),
        },
    ))
}

/// Parses list
///
/// There are multiple types of lists supported:
/// - bullet: every line is prepended with '-'
/// - num: every line is prepended with a number and a dot following it
/// - latin: every line is prepended with a latin letter and a dot following it. Number of entries is limited.
/// - cyrillic: every line is prepended with a cyrillic letter and a dot following it. Number of entries is limited.
///
/// Roman numerals lack support as of now, but I plan to support them in the future too
fn list<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexError>
        + 'source,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    context("list", _list_inner)(input)
}

#[deprecated]
fn _formatting_inner<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexError>
        + 'source,
>(
    input: &'source str,
    style: crate::data::Formatting,
) -> Result<Token<'source>, nom::Err<E>> {
    let (_, content) = all_consuming(terminated(inner_lex, space0))(input)?;
    Ok(Token::Formatted(style, Box::new(content)))
}

/// Parses formatting declaration (or whatever you wanna call that)
///
/// **WARNING**: I expect this parse to be kinda-heavy. Consider using it *as late as possible*, but not before regular text parser, obviously.
///
/// Basically, there are 3 types of formatting:
/// - Italic: indicated by single '*' or '_'
/// - Bold: indicated by double '*' or '_'
/// - Strikethrough: indicated by double '~'
///
/// Note: this parser will try to parse multiple times.
/// So PLEASE: be careful with the way you use '*' and '_', as it may prolong parsing time.
fn formatting<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexError>
        + 'source,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    // First, let's parse the empty space before paragraph
    let (input, space_before) = multispace0(input)?;
    // if there are newlines, this paragraph is_newline
    let is_newline = space_before.contains('\n');
    let (rest, delimiter) = alt((tag("**"), tag("__"), tag("*"), tag("_"), tag("~~")))(input)?;
    // Anything starting from these characters IS a formatting token.
    // Committed to parsing after this point
    let formatting = match delimiter {
        "**" | "__" => Formatting::Bold,
        "*" | "_" => Formatting::Italic,
        "~~" => Formatting::StrikeThrough,
        _ => unreachable!(),
    };
    let possible_ends = rest
        .char_indices()
        .filter_map(|(ind, _)| (rest[ind..].starts_with(delimiter)).then_some(ind));
    for end_candidate in possible_ends {
        let inner_tokens = &rest[..end_candidate];
        if let Ok((_, (content, follower))) =
            all_consuming(terminated(paragraph_inner::<'source, E>, space0))(inner_tokens)
        {
            return if follower.is_some() {
                Err(nom::Err::Error(E::add_context(
                    input,
                    "formatting",
                    E::from_error_kind(input, ErrorKind::Fail),
                )))
            } else {
                Ok((
                    &rest[(end_candidate + delimiter.len())..],
                    Token::Paragraph {
                        is_newline,
                        formatting: Some(formatting),
                        content,
                    },
                ))
            };
        }
    }
    // If this point was reached, there is no correct variant for pairing delimiter.
    Err(nom::Err::Error(E::add_context(
        input,
        "formatting",
        E::from_error_kind(input, ErrorKind::Eof),
    )))
}

/// Parses inline math, delimited by '$'
fn inline_math<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    context(
        "inline math",
        preceded(
            multispace0,
            delimited(char('$'), take_till1(|c| c == '\n' || c == '$'), char('$')),
        ),
    )
    .map(|content: &str| Token::InlineMathmode {
        content: content.trim().into(),
    })
    .parse(input)
}

/// Parses object reference, inside a document
///
/// Basic syntax: [@<REF>], where <REF> can be
/// - fig:<IDENT> for figure
/// - tab:<IDENT> for table
/// - eq:<IDENT> for equation
///
/// In fact, these are not validated. Any sort of missing object would be detected and reported during document generation
///
/// Also note that any whitespace at the beginning and the end (of the text inside brackets) would be trimmed away
fn reference<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, ident) = context(
        "reference",
        delimited(
            pair(pair(multispace0, tag("[@")), space0),
            ident_kernel,
            pair(space0, char(']')),
        ),
    )(input)?;
    Ok((
        rest,
        Token::Reference {
            ident: ident.trim().into(),
        },
    ))
}

/// Parses a footnote reference
///
/// Acts basically the same as regular reference, but uses different symbol for identification
/// (since this sort of reference is actually supported by markdown!)
fn footnote_reference<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, ident) = context(
        "footnote reference",
        delimited(
            pair(multispace0, tag("[^")),
            delimited(space0, ident_kernel, space0),
            char(']'),
        ),
    )(input)?;
    Ok((
        rest,
        Token::FootNoteReference {
            ident: ident.trim().into(),
        },
    ))
}

/// Parses definition of a footnote content
///
/// WARNING! This parser must be checked BEFORE `footnote_reference` one.
/// This is a consequence of footnote reference and footnote definition having potentially the same syntax, but footnote definition having more of it.
///
/// basic syntax: \n[^<NAME>]:<CONTENT>\n
fn footnote_content<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexError>
        + 'source,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    context(
        "footnote context",
        delimited(
            pair(_multispace_line_break("[^"), space0),
            ident_kernel,
            pair(space0, tag("]:")),
        )
        .and(preceded(
            space0,
            // Content can end at the end of this line, or at the end of file
            alt((take_until1("\n"), take_while(|_| true)))
                .and_then(cut(all_consuming(terminated(inner_lex, space0)))),
        )),
    )
    .map(|(ident, content)| Token::FootNoteContent {
        content: Box::new(content),
        ident: ident.trim().into(),
    })
    .parse(input)
}

fn paragraph_inner<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexError>
        + 'source,
>(
    input: &'source str,
) -> IResult<&'source str, (Cow<'source, str>, Option<Token<'source>>), E> {
    // At this point we assume, that whitespace before was parsed, including any possible newlines

    // If input turns out empty after that, return error
    if input.is_empty() {
        return Err(nom::Err::Error(E::from_error_kind(input, ErrorKind::Eof)));
    }

    // Any token starting with a newline is out of the question
    // However, this still leaves a couple of candidates:
    // - href (starting with '[')
    // - formatting (starting with '*', '_' or '~')
    // - inline_math (starting with '$')
    // - reference (starting with "[@")
    // - footnote_reference (starting with "[^")

    // First, let's isolate input to a newline (if there is no newline, stop at last char)
    let interrupt_ind = input.find('\n').unwrap_or(input.len());
    let cut_input = &input[..interrupt_ind];

    // Now, let's try finding interrupting token
    let interruption = cut_input
        .char_indices()
        // Parsing continues until new, Option<Token<'source>>)line is reached
        .take_while(|(_, c)| c != &'\n')
        // Possible breakpoints are at characters '[', '*', '_', '~', and '$'
        .filter(|(_, c)| ['[', '*', '_', '~', '$'].contains(c))
        // Now, try a proper parser for each one
        .map(|(ind, c)| {
            let cut_cut_input = &input[ind..];
            let parse_res = match c {
                '[' => {
                    if cut_input[ind..].starts_with("[@") {
                        reference(cut_cut_input)
                    } else if cut_input[ind..].starts_with("[^") {
                        footnote_reference(cut_cut_input)
                    } else {
                        href(cut_cut_input)
                    }
                }
                '*' | '_' | '~' => formatting(cut_cut_input),
                '$' => inline_math(cut_cut_input),
                _ => unreachable!(),
            };
            (ind, parse_res)
        })
        .try_fold((), |_, (ind, parse_res)| match parse_res {
            // if there is a match for the following token -- return this "Ok" wrapped into "Err", forcing iterator to stop
            Ok(ok) => Err(Ok((ind, ok))),
            // if there is not a match, but a FAUILURE -- return it as "Err" wrapped in "Err", forcing iterator to stop
            Err(nom::Err::Failure(err)) => Err(Err(nom::Err::Failure(err))),
            // if parsing had failed, continue driving iterator by returning "Ok"
            Err(_) => Ok(()),
        })
        .err();

    let interruption = if let Some(interruption) = interruption {
        // there was an interruption!
        // but it might be a FAILURE ONE. That should bubble up
        Some(interruption?)
    } else {
        None
    };

    // Now, construct the output
    let this_token_end = interruption
        .as_ref()
        .map(|(ind, _)| *ind)
        .unwrap_or(interrupt_ind);
    let rest_input = interruption
        .as_ref()
        .map(|(_, (rest, _))| *rest)
        .unwrap_or(&input[this_token_end..]);
    let follower = interruption.map(|(_, (_, f))| f);
    Ok((
        rest_input,
        (Cow::from(cut_input[..this_token_end].trim()), follower),
    ))
}

/// Parses plaintext paragraph, up to at most the end of input/line
///
/// WARNING: must be used AFTER ALL OTHER PARSERS.
/// This parser is able to parse virtually anything, but in most cases you expect input being interpreted in some other way.
///
/// This is the most tricky of the parsers, since it must stop, if any other parser *might* be possible to use.
/// The only way to check, if any other parser may be used - is to ask it directly, "Ok" result meaning that it had succeeded.
///
/// I don't feel like discarding potentially-allocating and deeply-nested parsed tokens.
/// Because of that, this parser actually outputs one OR two tokens: one is always the paragraph one, and the other - some sort of different token it had managed to parse.
fn paragraph<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexError>
        + 'source,
>(
    outer_input: &'source str,
) -> IResult<&'source str, (Token<'source>, Option<Token<'source>>), E> {
    // First, let's parse the empty space before paragraph
    let (input, space_before) = multispace0(outer_input)?;
    // if there are newlines, this paragraph is_newline
    let is_newline = space_before.contains('\n');

    let (rest, (content, follower)) = paragraph_inner(input)?;

    let this_token = Token::Paragraph {
        is_newline,
        content,
        formatting: None,
    };
    Ok((rest, (this_token, follower)))
}

fn __equalizer(token: Token<'_>) -> (Token<'_>, Option<Token<'_>>) {
    (token, None)
}

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum KyomatoLexError {
    #[error("{}", .0)]
    Single(Box<str>, KyomatoLexErrorStackEntry),
    #[error("STACK: {:?}", .0)]
    Multiple(Vec<(Box<str>, KyomatoLexErrorStackEntry)>),
    #[error("ALT: {:?}", .0)]
    Alt(Vec<KyomatoLexError>),
}

impl KyomatoLexError {
    fn no_tokens(pos: &str) -> Self {
        Self::Single(Box::<str>::from(pos), KyomatoLexErrorStackEntry::NoTokens)
    }

    fn forbidden_token(pos: &str, token: &'static str, reason: &'static str) -> Self {
        Self::Single(
            Box::<str>::from(pos),
            KyomatoLexErrorStackEntry::ForbiddenToken { token, reason },
        )
    }

    fn bad_indent(pos: &str, error: ParseIntError) -> Self {
        Self::Single(
            Box::<str>::from(pos),
            KyomatoLexErrorStackEntry::BadIdent(error),
        )
    }

    fn too_long_list(pos: &str) -> Self {
        Self::Single(
            Box::<str>::from(pos),
            KyomatoLexErrorStackEntry::TooLongList,
        )
    }

    fn unknown_list_type(pos: &str) -> Self {
        Self::Single(
            Box::<str>::from(pos),
            KyomatoLexErrorStackEntry::UnknownListType,
        )
    }

    fn bad_url(pos: &str, err: url::ParseError) -> Self {
        Self::Single(
            Box::<str>::from(pos),
            KyomatoLexErrorStackEntry::BadUrl(err),
        )
    }

    fn too_deep_header(pos: &str, fold: usize) -> Self {
        Self::Single(
            Box::<str>::from(pos),
            KyomatoLexErrorStackEntry::TooDeepHeader { fold },
        )
    }

    fn _stack(self) -> Vec<(Box<str>, KyomatoLexErrorStackEntry)> {
        match self {
            KyomatoLexError::Single(pos, entry) => vec![(pos, entry)],
            KyomatoLexError::Multiple(stack) => stack,
            err @ KyomatoLexError::Alt(_) => {
                vec![(
                    Box::from("ALT"),
                    KyomatoLexErrorStackEntry::Inner(Box::new(err)),
                )]
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum KyomatoLexErrorStackEntry {
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
    BadUrl(url::ParseError),
    BadIdent(ParseIntError),
    UnknownListType,
    TooLongList,
    External(Equivalent<Box<dyn Error>>),
    Nom(ErrorKind),
    Context(&'static str),
    Inner(Box<KyomatoLexError>),
}

impl<'source> ParseError<&'source str> for KyomatoLexError {
    fn from_error_kind(input: &'source str, kind: ErrorKind) -> Self {
        Self::Single(
            Box::<str>::from(input),
            KyomatoLexErrorStackEntry::Nom(kind),
        )
    }

    fn append(input: &'source str, kind: ErrorKind, other: Self) -> Self {
        let mut stack = other._stack();
        stack.push((
            Box::<str>::from(input),
            KyomatoLexErrorStackEntry::Nom(kind),
        ));
        Self::Multiple(stack)
    }

    fn or(self, other: Self) -> Self {
        Self::Alt(vec![self, other])
    }
}

impl<'source> ContextError<&'source str> for KyomatoLexError {
    fn add_context(input: &'source str, ctx: &'static str, other: Self) -> Self {
        let mut stack = other._stack();
        stack.push((
            Box::<str>::from(input),
            KyomatoLexErrorStackEntry::Context(ctx),
        ));
        Self::Multiple(stack)
    }
}

impl<'source, E: Error + 'static> FromExternalError<&'source str, E> for KyomatoLexError {
    fn from_external_error(input: &'source str, kind: ErrorKind, e: E) -> Self {
        Self::Multiple(vec![
            (
                Box::<str>::from(input),
                KyomatoLexErrorStackEntry::Nom(kind),
            ),
            (
                Box::<str>::from("EXTERNAL"),
                KyomatoLexErrorStackEntry::External(Equivalent(Box::new(e) as _)),
            ),
        ])
    }
}

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
        + FromExternalError<&'source str, KyomatoLexError>
        + 'kernel,
>(
    mut tokens_kernel: impl FnMut(&'source str) -> IResult<&'source str, (Token<'source>, Option<Token<'source>>), E>
        + 'kernel,
) -> impl Parser<&'source str, Token<'source>, E> + 'kernel {
    enum InnerResult<'source> {
        None, // Will be converted into `KyomatoLexErrorKind::NoTokens`
        Single(Token<'source>),
        Multiple(Vec<Token<'source>>),
    }
    move |outer_input| {
        map_res(
            |input| {
                // This iterator it kinda tricky to operate.
                // Basically, as far as I can see - it won't return error at each step.
                // Instead, error will be silently stored inside it, until it's `finish` function is called to extract it.
                // Basically, this means that each return from this function MUST INVOLVE `tokens.finish()?`!!!
                let mut iterator = iterator(input, &mut tokens_kernel);

                // First, let's try getting first token
                let Some((first_token, first_follower)) = (&mut iterator).next() else {
                    // There is no first token!
                    // Let's give iterator a chance to explain itself:
                    let (rest, _) = iterator.finish()?;
                    // Well, seems like there are no tokens and no error
                    // (most likely input is just empty or th)
                    // Return empty variant
                    return Ok((rest, InnerResult::None));
                };

                // Here's a little tricky part: we'll need to check is there are multiple tokens in the input,
                // but there are two variants for that: follower of the first token (we have now),
                // or just the second token iterator would yield.
                // That said, I find it natural to make this check in one go with `match`.
                // The following block will continue execution if and only there are multiple tokens,
                // and there are (possibly) more tokens to parse
                let t1 = first_token;
                let mut tokens = match (first_follower, (&mut iterator).next()) {
                    // Meaning there are no more tokens (or iterator failed) - return single result
                    (None, None) => {
                        let (rest, _) = iterator.finish()?;
                        return Ok((rest, InnerResult::Single(t1)));
                    }
                    // There are exactly two tokens (or iterator failed) - return exactly two tokens
                    (Some(t2), None) => {
                        let (rest, _) = iterator.finish()?;
                        return Ok((rest, InnerResult::Multiple(vec![t1, t2])));
                    }
                    // There are at least two tokens
                    (None, Some((t2, None))) => {
                        vec![t1, t2]
                    }
                    // There are at least three tokens
                    (None, Some((t2, Some(t3)))) | (Some(t2), Some((t3, None))) => {
                        vec![t1, t2, t3]
                    }
                    // There are at least four tokens
                    (Some(t2), Some((t3, Some(t4)))) => {
                        vec![t1, t2, t3, t4]
                    }
                };

                // Parse rest of the tokens
                for (token, follower) in &mut iterator {
                    tokens.push(token);
                    if let Some(follower) = follower {
                        tokens.push(follower);
                    }
                }
                // Iterator might have failed silently, so let them a chance to tell about it
                let (rest, _) = iterator.finish()?;
                // Return multiple variant
                Ok((rest, InnerResult::Multiple(tokens)))
            },
            // Lastly, map this inner result into a single actual token.
            // Oh, and if there were no tokens - that's an error, so might return it as well.
            |inner_result| match inner_result {
                InnerResult::None => Err(KyomatoLexError::no_tokens(outer_input)),
                InnerResult::Single(token) => Ok(token),
                InnerResult::Multiple(tokens) => Ok(Token::Text {
                    tokens: Tokens::new(tokens),
                }),
            },
        )
        .parse(outer_input)
    }
}

/// Main entry point into the lexer
///
/// Basically, it attempts to parse as many tokens as possible,
/// and returns a single one, representing them all (a `Text` variant if there are more than one token)
pub fn lex<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexError>
        + 'source,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    context(
        "unlimited lex",
        all_consuming(terminated(
            tokens_many1(alt((
                // Allow them all!
                div_page.map(__equalizer),
                header.map(__equalizer),
                equation.map(__equalizer),
                table.map(__equalizer),
                figure.map(__equalizer),
                href.map(__equalizer),
                ayano.map(__equalizer),
                code_block.map(__equalizer),
                list.map(__equalizer),
                formatting.map(__equalizer),
                inline_math.map(__equalizer),
                reference.map(__equalizer),
                footnote_content.map(__equalizer),
                footnote_reference.map(__equalizer),
                paragraph,
            ))),
            multispace0,
        )),
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
        + FromExternalError<&'source str, KyomatoLexError>,
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
            KyomatoLexError::forbidden_token(input, token, reason),
        )))
    }
}

/// Auxiliary entry point into the lexer, used to lex inner fields
///
/// Functions similarly to `lex`, but forbids certain token types
pub fn inner_lex<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexError>
        + 'source,
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
                )
                .map(__equalizer),
                forbid(header, "header", "you are not supposed to do that").map(__equalizer),
                forbid(
                    equation,
                    "display equation",
                    "LaTeX does not support display math inside captions/footnotes",
                )
                .map(__equalizer),
                forbid(table, "table", "table can only be an outer element").map(__equalizer),
                forbid(figure, "figure", "figure can only be an outer element").map(__equalizer),
                forbid(
                    ayano,
                    "ayano",
                    "Ayano on the inner layer is not supported, as of now",
                )
                .map(__equalizer),
                forbid(code_block, "code block", "you are not supposed to do that")
                    .map(__equalizer),
                forbid(
                    list,
                    "list",
                    "LaTeX does not support lists inside captions and footnotes well",
                )
                .map(__equalizer),
                forbid(
                    footnote_content,
                    "footnote content",
                    "All footnote content must be outer-layer definitions",
                )
                .map(__equalizer),
                // Rest are ok to be parsed!
                href.map(__equalizer),
                formatting.map(__equalizer),
                inline_math.map(__equalizer),
                reference.map(__equalizer),
                footnote_reference.map(__equalizer),
                paragraph,
            ))
        }),
    )
    .parse(input)
}

#[cfg(test)]
mod tests {
    use nom::character::complete::alphanumeric1;

    use super::*;

    macro_rules! _assert_inner {
        ($result:expr, $expected:pat, $message:literal) => {};
        ($result:expr, $expected:expr, $message:literal) => {
            assert_eq!($result, $expected, $message)
        };
    }

    macro_rules! test {
        {$name:ident, $parser:expr, $input:literal, e: $expected:expr} => {
            #[test]
            fn $name() {
                // arrange

                // act
                let result: Result<_, nom::Err<KyomatoLexError>>;
                result =  $parser.parse($input);

                // assert
                let expected = $expected;
                if result != expected {
                    panic!("Parser did not produce expected result. Structure comparison:\n\tExpected:{:#?}\n\n\tActual:{:#?}\n\nPlain comparison:\nE:{:?}\nR:{:?}", expected, result, expected, result);
                }
            }
        };
        {$name:ident, $parser:expr, $input:literal, p: $expected:pat} => {
            #[test]
            fn $name() {
                // arrange

                // act
                let result: Result<_, nom::Err<nom::error::VerboseError<&'static str>>>;
                result = $parser.parse($input);

                // assert
                if !matches!(result, $expected) {
                    panic!("{}: {:?}", "Parser did not produce complying result:", result);
                }
            }
        };
    }

    // Tests were written in order they depend on each other

    // `_multispace_line_break` tests
    test! {multispace_line_break1, _multispace_line_break(""), "\n", e: Ok(("", ("\n", "")))}
    test! {multispace_line_break2, _multispace_line_break("1"), "\n", p: Err(_)}
    test! {multispace_line_break3, _multispace_line_break("1"), "    \t\t\n1benzene", e: Ok(("benzene", ("    \t\t\n", "1")))}
    test! {multispace_line_break4, _multispace_line_break("ben"), "    \t\t\nbenzene", e: Ok(("zene", ("    \t\t\n", "ben")))}
    test! {multispace_line_break5, _multispace_line_break("ben"), "l    \t\t\nbenzene", p: Err(_)}

    // `parse_args!` tests
    test! {parse_args1, parse_args!["a" = digit1], "", e: Ok(("", (None,)))}
    test! {parse_args2, parse_args!["a" = digit1], "{}", e: Ok(("{}", (None,)))}
    test! {parse_args3, parse_args!["a" = digit1], "\n{a = 34522}", e: Ok(("", (Some("34522"), )))}
    test! {parse_args3_2, parse_args!["a" = digit1], "{a = 34522}", e: Ok(("{a = 34522}", (None,)))}
    test! {parse_args3_3, parse_args!["a" = digit1], "\n{a = ", p: Err(_)}
    test! {parse_args4, parse_args!["a" = digit1, "b" = alphanumeric1], "\n{b = abc12, a = 34522 }abc", e: Ok(("abc", (Some("34522"), Some("abc12"))))}

    // `div_page` tests
    test! {div_page1, div_page, "", p: Err(_)}
    test! {div_page2, div_page, "---", p: Err(_)}
    test! {div_page3, div_page, "\n------", e: Ok(("", Token::PageDiv))}
    test! {div_page3_2, div_page, "------", p: Err(_)}
    test! {div_page4, div_page, "\n   ------", p: Err(_)}
    test! {div_page5, div_page, "    \t\n------\t\n", e: Ok(("\t\n", Token::PageDiv))}

    macro_rules! head {
        [# $header:literal] => {
            head![1, $header]
        };
        [## $header:literal] => {
            head![2, $header]
        };
        [### $header:literal] => {
            head![3, $header]
        };
        [#### $header:literal] => {
            head![4, $header]
        };
        [##### $header:literal] => {
            head![5, $header]
        };
        [###### $header:literal] => {
            head![6, $header]
        };
        [$order:literal, $header:literal] => {
            Token::Header {order: $order, content: Cow::Borrowed($header)}
        };
    }
    mod header {
        use super::*;
        macro_rules! test_ok {
            {$name:ident, $input:literal, $output:expr, $left_over:literal} => {
                test!{$name, header, $input, e: Ok(($left_over, $output))}
            };
        }
        macro_rules! test_err {
            {$name:ident, $input:literal} => {
                test!{$name, header, $input, p: Err(_)}
            };
        }

        test_err! {err_empty, ""}
        test_ok! {ok_merged, "\n#Ababa\n", head!(#"Ababa"), "\n"}
        test_ok! {ok_spread, "\n#       Ababa    \t  \t\n", head!(#"Ababa"), "\n"}
        test_ok! {ok_spread_3,  "\n###  Ababa  \n", head!(###"Ababa"), "\n"}
        test_ok! {ok_cyrillic_6, "\n######  Заголовок українською \n", head!(######"Заголовок українською"), "\n"}
        test_err! {err_header_7, "\n#######  Заголовок українською \n"}
    }

    // `equation` tests
    test! {equation1, equation, "", p: Err(_)}
    test! {equation2, equation, "$$", p: Err(_)}
    test! {equation3, equation, "$$$$", p: Err(_)} // NOTE: this thing is technically allowed in markdown, but you probably shouldn't do it
    test! {equation4, equation, r"
    $$
    y = x^2
    $$
    ", p: Err(_)} // NOTE: same here. Please start your equations at the start of the line
    test! {equation5, equation, r"
$$
y = x^2
$$
12344", e: Ok(("\n12344", Token::Equation { content: Cow::Borrowed("y = x^2"), ident: None }))}
    // Like that!
    test! {equation6, equation, r"
$$y = x^2
z = y;
$$
12344", p: Err(_)} // Don't do this and the following, too
    test! {equation7, equation, r"
$$
y = x^2
z = y$$;
12344", p: Err(_)}
    test! {equation8, equation, r"
$$
y = x^2
z = y
$$;12344", e: Ok((";12344", Token::Equation { content: "y = x^2\nz = y".into(), ident: None }))}
    // This is alright, though
    test! {equation9, equation, r#"
$$
y = x^2
$$
{ref = parabola}"#, e: Ok(("", Token::Equation { content: "y = x^2".into(), ident: Some("parabola".into()) }))}
    test! {equation9_5, equation, r#"
$$
y = x^2
$$
{ref = parabola_ref}"#, e: Ok(("", Token::Equation { content: "y = x^2".into(), ident: Some("parabola_ref".into()) }))}
    // Equation ident example
    test! {equation10, equation, r#"
$$
y = x^2
$$
{ref = parabola_1}"#, e: Ok(("", Token::Equation { content: "y = x^2".into(), ident: Some("parabola_1".into()) }))}
    // This is not ok
    // TODO although that's subject to change
    test! {equation11, equation, r#"
    $$
    y = x^2
    $$
    {tel = +4204206969}"#, p: Err(_)}
    // Don't put random garbage there!
    test! {equation12, equation, r#"
$$
y = x^2   
$$
{ref=11    } якийсь текст тому що чому ні"#, e: Ok((" якийсь текст тому що чому ні", Token::Equation { content: "y = x^2   ".into(), ident: Some("11".into()) }))}
    // Any sort of whitespaces are preserved
    test! {equation13, equation, r#"
$$
y = x^2   
$$
{} якийсь текст тому що чому ні"#, e: Ok((" якийсь текст тому що чому ні", Token::Equation { content: "y = x^2   ".into(), ident: None }))}
    // You can explicitly provide no args - fun fact :idk:

    // `href` tests
    mod href {
        use super::*;

        macro_rules! href_ok {
        {$name:ident, $input:expr, $output_url:expr, $output_display:expr, $left_over:literal} => {
            test! {$name, href, $input, e: Ok(($left_over, Token::Href { url: $output_url.parse().expect("This is a valid url"), display: $output_display.into() }))}
        };
    }
        test! {href1, href, "[link text]()", p: Err(_)} // empty url is not ok
        test! {href2, href, "[](homepage.com)", p: Err(_)} // empty display is not ok too
        test! {href3, href, "[невалідне посилання](http://ref    ?????)", p: Err(_)} // random garbage instead of url is not accepted
        href_ok! {href4, "[link text](ftp://somewebsite/files/01234)", "ftp://somewebsite/files/01234", "link text", ""} // url example
        href_ok! {href5, "[текст посилання](ftp://somewebsite/files/01234)", "ftp://somewebsite/files/01234", "текст посилання", ""} // cyrillic test
        href_ok! {href6, "[link text](ftp://somewebsite/files/01234), ще текст після нього", "ftp://somewebsite/files/01234", "link text", ", ще текст після нього"}
        // text after url is alright
        href_ok! {href7, "       [link text](ftp://somewebsite/files/01234), ще текст після нього", "ftp://somewebsite/files/01234", "link text", ", ще текст після нього"}
        // space before url is ignored
        href_ok! {href8, "[    link text](ftp://somewebsite/files/01234).", "ftp://somewebsite/files/01234", "link text", "."}
        // display text will be trimmed
    }

    // `code_block` tests
    mod code_block {
        use super::*;
        macro_rules! test_ok {
            {$name:ident, $input:expr, $output_lang:expr, $output_code:expr, $left_over:expr} => {
                test!{$name, code_block, $input, e: Ok(($left_over, Token::CodeBlock {code: $output_code.into(), language: $output_lang.map(Cow::from)} ))}
            };
        }

        macro_rules! test_err {
            {$name:ident, $input:literal} => {
                test!{$name, code_block, $input, p: Err(_)}
            };
        }

        test_ok! {ok_empty, "   \n```\n\n```\n", None::<&'static str>, "", "\n"} // a single empty line is ok
        test_err! {err_no_line, "    \n```\n```\n"} // no lines in the block at all - that's bad
        test_ok! {ok_lang, "    \n```C sus sus\n\t senpai, you are sus!\n```", Some("C sus sus"), "\t senpai, you are sus!", ""}
        // you may specify whatever language you want
        test_ok! {ok_cyrillic, "    \n```C sus sus\n\t тиск = сила / площа;\n// (і мотивація то є сильна)\n```",
        Some("C sus sus"), "\t тиск = сила / площа;\n// (і мотивація то є сильна)", ""}
        // you may write whatever you want inside
        test_ok! {ok_text_after, "    \n```C sus sus\n\t тиск = сила / площа;\n// (і мотивація то є сильна)\n``` It's dangerous - being alone",
        Some("C sus sus"), "\t тиск = сила / площа;\n// (і мотивація то є сильна)", " It's dangerous - being alone"}
        // text just after the code block is ok too
    }

    // `inline math` tests
    mod inline_math {
        use super::*;

        macro_rules! test_ok {
            {$name:ident, $input:literal, $formula:literal, $left_over:literal} => {
                test!{$name, inline_math, $input, e: Ok(($left_over, Token::InlineMathmode{content:$formula.into()}))}
            };
        }

        test! {err_empty, inline_math, "$$", p: Err(_)}
        // completely empty inline math is not ok (as it's ambiguous with display math)
        test_ok! {ok_space, "$ $", "", ""} // empty space is ok though (but it will be stripped)
        test_ok! {ok_char, "$     x     $", "x", ""}
        // you can specify a single character, but empty spaces would be tripped
        test! {err_unclosed, inline_math, "$ x", p: Err(_)}
        // unclosed mathmode is obviously disallowed
        test_ok! {ok_cyrillic, "$  текст українською $", "текст українською", ""}
        // you can put there whatever you want, however pay attention to latex's rules
        test_ok! {ok_spaces, "\t   \n\t  \n    \t $   x $", "x", ""}
        // got some spaces before? don't care
        test_ok! {ok_text_after, "$   x$some text after it, українською в тому числі",
        "x", "some text after it, українською в тому числі"}
        // text after is is ok
    }

    // tests for `reference`
    mod reference {
        use super::*;

        macro_rules! test_ok {
            {$name:ident, $input:literal, $reference:literal, $left_over:literal} => {
                test!{$name, reference, $input, e: Ok(($left_over, Token::Reference{ident: $reference.into()}))}
            };
        }

        test! {err_empty, reference, "[@      ]", p: Err(_)} // empty reference is not ok
        test_ok! {ok_char, "[@ s]", "s", ""} // single char as identifier is ok
        test_ok! {ok_eq, "[@ eq:parabola]", "eq:parabola", ""} // equation ref example
        test_ok! {ok_fig, "[@ fig:my_best_drawing   ]", "fig:my_best_drawing", ""} // figure ref example
        test_ok! {ok_tab, "[@ tab:boring_stat     ]", "tab:boring_stat", ""} // table ref example
        test_ok! {ok_space_before, "    \n \t  [@ ref]", "ref", ""}
        test_ok! {ok_text_after, "    \n \t  [@ ref] кирилиця or th", "ref", " кирилиця or th"}
    }

    // tests for `footnote reference`
    mod footnote_reference {
        use super::*;

        macro_rules! test_ok {
            {$name:ident, $input:literal, $reference:literal, $left_over:literal} => {
                test!{$name, footnote_reference, $input, e: Ok(($left_over, Token::FootNoteReference{ident:$reference.into()}))}
            };
        }

        test! {err_empty, reference, "[^      ]", p: Err(_)} // empty reference is not ok
        test_ok! {ok_char, "[^s]", "s", ""} // single char as identifier is ok
        test_ok! {ok_space_before, "    \n \t  [^explanation_of_42]", "explanation_of_42", ""}
        test_ok! {ok_text_after, "    \n \t  [^y_u_do_dis_2_me] кирилиця or th", "y_u_do_dis_2_me", " кирилиця or th"}
    }

    // tests for `paragraph`
    mod paragraph {
        use super::*;

        macro_rules! test_ok {
            {$name:ident, $input:literal, $paragraph:literal, $follower_token:expr, $left_over:literal} => {
                test!{$name, paragraph, $input, e: Ok(($left_over, (Token::Paragraph{
                    is_newline: false,
                    formatting: None,
                    content: $paragraph.into(),
                    }, $follower_token)))}
            };
            {$name:ident, $input:literal, $paragraph:literal, $follower_token:expr, $left_over:literal, |} => {
                test!{$name, paragraph, $input, e: Ok(($left_over, (Token::Paragraph{
                    is_newline: true,
                    formatting: None,
                    content: $paragraph.into(),
                    }, $follower_token)))}
            };
        }

        test! {err_empty, paragraph, "", p: Err(_)} // paragraph won't be parsed where it's empty
        test! {err_whitespace, paragraph, "   \t   \n\n \t", p: Err(_)} // nor if it's all whitespace
        test_ok! {ok_char, "  \t\n c\nfff", "c", None, "\nfff", |} // single char is ok as a paragraph
        test_ok! {ok_sentence, "\t    \n I'm absolutely increative, so here's a bunch of garbage: ]42[t5(2[24tj4-0)jt3_94j03\nand some rest",
        "I'm absolutely increative, so here's a bunch of garbage: ]42[t5(2[24tj4-0)jt3_94j03", None, "\nand some rest", |}
        test_ok! {ok_formatting, "\t    \n You can use *bold*!", "You can use", Some(Token::Paragraph{is_newline: false, formatting: Some(Formatting::Italic), content: "bold".into()}), "!", |}
        // FIXME NOT SUPPORTED
        /* test_ok! {ok_formatting_in_formatting, "\t    \n You can even use ~~__double-formatted__~~ things!", "You can even use",
        Some(Token::Formatted(Formatting::StrikeThrough, Box::new(Token::Formatted(Formatting::Bold, Box::new(Token::text("double-formatted")))))), " things!"} */
        test_ok! {ok_equation, "    \t Inline math is fine too: $y = x_{\text{поч}} + x_0$.", "Inline math is fine too:",
        Some(Token::InlineMathmode{content: "y = x_{\text{поч}} + x_0".into()}), "."}
        test_ok! {ok_href, "   \t\t\n You can even [залишити посилання на свою сторінку на Гітхабі   ](https://www.github.com/Dzuchun)!", "You can even",
        Some(Token::Href { url: "https://www.github.com/Dzuchun".parse().expect("That's a valid url"), display: "залишити посилання на свою сторінку на Гітхабі".into() }), "!", |}
        test_ok! {ok_footnote_ref, "\t Footnote refs[^1] are also ok too!", "Footnote refs", Some(Token::FootNoteReference{ident: "1".into()}), " are also ok too!"}
        test_ok! {ok_obj_ref, "\n\t Thi[[ngs lik_e tables* ([@tab:results]) can also be referenced", "Thi[[ngs lik_e tables* (",
        Some(Token::Reference{ident: "tab:results".into()}), ") can also be referenced", |}
    }

    // `inner_lex` tests
    // this is one of open interfaces, so extensive testing is required here
    // to achieve that, here are some macro to simplify token creation:
    macro_rules! div {
        () => {
            Token::PageDiv
        };
    }
    macro_rules! eq {
        ($eq:literal) => {
            eq!($eq, None)
        };
        ($eq:literal, ref = $ident:literal) => {
            eq!($eq, Some(Cow::Borrowed($ident)))
        };
        ($eq:literal, $ident:expr) => {
            Token::Equation {
                content: Cow::Borrowed($eq),
                ident: $ident,
            }
        };
        (!$eq:literal) => {
            Token::InlineMathmode {
                content: Cow::Borrowed($eq),
            }
        };
    }
    macro_rules! hr {
        ([$display:literal]($url:literal)) => {
            Token::Href {
                url: Url::from_str($url).expect("Should be a valid url"),
                display: Cow::Borrowed($display),
            }
        };
    }
    macro_rules! code {
        ($code:literal, lang = $lang:literal) => {
            code!($code, Some(Cow::Borrowed($lang)))
        };
        ($code:literal) => {
            code!($code, None)
        };
        ($code:literal, $lang:expr) => {
            Token::CodeBlock {
                code: Cow::Borrowed($code),
                language: $lang,
            }
        };
    }
    macro_rules! rf {
        (@ $ident:literal) => {
            Token::Reference {
                ident: Cow::Borrowed($ident),
            }
        };
        (^ $ident:literal) => {
            Token::FootNoteReference {
                ident: Cow::Borrowed($ident),
            }
        };
    }
    macro_rules! tx {
        ($text:literal) => {
            tx!($text, None)
        };
        ($text:literal, |) => {
            tx!($text, None, |)
        };
        ($text:literal, $formatting:expr) => {
            tx!($text, $formatting, false)
        };
        ($text:literal, $formatting:expr, |) => {
            tx!($text, $formatting, true)
        };
        ($text:literal, $formatting:expr, $newline:literal) => {
            Token::Paragraph {
                is_newline: $newline,
                formatting: $formatting,
                content: Cow::Borrowed($text),
            }
        };
    }
    macro_rules! tks {
        [$($token:expr), +] => {
            Token::Text{tokens: Tokens::new([$($token), +])}
        };
    }
    mod inner_lex {
        use super::*;

        macro_rules! test_ok {
            ($name:ident, $input:literal, $expected:expr) => {
                test! {$name, inner_lex, $input, e: Ok(("", $expected))}
            };
        }

        macro_rules! test_err {
            ($name:ident, $input:literal) => {
                test! {$name, inner_lex, $input, p: Err(_)}
            };
        }

        test_err! {err_empty, ""}
        test_ok! {ok_char_para, "a", tx!("a")}
        test_ok! {ok_formatted, "*виділений текст * і ще щось потім",
        tks![tx!("виділений текст", Some(Formatting::Italic)), tx!("і ще щось потім")]}
        test_ok! {ok_formatted_newlined, "\n*виділений текст * і ще щось потім",
        tks![tx!("виділений текст", Some(Formatting::Italic), |), tx!("і ще щось потім")]}
        test_ok! {ok_inline_math, "дивіться! ось формула для параболи: $y = x^2$",
        tks![tx!("дивіться! ось формула для параболи:"), eq!(! "y = x^2")]}
        test_err! {err_display_math, "якась страшна штука:\n$$\nf(x) = \\int\\lms_{0}^{x} \\log(x - 2) dx\n$$\n{ref = scary}\n\tКраще не чіпати її"}
        // display math is not allowed in the inner_lex
        test_ok! {ok_href, "посилання на мою [Github сторінку](https://www.example.com) - ха-ха, жартую;\tОсь вам рівняння: $x^4 + 5x^2 - 100 = 0$.",
        tks![tx!("посилання на мою"), hr!(["Github сторінку"]("https://www.example.com")), tx!("- ха-ха, жартую;\tОсь вам рівняння:"), eq!(!"x^4 + 5x^2 - 100 = 0"), tx!(".")]}
        test_ok! {ok_ref, "розрахунки наведено у таблиці ([@tab:calc])[^3].",
        tks![tx!("розрахунки наведено у таблиці ("), rf!(@"tab:calc"), tx!(")"), rf!(^"3"), tx!(".")]}
    }

    macro_rules! tab {
        ($($header:expr), +; $($($cell:expr), +;), +) => {
            tab!($($header), +; $($($cell), +;), +; {ref = None::<&'_ str>, caption = None})
        };
        ($($header:expr), +; $($($cell:expr), +;), +; {ref = $ident:expr, caption = $caption:expr}) => {
            Token::Table {header: Tokens::new([$($header), +]), cells: Tokens::new([$($($cell), +), +]), ident: $ident.map(Cow::from), caption: $caption.map(Box::new)}
        };
    }
    mod table {
        use super::*;
        macro_rules! test_ok {
            {$name:ident, $input:literal, $output:expr, $left_over:literal} => {
                test!{$name, table, $input, e: Ok(($left_over, $output))}
            };
        }

        macro_rules! test_err {
            {$name:ident, $input:literal} => {
                test!{$name, table, $input, p: Err(_)}
            };
        }

        test_err! {err_empty, ""}
        test_err! {err_no_rows, "|header|\n"}
        test_ok! {ok_one_row, "\n\t\n|header|\n|:---:|\n|cell|\n", tab!(tx!("header"); tx!("cell");), "\n"}
        test_ok! {ok_more_rows, "   \n|header1 |     header_2 |\n|:---:|:---:|\n|cell 11|cell[^12]|\n|$cell_{21}$|cell[@tab:22]|\n",
        tab!(tx!("header1"), tx!("header_2");
            tx!("cell 11"), tks![tx!("cell"), rf!(^ "12")];,
            eq!(!"cell_{21}"), tks![tx!("cell"), rf!(@"tab:22")];), "\n"}
        test_ok! {ok_empty_args, "   \n|header1 |     header_2 |\n|:---:|:---:|\n|cell 11|cell[^12]|\n|$cell_{21}$|cell[@tab:22]|\n{}",
        tab!(tx!("header1"), tx!("header_2");
            tx!("cell 11"), tks![tx!("cell"), rf!(^ "12")];,
            eq!(!"cell_{21}"), tks![tx!("cell"), rf!(@"tab:22")];), ""}
        test_ok! {ok_ident, "   \n|header1 |     header_2 |\n|:---:|:---:|\n|cell 11|cell[^12]|\n|$cell_{21}$|cell[@tab:22]|\n{  ref = example}",
        tab!(tx!("header1"), tx!("header_2");
            tx!("cell 11"), tks![tx!("cell"), rf!(^ "12")];,
            eq!(!"cell_{21}"), tks![tx!("cell"), rf!(@"tab:22")];;
            {ref = Some("example"), caption = None}), ""}
        test_ok! {ok_caption, "   \n|header1 |     header_2 |\n|:---:|:---:|\n|cell 11|cell[^12]|\n|$cell_{21}$|cell[@tab:22]|\n{caption = \"This table can be seen as an \\\"example\\\"\"}",
        tab!(tx!("header1"), tx!("header_2");
            tx!("cell 11"), tks![tx!("cell"), rf!(^ "12")];,
            eq!(!"cell_{21}"), tks![tx!("cell"), rf!(@"tab:22")];;
            {ref = None::<&str>, caption = Some(tx!("This table can be seen as an \\\"example\\\""))}), ""}
        test_ok! {ok_both, "   \n|header1 |     header_2 |\n|:---:|:---:|\n|cell 11|cell[^12]|\n|$cell_{21}$|cell[@tab:22]|\n{ref = example_table, caption = \"This table can be seen as an \\\"example\\\"\"}",
        tab!(tx!("header1"), tx!("header_2");
            tx!("cell 11"), tks![tx!("cell"), rf!(^ "12")];,
            eq!(!"cell_{21}"), tks![tx!("cell"), rf!(@"tab:22")];;
            {ref = Some("example_table"), caption = Some(tx!("This table can be seen as an \\\"example\\\""))}), ""}
        test_ok! {ok_both_reversed, "   \n|header1 |     header_2 |\n|:---:|:---:|\n|cell 11|cell[^12]|\n|$cell_{21}$|cell[@tab:22]|\n{caption = \"This table can be seen as an \\\"example\\\"\", ref = example_table    }",
        tab!(tx!("header1"), tx!("header_2");
            tx!("cell 11"), tks![tx!("cell"), rf!(^ "12")];,
            eq!(!"cell_{21}"), tks![tx!("cell"), rf!(@"tab:22")];;
            {ref = Some("example_table"), caption = Some(tx!("This table can be seen as an \\\"example\\\""))}), ""}
    }

    macro_rules! fig {
        ($path:literal) => {
            fig!($path, {ref = None::<&'static str>, caption = None, width = None})
        };
        ($path:literal, {ref = $ident:expr, caption = $caption:expr, width = $width:expr}) => {
            Token::Figure {
                src_name: Cow::Borrowed(Path::new($path)),
                ident: $ident.map(Cow::from),
                caption: $caption.map(Box::new),
                width: $width,
            }
        };
    }
    mod figure {
        use super::*;

        macro_rules! test_ok {
            {$name:ident, $input:literal, $output:expr, $left_over:literal} => {
                test!{$name, figure, $input, e: Ok(($left_over, $output))}
            };
        }

        macro_rules! test_err {
            {$name:ident, $input:literal} => {
                test!{$name, figure, $input, p: Err(_)}
            };
        }

        test_err! {err_empty, ""}
        test_err! {err_unclosed, "\n![[path/to/file]"}
        // test_err! {err_bad_path, "\n![[/path/to////////]]"} // not sure about that?
        test_ok! {ok, "\n![[path/to/file.jpg]]", fig!("path/to/file.jpg"), ""}
        test_ok! {ok_empty_before, "\t\t\t\n   \n\n![[path/to/file.jpg]]", fig!("path/to/file.jpg"), ""}
        test_ok! {ok_text_after, "\t\t\t\n   \n\n![[path/to/file.jpg]] і ще щось написане", fig!("path/to/file.jpg"), " і ще щось написане"}
        test_ok! {ok_explicit_no_params, "\n![[path/to/file.jpg]]\n{}\n", fig!("path/to/file.jpg"), "\n"}
        test_ok! {ok_ident, "\n![[path/to/file.jpg]]\n{ref = figure_of_doom    }\n", fig!("path/to/file.jpg", {ref = Some("figure_of_doom"), caption = None, width = None}), "\n"}
        test_ok! {ok_caption, "\n![[path/to/file.jpg]]\n{caption = \"Ця картинка показує графік рівняння $y = x^2$\"    }\n",
        fig!("path/to/file.jpg", {ref = None::<&str>, caption = Some(tks![tx!("Ця картинка показує графік рівняння"), eq!(!"y = x^2")]), width = None}), "\n"}
        test_ok! {ok_both, "\n![[path/to/file.jpg]]\n{   ref = mc_erat, caption = \"This is a mee from our $\\text{Nomifactory}^{2*}$ world.\"    }\n",
        fig!("path/to/file.jpg", {ref = Some("mc_erat"), caption = Some(tks!(tx!("This is a mee from our"), eq!(!"\\text{Nomifactory}^{2*}"), tx!("world."))), width = None}), "\n"}
        test_ok! {ok_both_reversed, "\n![[path/to/file.jpg]]\n{   caption = \"This is a meme from our $\\text{Nomifactory}^{2*}$ world.\" ,    ref = mc_erat    }\n",
        fig!("path/to/file.jpg", {ref = Some("mc_erat"), caption = Some(tks!(tx!("This is a meme from our"), eq!(!"\\text{Nomifactory}^{2*}"), tx!("world."))), width = None}), "\n"}
        test_ok! {ok_both_left_over, "\n![[path/to/file.jpg]]\n{   caption = \"This is a meme from our $\\text{Nomifactory}^{2*}$ world.\" ,    ref = mc_erat    }\nІще трохи тексту",
        fig!("path/to/file.jpg", {ref = Some("mc_erat"), caption = Some(tks!(tx!("This is a meme from our"), eq!(!"\\text{Nomifactory}^{2*}"), tx!("world."))), width = None}), "\nІще трохи тексту"}
        test_ok! {ok_width, "\n![[path/to/file.jpg]]\n{    width = 0.5}\nTh else", fig!("path/to/file.jpg", {ref = None::<&str>, caption = None, width = Some(0.5)}), "\nTh else"}
    }

    macro_rules! ls {
        [1 $($item:expr), +] => {
            ls![ListType::Num; $($item), +]
        };
        [a $($item:expr), +] => {
            ls![ListType::Latin; $($item), +]
        };
        [а $($item:expr), +] => {
            ls![ListType::Cyrillic; $($item), +]
        };
        [- $($item:expr), +] => {
            ls![ListType::Bullet; $($item), +]
        };
        [$list_type:expr; $($item:expr), +] => {
            Token::List {list_type: $list_type, content: Tokens::new([$($item), +])}
        };
    }
    mod list {
        use super::*;
        macro_rules! test_ok {
            {$name:ident, $input:literal, $output:expr, $left_over:expr} => {
                test!{$name, list, $input, e: Ok(($left_over, $output))}
            };
        }

        macro_rules! test_err {
            {$name:ident, $input:literal} => {
                test!{$name, list, $input, p: Err(_)}
            };
        }

        test_err! {err_empty, ""}
        test_ok! {ok_single, "\n1. Item 1\n", ls![1 tx!("Item 1")], "\n"}
        test_ok! {ok_multiple, "\n1. Item 1 \n2. Gamma-function def: $\\Gamma(\\alpha) = \\int\\limits_{0}^{+\\infinity} t^{\\alpha - 1} e^{-t} dt$.\n3. Here's some table ref: ([@tab:function    ])   \n4. Explained in [^source2]\n",
        ls![1 tx!("Item 1"),
            tks![tx!("Gamma-function def:"), eq!(!"\\Gamma(\\alpha) = \\int\\limits_{0}^{+\\infinity} t^{\\alpha - 1} e^{-t} dt"), tx!(".")],
            tks![tx!("Here's some table ref: ("), rf!(@"tab:function"), tx!(")")],
            tks![tx!("Explained in"), rf!(^"source2")]
            ], "\n"}
        test_ok! {ok_latin, "\na. Item 1 \nb. Gamma-function def: $\\Gamma(\\alpha) = \\int\\limits_{0}^{+\\infinity} t^{\\alpha - 1} e^{-t} dt$.\nc. Here's some table ref: ([@tab:function    ])   \nd. Explained in [^source2]\n",
        ls![a tx!("Item 1"),
            tks![tx!("Gamma-function def:"), eq!(!"\\Gamma(\\alpha) = \\int\\limits_{0}^{+\\infinity} t^{\\alpha - 1} e^{-t} dt"), tx!(".")],
            tks![tx!("Here's some table ref: ("), rf!(@"tab:function"), tx!(")")],
            tks![tx!("Explained in"), rf!(^"source2")]
            ], "\n"}
        test_ok! {ok_cyrillic, "\nа. Item 1 \nб. Gamma-function def: $\\Gamma(\\alpha) = \\int\\limits_{0}^{+\\infinity} t^{\\alpha - 1} e^{-t} dt$.\nв. Here's some table ref: ([@tab:function    ])   \nг. Explained in [^source2]\n",
        ls![а tx!("Item 1"),
            tks![tx!("Gamma-function def:"), eq!(!"\\Gamma(\\alpha) = \\int\\limits_{0}^{+\\infinity} t^{\\alpha - 1} e^{-t} dt"), tx!(".")],
            tks![tx!("Here's some table ref: ("), rf!(@"tab:function"), tx!(")")],
            tks![tx!("Explained in"), rf!(^"source2")]
            ], "\n"}
        test_ok! {ok_bullet, "\n- Item 1 \n- Gamma-function def: $\\Gamma(\\alpha) = \\int\\limits_{0}^{+\\infinity} t^{\\alpha - 1} e^{-t} dt$.\n- Here's some table ref: ([@tab:function    ])   \n- Explained in [^source2]\n",
        ls![- tx!("Item 1"),
            tks![tx!("Gamma-function def:"), eq!(!"\\Gamma(\\alpha) = \\int\\limits_{0}^{+\\infinity} t^{\\alpha - 1} e^{-t} dt"), tx!(".")],
            tks![tx!("Here's some table ref: ("), rf!(@"tab:function"), tx!(")")],
            tks![tx!("Explained in"), rf!(^"source2")]
            ], "\n"}
        test_ok! {ok_left_over, "\n- Item 1 \n- Gamma-function def: $\\Gamma(\\alpha) = \\int\\limits_{0}^{+\\infinity} t^{\\alpha - 1} e^{-t} dt$.\n- Here's some table ref: ([@tab:function    ])   \n- Explained in [^source2]\nІще трохи тексту після списку",
        ls![- tx!("Item 1"),
            tks![tx!("Gamma-function def:"), eq!(!"\\Gamma(\\alpha) = \\int\\limits_{0}^{+\\infinity} t^{\\alpha - 1} e^{-t} dt"), tx!(".")],
            tks![tx!("Here's some table ref: ("), rf!(@"tab:function"), tx!(")")],
            tks![tx!("Explained in"), rf!(^"source2")]
            ], "\nІще трохи тексту після списку"}
    }

    macro_rules! foot {
        (^$ident:literal: $content:expr) => {
            Token::FootNoteContent {
                ident: Cow::Borrowed($ident),
                content: Box::new($content),
            }
        };
    }
    mod footnote_content {
        use super::*;

        macro_rules! test_ok {
            {$name:ident, $input:literal, $output:expr, $left_over:literal} => {
                test!{$name, footnote_content, $input, e: Ok(($left_over, $output))}
            };
        }

        macro_rules! test_err {
            {$name:ident, $input:literal} => {
                test!{$name, footnote_content, $input, p: Err(_)}
            };
        }

        test_err! {err_empty, ""}
        test_err! {err_no_newline, "[^1]: Content!"}
        test_err! {err_unclosed, "\n[^1: Content!"}
        test_err! {err_no_ident, "\n[^]: Content!"}
        test_err! {err_no_content, "\n[^]: \n"}
        test_ok! {ok_text, "\n[^1]: Content!", foot!(^"1": tx!("Content!")), ""}
        test_ok! {ok_complex_ident, "\n[^    source_2_3]: Here's some more explanation", foot!(^"source_2_3": tx!("Here's some more explanation")), ""}

        test_ok! {ok_idk1, "\n\n[^1]: Для поливу.", foot!(^"1": tx!("Для поливу.")), ""}
    }

    macro_rules! ayano {
        {!$is_static:literal, *$description:expr, ~$insert_path:expr, #$_id:expr, $code:literal} => {
            Token::Ayano{data: AyanoBlock{
                is_display: $description.is_some(),
                is_static: $is_static,
                code: Cow::Borrowed($code),
                insert_path: $insert_path.map(Path::new).map(Cow::from),
        }}
        };
    }
    mod ayano {
        use super::*;

        macro_rules! test_ok {
            {$name:ident, $input:literal, $output:expr, $left_over:literal} => {
                test!{$name, ayano, $input, e: Ok(($left_over, $output))}
            };
        }

        macro_rules! test_err {
            {$name:ident, $input:literal} => {
                test!{$name, ayano, $input, p: Err(_)}
            };
        }

        test_err! {err_empty, ""}
        test_ok! {ok_simple, "\n```python, Ayano * \"Code description\" ~ path/to/file # 1\nx = 1\ny = 2\nprint(x + y)\n```\n",
        ayano!(!false, *Some(tx!("Code description")), ~Some("path/to/file"), #Some(1), "x = 1\ny = 2\nprint(x + y)"), "\n"}
        test_ok! {ok_some, "\n```python, Ayano ! ~ path/to/file # 1\nx = 1\ny = 2\nprint(x + y)\n```\n",
        ayano!(!true, *None::<&str>, ~Some("path/to/file"), #Some(1), "x = 1\ny = 2\nprint(x + y)"), "\n"}
        test_ok! {ok_no_args, "\n```python, Ayano  \nx = 1\ny = 2\nprint(x + y)\n```\n",
        ayano!(!false, *None::<&str>, ~None::<&str>, #None, "x = 1\ny = 2\nprint(x + y)"), "\n"}
    }
    mod lex {
        use super::*;
        macro_rules! test_ok {
            {$name:ident, $input:literal, $output:expr} => {
                test!{$name, lex, $input, e: Ok(("", $output))}
            };
        }

        macro_rules! test_err {
            {$name:ident, $input:literal} => {
                test!{$name, lex, $input, p: Err(_)}
            };
        }

        test_err! {err_empty, ""}
        test_ok! {ok_some_text, "\t\t\nSome text\n", tx!("Some text", |)}
        test_ok! {ok_multiple_test, "\t\t\nSome text\nІ ще отакий текст", tks![tx!("Some text", |), tx!("І ще отакий текст", |)]}
        test_ok! {ok_multiple_test2, "\t\t\n# Header\n## header2\n\n\tЦе початок тексту.\n- А це перший елемент списку\n- І другий елемент списку\nА оце буде рівняння: \n$$\ny = x ^ 2\n$$\n",
        tks![head!(# "Header"), head!(## "header2"), tx!("Це початок тексту.", |), ls!(- tx!("А це перший елемент списку"), tx!("І другий елемент списку")), tx!("А оце буде рівняння:", |), eq!("y = x ^ 2")]}

        test_ok! {ok_idk1, "На світлині [@ fig:refurb] можна бачити принципову схему влаштування рефурбалідзера. Неважно помитити великі очі[^1], і два термоядерних реактори, що оточують їх. Точні реакції всередині невідомі, але цілком можливо що там женуть $du + t -> {}^{5}_{6}C$.\n\n[^1]: Для поливу.",
        tks![tx!("На світлині"), rf!(@ "fig:refurb"),
        tx!("можна бачити принципову схему влаштування рефурбалідзера. Неважно помитити великі очі"), rf!(^"1"),
        tx!(", і два термоядерних реактори, що оточують їх. Точні реакції всередині невідомі, але цілком можливо що там женуть"), eq!(!"du + t -> {}^{5}_{6}C"),
        tx!("."), foot!(^"1": tx!("Для поливу."))]}
        test_ok! {ok_idk2, "\n\n## Підпункт\n\n", head!(##"Підпункт")}
    }
}

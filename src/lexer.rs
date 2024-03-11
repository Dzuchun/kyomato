use std::{borrow::Cow, num::ParseIntError, path::Path, str::FromStr, sync::RwLock};

use nom::{
    branch::{alt, permutation},
    bytes::{
        complete::{escaped, tag, take_till1, take_until, take_until1, take_while},
        streaming::tag_no_case,
    },
    character::{
        complete::{alphanumeric1, char, digit1, multispace0, newline, space0},
        is_space,
    },
    combinator::{all_consuming, cut, eof, iterator, map_res, not, opt, recognize, success},
    error::{context, ContextError, ErrorKind, FromExternalError, ParseError},
    multi::{many1, many1_count, separated_list1},
    number::complete::float,
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult, Parser,
};
use url::Url;

use crate::{
    data::{AyanoBlock, Font, Formatting, ListType, Token, Tokens},
    util::IteratorArrayCollectExt,
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
/// TODO FIXME add a mention about caption actually TERMINATING after first unescaped double-quote (and there is no cure),
/// and where exactly escaping backslashes are removed
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
#[inline]
fn div_page<'source, E: ParseError<&'source str> + ContextError<&'source str>>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    context("page divider", _multispace_line_break("------"))
        .map(|_| Token::PageDiv)
        .parse(input)
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
/// - Possible "ref" argument (for referring to this equation) is matched afterwards
fn equation<
    'source,
    E: ParseError<&'source str>
        + ContextError<&'source str>
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, (content, _, (ident,))) = context(
        "display mathmode",
        preceded(
            _multispace_line_break("$$"),
            // if mathmode opening found, lock into equation match
            cut(tuple((
                take_until1("\n$$"),
                tag("\n$$"),
                parse_args!["ref" = alphanumeric1],
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
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, (_, headers, (_, _, cells, (ident, caption)))) = context(
        "table",
        tuple((
            // first, find the start
            _multispace_line_break("|"),
            terminated(separated_list1(char('|'), inner_lex), char('|')),
            // at this point this is guaranteed to be a table token, so we lock in
            cut(tuple((
                // first, match until the end of this line
                pair(take_until("\n"), char('\n')),
                // then, match format line (it's discarded right now)
                pair(take_until("\n"), char('\n')),
                // from here, match table cells
                // while this is possible to do by just per-line collection of them into a vectors
                // (and merge them afterwards),
                // I can't stand unnecessary heap allocations, so here is my solution:
                separated_list1(
                    preceded(preceded(space0, char('|')), opt(pair(space0, tag("\n|")))),
                    inner_lex,
                ),
                // finally, here come the arguments
                parse_args!["ref" = alphanumeric1, "caption" = caption_kernel],
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
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    // TODO add width parameter to figure, I guess
    let (rest, (_, (path, _, (ident, caption, _width)))) = context(
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
                    "ref" = alphanumeric1,
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
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, (display, url)) = context(
        "href",
        tuple((
            delimited(char('['), take_until("]"), char(']')),
            // url should be parsed into proper format
            map_res(
                delimited(char('('), take_until(")"), char(')')),
                |url: &str| Url::from_str(url).map_err(KyomatoLexErrorKind::BadUrl),
            ),
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
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
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
                    permutation((
                        opt(preceded(
                            pair(char('*'), space0),
                            opt(terminated(caption_kernel, space0)),
                        )),
                        opt(preceded(
                            pair(char('~'), space0),
                            terminated(take_till1(char::is_whitespace), space0),
                        )),
                        opt(preceded(
                            pair(char('#'), space0),
                            terminated(
                                map_res(digit1, |n| {
                                    u16::from_str(n).map_err(KyomatoLexErrorKind::BadIdent)
                                }),
                                space0,
                            ),
                        )),
                        opt(terminated(char('!'), space0)),
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
    // TODO add support for codeblock identifiers
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
                if let Some(res) = read.get(i) {
                    return &**res;
                }
                // Hmmm, that didn't worked.
                // Give away read lock, and ask for modification
                drop(read);
                let mut write = STRS.write().unwrap();
                // Append all necessary strings for the array
                while write.len() <= i {
                    let j = write.len() + 1;
                    write.push(format!("{}.", j).leak());
                }
                write
                    .get(i)
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
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    // First, we need to actually detect list type

    let (new_input, first_discriminator) = preceded(
        _multispace_line_break(""),
        recognize(take_till1(char::is_whitespace)),
    )(input)?;
    let list_type = match first_discriminator {
        "1." => ListType::Num,
        "a." => ListType::Latin,
        "а." => ListType::Cyrillic,
        "-" => ListType::Bullet,
        "I" => unimplemented!("Roman numerated lists are not supported yet"),
        _ => {
            // that's an unknown list type
            return Err(nom::Err::Error(E::from_external_error(
                new_input,
                ErrorKind::Char,
                KyomatoLexErrorKind::UnknownListType,
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
                KyomatoLexErrorKind::TooLongList,
            )));
        };
        // Match current item start and item itself
        preceded(pair(tag(this_start), space0), inner_lex)(input)
    })(new_input)?;
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
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    context("list", preceded(_multispace_line_break(""), _list_inner))(input)
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
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    let (rest, delimiter) = preceded(
        pair(opt(char('\n')), space0),
        alt((tag("**"), tag("__"), tag("*"), tag("_"), tag("~~"))),
    )(input)?;
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
        if let Ok((_, inner)) = all_consuming(inner_lex::<'source, E>)(inner_tokens) {
            return Ok((
                &rest[(end_candidate + delimiter.len())..],
                Token::Formatted(formatting, Box::new(inner)),
            ));
        }
    }
    // If this point was reached, there is no correct variant for pairing delimiter.
    Err(nom::Err::Failure(E::add_context(
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
        delimited(char('$'), take_until1("$"), char('$')),
    )
    .map(|content: &str| Token::InlineMathmode(content.into()))
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
        delimited(pair(space0, tag("[@")), cut(take_until1("]")), char(']')),
    )(input)?;
    Ok((rest, Token::Reference(ident.trim().into())))
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
        delimited(pair(space0, tag("[^")), cut(take_until1("]")), char(']')),
    )(input)?;
    Ok((rest, Token::FootNoteReference(ident.trim().into())))
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
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    input: &'source str,
) -> IResult<&'source str, Token<'source>, E> {
    context(
        "footnote context",
        delimited(
            _multispace_line_break("[^"),
            cut(take_until1("]")),
            tag("]:"),
        )
        .and(preceded(
            space0,
            // Content can end at the end of this line, or at the end of file
            alt((take_until1("\n"), take_while(|_| true))).and_then(cut(all_consuming(inner_lex))),
        )),
    )
    .map(|(ident, content)| Token::FootNoteContent {
        content: Box::new(content),
        ident: ident.into(),
    })
    .parse(input)
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
        + FromExternalError<&'source str, KyomatoLexErrorKind>,
>(
    outer_input: &'source str,
) -> IResult<&'source str, (Token<'source>, Option<Token<'source>>), E> {
    preceded(multispace0, |input: &'source str| {
        // At this point we assume, that whitespace before was parsed, including any possible newlines

        // Any token starting with a newline is out of the question
        // However, this still leaves a couple of candidates:
        // - href (starting with '[')
        // - formatting (starting with '*', '_' or '~')
        // - inline_math (starting with '$')
        // - reference (starting with "[@")
        // - footnote_reference (starting with "[^")

        // First, let's isolate input to a newline (if there is no newline, stop at last char)
        let interrupt_ind = input.find('\n').unwrap_or(input.len() - 1);
        let cut_input = &input[..=interrupt_ind];

        // Now, let's try finding interrupting token
        let interuption = cut_input
            .char_indices()
            // Parsing continues until newline is reached
            .take_while(|(_, c)| c != &'\n')
            // Possible breakpoints are at characters '[', '*', '_', '~', and '$'
            .filter(|(_, c)| ['[', '*', '_', '~', '$'].contains(c))
            // Now, try a proper parser for each one
            .map(|(ind, c)| {
                let cut_cut_input = &cut_input[ind..];
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

        let interruption = if let Some(interruption) = interuption {
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
        let this_token = Token::Paragraph(Font::Normal, Cow::from(&cut_input[..this_token_end]));
        let rest_input = interruption
            .as_ref()
            .map(|(_, (rest, _))| *rest)
            .unwrap_or(&cut_input[this_token_end..]);
        let follower = interruption.map(|(_, (_, f))| f);
        Ok((rest_input, (this_token, follower)))
    })(outer_input)
}

fn __equalizer(token: Token<'_>) -> (Token<'_>, Option<Token<'_>>) {
    (token, None)
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
    BadUrl(url::ParseError),
    BadIdent(ParseIntError),
    UnknownListType,
    TooLongList,
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
    mut tokens_kernel: impl FnMut(&'source str) -> IResult<&'source str, (Token<'source>, Option<Token<'source>>), E>
        + 'kernel,
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
            div_page.map(__equalizer),
            header.map(__equalizer),
            equation.map(__equalizer),
            table.map(__equalizer),
            figure.map(__equalizer),
            href.map(__equalizer),
            code_block.map(__equalizer),
            ayano.map(__equalizer),
            list.map(__equalizer),
            formatting.map(__equalizer),
            inline_math.map(__equalizer),
            reference.map(__equalizer),
            footnote_content.map(__equalizer),
            footnote_reference.map(__equalizer),
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

/// Auxiliary entry point into the lexer, used to lex inner fields
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
                forbid(code_block, "code block", "you are not supposed to do that")
                    .map(__equalizer),
                forbid(
                    list,
                    "list",
                    "LaTeX does not support lists inside captions and footnotes well",
                )
                .map(__equalizer),
                forbid(
                    ayano,
                    "ayano",
                    "Ayano on the inner layer is not supported, as of now",
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
                let result: Result<_, nom::Err<nom::error::VerboseError<&'static str>>>;
                result = $parser.parse($input);

                // assert
                assert_eq!(result, $expected, "Parser did not produce expected result");
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

    // `_multispace_line_break` tests
    test! {multispace_line_break1, _multispace_line_break(""), "\n", e: Ok(("", ("\n", "")))}
    test! {multispace_line_break2, _multispace_line_break("1"), "\n", p: Err(_)}
    test! {multispace_line_break3, _multispace_line_break("1"), "    \t\t\n1benzene", e: Ok(("benzene", ("    \t\t\n", "1")))}
    test! {multispace_line_break4, _multispace_line_break("ben"), "    \t\t\nbenzene", e: Ok(("zene", ("    \t\t\n", "ben")))}
    test! {multispace_line_break5, _multispace_line_break("ben"), "l    \t\t\nbenzene", p: Err(_)}

    // `parse_args!` tests
    test! {parse_args1, parse_args!["a" = digit1], "", p: Err(_)}
    test! {parse_args2, parse_args!["a" = digit1], "{}", p: Err(_)}
    test! {parse_args3, parse_args!["a" = digit1], "\n{a = 34522}", e: Ok(("", (Some("34522"), )))}
    test! {parse_args3_2, parse_args!["a" = digit1], "{a = 34522}", p: Err(_)}

    // `div_page` tests
    test! {div_page1, div_page, "", p: Err(_)}
    test! {div_page2, div_page, "---", p: Err(_)}
    test! {div_page3, div_page, "\n------", e: Ok(("", Token::PageDiv))}
    test! {div_page3_2, div_page, "------", p: Err(_)}
    test! {div_page4, div_page, "\n   ------", p: Err(_)}
    test! {div_page5, div_page, "    \t\n------\t\n", e: Ok(("\t\n", Token::PageDiv))}
}

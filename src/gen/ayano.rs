use cached::proc_macro::cached;
use itertools::{repeat_n, Itertools};
use once_cell::sync::Lazy;
use pyo3::{
    prelude::PyErr,
    types::{PyList, PyTuple},
    PyAny, Python,
};
use regex::Regex;
use std::{
    borrow::Cow,
    collections::{hash_map::DefaultHasher, HashMap},
    fmt::{Arguments, Debug, Write},
    fs::File,
    hash::{Hash, Hasher},
    io::{BufRead, BufReader},
    num::{ParseFloatError, ParseIntError},
    path::PathBuf,
    str::FromStr,
};

use crate::{
    data::{AyanoBlock, Token, Tokens},
    lexer::KyomatoLexError,
    util::{
        Equivalent, GenericRange, GenericRangeParseError, Immutable, InlinedStack,
        InlinedStackAccessor,
    },
};

use self::python::{init, InitToken};

mod python;

#[derive(Debug)]
pub struct DisplayInfo {
    pub code: String,
    pub name: String,
}

#[derive(Debug, PartialEq)]
struct CodelessBlockInfo {
    ident: u64,
    function_name: Option<String>,
    display_name: Option<String>,
    insert_path: Option<PathBuf>,
}

#[derive(Debug)]
struct BlockInfo {
    ident: u64,
    function_name: Option<String>,     // none means it's a static block
    display_info: Option<DisplayInfo>, // none means it's not meant to be displayed
    insert_path: Option<PathBuf>,      // none means there's no script to insert additionally
}

#[derive(Debug)]
pub struct AyanoBuilder {
    code: String,
    blocks: HashMap<u64, BlockInfo>,
}

impl Default for AyanoBuilder {
    fn default() -> Self {
        Self::new()
    }
}

impl AyanoBuilder {
    pub fn new() -> Self {
        Self {
            code: String::new(),
            blocks: HashMap::new(),
        }
    }

    pub fn add_block(&mut self, block: &AyanoBlock<'_>) -> Result<(), SyntaxError> {
        let (transformed_code, info) = apply_ayano(block)?;
        let ident = info.ident;

        // Now, append the code to python handler
        if let Some(function_name) = info.function_name.as_ref() {
            python::append_function(&mut self.code, &function_name, &transformed_code);
        } else {
            python::append_static(&mut self.code, &transformed_code);
        }
        self.blocks.insert(
            ident,
            BlockInfo {
                ident,
                function_name: info.function_name,
                display_info: info.display_name.map(|name| DisplayInfo {
                    code: transformed_code,
                    name,
                }),
                insert_path: info.insert_path,
            },
        );
        Ok(())
    }

    pub fn initialize(self) -> Result<AyanoExecutor, PyErr> {
        let Self { code, blocks } = self;
        let token = init(&code.as_str())?;
        Ok(AyanoExecutor {
            token,
            blocks: blocks.into(),
        })
    }
}

#[derive(Debug)]
pub struct AyanoExecutor {
    token: InitToken,
    blocks: Immutable<HashMap<u64, BlockInfo>>,
}

#[derive(Debug, derive_more::From, PartialEq, thiserror::Error)]
pub enum AyanoError<'b> {
    #[error("Python error occured: {:?}", .0)]
    Python(Equivalent<PyErr>),
    #[error("There's no function for ayano block: {:?}", .0)]
    NoFunction(AyanoBlock<'b>),
    #[error("{}", .0)]
    Applying(SyntaxError),
    #[error("{}", .0)]
    Parsing(ParsingError),
}

impl From<PyErr> for AyanoError<'_> {
    fn from(value: PyErr) -> Self {
        Equivalent(value).into()
    }
}

type AyanoResult<'b, T = ()> = Result<T, AyanoError<'b>>;

impl AyanoExecutor {
    // this could easily be part of `display_token`, but I separated it for debugging purposes
    fn call_block<'token, 'source: 'token, Out, Transform>(
        &self,
        token: &'token AyanoBlock<'source>,
        transform: Transform,
    ) -> AyanoResult<'source, Out>
    where
        for<'py> Transform: FnOnce(&'py PyAny, Python<'py>) -> AyanoResult<'source, Out>,
    {
        let hash = calculate_hash(token);
        let Some(info) = self.blocks.get(&hash) else {
            // this block was not passed here before
            return Err(AyanoError::NoFunction(token.clone()));
        };
        let Some(function_name) = info.function_name.as_ref() else {
            // this block was passed here before, but it's not a function
            return Err(AyanoError::NoFunction(token.clone()));
        };

        python::call_function(&self.token, function_name, transform)
    }

    pub fn display_token<'token, 'source: 'token>(
        &self,
        token: &'token AyanoBlock<'source>,
    ) -> AyanoResult<'source, Token<'static>> {
        self.call_block(token, |res, _| Ok(parse_ayano(res)?))
    }

    pub fn display_blocks<'s>(&'s self) -> impl IntoIterator<Item = &'s DisplayInfo> {
        self.blocks
            .values()
            .filter_map(|info| info.display_info.as_ref())
    }
}

#[cfg(test)]
mod ayano_tests {
    use super::*;
    macro_rules! function {
        {$name:ident, $code:literal, $output:literal} => {
            #[test]
            fn $name() {
                // arrange
                let block = AyanoBlock {
                    is_display: false,
                    is_static: false,
                    code: $code.into(),
                    insert_path: None,
                    is_space_before: false,
                };
                let mut builder = AyanoBuilder::new();

                // act
                builder.add_block(&block).expect("Should be able to add block");
                // dbg!(&handler);
                let executor = builder.initialize().expect("Should be able to initialize");
                let output = executor.call_block(&block, |res, _| Ok::<_, super::AyanoError<'_>>(res.str()?.to_string())).expect("Should be able to call this function");

                // assert
                assert_eq!(output, $output);
            }
        };
    }

    function! {number1, "1", "1"}
    function! {number2, r"
x = 5
y = 2 + x
x, y",
    "(5, 7)"}
    function! {value_err1, r"
x = 5
y = x / 100
@dev: x, y
","('err', 5, 0.05)"}
    function! {csv_table1, "@csv_table: src=\"src/gen/ayano/test_table.csv\", columns=[(\"a\", \"a_err\")]",
    "('tab', ['a'], [[('err', '1.0', '0.001')], [('err', '1.0', '0.001')], [('err', '1.0', '0.001')], [('err', '1.0', '0.001')], [('err', '1.0', '0.001')], [('err', '1.0', '0.001')], [('err', '1.0', '0.001')], [('err', '1.0', '0.001')]], None, None)"}

    function! {csv_table2, "@csv_table: src=\"src/gen/ayano/test_table.csv\", rows = 3..5, columns=[(\"a\", \"a_err\"), \"b\", (\"d\", \"d_err\")]",
    "('tab', ['a', 'b', 'd'], [[('err', '1.0', '0.001'), '3', ('err', '-5', '0.001')]], None, None)"}

    function! {gen_table1, "@gen_table: lambda r,c: r + c; rows=4, columns=2",
    "('tab', [[0, 1], [1, 2], [2, 3], [3, 4]], None, None)"}

    macro_rules! static_function {
        {$name:ident, $static_code:literal, $function_code:literal, $expected_output:literal} => {
            #[test]
            fn $name() {
                // arrange
                let mut builder = AyanoBuilder::new();
                let static_block = AyanoBlock {
                    code: $static_code.into(),
                    insert_path: None,
                    is_display: false,
                    is_static: true,
                    is_space_before: false,
                };
                let function_block = AyanoBlock {
                    code: $function_code.into(),
                    insert_path: None,
                    is_display: false,
                    is_static: false,
                    is_space_before: false,
                };
                builder.add_block(&static_block).expect("Should be able to add static block");
                builder.add_block(&function_block).expect("Should be able to add function block");
                // dbg!(&builder);
                let executor = builder.initialize().expect("Should be able to initialize");

                // act
                let static_output = executor.call_block(&static_block, |res, _| Ok(res.str()?.to_string()));
                let function_output = executor.call_block(&function_block, |res, _| Ok(res.str()?.to_string()));

                // assert
                assert_eq!(static_output, Err(AyanoError::NoFunction(static_block.clone())), "Static block shouldn't be callable!");
                assert_eq!(function_output, Ok($expected_output.to_string()), "Function should give expected result");
            }
        };
    }

    static_function! {function_return, "x=1", "x", "1"}
    static_function! {function_mutate, "x=1",
r"y = x + 1
y", "2"}
    static_function! {function_static_err, "x, x_err = 1.0, 0.025", "@dev: x, x_err", "('err', 1.0, 0.025)"}
    static_function! {function_static_gen_tab1, "table = [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10, 11, 12]]", "@gen_table: lambda r,c: table[r][c]; rows = 2, columns = 3",
    "('tab', [[1, 2, 3], [5, 6, 7]], None, None)"}

    macro_rules! static_function_function {
        {$name:ident, $static_code:literal, $function1_code:literal, $function2_code:literal, $expected_output1:literal, $expected_output2:literal} => {
            #[test]
            fn $name() {
                // arrange
                let mut builder = AyanoBuilder::new();
                let static_block = AyanoBlock {
                    code: $static_code.into(),
                    insert_path: None,
                    is_display: false,
                    is_static: true,
                    is_space_before: false,
                };
                let function1_block = AyanoBlock {
                    code: $function1_code.into(),
                    insert_path: None,
                    is_display: false,
                    is_static: false,
                    is_space_before: false,
                };
                let function2_block = AyanoBlock {
                    code: $function2_code.into(),
                    insert_path: None,
                    is_display: false,
                    is_static: false,
                    is_space_before: false,
                };
                builder.add_block(&static_block).expect("Should be able to add static block");
                builder.add_block(&function1_block).expect("Should be able to add function block");
                builder.add_block(&function2_block).expect("Should be able to add function block");
                // dbg!(&builder);
                let executor = builder.initialize().expect("Should be able to initialize");

                // act
                let static_output = executor.call_block(&static_block, |res, _| Ok(res.str()?.to_string()));
                let function1_output = executor.call_block(&function1_block, |res, _| Ok(res.str()?.to_string()));
                let function2_output = executor.call_block(&function1_block, |res, _| Ok(res.str()?.to_string()));

                // assert
                assert_eq!(static_output, Err(AyanoError::NoFunction(static_block.clone())), "Static block shouldn't be callable!");
                assert_eq!(function1_output, Ok($expected_output1.to_string()), "Function should give expected result");
                assert_eq!(function2_output, Ok($expected_output2.to_string()), "Function should give expected result");
            }
        };
    }

    static_function_function! {function_mutate_changes, "x=0", "global x\nx += 1\nx", "global x\nx += 1\nx", "1", "2"}

    // TODO add function for display block tests
}

fn function_name(ident: u64) -> String {
    format!("f_{ident}")
}

fn block_name(ident: u64) -> String {
    format!("ayano_block__{ident}")
}

fn calculate_hash<T: Hash>(t: &T) -> u64 {
    let mut s = DefaultHasher::new();
    t.hash(&mut s);
    s.finish()
}

#[derive(
    Debug,
    derive_more::From,
    derive_more::Into,
    derive_more::Deref,
    derive_more::DerefMut,
    PartialEq,
)]
pub struct UnknownArgument(String);

#[derive(
    Debug,
    derive_more::From,
    derive_more::Into,
    derive_more::Deref,
    derive_more::DerefMut,
    PartialEq,
)]
pub struct CsvColumnsFormat(String);

#[derive(Debug, PartialEq)]
enum CsvTableColumn<'l> {
    Single(&'l str),
    ValueError(&'l str, &'l str),
}

impl CsvTableColumn<'_> {
    fn write_to<W: Write + ?Sized>(&self, dest: &mut CodeWriter<'_, W>) -> std::fmt::Result {
        dest.write_str("\"")?;
        dest.write_str(match self {
            CsvTableColumn::Single(column) => column,
            CsvTableColumn::ValueError(value, _) => value,
        })?;
        dest.write_str("\"")?;
        Ok(())
    }
}

fn apply_ayano<'token, 'source: 'token>(
    block: &'token AyanoBlock<'source>,
) -> Result<(String, CodelessBlockInfo), SyntaxError> {
    let ident = calculate_hash(block);
    // FIRST OF ALL,
    // read the code from insert-file (if present)
    let insert_path = block.insert_path.as_ref().map(|path| path.to_path_buf());
    let mut res = if let Some(insert_path) = insert_path.as_ref() {
        let mut res = String::new();
        for line in BufReader::new(File::open(insert_path).map_err(Equivalent::from)?).lines() {
            let line = line.map_err(Equivalent::from)?;
            if line.trim().starts_with("# TOSHINO KYOKO!") {
                break;
            }
            res.push_str(&line);
            res.push('\n');
        }
        res
    } else {
        String::new()
    };
    let mut code_lines = block.code.lines();
    // I used to have this filter here, but I don't think that's a good thing anymore
    // There's no harm in preserving basic formatting for better debugging
    // TODO probably and #[cfg(test)] switch, that would simplify the code is it's added to Python module itself (i.e. this is to be done in `python` module)
    /*.filter(|line| {
        let line = line.trim();
        !line.is_empty() && !line.starts_with('#') // remove comments
    })
    */

    // create display name
    let display_name = if block.is_display {
        Some(block_name(ident))
    } else {
        None
    };

    // return out static block here
    if block.is_static {
        res.extend(itertools::intersperse(code_lines, "\n"));
        return Ok((
            res,
            CodelessBlockInfo {
                ident,
                function_name: None,
                display_name,
                insert_path,
            },
        ));
    }
    // block is a function-block at this point
    let function_name = Some(function_name(ident));

    // take out last line, as it may contain ayano syntax
    let Some(last_line) = code_lines.next_back() else {
        // there are no lines at all, it's an empty block, or th
        // return inserted code anyway, to allow static empty blocks that just insert the code.
        // no Ayano syntax is expected there, and that code should not be altered
        return Ok((
            res,
            CodelessBlockInfo {
                ident,
                function_name,
                display_name,
                insert_path,
            },
        ));
    };

    // push all other lines
    for line in code_lines {
        res.push_str(line); // append the line
        res.push('\n'); // and a newline character
    }

    // extract syntax from the last line,
    let syntax = SyntaxData::try_from(last_line)?;
    // and write code corresponding to it
    syntax
        .write_to(&mut res)
        .expect("Should always be able to write into an owned String");
    res.push('\n'); // newline for a good measure

    Ok((
        res,
        CodelessBlockInfo {
            ident,
            function_name,
            insert_path,
            display_name,
        },
    ))
}

#[cfg(test)]
mod apply_ayano_tests {
    use super::*;

    macro_rules! test {
        {$name:ident, $input:literal, l = $output:literal} => {
            test!{$name, $input, l = $output, true}
        };
        {$name:ident, $input:literal, l = $output:literal, $is_function:literal} => {
            test!{$name, $input, Ok($output.to_string()), $is_function}
        };
        {$name:ident, $input:literal, $output:expr} => {
            test!{$name, $input, $output, true}
        };
        {$name:ident, $input:literal, $output:expr, $is_function:literal} => {
            #[test]
            fn $name() {
                // arrange
                let block = AyanoBlock {
                    code: $input.into(),
                    insert_path: None,
                    is_display: false,
                    is_static: false,
                    is_space_before: false,
                };

                // act
                let actual = apply_ayano(&block).map(|(s, _)| s);

                // assert
                assert_eq!(actual, $output, "Should yield expected result");
            }
        };
    }

    test! {empty, "", l = ""}
    test! {empty2, r"


    ", l = r"


    return 
"}
    test! {comment1, r"
# TOSHINO KYOKO! (this is not actually supposed to trigger any sort of syntax, it's just a test)
# Some other comments bla-bla-bla", l = r"
# TOSHINO KYOKO! (this is not actually supposed to trigger any sort of syntax, it's just a test)
return # Some other comments bla-bla-bla
"}
    test! {static_code, r"
x = 4
y = x + 2
# Some comment
    ", l =
r"
x = 4
y = x + 2
# Some comment
    return 
", false}
    test! {function_code, r"
x = 4
y = x + 2
# Some comment
y
", l =
r"
x = 4
y = x + 2
# Some comment
return y
", true}
    test! {function_return, r"
x = 4
# Even more comments

y = x + 2
# Some comment
return y
", l =
r"
x = 4
# Even more comments

y = x + 2
# Some comment
return y
", true}
    test! {indent1, r"
x = 4
# Even more comments

y = x + 2
# Some comment
    y
", l =
r"
x = 4
# Even more comments

y = x + 2
# Some comment
    return y
", true}
    test! {bad_syntax, r"@dev: 5,", Err(SyntaxError::BadSyntax), true}

    // TODO I don't feel like testing all of that again here. maybe later?

    #[test]
    fn insert_test() {
        // arrange
        let insert_path: PathBuf = "src/gen/ayano/test_code.py"
            .parse()
            .expect("Should be a valid path");

        let code = "@dev: x,y";
        let block = AyanoBlock {
            is_display: false,
            code: code.into(),
            insert_path: Some(insert_path.into()),
            is_static: false,
            is_space_before: false,
        };

        // act
        let transformed_code =
            apply_ayano(&block).expect("Should be able to apply ayano transform");

        // assert
        assert_eq!(
            transformed_code.0,
            r#"x = 5
y = x ** 10
return "err", x, y
"#
        );
    }
}

#[derive(Debug, PartialEq)]
struct SyntaxData<'l> {
    indent: usize,
    syntax: Syntax<'l>,
}

#[derive(Debug, PartialEq)]
enum Syntax<'source> {
    Nothing(&'source str),
    ValueError {
        value: &'source str,
        error: &'source str,
    },
    Figure {
        src: &'source str,
        ident: Option<&'source str>,
        caption: Option<&'source str>,
        width: Option<&'source str>,
    },
    CsvTable {
        src: &'source str,
        rows: GenericRange,
        columns: Option<Vec<CsvTableColumn<'source>>>,
        ident: Option<&'source str>,
        caption: Option<&'source str>,
    },
    GenTable {
        generator: &'source str,
        rows: usize,
        columns: usize,
        ident: Option<&'source str>,
        caption: Option<&'source str>,
    },
}

#[derive(Debug, derive_more::From, PartialEq, thiserror::Error)]
pub enum SyntaxError {
    #[error("Required argument is missing: {}", .0)]
    MissingArgument(&'static str),
    #[error("Unknown argument is present: {}", .0)]
    UnknownArgument(String),
    /// Right now this only means that you don't have a generator in a table syntax
    #[error("Ayano syntax is lacking fields, or it's punctuation is wrong")]
    BadSyntax,
    #[error(transparent)]
    RangeParse(GenericRangeParseError),
    #[error(transparent)]
    IntParse(ParseIntError),
    #[error("{}", **.0)]
    InsertIo(Equivalent<std::io::Error>),
}

macro_rules! or_bs {
    ($input:expr) => {
        ($input).ok_or(SyntaxError::BadSyntax)?
    };
}

// CHIPPI CHIPPI
// CHAPPA CHAPPA
macro_rules! chip_literal {
    ($s:ident, $word:literal, ?) => {{
        chip_literal!($s, $word).unwrap_or($s)
    }};
    ($s:ident, $word:literal) => {{
        static REGEX: Lazy<Regex> = Lazy::new(|| {
            Regex::new(concat!("^", $word, r"\s+(.+?)$")).expect("Should be a valid regex")
        });
        REGEX
            .captures($s)
            .and_then(|c| c.get(1))
            .map(|c| c.as_str())
    }};
}

#[cfg(test)]
mod chip_literal_tests {
    use super::*;

    macro_rules! test {
        {$name:ident, $input:literal, $literal:literal} => {
            test!{$name, $input, $literal, None}
        };
        {$name:ident, $input:literal, $literal:literal, +, $output:literal} => {
            test!{$name, $input, $literal, Some($output)}
        };
        {$name:ident, $input:literal, $literal:literal, $output:expr} => {
            #[test]
            fn $name() {
                // arrange
                let s = $input;

                // act
                let output = chip_literal!(s, $literal);

                // assert
                assert_eq!(output, $output);
            }
        };
    }

    test! {none1, "abcd abc", "a"}
    test! {some1, "abcd abc", "abcd", +, "abc"}
    test! {some1_simple, "a    b", "a", +, "b"}
    test! {return1, "return x", "return", +, "x"}

    macro_rules! test_optional {
        {$name:ident, $input:literal, $literal:literal, $output:literal} => {
            #[test]
            fn $name() {
                // arrange
                let mut s = $input;

                // act
                s = chip_literal!(s, $literal, ?);

                // assert
                assert_eq!(s, $output);
            }
        };
    }
    test_optional! {edge1, r#"@gen_table: lambda row, col: 1; rows   = 10, columns =  10 caption = "Some awesome caption""#, "return",
        r#"@gen_table: lambda row, col: 1; rows   = 10, columns =  10 caption = "Some awesome caption""#
    }

    #[test]
    fn return2() {
        // arrange
        let mut s = "return x";

        // act
        s = chip_literal!(s, "return", ?);

        // assert
        assert_eq!(s, "x");
    }
}

macro_rules! regex {
    ($regex:literal) => {{
        const STR: &'static str = $regex;
        static REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(STR).expect("Should be a valid regex"));
        &*REGEX
    }};
    ($regex:literal, $s:ident) => {
        or_bs!(regex!($regex).captures($s))
    };
}

macro_rules! _parse_arg {
    (continue, $_:expr) => {
        continue
    };
    ($var:ident, $val:expr) => {
        $var = Some($val)
    };
}

macro_rules! parse_args {
    ($s:ident, $($var:tt, $name:literal), *) => {
        for (kw, val) in find_args($s) {
            match kw {
                $($name => _parse_arg!($var, val),)*
                _ => return Err(SyntaxError::UnknownArgument(kw.to_string())),
            };
        }
    };
}

macro_rules! needed {
    ($var:ident, $name:literal) => {
        let Some($var) = $var else {
            return Err(SyntaxError::MissingArgument($name));
        };
    };
}

// Can't use FromStr here, as if requires parsing from &str with any lifetime
impl<'l> TryFrom<&'l str> for SyntaxData<'l> {
    type Error = SyntaxError;

    fn try_from(mut s: &'l str) -> Result<Self, Self::Error> {
        // First, cut off empty space (but record number of them)
        let indent = s
            .find(|ch: char| !ch.is_whitespace())
            .unwrap_or(s.chars().count()); // TODO POSSIBLE BUG: `str::len` is a length of underlying byte array, not string itself
        s = &s[indent..];
        s = s.trim_end(); // also remove whitespace at the end

        // Then, chip off return statement (will be added regardless)
        s = chip_literal!(s, "return", ?);

        // Check for @dev syntax
        if let Some(s) = chip_literal!(s, "@dev:") {
            let m = regex!(r"^\s*(.+?)\s*,\s*(.+?)$", s);
            return Ok(Self {
                indent,
                syntax: Syntax::ValueError {
                    value: or_bs!(m.get(1)).as_str(),
                    error: or_bs!(m.get(2)).as_str(),
                },
            });
        }

        if let Some(s) = chip_literal!(s, "@fig:") {
            let mut src = None;
            let mut ident = None;
            let mut caption = None;
            let mut width = None;
            parse_args!(s, src, "src", ident, "ident", caption, "caption", width, "width");
            needed!(src, "src");
            return Ok(Self {
                indent,
                syntax: Syntax::Figure {
                    src,
                    caption,
                    ident,
                    width,
                },
            });
        }

        if let Some(s) = chip_literal!(s, "@csv_table:") {
            let mut src = None;
            let mut rows = None;
            let mut columns = None;
            let mut ident = None;
            let mut caption = None;
            parse_args!(
                s, src, "src", rows, "rows", columns, "columns", ident, "ident", caption, "caption"
            );
            needed!(src, "src");
            let rows = rows
                .map(|val| val.parse())
                .unwrap_or(Ok(GenericRange::Full))?;

            let columns = if let Some(val) = columns.as_ref() {
                Some(parse_columns(&val))
            } else {
                None
            };

            return Ok(Self {
                indent,
                syntax: Syntax::CsvTable {
                    src,
                    rows,
                    columns,
                    ident,
                    caption,
                },
            });
        }

        if let Some(s) = chip_literal!(s, "@gen_table:") {
            let Some((generator, s)) = s.split_once(';') else {
                return Err(SyntaxError::BadSyntax);
            };
            let mut rows = None;
            let mut columns = None;
            let mut ident = None;
            let mut caption = None;
            parse_args!(s, rows, "rows", columns, "columns", ident, "ident", caption, "caption");
            needed!(rows, "rows");
            needed!(columns, "columns");
            return Ok(Self {
                indent,
                syntax: Syntax::GenTable {
                    generator,
                    rows: rows.parse()?,
                    columns: columns.parse()?,
                    ident,
                    caption,
                },
            });
        }

        // If executing reaches here, this is a no-syntax-thing
        Ok(Self {
            indent,
            syntax: Syntax::Nothing(s),
        })
    }
}

#[cfg(test)]
mod syntax_data_tests {
    use super::*;
    macro_rules! test {
        ($name:ident, $input:literal, $output:expr) => {
            #[test]
            fn $name() {
                // arrange

                // act
                let output = SyntaxData::try_from($input).expect("Should be a valid syntax");

                // assert
                assert_eq!(output, $output, "Should produce expected result");
            }
        };
    }

    macro_rules! c {
        ($col:literal) => {
            CsvTableColumn::Single($col)
        };
        ($value:literal, $error:literal) => {
            CsvTableColumn::ValueError($value, $error)
        };
    }

    test! {nothing1, "a", SyntaxData {indent: 0, syntax: Syntax::Nothing("a")}}
    test! {nothing2, "    a", SyntaxData {indent: 4, syntax: Syntax::Nothing("a")}}
    test! {nothing3, "    a, lala, c    ", SyntaxData {indent: 4, syntax: Syntax::Nothing("a, lala, c")}}
    test! {return1, "    return x", SyntaxData {indent: 4, syntax: Syntax::Nothing("x")}}
    test! {value_error1, "@dev: 1, 2", SyntaxData {indent: 0, syntax: Syntax::ValueError { value: "1", error: "2" }}}
    test! {value_error2, "@dev:  1,    2   ", SyntaxData {indent: 0, syntax: Syntax::ValueError { value: "1", error: "2" }}}
    test! {value_error3, "@dev:  x + 1,  (2 + 3) / 10   ", SyntaxData {indent: 0, syntax: Syntax::ValueError { value: "x + 1", error: "(2 + 3) / 10" }}}
    test! {fig1, r#"    @fig:  src = "path.png" , caption   =  "Wow, such caption", ident="path"  "#, SyntaxData {indent: 4, syntax: Syntax::Figure{ src:"\"path.png\"", caption: Some("\"Wow, such caption\""), ident: Some("\"path\""), width: None}}}
    test! {fig2, r#"    @fig:   caption   =  "Wow, such caption", ident="path" ,src = "path.png"  "#, SyntaxData {indent: 4, syntax: Syntax::Figure{ src: "\"path.png\"", caption: Some("\"Wow, such caption\""), ident: Some("\"path\""), width: None}}}
    test! {fig3_width, r#"    @fig:   caption   =  "Wow, such caption", width=   0.125, ident="path" ,src = "path.png"  "#, SyntaxData {indent: 4, syntax: Syntax::Figure{ src: "\"path.png\"", caption: Some("\"Wow, such caption\""), ident: Some("\"path\""), width: Some("0.125")}}}
    test! {fig4_width_expr, r#"    @fig:   caption   =  "Wow, such caption", width=    WIDTH + 3 , ident="path" ,src = "path.png"  "#,
    SyntaxData {indent: 4, syntax: Syntax::Figure{ src: "\"path.png\"", caption: Some("\"Wow, such caption\""), ident: Some("\"path\""), width: Some("WIDTH + 3")}}}
    test! {csv_table1, r#"   @csv_table: src = "path/to/table.csv", caption = "such caption, wow""#, SyntaxData {indent: 3, syntax: Syntax::CsvTable { src: "\"path/to/table.csv\"", rows: GenericRange::Full, columns: None, ident: None, caption: Some("\"such caption, wow\"") }}}
    test! {csv_table2, r#"   @csv_table: src = "path/to/table.csv", rows  =   1..1000"#, SyntaxData {indent: 3, syntax: Syntax::CsvTable { src: "\"path/to/table.csv\"", rows: GenericRange::DoubleBounded(1..1000), columns: None, ident: None, caption: None, }}}
    test! {csv_table3, r#"   @csv_table: src = "path/to/table.csv", rows  =   2.."#, SyntaxData {indent: 3, syntax: Syntax::CsvTable { src: "\"path/to/table.csv\"", rows: GenericRange::From(2..), columns: None, ident: None, caption: None }}}
    test! {csv_table4, r#"   @csv_table: src = "path/to/table.csv", rows  =   2.., columns = ["a", ("b", "b_err")]"#, SyntaxData {indent: 3, syntax: Syntax::CsvTable { src: "\"path/to/table.csv\"", rows: GenericRange::From(2..), columns: Some(vec![c!("a"),c!("b","b_err")]), caption: None, ident: None }}}
    test! {csv_table5, r#"    @csv_table: src = "path/to/table.csv", rows  =   2.., columns = ["a", ("b", "b_err")], caption = "Lol, there's a bunch of random stuff going on, this should not be included into a syntax data""#,
    SyntaxData {indent: 4, syntax: Syntax::CsvTable { src: "\"path/to/table.csv\"", rows: GenericRange::From(2..), columns: Some(vec![c!("a"),c!("b","b_err")]),
    caption: Some("\"Lol, there's a bunch of random stuff going on, this should not be included into a syntax data\""), // ha-ha, get spec-changed, lol
     ident: None, }}}
    test! {get_table1, r#"    @gen_table: lambda row, col: 1; rows   = 10, columns =  10, caption = "Some awesome caption""#,
    SyntaxData {indent: 4, syntax: Syntax::GenTable { generator: "lambda row, col: 1", rows: 10, columns: 10, caption: Some("\"Some awesome caption\""), ident: None, }}}
}

struct CodeWriter<'w, W: ?Sized> {
    inner: &'w mut W,
    indent: &'static str,
}

#[cached]
fn get_indent(indent: usize) -> &'static str {
    itertools::repeat_n(' ', indent).collect::<String>().leak()
}

impl<'w, W: Write + ?Sized> CodeWriter<'w, W> {
    fn new(inner: &'w mut W, indent: usize) -> Result<Self, std::fmt::Error> {
        let indent = get_indent(indent);
        inner.write_str(indent)?;
        Ok(Self { inner, indent })
    }

    fn breakline(&mut self) -> std::fmt::Result {
        self.inner.write_str("\n")?;
        self.inner.write_str(self.indent)?; // start off by writing out indentation
        Ok(())
    }

    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.inner.write_str(s);
        Ok(())
    }

    fn write_fmt(&mut self, fmt: Arguments<'_>) -> std::fmt::Result {
        self.inner.write_fmt(fmt)?;
        Ok(())
    }

    fn write_line(&mut self, s: &str) -> std::fmt::Result {
        self.write_str(s)?;
        self.breakline()?;
        Ok(())
    }
}

impl SyntaxData<'_> {
    fn write_to<'w, W: Write + ?Sized>(&self, dest: &'w mut W) -> std::fmt::Result {
        let mut writer: CodeWriter<'w, W> = CodeWriter::new(dest, self.indent)?;
        self.syntax.write_code(&mut writer)?;
        Ok(())
    }
}

impl Syntax<'_> {
    fn write_code<W: Write + ?Sized>(&self, writer: &mut CodeWriter<'_, W>) -> std::fmt::Result {
        match self {
            Syntax::Nothing(s) => {
                writer.write_str("return ")?;
                writer.write_str(s)?;
            }
            Syntax::ValueError { value, error } => {
                writer.write_str("return \"err\", ")?;
                writer.write_str(value)?;
                writer.write_str(", ")?;
                writer.write_str(error)?;
            }
            Syntax::Figure {
                src,
                ident,
                caption,
                width,
            } => {
                writer.write_str("return \"fig\", ")?;
                writer.write_str(src)?;
                writer.write_str(", ")?;
                if let Some(ident) = ident {
                    writer.write_str(ident)?;
                } else {
                    writer.write_str("None")?;
                }
                writer.write_str(", ")?;
                if let Some(caption) = caption {
                    writer.write_str(caption)?;
                } else {
                    writer.write_str("None")?;
                }
                writer.write_str(", ")?;
                if let Some(width) = width {
                    writer.write_str(&width.to_string())?;
                } else {
                    writer.write_str("None")?;
                }
            }
            Syntax::CsvTable {
                src,
                rows,
                columns,
                ident,
                caption,
            } => {
                writer.write_line("import csv")?;
                writer.write_str("with open(")?;
                writer.write_str(src)?;
                writer.write_line(") as ___csv_file___:")?;
                writer.write_line("    ___reader___ = csv.DictReader(___csv_file___, delimiter=',', skipinitialspace=True, strict=True)")?;
                // if there is a lower bound, skip some lines
                if let Some(mut from) = rows.from() {
                    // if it's 0 or 1, there's effectively no lines to skip
                    if from > 1 {
                        // line 1 refers to a first line of file
                        from -= 1;
                        write!(writer, "    for i in range({from}):")?; // it's a number here, so it can't really be more optimized than that
                        writer.breakline()?;
                        writer.write_line("        ___reader___.__next__()")?;
                    }
                }

                let headers = |writer: &mut CodeWriter<'_, W>| {
                    if let Some(columns) = columns.as_ref() {
                        writer.write_str("[")?;
                        for i in 0..columns.len() {
                            columns[i].write_to(writer)?;
                            writer.write_str(", ")?;
                        }
                        writer.write_str("]")?;
                    } else {
                        writer.write_str("list(___reader___.fieldnames)")?;
                    };
                    Ok::<_, std::fmt::Error>(())
                };

                let line_syntax = |writer: &mut CodeWriter<'_, W>| {
                    if let Some(columns) = columns.as_ref() {
                        writer.write_str("[")?;
                        for i in 0..columns.len() {
                            match columns[i] {
                                CsvTableColumn::Single(col) => {
                                    writer.write_str("line[\"")?;
                                    writer.write_str(col)?;
                                    writer.write_str("\"]")?;
                                }
                                CsvTableColumn::ValueError(value, error) => {
                                    writer.write_str("(\"err\", line[\"")?;
                                    writer.write_str(value)?;
                                    writer.write_str("\"], line[\"")?;
                                    writer.write_str(error)?;
                                    writer.write_str("\"])")?;
                                }
                            }
                            writer.write_str(", ")?;
                        }
                        writer.write_str("]")?;
                    } else {
                        writer.write_str("list([line[f] for f in line])")?;
                    };
                    Ok::<_, std::fmt::Error>(())
                };

                // If a dedicated line generator is necessary, write out one now
                if let Some(rows_to) = rows.to() {
                    let line_iterations = rows_to - rows.from().unwrap_or_default();
                    writer.write_line("    def ___line_generator___():")?;
                    write!(writer, "        for i in range({line_iterations}):")?; // it's again numbers, can't do much better
                    writer.breakline()?;
                    writer.write_line("            line = ___reader___.__next__()")?;
                    writer.write_str("            yield ")?;
                    line_syntax(writer)?;
                    writer.breakline()?;
                }

                let line_generator = |writer: &mut CodeWriter<'_, W>| {
                    if rows.to().is_some() {
                        writer.write_str("___line_generator___()")?;
                    } else {
                        writer.write_str("[")?;
                        line_syntax(writer)?;
                        writer.write_str(" for line in ___reader___]")?
                    };
                    Ok::<_, std::fmt::Error>(())
                };

                writer.write_str("    return \"tab\", ")?;
                headers(writer)?;
                writer.write_str(", list(")?;
                line_generator(writer)?;
                writer.write_str("), ")?;
                if let Some(ident) = ident {
                    writer.write_str(ident)?;
                } else {
                    writer.write_str("None")?;
                }
                writer.write_str(", ")?;
                if let Some(caption) = caption {
                    writer.write_str(caption)?;
                } else {
                    writer.write_str("None")?;
                }
            }
            &Syntax::GenTable {
                generator,
                rows,
                columns,
                ident,
                caption,
            } => {
                writer.write_str("___generator___ = ")?;
                writer.write_line(generator)?;
                writer.write_str("return \"tab\", [")?;
                for row_index in 0..rows {
                    writer.write_str("[")?;
                    for column_index in 0..columns {
                        write!(writer, "___generator___({row_index}, {column_index}), ")?;
                        // numbers. can't really do much
                    }
                    writer.write_str("], ")?;
                }
                writer.write_str("], ")?;
                if let Some(ident) = ident {
                    writer.write_str(ident)?;
                } else {
                    writer.write_str("None")?;
                }
                writer.write_str(", ")?;
                if let Some(caption) = caption {
                    writer.write_str(caption)?;
                } else {
                    writer.write_str("None")?;
                }
            }
        };
        Ok(())
    }
}

#[cfg(test)]
mod syntax_display_tests {
    use super::*;
    macro_rules! test {
        {$name:ident, $input:expr, $output:literal} => {
            #[test]
            fn $name() {
                // arrange
                let expected_output = $output.to_string();
                let mut actual_output = String::new();

                // act
                $input.write_to(&mut actual_output).expect("Should be able to write into string");

                // assert
                assert_eq!(actual_output, expected_output, "Should produce expected result");
            }
        };
    }

    test! {empty1, SyntaxData {indent: 0, syntax: Syntax::Nothing("")}, "return "}
    test! {indent1, SyntaxData {indent: 4, syntax: Syntax::Nothing("")}, "    return "}
    test! {value_error1, SyntaxData {indent: 4, syntax: Syntax::ValueError{value:"x", error:"y"}}, "    return \"err\", x, y"}
    test! {value_error2, SyntaxData {indent: 0, syntax: Syntax::ValueError{value:"x + 2", error:"sqrt(x/2  +y)"}}, "return \"err\", x + 2, sqrt(x/2  +y)"}
    test! {figure1, SyntaxData {indent: 0, syntax: Syntax::Figure{src:"output_path", caption: Some("\"some caption text\""), ident: Some("\"fig1\""), width: None}}, "return \"fig\", output_path, \"fig1\", \"some caption text\", None"}
    test! {figure2_width, SyntaxData {indent: 0, syntax: Syntax::Figure{src:"output_path", caption: Some("\"some caption text\""), ident: Some("\"fig1\""), width: Some("0.75")}}, "return \"fig\", output_path, \"fig1\", \"some caption text\", 0.75"}
    test! {figure3_width_expr, SyntaxData {indent: 0, syntax: Syntax::Figure{src:"output_path", caption: Some("\"some caption text\""), ident: Some("\"fig1\""), width: Some("W + 2")}}, "return \"fig\", output_path, \"fig1\", \"some caption text\", W + 2"}
    test! {csv_table1, SyntaxData {
        indent: 0,
        syntax: Syntax::CsvTable {
            src: "file_path",
            rows: GenericRange::ToInclusive(..=10),
            columns: Some(vec![CsvTableColumn::Single("a")]),
            ident: Some("\"table1\""),
            caption: None
        }},
r#"import csv
with open(file_path) as ___csv_file___:
    ___reader___ = csv.DictReader(___csv_file___, delimiter=',', skipinitialspace=True, strict=True)
    def ___line_generator___():
        for i in range(10):
            line = ___reader___.__next__()
            yield [line["a"], ]
    return "tab", ["a", ], list(___line_generator___()), "table1", None"#
    }
    test! {gen_table1, SyntaxData {indent: 0, syntax: Syntax::GenTable { generator: "lambda r,c:1", rows: 2, columns: 3, ident: None, caption: Some("caption_variable") }},
r#"___generator___ = lambda r,c:1
return "tab", [[___generator___(0, 0), ___generator___(0, 1), ___generator___(0, 2), ], [___generator___(1, 0), ___generator___(1, 1), ___generator___(1, 2), ], ], None, caption_variable"#
    }
    test! {gen_table2, SyntaxData {indent: 4, syntax: Syntax::GenTable { generator: "lambda r,c:1", rows: 2, columns: 3, ident: Some("\"table2\""), caption: None }},
r#"    ___generator___ = lambda r,c:1
    return "tab", [[___generator___(0, 0), ___generator___(0, 1), ___generator___(0, 2), ], [___generator___(1, 0), ___generator___(1, 1), ___generator___(1, 2), ], ], "table2", None"#
    }
}

#[cfg(test)]
mod last_line_tests {
    use super::*;

    macro_rules! test {
        {$name:ident, $input:literal, -, $output:expr} => {
            test!{$name, $input, Err($output)}
        };
        {$name:ident, $input:literal, +, $output:expr} => {
            test!{$name, $input, Ok($output)}
        };
        {$name:ident, $input:literal, $output:literal} => {
            test!{$name, $input, +, $output.trim().to_string()}
        };
        {$name:ident, $input:literal, $output:expr} => {
            #[test]
            fn $name() {
                // arrange

                // act
                let actual_output = SyntaxData::try_from($input).map(move |data| {
                    let mut res = String::new();
                    data.write_to(&mut res).expect("Should be able to write");
                    res
                });

                // assert
                    assert_eq!(
                        $output,
                        actual_output,
                        "Should produce expected result"
                    );
            }
        };
    }

    test! {nothing1, "1, 2", "return 1, 2"}
    test! {nothing2, "\"resulting string!\"", "return \"resulting string!\""}
    test! {dev1, "@dev: 2.0, -0.34E4", "return \"err\", 2.0, -0.34E4"}

    test! {fig1, "@fig: src=\"./picture.jpg\"", "return \"fig\", \"./picture.jpg\", None, None, None"}
    test! {fig2, "@fig: caption=\"Some very cool picture\", src=\"./picture.jpg\"",
    "return \"fig\", \"./picture.jpg\", None, \"Some very cool picture\", None"}
    test! {fig3, "@fig: ident=\"cool\", caption=\"Some very cool picture\", src=\"./picture.jpg\"",
    "return \"fig\", \"./picture.jpg\", \"cool\", \"Some very cool picture\", None"}
    test! {fig4, "@fig: caption=\"Some very cool picture\", src=\"./picture.jpg\", ident=\"cool\"",
    "return \"fig\", \"./picture.jpg\", \"cool\", \"Some very cool picture\", None"}
    test! {fig5, "@fig: caption=\"Some very cool picture\", ident=\"cool\"", -, SyntaxError::MissingArgument("src".into())}
    test! {fig6, "@fig: caption=\"Some very cool picture\", ident=\"cool\", coolness=1000", -, SyntaxError::UnknownArgument("coolness".to_string().into())}
    test! {fig7, "@fig: caption=\"Some very cool picture\", ident=\"cool\", coolness=1000, src=\"path.png\"", -, SyntaxError::UnknownArgument("coolness".to_string().into())}

    test! {csv_tab1e_simple, "@csv_table: src=\"some/path/idk.csv\"", r#"
import csv
with open("some/path/idk.csv") as ___csv_file___:
    ___reader___ = csv.DictReader(___csv_file___, delimiter=',', skipinitialspace=True, strict=True)
    return "tab", list(___reader___.fieldnames), list([list([line[f] for f in line]) for line in ___reader___]), None, None
    "#}

    test! {csv_table_no_src_should_err, "@csv_table: caption=\"nice caption, bro!\"", -, SyntaxError::MissingArgument("src")}
    test! {csv_tab1e_rows1, "@csv_table: src=\"some/path/idk.csv\", rows=..", r#"
import csv
with open("some/path/idk.csv") as ___csv_file___:
    ___reader___ = csv.DictReader(___csv_file___, delimiter=',', skipinitialspace=True, strict=True)
    return "tab", list(___reader___.fieldnames), list([list([line[f] for f in line]) for line in ___reader___]), None, None
    "#}
    test! {csv_tab1e_rows2, "@csv_table: src=\"some/path/idk.csv\", rows=1..", r#"
import csv
with open("some/path/idk.csv") as ___csv_file___:
    ___reader___ = csv.DictReader(___csv_file___, delimiter=',', skipinitialspace=True, strict=True)
    return "tab", list(___reader___.fieldnames), list([list([line[f] for f in line]) for line in ___reader___]), None, None
    "#}
    test! {csv_tab1e_rows3, "@csv_table: src=\"some/path/idk.csv\", rows=10..", r#"
import csv
with open("some/path/idk.csv") as ___csv_file___:
    ___reader___ = csv.DictReader(___csv_file___, delimiter=',', skipinitialspace=True, strict=True)
    for i in range(9):
        ___reader___.__next__()
    return "tab", list(___reader___.fieldnames), list([list([line[f] for f in line]) for line in ___reader___]), None, None
    "#}
    test! {csv_tab1e_rows4, "@csv_table: src=\"some/path/idk.csv\", rows=..10", r#"
import csv
with open("some/path/idk.csv") as ___csv_file___:
    ___reader___ = csv.DictReader(___csv_file___, delimiter=',', skipinitialspace=True, strict=True)
    def ___line_generator___():
        for i in range(9):
            line = ___reader___.__next__()
            yield list([line[f] for f in line])
    return "tab", list(___reader___.fieldnames), list(___line_generator___()), None, None
    "#}
    test! {csv_tab1e_rows5, "@csv_table: src=\"some/path/idk.csv\", rows=5..=100", r#"
import csv
with open("some/path/idk.csv") as ___csv_file___:
    ___reader___ = csv.DictReader(___csv_file___, delimiter=',', skipinitialspace=True, strict=True)
    for i in range(4):
        ___reader___.__next__()
    def ___line_generator___():
        for i in range(95):
            line = ___reader___.__next__()
            yield list([line[f] for f in line])
    return "tab", list(___reader___.fieldnames), list(___line_generator___()), None, None
    "#}
    test! {csv_tab1e_rows6, "@csv_table: src=\"some/path/idk.csv\", rows=1..100", r#"
import csv
with open("some/path/idk.csv") as ___csv_file___:
    ___reader___ = csv.DictReader(___csv_file___, delimiter=',', skipinitialspace=True, strict=True)
    def ___line_generator___():
        for i in range(98):
            line = ___reader___.__next__()
            yield list([line[f] for f in line])
    return "tab", list(___reader___.fieldnames), list(___line_generator___()), None, None
    "#}
    test! {csv_tab1e_bad_rows, "@csv_table: src=\"some/path/idk.csv\", rows=10=..", -,
    SyntaxError::RangeParse(GenericRangeParseError::Format)}
    test! {csv_tab1e_columns1, r#"@csv_table: src="some/path/idk.csv", columns = []"#, r#"
import csv
with open("some/path/idk.csv") as ___csv_file___:
    ___reader___ = csv.DictReader(___csv_file___, delimiter=',', skipinitialspace=True, strict=True)
    return "tab", [], list([[] for line in ___reader___]), None, None
    "#}
    test! {csv_tab1e_columns2, r#"@csv_table: src="some/path/idk.csv", columns = ["a"]"#, r#"
import csv
with open("some/path/idk.csv") as ___csv_file___:
    ___reader___ = csv.DictReader(___csv_file___, delimiter=',', skipinitialspace=True, strict=True)
    return "tab", ["a", ], list([[line["a"], ] for line in ___reader___]), None, None
    "#}
    test! {csv_tab1e_columns3, r#"@csv_table: src="some/path/idk.csv", columns = ["a", ("b", "b_err")]"#, r#"
import csv
with open("some/path/idk.csv") as ___csv_file___:
    ___reader___ = csv.DictReader(___csv_file___, delimiter=',', skipinitialspace=True, strict=True)
    return "tab", ["a", "b", ], list([[line["a"], ("err", line["b"], line["b_err"]), ] for line in ___reader___]), None, None
    "#}
    test! {csv_table_combined1, r#"@csv_table: src="some/path/idk.csv", columns = ["a", ("b", "b_err")], rows =..10"#, r#"
import csv
with open("some/path/idk.csv") as ___csv_file___:
    ___reader___ = csv.DictReader(___csv_file___, delimiter=',', skipinitialspace=True, strict=True)
    def ___line_generator___():
        for i in range(9):
            line = ___reader___.__next__()
            yield [line["a"], ("err", line["b"], line["b_err"]), ]
    return "tab", ["a", "b", ], list(___line_generator___()), None, None
    "#}
    test! {csv_table_combined2, r#"@csv_table: src="some/path/idk.csv", columns = ["a", ("b", "b_err")], rows =10..=100"#, r#"
import csv
with open("some/path/idk.csv") as ___csv_file___:
    ___reader___ = csv.DictReader(___csv_file___, delimiter=',', skipinitialspace=True, strict=True)
    for i in range(9):
        ___reader___.__next__()
    def ___line_generator___():
        for i in range(90):
            line = ___reader___.__next__()
            yield [line["a"], ("err", line["b"], line["b_err"]), ]
    return "tab", ["a", "b", ], list(___line_generator___()), None, None
    "#}
    test! {csv_table_combined3, r#"@csv_table: src="some/path/idk.csv", columns = ["a", ("b", "b_err")], rows =10..100"#, r#"
import csv
with open("some/path/idk.csv") as ___csv_file___:
    ___reader___ = csv.DictReader(___csv_file___, delimiter=',', skipinitialspace=True, strict=True)
    for i in range(9):
        ___reader___.__next__()
    def ___line_generator___():
        for i in range(89):
            line = ___reader___.__next__()
            yield [line["a"], ("err", line["b"], line["b_err"]), ]
    return "tab", ["a", "b", ], list(___line_generator___()), None, None
    "#}

    test! {gen_table1, r#"@gen_table: lambda row, col: 1; rows= 5, columns =2"#,
r#"___generator___ = lambda row, col: 1
return "tab", [[___generator___(0, 0), ___generator___(0, 1), ], [___generator___(1, 0), ___generator___(1, 1), ], [___generator___(2, 0), ___generator___(2, 1), ], [___generator___(3, 0), ___generator___(3, 1), ], [___generator___(4, 0), ___generator___(4, 1), ], ], None, None"#}
    test! {gen_table2, r#"@gen_table: lambda row, col: -1;columns =2, rows= 5"#,
r#"___generator___ = lambda row, col: -1
return "tab", [[___generator___(0, 0), ___generator___(0, 1), ], [___generator___(1, 0), ___generator___(1, 1), ], [___generator___(2, 0), ___generator___(2, 1), ], [___generator___(3, 0), ___generator___(3, 1), ], [___generator___(4, 0), ___generator___(4, 1), ], ], None, None"#}
}

#[derive(Debug, PartialEq, thiserror::Error)]
pub enum ParsingError {
    #[error(transparent)]
    Float(ParseFloatError),
    #[error("Python could not generate output. str: {}, error: {:?}", .0, .1)]
    UnexpectedSyntax(String, Equivalent<PyErr>),
    #[error("Table syntax error: no header")]
    TableNoHeader,
    #[error(transparent)]
    Token(KyomatoLexError),
}

impl From<ParseFloatError> for ParsingError {
    fn from(value: ParseFloatError) -> Self {
        Self::Float(value)
    }
}

impl From<nom::Err<KyomatoLexError>> for ParsingError {
    fn from(value: nom::Err<KyomatoLexError>) -> Self {
        match value {
            nom::Err::Incomplete(_) => unreachable!("Input is always complete here"),
            nom::Err::Error(err) | nom::Err::Failure(err) => Self::Token(err),
        }
    }
}

fn parse_ayano(python_output: &PyAny) -> Result<Token<'static>, ParsingError> {
    // it was quite a journey! but I believe to be close to it's end.
    if let Ok(tuple) = python_output.downcast::<PyTuple>() {
        if let Ok(discriminator) = tuple
            .get_item(0)
            .and_then(|e| e.str())
            .and_then(|s| s.to_str())
        {
            macro_rules! throw_syntax {
                ($input:expr) => {
                    $input.map_err(|err| {
                        ParsingError::UnexpectedSyntax(
                            python_output
                                .str()
                                .map(|s| s.to_string())
                                .unwrap_or(String::from("<NON-STR OBJECT>")),
                            err.into(),
                        )
                    })
                };
            }

            macro_rules! throw_downcast {
                ($input:expr) => {
                    $input.map_err(|err| {
                        ParsingError::UnexpectedSyntax(
                            python_output
                                .str()
                                .map(|s| s.to_string())
                                .unwrap_or(String::from("<NON-STR OBJECT>")),
                            PyErr::from(err).into(),
                        )
                    })
                };
            }
            match discriminator {
                "err" => {
                    // this is a value-error syntax.
                    // any inconsistency will be treated as syntax error from now on.
                    // expected syntax: ('err', <value>, <error>)
                    let value = throw_syntax!(tuple
                        .get_item(1)
                        .and_then(|item| item.str())
                        .and_then(|s| s.to_str()))?;
                    let error = throw_syntax!(tuple
                        .get_item(2)
                        .and_then(|item| item.str())
                        .and_then(|s| s.to_str()))?;
                    return Ok(value_error_format(value, error)?);
                }
                "fig" => {
                    // this is a figure syntax.
                    // any inconsistency will be treated as syntax error from now on.
                    // expected syntax: ('fig', <src>, <ident>, <caption>)
                    let src: &str = throw_syntax!(tuple
                        .get_item(1)
                        .and_then(|item| item.str())
                        .and_then(|s| s.to_str()))?;
                    let ident = throw_syntax!(tuple.get_item(2))?;
                    let ident = (!ident.is_none())
                        .then_some(throw_syntax!(ident.str().and_then(|s| s.to_str()))?);
                    let caption = throw_syntax!(tuple.get_item(3))?;
                    let caption = if caption.is_none() {
                        None
                    } else {
                        let caption_str = throw_syntax!(caption.str().and_then(|s| s.to_str()))?;
                        let (_, caption_token) =
                            crate::lexer::inner_lex::<KyomatoLexError>(caption_str)?;
                        Some(Box::new(caption_token.to_static_token()))
                    };
                    let width = throw_syntax!(tuple.get_item(4))?;
                    let width = if width.is_none() {
                        None
                    } else {
                        let width_str = throw_syntax!(width.str().and_then(|s| s.to_str()))?;
                        Some(width_str.parse()?)
                    };
                    return Ok(Token::Figure {
                        src_name: Cow::Owned(PathBuf::from(src)),
                        caption,
                        ident: ident.map(|i| Cow::Owned(i.to_string())),
                        width,
                    });
                }
                "tab" => {
                    // this is a table syntax.
                    // any inconsistency will be treated as syntax error from now on.
                    // expected syntax: ('tab', [$([$(<table-cell>), +]), +], 'ident'|None, 'caption'|None)

                    let list =
                        throw_downcast!(throw_syntax!(tuple.get_item(1))?.downcast::<PyList>())?;
                    let mut rows = list.into_iter();
                    let header: Vec<_> = throw_downcast!(rows
                        .next()
                        .ok_or(ParsingError::TableNoHeader)?
                        .downcast::<PyList>())?
                    .into_iter()
                    .map(|h: &PyAny| parse_ayano(h))
                    .try_collect()?;
                    let cells: Vec<_> = rows
                        .map(|row| {
                            Ok(throw_downcast!(row.downcast::<PyList>())?
                                .into_iter()
                                .map(|c: &PyAny| parse_ayano(c)))
                        })
                        .try_fold(Vec::new(), |acc, next: Result<_, ParsingError>| {
                            // OLD: there might be a better way with iterators directly, but I kinda can't come up with it
                            // UPD: this looks... BAD
                            // but I guess that's iterators all the way
                            next?.try_fold(acc, |mut acc, next| {
                                acc.push(next?);
                                Ok::<_, ParsingError>(acc)
                            })
                        })?;

                    let ident = throw_syntax!(tuple.get_item(2))?;
                    let ident = (!ident.is_none())
                        .then_some(throw_syntax!(ident.str().and_then(|s| s.to_str()))?);
                    let caption = throw_syntax!(tuple.get_item(3))?;
                    let caption = if caption.is_none() {
                        None
                    } else {
                        let caption_str = throw_syntax!(caption.str().and_then(|s| s.to_str()))?;
                        let (_, caption_token) =
                            crate::lexer::inner_lex::<KyomatoLexError>(caption_str)?;
                        Some(Box::new(caption_token.to_static_token()))
                    };
                    return Ok(Token::Table {
                        header: Tokens::new(header),
                        cells: Tokens::new(cells),
                        ident: ident.map(|s| Cow::Owned(s.to_string())),
                        caption,
                    });
                }
                _ => {}
            }
        }
    }
    // any sort of parsing had failed - return regular text
    Ok(Token::Paragraph {
        is_newline: false,
        formatting: None,
        space_before: todo!(),
        content: Cow::Owned(python_output.to_string()),
    })
}

#[cfg(test)]
fn value_error_token(value: f64, error: f64) -> Token<'static> {
    Token::Multiple {
        tokens: Tokens::new([
            Token::text(value.to_string()),
            Token::InlineMath {
                content: r"\pm".into(),
                space_before: false,
            },
            Token::text(error.to_string()),
        ]),
    }
}

#[cfg(test)]
mod parsing_tests {

    use super::*;
    use pyo3::{
        exceptions::PyTypeError,
        types::{PyFloat, PyList, PyString, PyTuple},
        IntoPy, PyErr,
    };

    use crate::data::{Token, Tokens};

    macro_rules! test {
        ($name:ident, $input:expr, $output:expr) => {
            #[test]
            fn $name() {
                super::Python::with_gil(|py| {
                    // arrange
                    let python_output = $input(py);

                    // act
                    let result = super::parse_ayano(python_output);

                    // assert
                    assert_eq!(result, $output, "Parsing should produce expected tokens");
                });
            }
        };
    }

    macro_rules! py_err {
        ($py:ident) => {
            PyErr::new::<PyTypeError, _>("Error message").into()
        };
    }

    test! {text1, |py| PyString::new(py, "Some text".into()), Ok(Token::text("Some text"))}
    test! {text2, |py| PyFloat::new(py, 0.0025), Ok(Token::text("0.0025"))}
    test! {text3, |py| PyList::new(py, [1, 2, 3]), Ok(Token::text("[1, 2, 3]"))}
    test! {text4, |py| PyTuple::new(py, ["lol", "some_info"]), Ok(Token::text("('lol', 'some_info')"))}
    // TODO add tests with lexer
    test! {err1, |py| PyTuple::new(py, ["err".into_py(py), ["yet another not a number"].into_py(py)]),
    Err(crate::gen::ayano::ParsingError::UnexpectedSyntax("('err', ['yet another not a number'])".to_string(), py_err!(py)))}
    test! {value_error1, |py| PyTuple::new(py, ["err".into_py(py), 1.0.into_py(py), 0.5.into_py(py)]),
    Ok(Token::Multiple{tokens: Tokens::new([Token::text("1.0"), Token::InlineMath{content:r"\pm".into(), space_before: false}, Token::text("0.5")])})}
    test! {value_error2, |py| PyTuple::new(py, ["err".into_py(py), "1.0".into_py(py), 0.5.into_py(py)]),
    Ok(Token::Multiple{tokens:Tokens::new([Token::text("1.0"), Token::InlineMath{content:r"\pm".into(), space_before: false}, Token::text("0.5")])})}
    test! {value_error_format1, |py| PyTuple::new(py, ["err".into_py(py), "1.987654321".into_py(py), 0.00000123456.into_py(py)]),
    Ok(value_error_token(1.9876543, 0.0000012))}
    // TODO probably should add some more tests for value-error formatting

    test! {figure_nocaption1, |py| PyTuple::new(py, ["fig".into_py(py), "path/to/file.png".into_py(py), "circle".into_py(py), None::<()>.into_py(py), None::<f32>.into_py(py)]),
    Ok(Token::Figure {
        src_name: Cow::Owned(PathBuf::from("path/to/file.png")),
        caption: None,
        ident: Some("circle".into()),
        width: None,
    })}
    test! {figure_caption, |py| PyTuple::new(py, ["fig".into_py(py), "path/to/file.png".into_py(py), "circle".into_py(py), "figure caption".into_py(py), None::<f32>.into_py(py)]),
    Ok(Token::Figure {
        src_name: Cow::Owned(PathBuf::from("path/to/file.png")),
        caption: Some(Box::new(Token::text("figure caption"))),
        ident: Some("circle".into()),
        width: None,
    })}
    test! {figure_width, |py| PyTuple::new(py, ["fig".into_py(py), "path/to/file.png".into_py(py), "circle".into_py(py), "figure caption".into_py(py), 0.625.into_py(py)]),
    Ok(Token::Figure {
        src_name: Cow::Owned(PathBuf::from("path/to/file.png")),
        caption: Some(Box::new(Token::text("figure caption"))),
        ident: Some("circle".into()),
        width: Some(0.625),
    })}

    test! {table_nocaption, |py| {
        PyTuple::new(py, [
        "tab".into_py(py),
        PyList::new(py, [
            PyList::new(py, ["header1".into_py(py), 1.0.into_py(py)]),
            PyList::new(py, ["cell_11".into_py(py), 0.25.into_py(py)]),
            PyList::new(py, ["cell_21".into_py(py), (-2).into_py(py)]),
        ]).into_py(py),
        "table1".into_py(py),
        None::<()>.into_py(py),
        ])},
        Ok(Token::Table {
            header: Tokens::new([Token::text("header1"), Token::text("1.0")]),
            cells: Tokens::new([
                Token::text("cell_11"), Token::text("0.25"),
                Token::text("cell_21"), Token::text("-2"),
            ]),
            caption: None,
            ident: Some("table1".into())
        })
    }
    test! {table_value_error, |py| {
        PyTuple::new(py, [
        "tab".into_py(py),
        PyList::new(py, [
            PyList::new(py, ["header1".into_py(py), 1.0.into_py(py)]),
            PyList::new(py, [PyTuple::new(py, [
                "err".into_py(py),
                1.012345.into_py(py),
                "0.25".into_py(py)
                    ]).into_py(py), 0.25.into_py(py)]),
            PyList::new(py, ["cell_21".into_py(py), (-2).into_py(py)]),
        ]).into_py(py),
        "table1".into_py(py),
        None::<()>.into_py(py),
        ])},
        Ok(Token::Table {
            header: Tokens::new([Token::text("header1"), Token::text("1.0")]),
            cells: Tokens::new([
                value_error_token(1.01, 0.25), Token::text("0.25"),
                Token::text("cell_21"), Token::text("-2"),
            ]),
            caption: None,
            ident: Some("table1".into())
        })
    }
    test! {table_caption, |py| {
        PyTuple::new(py, [
        <&'static str as IntoPy<pyo3::Py<PyAny>>>::into_py("tab", py),
        PyList::new(py, [
            PyList::new(py, ["header1".into_py(py), 1.0.into_py(py)]),
            PyList::new(py, ["cell_11".into_py(py), 0.25.into_py(py)]),
            PyList::new(py, ["cell_21".into_py(py), (-2).into_py(py)]),
        ]).into_py(py),
        "table1".into_py(py),
        "table caption with some $formula$".into_py(py),
        ])},
        Ok(Token::Table {
            header: Tokens::new([Token::text("header1"), Token::text("1.0")]),
            cells: Tokens::new([
                Token::text("cell_11"), Token::text("0.25"),
                Token::text("cell_21"), Token::text("-2"),
            ]),
            caption: Some(Box::new(Token::Multiple{tokens:Tokens::new([Token::text("table caption with some"),Token::InlineMath{content:"formula".into(), space_before: false}])})),
            ident: Some("table1".into())
        })
    }
}

struct PrimitiveDecimal {
    digits: Vec<char>,
    zero: isize,
    sign: bool,
}

impl FromStr for PrimitiveDecimal {
    type Err = ParseFloatError;

    fn from_str(mut s: &str) -> Result<Self, Self::Err> {
        let _: f64 = s.parse()?;
        let sign = if s.starts_with('+') {
            false
        } else if s.starts_with('-') {
            true
        } else {
            false
        };
        s = s.trim_start_matches(['+', '-']);
        let (s, exponent) = if let Some(split) = s.split_once(['e', 'E']) {
            split
        } else {
            (s, "0")
        };
        let (integer, fraction) = if let Some(split) = s.split_once('.') {
            split
        } else {
            (s, "")
        };
        let zero = (isize::try_from(integer.len()).expect("Why.") - 1).try_into().expect("How in the world did you get 2B digits into the integer part? Things are certainly getting out of hand");
        let digits = integer.chars().chain(fraction.chars()).collect_vec();
        let mut res = Self { zero, digits, sign };

        // Account for exponent
        let exponent: isize = exponent
            .parse()
            .expect("Should be able to parse exponent, as initial input is correct"); // TODO ensure there are tests to check that in detail
        res.apply_exponent(exponent);
        Ok(res)
    }
}

impl ToString for PrimitiveDecimal {
    fn to_string(&self) -> String {
        let len = self.digits.len();
        let (integer_part, fraction_part) = if self.zero < 0 {
            let additinal: usize = (-self.zero - 1).try_into().expect("Why.?");
            // All the digits are in the fraction part :)
            (
                String::new(),
                repeat_n(&'0', additinal)
                    .chain(self.digits.iter())
                    .collect(),
            )
        } else {
            let zero = self.zero as usize;
            if zero + 1 >= len {
                // All the digits are in the integer part :)
                (
                    self.digits
                        .iter()
                        .chain(repeat_n(&'0', zero + 1 - len))
                        .collect(),
                    String::new(),
                )
            } else {
                let (integer, fraction) = self.digits.split_at(zero + 1);
                (
                    integer.into_iter().collect(),
                    fraction.into_iter().collect(),
                )
            }
        };
        let sign = if self.sign { "-" } else { "" };
        format!(
            "{sign}{}",
            match (integer_part.is_empty(), fraction_part.is_empty()) {
                (true, true) => String::from("0"),
                (true, false) => format!("0.{}", fraction_part),
                (false, true) => integer_part,
                (false, false) => format!("{}.{}", integer_part, fraction_part),
            }
        )
    }
}

impl PrimitiveDecimal {
    pub fn first_digit(&self) -> isize {
        self.digits
            .iter()
            .position(|ch| ch != &'0')
            .map(|pos| self.zero.wrapping_sub_unsigned(pos))
            .unwrap_or(isize::MIN)
    }

    pub fn get_digit(&self, digit: isize) -> char {
        let index = self.zero.wrapping_sub(digit);
        (index >= 0)
            .then_some(self.digits.iter().nth(index as usize).cloned())
            .flatten()
            .unwrap_or('0')
    }

    fn set_digit(&mut self, digit: isize, value: char) {
        #[cfg(test)]
        assert!(
            matches!(value, '0'..='9'),
            "New digit's value should be a valid decimal digit"
        );

        let index = self.zero.saturating_sub(digit);
        let len = self.digits.len();
        if index < 0 {
            let fillers: usize = (-index - 1).try_into().expect("Why.?");
            for _ in 0..fillers {
                self.digits.insert(0, '0');
            }
            self.zero -= index;
            self.digits.insert(0, value);
        } else {
            let index = index as usize;
            if index >= len {
                self.digits
                    .extend(repeat_n('0', index - len).chain(std::iter::once(value)));
            } else {
                // It's ok to just set the digit, lol
                self.digits[index] = value;
            }
        }
    }

    pub fn truncate_zeroes(&mut self) {
        if let Some(first_nonzero) = self.digits.iter().position(|ch| ch != &'0') {
            // Move zero pointer
            self.zero = self.zero.wrapping_sub_unsigned(first_nonzero);
            // Remove leading zeroes
            let _ = self.digits.drain(..first_nonzero);
            // Remove trailing zeroes
            let last_nonzero = self
                .digits
                .iter()
                .rposition(|ch| ch != &'0')
                .expect("There are non-zero digits not, for sure");
            if last_nonzero != self.digits.len() - 1 {
                let _ = self.digits.drain((last_nonzero + 1)..);
            }
        } else {
            self.digits.clear();
            self.zero = 0;
        }
    }

    pub fn apply_exponent(&mut self, exponent: isize) {
        self.zero += exponent;
    }

    pub fn round_to_digit(&mut self, digit: isize) {
        self.apply_exponent(-digit);

        // Round the way fractional part turns into zero
        if matches!(self.get_digit(-1), '5'..='9') {
            // Must perform rounding, I guess
            // While current digit is 9, set it to 0, and go up even more
            let mut tgt = 0;
            while self.get_digit(tgt) == '9' {
                self.set_digit(tgt, '0');
                tgt += 1;
            }
            // Once digit is not 9, go up by one
            self.set_digit(
                tgt,
                match self.get_digit(tgt) {
                    '0' => '1',
                    '1' => '2',
                    '2' => '3',
                    '3' => '4',
                    '4' => '5',
                    '5' => '6',
                    '6' => '7',
                    '7' => '8',
                    '8' => '9',
                    _ => unreachable!(),
                },
            )
        }

        // Lastly, discard the fractional part
        if self.zero > -2 {
            // fractional part is not the entire value
            if self.zero + 2 <= self.digits.len().try_into().expect("Wha...?") {
                // ... and is actually present in the chars array
                let _ = self.digits.drain(((self.zero + 1) as usize)..);
            }
        } else {
            // fractional part is the only thing left, discard the entire value
            self.digits.clear();
            self.zero = 0;
        }

        // .. and extend chars to the last rounding digit, if rounding is performed up to a fractional part digit
        if digit < 0 {
            self.set_digit(0, self.get_digit(0)); // yeah, this does the trick
        }

        self.apply_exponent(digit);
    }

    pub fn unsign(&mut self) {
        self.sign = false;
    }
}

#[cfg(test)]
mod primitive_decimal_tests {
    mod parsing {
        use super::super::*;

        macro_rules! test {
            {$name:ident, $input:literal, $digits:literal, $zero:literal} => {
                test!{$name, $input, $digits, $zero, +}
            };
            {$name:ident, $input:literal, $digits:literal, $zero:literal, +} => {
                test!{$name, $input, $digits, $zero, false}
            };
            {$name:ident, $input:literal, $digits:literal, $zero:literal, -} => {
                test!{$name, $input, $digits, $zero,  true}
            };
            {$name:ident, $input:literal, $digits:literal, $zero:literal, $sign:literal} => {
                #[test]
                fn $name() {
                    // arrange
                    let expected_digits = $digits.chars().collect_vec();

                    // act
                    let decimal: PrimitiveDecimal = $input.parse().expect("Should be able to parse valid float");

                    // assert
                    let PrimitiveDecimal { digits, zero, sign } = decimal;
                    assert_eq!(digits, expected_digits, "Digit vectors should be the same");
                    assert_eq!(zero, $zero, "Zero positions should be the same");
                    assert_eq!(sign, $sign, "Signs should be the same");
                }
            };
        }

        test! {zero_pos1, "0.0", "00", 0}
        test! {zero_pos2, "0.", "0", 0}
        test! {zero_pos3, ".000000", "000000", -1}
        test! {zero_pos4, "000000000", "000000000", 8}
        test! {zero_pos5, ".0", "0", -1}
        test! {zero_pos6, ".0", "0", -1}
        test! {zero_pos7, ".0", "0", -1}
        test! {zero_pos8, ".0", "0", -1}
        test! {sign1, "+1.0000", "10000", 0, +}
        test! {sign2, "-000.000", "000000", 2, -}
        test! {digits1, ".123456789", "123456789", -1}
        test! {digits2, "1.23456789", "123456789", 0}
        test! {digits3, "123456.789", "123456789", 5}
        test! {digits4, "123456789000", "123456789000", 11}
        test! {exponent1, "0e0", "0", 0}
        test! {exponent2, "0E0", "0", 0}
        test! {exponent3, "1E1", "1", 1}
        test! {exponent4, "1E-1", "1", -1}
        test! {exponent5, "1E-3", "1", -3}
        test! {exponent6, "-0001.001E-1", "0001001", 2, -}
        test! {exponent7, "-0001.001E10", "0001001", 13, -}
    }

    mod to_string {
        use super::super::*;

        macro_rules! test_noop {
            {$name:ident, $input:literal, $output:literal} => {
                #[test]
                fn $name() {
                    // arrange
                    let decimal: PrimitiveDecimal = $input.parse().expect("Should be able to parse");

                    // act
                    let output = decimal.to_string();

                    // assert
                    assert_eq!(output, $output, "Should produce expected result");
                }
            };
        }

        test_noop! {sign1, "0.0", "0.0"}
        test_noop! {sign2, "-0.0", "-0.0"}
        test_noop! {missing_decimal1, ".0", "0.0"}
        test_noop! {missing_decimal2, ".1234", "0.1234"}
        test_noop! {missing_decimal3, "1E-3", "0.001"}
        test_noop! {missing_fraction1, "12345", "12345"}
        test_noop! {missing_fraction2, "-0123", "-0123"}
        test_noop! {missing_fraction3, "0.1E2", "010"}
        test_noop! {missing_fraction4, "0.1E5", "010000"}

        macro_rules! test_truncate {
            {$name:ident, $input:literal, $output:literal} => {
                #[test]
                fn $name() {
                    // arrange
                    let mut decimal: PrimitiveDecimal = $input.parse().expect("Should be able to parse");
                    decimal.truncate_zeroes();

                    // act
                    let output = decimal.to_string();

                    // assert
                    assert_eq!(output, $output, "Should produce expected result");
                }
            };
        }

        test_truncate! {zero1, "0", "0"}
        test_truncate! {zero2, "0.0", "0"}
        test_truncate! {zero3, ".0", "0"}
        test_truncate! {zero4, "0.0e-100", "0"}
        test_truncate! {zero5, ".00000000000E50", "0"}
        test_truncate! {fraction1, "1.00000000", "1"}
        test_truncate! {fraction2, "123.1234000", "123.1234"}
        test_truncate! {fraction3, "-12.3400000000", "-12.34"}
        test_truncate! {fraction4, "-1.0", "-1"}
        test_truncate! {fraction5, "2.30", "2.3"}
        test_truncate! {fraction6, "3.0E1", "30"}
        test_truncate! {fraction7, "123.450000E2", "12345"}
        test_truncate! {fraction8, "-0.3400E1", "-3.4"}
        test_truncate! {integer1, "000012", "12"}
        test_truncate! {integer2, "012", "12"}
        test_truncate! {integer3, "-00123.45E3", "-123450"}
        test_truncate! {integer4, "001234.5E-2", "12.345"}
        test_truncate! {trim1, "01234.5670000", "1234.567"}
        test_truncate! {trim2, "-0001.0000", "-1"}
        test_truncate! {trim3, "-000.000", "-0"}
    }

    mod first_digit {
        use super::super::*;

        macro_rules! test {
            {$name:ident, $input:literal, $first_digit:expr} => {
                #[test]
                fn $name() {
                    // arrange
                    let mut decimal: PrimitiveDecimal = $input.parse().expect("Should be able to parse");
                    decimal.truncate_zeroes();

                    // act
                    let output = decimal.first_digit();

                    // assert
                    assert_eq!(output, $first_digit, "Should produce expected result");
                }
            };
        }

        test! {zero1, "0", isize::MIN}
        test! {zero2, "-0.00", isize::MIN}
        test! {zero3, "000.0000", isize::MIN}
        test! {one1, "-00001.00000", 0}
        test! {one2, "00010.00000E-1", 0}
        test! {one3, "-0.0001e4", 0}
        test! {test1, "123.000", 2}
        test! {test2, "0000123.30953", 2}
        test! {test3, "0000123.30953E4", 6}
        test! {test4, "0000123.30953E-10", -8}
        test! {test5, "0.1", -1}
        test! {test6, "0000000.0002e-0", -4}
    }

    mod get_digit {
        use super::super::*;

        macro_rules! test {
            {$name:ident, $input:literal, $range:expr, $output:literal} => {
                #[test]
                fn $name() {
                    // arrange
                    let decimal: PrimitiveDecimal = $input.parse().expect("Should be able to parse");

                    // act
                    let res = ($range).rev().into_iter().map(|i| decimal.get_digit(i));

                    // assert
                    assert_eq!(res.collect_vec(), $output.chars().collect_vec(), "Should produce expected output")
                }
            };
        }

        test! {test1, "1.01", -3..=1, "01010"}
        test! {test2, "-00012345.0001100", -7..=7, "000123450001100"}
        test! {test3, "-00012345.0001100e2", -7..=7, "012345000110000"}
    }

    mod set_digit {
        use super::super::*;

        macro_rules! test {
            {$name:ident, $input:literal, $digit:literal, $value:literal, $output:literal} => {
                #[test]
                fn $name() {
                    // arrange
                    let mut decimal: PrimitiveDecimal = $input.parse().expect("Should be able to parse");

                    // act
                    decimal.set_digit($digit, $value);
                    let output = decimal.to_string();

                    // assert
                    assert_eq!(output, $output, "Should produce expected output")
                }
            };
        }

        test! {test1, "0.0", 0, '1', "1.0"}
        test! {test2, "0.0", 10, '1', "10000000000.0"}
        test! {test3, "-100", 2, '2', "-200"}
        test! {test4, "1", -1, '1', "1.1"}
        test! {test5, "1e4", -10, '1', "10000.0000000001"}
        test! {test6, "1e-5", 4, '2', "20000.00001"}
        test! {test7, "-6543210.023456", -1, '1', "-6543210.123456"}
    }

    mod round {
        use std::ops::RangeInclusive;

        use rand::{random, seq::IteratorRandom};

        use super::super::*;

        macro_rules! test {
            {$name:ident, $input:literal, $digit:literal, $output:literal} => {
                #[test]
                fn $name() {
                    // arrange
                    let mut decimal: PrimitiveDecimal = $input.parse().expect("Should be able to parse");

                    // act
                    decimal.round_to_digit($digit);
                    let output = decimal.to_string();

                    // assert
                    assert_eq!(output, $output, "Should produce expected output")
                }
            };
        }

        test! {zero1, "0", 0, "0"}
        test! {zero2, "-0", 0, "-0"}
        test! {zero3, "00.0000", 0, "00"}
        test! {zero4, "0", 1, "0"}
        test! {zero5, "0e-5", 0, "0"}
        test! {test1, "1.1", 0, "1"}
        test! {test2, "1.5", 0, "2"}
        test! {test3, "9.5", 0, "10"}
        test! {test4, "9999.5", 0, "10000"}
        test! {test5, "8999.5", 0, "9000"}
        test! {test6, "1.23", -2, "1.23"}
        test! {test7, "1.23", -3, "1.230"}
        test! {test8, "-1.23", -5, "-1.23000"}
        test! {test9, "1.23999", -4, "1.2400"}

        #[test]
        fn fuzz() {
            const TOTAL_TESTS: usize = 1_000_000;
            const MAX_ABS: f64 = 1E6f64;
            const EXP_PROB: f64 = 0.2f64;
            const DIGIT_RANGE: RangeInclusive<isize> = -10..=10;
            let mut dev_sum = 0.0;
            for _ in 0..TOTAL_TESTS {
                // arrange
                let number: f64 = (random::<f64>() - 0.5f64) * 2.0f64 * MAX_ABS;
                let string_repr = if random::<f64>() < EXP_PROB {
                    format!("{number:e}")
                } else {
                    number.to_string()
                };
                let mut decimal: PrimitiveDecimal = string_repr.parse().expect(
                    "Should be able to parse, as this string was just generated by Rust, lol",
                );
                let digit = DIGIT_RANGE
                    .choose(&mut rand::thread_rng())
                    .expect("Range is not empty");

                // act
                decimal.round_to_digit(digit);

                // assert
                let rounded: f64 = decimal
                    .to_string()
                    .parse()
                    .expect("Should be able to parse, as decimal value generates correct strings");
                // after correct rounding, result must differ by no more than value of a corresponding digit
                let diff = (number - rounded).abs();
                let ok_diff = 10.0f64.powi(digit.try_into().expect("Huh."));
                assert!(diff <= ok_diff, "Difference is larger than expected for number = {number} rounding to digit = {digit}. Rounding result: {rounded}, ok difference: {ok_diff}, actual difference: {diff}");
                dev_sum += diff / ok_diff;
            }
            println!("Average dev: {:.4}%", dev_sum / (TOTAL_TESTS as f64));
        }
    }
}

pub fn value_error_format<'v, 'e>(
    value: impl Into<Cow<'v, str>>,
    error: impl Into<Cow<'e, str>>,
) -> Result<Token<'static>, ParsingError> {
    let (value, error) = value_error_format_inner(value.into().as_ref(), error.into().as_ref())?;
    // Result is a combination of text tokens with mathmode +- in between
    Ok(Token::Multiple {
        tokens: Tokens::new([
            Token::Paragraph {
                is_newline: false,
                formatting: None,
                content: value.into(),
                space_before: false,
            },
            Token::InlineMath {
                content: r"\pm".into(),
                space_before: false,
            },
            Token::Paragraph {
                is_newline: false,
                formatting: None,
                content: error.into(),
                space_before: false,
            },
        ]),
    })
}

fn value_error_format_inner<'v, 'e>(
    value: &'v str,
    error: &'e str,
) -> Result<(String, String), ParsingError> {
    // first, parse values into primitive decimals, for easier manipulation
    let mut value: PrimitiveDecimal = value.parse()?;
    let mut error: PrimitiveDecimal = error.parse()?;

    // Trim zeroes (just in case)
    value.truncate_zeroes();
    error.truncate_zeroes();

    // Get error's first non-zero digit
    let mut round_digit = error.first_digit();
    if matches!(error.get_digit(round_digit), '1'..='2') {
        // If first digit's value is 1-3, round to one more digit behind it
        round_digit -= 1;
    }

    // Lastly, round value and error to the same digit
    value.round_to_digit(round_digit);
    error.round_to_digit(round_digit);

    Ok((value.to_string(), error.to_string()))
}

#[cfg(test)]
mod value_error_tests {
    use super::*;
    macro_rules! test {
        {$name:ident, [$value:literal, $error:literal] => [$output_value:literal, $output_error:literal]} => {
            #[test]
            fn $name() {
                // act
                let (output_value, output_error) = value_error_format_inner($value, $error).expect("Should be able to parse");

                // assert
                assert_eq!(output_value, $output_value, "Values should be equal");
                assert_eq!(output_error, $output_error, "Errors should be equal");
            }
        };
    }

    // These are examples from docs
    test! {doc1, ["5", "0.1"] => ["5.00", "0.10"]}
    test! {doc2, ["12252", "54"] => ["12250", "50"]}
    test! {doc3, ["1232", "12"] => ["1232", "12"]}

    // These are examples from place I actually get to know about this formatting (these are hidden in the code anyway, so I guess credit is optional here)
    test! {src1, ["77.567439", "4.8561133"] => ["78", "5"]}
    test! {src2, ["1.3354792", "0.02417797"] => ["1.335", "0.024"]}
    test! {src3, ["157863.44", "377.36711"] => ["157900", "400"]}
    test! {src4, ["0.1437765", "0.019734"] => ["0.144", "0.020"]}
}

fn find_args<'l>(input: &'l str) -> impl IntoIterator<Item = (&'l str, &'l str)> {
    // I sincerely tried to use regex here, but apparently, I'm too dumb to actually figure this out
    // my final guess: r#"\s*([0-9A-Za-z_]+)\s*=\s*((["'].*[^\\]["'])|([^["=,\s\[\]]][^[",\s\[\]]]*)|(\[.*\]))"#
    // Here is a state machine instead:
    // States:
    // - (1) Normal (start state, final state)
    // - (2) Inside name
    // - (3) Before equals
    // - (4) After equals
    // - (5) Inside value(quotes)
    // - (6) Before comma (final state)
    // Transitions:
    // (1) + ' ' -> (1)
    // (1) + [a-zA-Z] -> (2)
    // (2) + [a-zA-z0-9] -> (2)
    // (2) + ' ' -> (3)
    // (2) + '=' -> (4)
    // (3) + ' ' -> (3)
    // (3) + '=' -> (4)
    // (4) + ' ' -> (4)
    // (4) + ^' ' -> (5)()
    // (5)(q) + "\"" -> (5)(q)
    // (5)(q) + "\'" -> (5)(q)
    // (5)() + ["'] -> (5)(["'])
    // (5)(["']) + ["'] -> (5)()
    // (5)(["']) + ['"] -> (5)(["'])
    // (5)() + ' '|',' -> (6)
    // (6) + ' ' -> (6)
    // (6) + ',' -> (1)
    enum State<'l> {
        Initial,    // (1)
        InsideName, // (2)
        BeforeEquals {
            name: &'l str,
        }, // (3)
        AfterEquals {
            name: &'l str,
        }, // (4)
        InsideValue {
            name: &'l str,
            value_rest: &'l str,
            len: usize,
            quote: Option<Quote>,
        }, // (5)
        BeforeComa, // (6)
    }
    #[derive(Debug, PartialEq)]
    enum Quote {
        Single,
        Double,
    }
    fn inner<'l>(
        state: State<'l>,
        rest: &'l str,
        mut result: InlinedStackAccessor<(&'l str, &'l str)>,
    ) -> bool {
        match state {
            State::Initial => {
                // (1) -> fin
                let Some(first_char) = rest.find(|c| c != ' ') else {
                    // fin
                    return true;
                };
                // (1) + ' ' -> (1) is done through finding first non-empty char
                let inner_rest = &rest[first_char..];
                // (1) + [a-zA-z] -> (2)
                if inner_rest.starts_with(|c| matches!(c, 'A'..='Z' | 'a'..='z')) {
                    return inner(State::InsideName, inner_rest, result);
                }
            }
            State::InsideName => {
                // (2) + [A-Za-z0-9] -> (2)
                let Some(after_name) =
                    rest.find(|c| !matches!(c, 'A'..='Z' | 'a'..='z' | '0'..='9'))
                else {
                    // fail
                    return false;
                };
                let name = &rest[..after_name];
                let inner_rest = &rest[after_name..];
                // (2) + ' ' -> (3)
                if inner_rest.starts_with(' ') {
                    return inner(State::BeforeEquals { name }, inner_rest, result);
                }
                // (2) + '=' -> (4)
                if inner_rest.starts_with('=') {
                    return inner(State::AfterEquals { name }, &inner_rest[1..], result);
                }
            }
            State::BeforeEquals { name } => {
                // (3) + ' ' -> (3)
                let Some(equals) = rest.find(|c| c != ' ') else {
                    // fail
                    return false;
                };
                let inner_rest = &rest[equals..];

                // (3) + '=' -> (4)
                if inner_rest.starts_with('=') {
                    let inner_after_equals = &inner_rest[1..];
                    return inner(State::AfterEquals { name }, inner_after_equals, result);
                }
            }
            State::AfterEquals { name } => {
                // (4) + ' ' -> (4)
                let Some(first_char) = rest.find(|c| c != ' ') else {
                    // fail
                    return false;
                };
                let inner_rest = &rest[first_char..];
                // (4) + ["'] -> (5)(["'])
                return inner(
                    State::InsideValue {
                        name,
                        value_rest: inner_rest,
                        len: 0,
                        quote: None,
                    },
                    inner_rest,
                    result,
                );
            }
            State::InsideValue {
                name,
                value_rest,
                len,
                quote,
            } => {
                // Here lies the whole complexity
                // Basically, there's no actual way to know, if value had finished, so backtracking is the only option
                // Fortunately, I assume one thing about values: they must have exited all of their quotes
                // This assumption is based on python syntax -- there would indeed be no way to insert something without quotes.
                // I HOPE that this is enough, and would appreciate any advice here.

                // (5)(q) + "\"" -> (5)(q)
                // (5)(q) + "\'" -> (5)(q)
                if rest.starts_with(r#"\""#) | rest.starts_with(r#"\'"#) {
                    const OFFSET: usize = r#"\""#.len();
                    return inner(
                        State::InsideValue {
                            name,
                            value_rest,
                            len: len + OFFSET,
                            quote,
                        },
                        &rest[OFFSET..],
                        result,
                    );
                }

                fn beginning_quote(s: &str) -> Option<Quote> {
                    match s.chars().next() {
                        Some('\'') => Some(Quote::Single),
                        Some('\"') => Some(Quote::Double),
                        Some(_) => None,
                        None => None,
                    }
                }

                if let Some(some_quote) = quote.as_ref() {
                    if let Some(rest_quote) = beginning_quote(rest) {
                        const OFFSET: usize = "\'".len();
                        // (5)(["']) + ["'] -> (5)()
                        if some_quote == &rest_quote {
                            return inner(
                                State::InsideValue {
                                    name,
                                    value_rest,
                                    len: len + OFFSET,
                                    quote: None, // quote closed
                                },
                                &rest[1..],
                                result,
                            );
                        }
                        // (5)(["']) + ['"] -> (5)(["'])
                        return inner(
                            State::InsideValue {
                                name,
                                value_rest,
                                len: len + OFFSET,
                                quote, // quote unchanged
                            },
                            &rest[1..],
                            result,
                        );
                    }
                    // no action required in this case, one character would be consumed anyway
                } else {
                    // no quote
                    // (5)() + ' ' -> (6)
                    if rest.starts_with([' ', ',']) || rest.is_empty() {
                        // the most tricky part here
                        let value = &value_rest[..len];
                        result.push((name, value)); // push this name-value guess to the result
                        let inner_succeeded = inner(State::BeforeComa, rest, result.accessor());
                        if inner_succeeded {
                            // found a match inside!
                            return true;
                        }
                        // it's too early to break out of the value, then
                        result.revert(); // revert the changes

                        // this block is an optional match
                    }

                    // (5)() + ["'] -> (5)(["'])
                    if let Some(quote) = beginning_quote(rest) {
                        const OFFSET: usize = "\"".len();
                        return inner(
                            State::InsideValue {
                                name,
                                value_rest,
                                len: len + OFFSET,
                                quote: Some(quote),
                            },
                            &rest[OFFSET..],
                            result,
                        );
                    }
                }

                // this state is not fin
                if rest.is_empty() {
                    // reached the end, yet value has not ended
                    // fail
                    return false;
                }
                if rest.starts_with(|c| len > 0 || c != '=') {
                    let next_char = value_rest
                        .char_indices()
                        .nth(1) // I need an index of a 2nd character here
                        .map(|(i, _)| i)
                        .unwrap_or(value_rest.len()); // If there's no 2nd character, inner_rest should be empty
                    let inner_rest = &rest[next_char..];
                    return inner(
                        State::InsideValue {
                            name,
                            value_rest,
                            len: len + next_char,
                            quote,
                        },
                        inner_rest,
                        result,
                    );
                }
            }
            State::BeforeComa => {
                // (6) + ' ' -> (6)
                let Some(first_char) = rest.find(|c| c != ' ') else {
                    // fin
                    return true;
                };
                let rest_inner = &rest[first_char..];
                // (6) + ',' -> (1)
                if rest_inner.starts_with(',') {
                    return inner(State::Initial, &rest_inner[1..], result);
                }
            }
        }
        false
    }
    let mut result = Vec::new();
    let mut result_stack = InlinedStack::new(&mut result);
    let succeeded = inner(State::Initial, input, result_stack.accessor());
    if !succeeded {
        let _ = result.drain(..);
    }
    result.into_iter()
}

#[cfg(test)]
mod find_args_tests {
    use super::*;
    macro_rules! test {
        {$name:ident, $input:literal, [$(($output_name:literal, $output_value:literal)), *]} => {
            #[test]
            fn $name() {
                // arrange

                // act
                let output = find_args($input).into_iter().collect_vec();

                // assert
                assert_eq!(output, [$(($output_name, $output_value)), *], "Should produce expected result");
            }
        };
    }

    test! {empty1, "", []}
    test! {empty2, "    \n   \n", []}
    test! {empty3, "=", []}
    test! {empty4, r#"===""""#, []}
    test! {empty5, r#"  src   == "man""#, []}
    test! {single1, r#"x=1"#, [("x", "1")]}
    test! {single2, r#" x   =1   "#, [("x", "1")]}
    test! {single3, r#" src   = "path/to/th.png"   "#, [("src", "\"path/to/th.png\"")]}
    test! {single4, r#" src   = "path/to   /th.png"   "#, [("src", "\"path/to   /th.png\"")]}
    test! {single5, r#"caption = "Some really cool text, expected to handle that comma too"   "#, [("caption", "\"Some really cool text, expected to handle that comma too\"")]}
    test! {double1, r#"src = "pat   h/to/th.png",    rows=50   "#, [("src", "\"pat   h/to/th.png\""), ("rows", "50")]}
    test! {quote1, r#"caption = "Text using \"quotes\" should be handled too"   "#, [("caption", r#""Text using \"quotes\" should be handled too""#)]}
    test! {quote2, r#"caption = "There can be a lot of stuff in caption, like a casual ref: [@fig:lol], or equation: $(x+1)^2=x^2+2x+1$ = lunar death of landlord Grim, ha-ha"   "#,
    [("caption", r#""There can be a lot of stuff in caption, like a casual ref: [@fig:lol], or equation: $(x+1)^2=x^2+2x+1$ = lunar death of landlord Grim, ha-ha""#)]}

    // These two are not even going to happen, lol
    test! {doc1, r#""very important info, that definitely couldn't be inserted into a document statically""#, []}
    test! {doc2, r#"4.54535, 0.2342"#, []}
    test! {doc3, r#"src="line.png", ident="line", caption="Some caption for a line drawing. This is parsed as a regular document text, so you may [@eq:line] and $f(x) = A \cdot x^{1} + B \cdot x^{0}$ here, for example""#,
    [("src", "\"line.png\""), ("ident", "\"line\""), ("caption", r#""Some caption for a line drawing. This is parsed as a regular document text, so you may [@eq:line] and $f(x) = A \cdot x^{1} + B \cdot x^{0}$ here, for example""#)]}
    test! {doc4, r#"src="file.csv", rows=1..100, columns=["type", ("a", "a_err")]"#,
    [("src", "\"file.csv\""), ("rows", "1..100"), ("columns", r#"["type", ("a", "a_err")]"#)]}

    test! {idk1, "lines=5, rows = 5..100, a=\"1\"",
    [("lines", "5"), ("rows", "5..100"), ("a", "\"1\"")]}
    test! {idk2, "src=\"some/path/idk.csv\", rows=5..=100", [("src", "\"some/path/idk.csv\""), ("rows", "5..=100")]}
    test! {idk3, "src=\"some/path/idk.csv\", columns = []", [("src", "\"some/path/idk.csv\""), ("columns", "[]")]}

    test! {edge1, r#"src = 'path/to/image.jpg', caption = 'On this figure you can see "literally nothing"', ident = "meow""#,
    [("src", "\'path/to/image.jpg\'"), ("caption", r#"'On this figure you can see "literally nothing"'"#), ("ident", "\"meow\"")]}
}

fn parse_columns<'l>(input: &'l str) -> Vec<CsvTableColumn<'l>> {
    use regex::Captures;
    const REGEX_STR: &str = r#"("([^"]+)"|\("([^"])",\s*"([^"]+)"\))"#;
    static REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(REGEX_STR).expect("Bad regex syntax"));
    REGEX
        .captures_iter(input)
        .map(|cap: Captures<'l>| {
            // dbg!(&cap);
            match (cap.get(2), cap.get(3), cap.get(4)) {
                (Some(col), _, _) => CsvTableColumn::Single(col.as_str().into()),
                (_, Some(value), Some(error)) => {
                    CsvTableColumn::ValueError(value.as_str(), error.as_str())
                }
                _ => unreachable!(),
            }
        })
        .collect_vec()
}

#[cfg(test)]
mod parse_columns_tests {
    use super::*;
    macro_rules! c {
        ($col:literal) => {
            CsvTableColumn::Single($col)
        };
    }
    macro_rules! ve {
        ($value:literal, $error:literal) => {
            CsvTableColumn::ValueError($value, $error)
        };
    }

    macro_rules! test {
        {$name:ident, $input:literal, [$($out:expr), *]} => {
            #[test]
            fn $name() {
                // arrange

                // act
                let output = parse_columns($input);

                // assert
                assert_eq!(output, vec![$($out), *], "Should produce expected result");
            }
        };
    }

    test! {empty, r#""#, []}
    test! {single, r#""a""#, [c!("a")]}
    test! {value_error, r#""a", ("b", "c")"#, [c!("a"), ve!("b", "c")]}
    test! {master, r#""no", ("x", "x_err"), "y", ("z", "z_err")"#, [c!("no"), ve!("x", "x_err"), c!("y"), ve!("z", "z_err")]}
    test! {doc1, r#""type",("a", "a_err")"#, [c!("type"), ve!("a", "a_err")]}
}

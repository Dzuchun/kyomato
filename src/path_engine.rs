use std::{
    borrow::Cow,
    error::Error,
    path::{Path, PathBuf},
};

/// Represents a context of occurred error
#[derive(Debug, Clone, Copy)]
pub enum ErrorContext {
    Source,
    Ayano,
    Specific,
    Execution,
    None,
}

pub trait PathEngineError: Error {
    fn from_generic(error: Box<dyn Error>, context: ErrorContext) -> Self;
    fn append(&mut self, other: Box<dyn Error>, context: ErrorContext);
}

/// An error type with no state - It's only mission is to report an error happening
#[derive(Debug, thiserror::Error, PartialEq)]
#[error("Could not resolve a path")]
pub struct MarkerError;

impl PathEngineError for MarkerError {
    fn append(&mut self, _other: Box<dyn Error>, _context: ErrorContext) {}

    fn from_generic(error: Box<dyn Error>, context: ErrorContext) -> Self {
        MarkerError
    }
}

/// An error holding rich information about reason resolution had failed
#[derive(Debug, thiserror::Error, Default)]
#[error("{:?}", self)]
pub struct InformativeError {
    source: Option<Box<dyn Error>>,
    ayano: Option<Box<dyn Error>>,
    specific: Option<Box<dyn Error>>,
    execution: Option<Box<dyn Error>>,
    none: Option<Box<dyn Error>>,
}

impl PathEngineError for InformativeError {
    fn append(&mut self, other: Box<dyn Error>, context: ErrorContext) {
        match context {
            ErrorContext::Source => self.source.insert(other),
            ErrorContext::Ayano => self.ayano.insert(other),
            ErrorContext::Specific => self.specific.insert(other),
            ErrorContext::Execution => self.execution.insert(other),
            ErrorContext::None => self.none.insert(other),
        };
    }

    fn from_generic(error: Box<dyn Error>, context: ErrorContext) -> Self {
        let mut this = Self::default();
        this.append(error, context);
        this
    }
}

#[cfg(test)]
impl PartialEq for InformativeError {
    fn eq(&self, other: &Self) -> bool {
        format!("{self:?}") == format!("{other:?}")
    }
}

type CheckType = fn(&Path) -> Result<(), Box<dyn Error>>;

/// A trait representing a `thing` that controls file path finding for `OutputGenerator`.
///
/// You can implement it, but you are not supposed to. There are composition methods to build one yourself along with predefined presets.
pub trait PathEngine<E> {
    fn image<'p>(
        &self,
        path: impl Into<Cow<'p, Path>>,
        ayano_insert: Option<&Path>,
    ) -> Result<Cow<'p, Path>, E>;
    fn csv<'p>(
        &self,
        path: impl Into<Cow<'p, Path>>,
        ayano_insert: Option<&Path>,
    ) -> Result<Cow<'p, Path>, E>;
    fn ayano_insert<'p>(&self, path: impl Into<Cow<'p, Path>>) -> Result<Cow<'p, Path>, E>;
    fn set_check(&mut self, new_check: CheckType);
    fn get_check(&self) -> CheckType;
}

pub trait PathEngineExt<E>: PathEngine<E> + Sized {
    fn with_ayano(self) -> AyanoRespecting<Self> {
        AyanoRespecting(self)
    }

    fn with_image_folder(self, folder: impl Into<PathBuf>) -> WithImageFolder<Self> {
        WithImageFolder(self, folder.into())
    }

    fn with_execution_folder(self, folder: impl Into<PathBuf>) -> WithExecutionFolder<Self> {
        WithExecutionFolder(self, folder.into())
    }

    fn with_source_folder(self, folder: impl Into<PathBuf>) -> WithSourceFolder<Self> {
        WithSourceFolder(self, folder.into())
    }

    fn with_check(mut self, new_check: CheckType) -> impl PathEngine<E> + Sized {
        self.set_check(new_check);
        self
    }

    fn with_error_type<NewE: PathEngineError>(self) -> impl PathEngine<NewE> + Sized
    where
        Self: PathEngine<NewE>,
    {
        self
    }
}

impl<E, PE> PathEngineExt<E> for PE where PE: PathEngine<E> {}

/// A primitive path engine, that returns, well, exact paths it's given. It literally does nothing.
pub struct Engine(CheckType);
impl<E: PathEngineError> PathEngine<E> for Engine {
    fn image<'p>(
        &self,
        path: impl Into<Cow<'p, Path>>,
        ayano_insert: Option<&Path>,
    ) -> Result<Cow<'p, Path>, E> {
        let path = path.into();
        let _ = (self.0)(&path).map_err(|err| E::from_generic(err, ErrorContext::None))?;
        Ok(path)
    }

    fn csv<'p>(
        &self,
        path: impl Into<Cow<'p, Path>>,
        ayano_insert: Option<&Path>,
    ) -> Result<Cow<'p, Path>, E> {
        let path = path.into();
        let _ = (self.0)(&path).map_err(|err| E::from_generic(err, ErrorContext::None))?;
        Ok(path)
    }

    fn ayano_insert<'p>(&self, path: impl Into<Cow<'p, Path>>) -> Result<Cow<'p, Path>, E> {
        let path = path.into();
        let _ = (self.0)(&path).map_err(|err| E::from_generic(err, ErrorContext::None))?;
        Ok(path)
    }

    fn set_check(&mut self, new_check: CheckType) {
        self.0 = new_check;
    }

    fn get_check(&self) -> CheckType {
        self.0
    }
}

/// Represents various reasons file path may not be correct
#[derive(Debug, derive_more::From, thiserror::Error)]
pub enum VariantError {
    #[error(transparent)]
    Io(std::io::Error), // missing permissions, or broken symbolic links
    #[error("file does not exist")]
    NotExist, // file does not in fact exist in the filesystem
    #[error("path is not of a file")]
    NotAFile, // path resolves to a directory, not a file
}

/// A path-checking function always positive about path being correct
fn no_check(path: &Path) -> Result<(), Box<dyn Error>> {
    Ok(())
}

/// A path-checking function actually checking if file exists via normal means
fn fs_check(path: &Path) -> Result<(), Box<dyn Error>> {
    match path.try_exists() {
        Ok(true) => {
            if path.is_file() {
                Ok(())
            } else {
                Err(Box::new(VariantError::NotAFile))
            }
        }
        Err(io) => Err(Box::new(VariantError::Io(io))),
        Ok(false) => Err(Box::new(VariantError::NotExist)),
    }
}

macro_rules! try_variant {
    ($this_path:expr, $this_variant:ident, $self:ident, $method:ident, $path:ident $(, $arg:tt)*) => {{
        let mut path = $path.into();
        // first, try this path
        let this_error = if let Some(this_path) = $this_path.as_ref() {
            let this_guess = this_path.to_owned().join(&path);
            let Err(err) = $self.get_check()(&this_guess) else {
                return Ok(Cow::Owned(this_guess));
            };
            Some(err)
        } else {
            None
        };
        // Then, attempt to delegate deeper
        match $self.0.$method(path $(, $arg)*) {
            // if inner engine is successful, return it's result
            Ok(res) => Ok(res),
            // if it's not -- append this error to it's error and propagate
            Err(mut err) => {
                if let Some(this_error) = this_error {
                    err.append(this_error, ErrorContext::$this_variant);
                }
                Err(err)
            }
        }
    }};
}

macro_rules! impl_check_delegate {
    () => {
        fn set_check(&mut self, new_check: CheckType) {
            self.0.set_check(new_check)
        }

        fn get_check(&self) -> CheckType {
            self.0.get_check()
        }
    };
}
pub struct AyanoRespecting<PE>(PE);
impl<E: PathEngineError, PE: PathEngine<E>> PathEngine<E> for AyanoRespecting<PE> {
    fn image<'p>(
        &self,
        path: impl Into<Cow<'p, Path>>,
        ayano_insert: Option<&Path>,
    ) -> Result<Cow<'p, Path>, E> {
        try_variant!(ayano_insert, Ayano, self, image, path, ayano_insert)
    }

    fn csv<'p>(
        &self,
        path: impl Into<Cow<'p, Path>>,
        ayano_insert: Option<&Path>,
    ) -> Result<Cow<'p, Path>, E> {
        try_variant!(ayano_insert, Ayano, self, csv, path, ayano_insert)
    }

    fn ayano_insert<'p>(&self, path: impl Into<Cow<'p, Path>>) -> Result<Cow<'p, Path>, E> {
        self.0.ayano_insert(path)
    }

    impl_check_delegate! {}
}

pub struct WithImageFolder<PE>(PE, PathBuf);
impl<E: PathEngineError, PE: PathEngine<E>> PathEngine<E> for WithImageFolder<PE> {
    fn image<'p>(
        &self,
        path: impl Into<Cow<'p, Path>>,
        ayano_insert: Option<&Path>,
    ) -> Result<Cow<'p, Path>, E> {
        try_variant!(Some(&self.1), Specific, self, image, path, ayano_insert)
    }

    fn csv<'p>(
        &self,
        path: impl Into<Cow<'p, Path>>,
        ayano_insert: Option<&Path>,
    ) -> Result<Cow<'p, Path>, E> {
        self.0.csv(path, ayano_insert)
    }

    fn ayano_insert<'p>(&self, path: impl Into<Cow<'p, Path>>) -> Result<Cow<'p, Path>, E> {
        self.0.ayano_insert(path)
    }

    impl_check_delegate! {}
}

pub struct WithSourceFolder<PE>(PE, PathBuf);
impl<E: PathEngineError, PE: PathEngine<E>> PathEngine<E> for WithSourceFolder<PE> {
    fn image<'p>(
        &self,
        path: impl Into<Cow<'p, Path>>,
        ayano_insert: Option<&Path>,
    ) -> Result<Cow<'p, Path>, E> {
        try_variant!(Some(&self.1), Source, self, image, path, ayano_insert)
    }

    fn csv<'p>(
        &self,
        path: impl Into<Cow<'p, Path>>,
        ayano_insert: Option<&Path>,
    ) -> Result<Cow<'p, Path>, E> {
        try_variant!(Some(&self.1), Source, self, csv, path, ayano_insert)
    }

    fn ayano_insert<'p>(&self, path: impl Into<Cow<'p, Path>>) -> Result<Cow<'p, Path>, E> {
        try_variant!(Some(&self.1), Source, self, ayano_insert, path)
    }
    impl_check_delegate! {}
}

pub struct WithExecutionFolder<PE>(PE, PathBuf);
impl<E: PathEngineError, PE: PathEngine<E>> PathEngine<E> for WithExecutionFolder<PE> {
    fn image<'p>(
        &self,
        path: impl Into<Cow<'p, Path>>,
        ayano_insert: Option<&Path>,
    ) -> Result<Cow<'p, Path>, E> {
        try_variant!(Some(&self.1), Execution, self, image, path, ayano_insert)
    }

    fn csv<'p>(
        &self,
        path: impl Into<Cow<'p, Path>>,
        ayano_insert: Option<&Path>,
    ) -> Result<Cow<'p, Path>, E> {
        try_variant!(Some(&self.1), Execution, self, csv, path, ayano_insert)
    }

    fn ayano_insert<'p>(&self, path: impl Into<Cow<'p, Path>>) -> Result<Cow<'p, Path>, E> {
        try_variant!(Some(&self.1), Execution, self, ayano_insert, path)
    }
    impl_check_delegate! {}
}

/// A basic engine
///
/// Generic method. Rust will most likely complain about type inference if you use it directly;
/// Make sure to add at least one `with_th_error`.
pub fn base_gen<E: PathEngineError>() -> impl PathEngine<E> + Sized {
    Engine(no_check)
}

/// The most primitive engine setup, performs no checks and has no special folders
pub fn primitive() -> impl PathEngine<MarkerError> + Sized {
    base_gen()
}

/// A primitive engine, but with informative error and `fs_check`
pub fn primitive_dbg() -> impl PathEngine<InformativeError> + Sized {
    base_gen()
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str::FromStr;

    #[derive(Debug, thiserror::Error)]
    #[error("A path {} does not exist", .0.to_string_lossy())]
    struct DontExist(PathBuf);

    macro_rules! test_check {
        ($($existing_path:literal), *) => {{
            fn test_check(path: &Path) -> Result<(), Box<dyn Error>> {
                const EXISTING_PATHS: &[&str] = &[$($existing_path), *]; // elided array sizes are unstable :(
                if EXISTING_PATHS.contains(&&*path.to_string_lossy()) {
                    Ok(())
                } else {
                    Err(Box::new(DontExist(path.to_owned())))
                }
            }
            test_check
        }};
    }

    macro_rules! test_ok {
        {$name:ident, $engine:expr, $method:ident, $test_path:literal, $ayano_path:expr, $expected_res:expr} => {
            test_ok!{$name, $engine.$method(Path::new($test_path), $ayano_path.map(|p: &str| Path::new(p).into())), $expected_res}
        };
        {$name:ident, $engine:expr, $test_path:literal, $expected_res:expr} => {
            test_ok!{$name, $engine.ayano_insert(Path::new($test_path)), $expected_res}
        };
        {$name:ident, $call_expr:expr, $expected_res:expr} => {
            #[test]
            fn $name() {
                // arrange

                // act
                let engine_result = $call_expr.expect("Should be ok");

                // assert
                assert_eq!(Path::new($expected_res), engine_result, "Path engine should produce expected result");
            }
        };
    }

    macro_rules! test_expr {
        {$name:ident, $engine:expr, $method:ident, $test_path:literal, $ayano_path:expr, $expected_res:expr} => {
            test_expr!{$name, $engine.$method(Path::new($test_path), $ayano_path.map(|p: &str| Path::new(p).into())), $expected_res}
        };
        {$name:ident, $engine:expr, $test_path:literal, $expected_res:expr} => {
            test_expr!{$name, $engine.ayano_insert(Path::new($test_path)), $expected_res}
        };
        {$name:ident, $call_expr:expr, $expected_res:expr} => {
            #[test]
            fn $name() {
                // arrange

                // act
                let engine_result = $call_expr;

                // assert
                assert_eq!($expected_res, engine_result, "Path engine should produce expected result");
            }
        };
    }
    macro_rules! test_pat {
        {$name:ident, $engine:expr, $method:ident, $test_path:literal, $ayano_path:expr, $expected_res:pat} => {
            test_pat!{$name, $engine.$method(Path::new($test_path), $ayano_path.map(|p: &str| Path::new(p).into())), $expected_res}
        };
        {$name:ident, $engine:expr, $test_path:literal, $expected_res:pat} => {
            test_pat!{$name, $engine.ayano_insert(Path::new($test_path)), $expected_res}
        };
        {$name:ident, $call_expr:expr, $expected_res:pat} => {
            #[test]
            fn $name() {
                // arrange

                // act
                let engine_result = $call_expr;

                // assert
                if !matches!(engine_result, $expected_res) {
                    panic!("Path engine should produce matching result: {engine_result:?}");
                }
            }
        };
    }

    test_ok! {ok_simple_ayano, primitive(), "scripts/calc.py", "scripts/calc.py"}
    test_pat! {err_deny_ayano, primitive().with_check(test_check!()), "scripts/calc.py", Err(_)}
    macro_rules! paths_set_engine {
        ($($existing_path:literal), *) => {
            primitive_dbg()
                .with_check(test_check!($($existing_path), *))
                .with_execution_folder(PathBuf::from_str("/execution").unwrap())
                .with_source_folder(PathBuf::from_str("/source").unwrap())
                .with_image_folder(PathBuf::from_str("/image").unwrap())
                .with_ayano()
        };
    }
    test_ok! {ok_find_ayano_correct1, paths_set_engine!("/execution/calc.py", "/image/calc.py"), "calc.py", "/execution/calc.py"}
    test_ok! {ok_find_ayano_correct2, paths_set_engine!("/execution/calc.py", "/image/calc.py", "/source/calc.py"), image, "calc.py", None, "/image/calc.py"}
    test_ok! {ok_find_ayano_correct2_2, paths_set_engine!("/execution/calc.py", "/source/calc.py"), image, "calc.py", None, "/source/calc.py"}
    test_ok! {ok_find_ayano_correct2_3, paths_set_engine!("/execution/calc.py"), image, "calc.py", None, "/execution/calc.py"}
    test_ok! {ok_find_ayano_correct2_4, paths_set_engine!("/execution/calc.py", "/image/calc.py", "/source/calc.py", "/ayano/calc.py"), image, "calc.py", Some("/ayano"), "/ayano/calc.py"}
    test_ok! {ok_find_ayano_correct3, paths_set_engine!("/execution/table.csv", "/image/table.csv", "/source/table.csv"), csv, "table.csv", None, "/source/table.csv"}
    test_ok! {ok_find_ayano_correct3_2, paths_set_engine!("/execution/table.csv", "/image/table.csv"), csv, "table.csv", None, "/execution/table.csv"}
    test_pat! {err_find_ayano_correct3_3, paths_set_engine!("/image/table.csv"), csv, "table.csv", None, Err(_)}

    macro_rules! err {
        ($path:literal) => {{
            Some(Box::new(DontExist(PathBuf::from_str($path).unwrap())) as Box<dyn Error>)
        }};
    }
    test_expr! {err_reaction1, primitive_dbg().with_check(test_check!()), csv, "file.txt", None,
    Err(InformativeError {
        source: None,
        ayano: None,
        specific: None,
        execution: None,
        none: err!("file.txt")
    })}
    test_expr! {err_reaction2, primitive_dbg().with_ayano().with_check(test_check!()), csv, "file.txt", Some("/ayano"),
    Err(InformativeError {
        source: None,
        ayano: err!("/ayano/file.txt"),
        specific: None,
        execution: None,
        none: err!("file.txt")
    })}
    test_expr! {err_reaction3, primitive_dbg().with_ayano().with_execution_folder(Path::new("/execution")).with_check(test_check!()), csv, "file.txt", Some("/ayano"),
    Err(InformativeError {
        source: None,
        ayano: err!("/ayano/file.txt"),
        specific: None,
        execution: err!("/execution/file.txt"),
        none: err!("file.txt")
    })}
    test_expr! {err_reaction4, primitive_dbg().with_ayano().with_image_folder(Path::new("/image")).with_execution_folder(Path::new("/execution")).with_check(test_check!()), csv, "file.txt", Some("/ayano"),
    Err(InformativeError {
        source: None,
        ayano: err!("/ayano/file.txt"),
        specific: None,
        execution: err!("/execution/file.txt"),
        none: err!("file.txt")
    })}
    test_expr! {err_reaction5, primitive_dbg().with_ayano().with_image_folder(Path::new("/image")).with_execution_folder(Path::new("/execution")).with_check(test_check!()), image, "file.txt", Some("/ayano"),
    Err(InformativeError {
        source: None,
        ayano: err!("/ayano/file.txt"),
        specific: err!("/image/file.txt"),
        execution: err!("/execution/file.txt"),
        none: err!("file.txt")
    })}
    test_expr! {err_reaction6, primitive_dbg().with_ayano().with_source_folder(Path::new("/source")).with_image_folder(Path::new("/image")).with_execution_folder(Path::new("/execution")).with_check(test_check!()), image, "file.txt", Some("/ayano"),
    Err(InformativeError {
        source: err!("/source/file.txt"),
        ayano: err!("/ayano/file.txt"),
        specific: err!("/image/file.txt"),
        execution: err!("/execution/file.txt"),
        none: err!("file.txt")
    })}
}

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
#[derive(Debug, thiserror::Error)]
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

    fn with_check(mut self, new_check: CheckType) -> impl PathEngine<E> {
        self.set_check(new_check);
        self
    }

    fn with_error_type<NewE: PathEngineError>(self) -> impl PathEngine<NewE>
    where
        Self: PathEngine<NewE>,
    {
        self
    }
}

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

/// A basic engine
///
/// Generic method. Rust will most likely complain about type inference if you use it directly;
/// Make sure to add at least one `with_th_error`.
pub fn base_gen<E: PathEngineError>() -> impl PathEngine<E> {
    Engine(no_check)
}

/// The most primitive engine setup, performs no checks and has no special folders
pub fn primitive() -> impl PathEngine<MarkerError> {
    base_gen()
}

/// A primitive engine, but with informative error and `fs_check`
pub fn primitive_dbg() -> impl PathEngine<InformativeError> {
    base_gen()
}

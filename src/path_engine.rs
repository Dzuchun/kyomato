use std::path::{Path, PathBuf};

#[derive(Debug, derive_more::From)]
pub enum VariantError {
    Io(std::io::Error), // missing permissions, or broken symbolic links
    NotExist,           // file does not in fact exist in the filesystem
    NotAFile,           // path resolves to a directory, not a file
}

#[derive(Debug)]
pub struct Error {
    source: Option<VariantError>, // error resolving path relative to the target file (if present)
    ayano: Option<VariantError>,  // error resolving relative to ayano-inserted code
    specific: Option<VariantError>, // error of an option specific to some method
    execution: VariantError,      // error of path resolution relative to execution directory
}

// TODO make this thing modular (probably use tower or th)
pub trait PathEngine {
    fn image(&self, path: &Path, ayano_insert: Option<&Path>) -> Result<PathBuf, Error>;
    fn csv(&self, path: &Path, ayano_insert: Option<&Path>) -> Result<PathBuf, Error>;
    fn ayano_insert(&self, path: &Path) -> Result<PathBuf, Error>;
}

pub use default::Engine as DefaultEngine;
pub use Error as DefaultError;

mod default {
    use std::path::{Path, PathBuf};

    use super::{Error, PathEngine};

    #[derive(Debug)]
    pub struct Engine {
        source_folder: Option<PathBuf>,
        image_folder: Option<PathBuf>,
        execution_folder: PathBuf,
    }

    macro_rules! try_path {
        ($path:ident) => {
            if $path.try_exists().is_ok_and(std::convert::identity)
            // path exists and is reachable
            && $path.is_file()
            // and path points to the file
        {
            // found a match!
            return Ok($path);
        }
        };
    }

    macro_rules! op_path {
        ($path:ident) => {
            $path = if let Some(path) = $path {
                try_path!(path);
                Some(path)
            } else {
                None
            }
        };
    }

    macro_rules! path_error {
        ($path:ident) => {
            match $path.try_exists() {
                Err(io) => super::VariantError::Io(io),
                Ok(false) => super::VariantError::NotExist,
                Ok(true) => {
                    if !$path.is_file() {
                        super::VariantError::NotAFile
                    } else {
                        unreachable!("These should be all options for possible path errors. If you can see this, it's a oversight, please contact me.")
                    }
                }
            }
        };
    }

    impl Engine {
        pub fn create(
            target_folder: impl Into<Option<PathBuf>>,
            image_folder: impl Into<Option<PathBuf>>,
        ) -> std::io::Result<Self> {
            use std::env::current_dir as cd;
            Ok(Self {
                source_folder: target_folder.into(),
                image_folder: image_folder.into(),
                execution_folder: cd()?,
            })
        }
    }

    impl<'l> PathEngine for &'l Engine {
        fn image(&self, path: &Path, ayano_insert: Option<&Path>) -> Result<PathBuf, Error> {
            // first, try resolving in ayano-inserted folder
            let mut ayano_attempt = ayano_insert.as_ref().map(|p| p.join(path));
            op_path!(ayano_attempt);

            // next, try resolving in a dedicated image folder
            let mut image_attempt = self.image_folder.as_ref().map(|p| p.join(path));
            op_path!(image_attempt);

            // next, try resolving in the source folder
            let mut source_attempt = self.source_folder.as_ref().map(|p| p.join(path));
            op_path!(source_attempt);

            // next, try resolving relative to execution folder
            let execution_attempt = self.execution_folder.join(path);
            try_path!(execution_attempt);

            // well, all options failed. not let's construct informative error:
            Err(Error {
                execution: path_error!(execution_attempt),
                ayano: ayano_attempt.map(|p| path_error!(p)),
                source: source_attempt.map(|p| path_error!(p)),
                specific: image_attempt.map(|p| path_error!(p)),
            })
        }

        fn csv(&self, path: &Path, ayano_insert: Option<&Path>) -> Result<PathBuf, Error> {
            // first, try resolving in ayano-inserted folder
            let mut ayano_attempt = ayano_insert.as_ref().map(|p| p.join(path));
            op_path!(ayano_attempt);

            // next, try resolving in the source folder
            let mut source_attempt = self.source_folder.as_ref().map(|p| p.join(path));
            op_path!(source_attempt);

            // next, try resolving relative to execution folder
            let execution_attempt = self.execution_folder.join(path);
            try_path!(execution_attempt);

            // well, all options failed. not let's construct informative error:
            Err(Error {
                execution: path_error!(execution_attempt),
                ayano: ayano_attempt.map(|p| path_error!(p)),
                source: source_attempt.map(|p| path_error!(p)),
                specific: None,
            })
        }

        fn ayano_insert(&self, path: &Path) -> Result<PathBuf, Error> {
            // first, try resolving in the source folder
            let mut source_attempt = self.source_folder.as_ref().map(|p| p.join(path));
            op_path!(source_attempt);

            // next, try resolving relative to execution folder (TODO should O remove that?)
            let execution_attempt = self.execution_folder.join(path);
            try_path!(execution_attempt);

            // well, all options failed. not let's construct informative error:
            Err(Error {
                execution: path_error!(execution_attempt),
                ayano: None,
                source: source_attempt.map(|p| path_error!(p)),
                specific: None,
            })
        }
    }
}

pub use primitive::Engine as PrimitiveEngine;
mod primitive {
    use super::PathEngine;

    pub struct Engine;

    impl PathEngine for &Engine {
        fn image(
            &self,
            path: &std::path::Path,
            _: Option<&std::path::Path>,
        ) -> Result<std::path::PathBuf, super::DefaultError> {
            Ok(path.to_owned())
        }

        fn csv(
            &self,
            path: &std::path::Path,
            _: Option<&std::path::Path>,
        ) -> Result<std::path::PathBuf, super::DefaultError> {
            Ok(path.to_owned())
        }

        fn ayano_insert(
            &self,
            path: &std::path::Path,
        ) -> Result<std::path::PathBuf, super::DefaultError> {
            Ok(path.to_owned())
        }
    }
}

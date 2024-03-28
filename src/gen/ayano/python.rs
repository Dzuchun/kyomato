use std::{borrow::Cow, path::Path};

use pyo3::prelude::*;
use rand::random;

static MODULE_NAME: &'static str = "ayano";

#[derive(Debug)]
pub struct InitToken(pub(self) String);

impl InitToken {
    fn new() -> Self {
        InitToken(format!("{}_{}", MODULE_NAME, random::<u32>()))
    }
    fn module_name(&self) -> &str {
        &self.0
    }

    fn module<'py>(&self, py: Python<'py>) -> Result<&'py PyModule, PyErr> {
        PyModule::import(py, self.module_name())
    }
}

pub fn init(code: &str) -> Result<InitToken, PyErr> {
    Python::with_gil(move |py| {
        let token = InitToken::new();
        PyModule::from_code(py, code, "", token.module_name())?;
        Ok(token)
    })
}

pub fn append_static(target: &mut String, code: &str, execution_path: impl AsRef<Path>) {
    target.push_str(
        format!(
            "\nimport os\n___cwd___ = os.getcwd()\nos.chdir('{}')",
            execution_path.as_ref().to_string_lossy()
        )
        .as_str(),
    );
    for line in code.lines() {
        target.push_str(format!("\n{line}").as_str())
    }
    target.push_str("\nos.chdir(___cwd___)");
}

pub fn append_function(target: &mut String, name: &str, code: &str) {
    // First, declare a function
    target.push_str(format!("\ndef {name}():").as_str());
    for line in code.lines() {
        target.push_str(format!("\n    {line}").as_str())
    }
}

pub fn call_function<Out, Err, Transform>(
    token: &InitToken,
    name: &str,
    transform: Transform,
    execution_path: impl AsRef<Path>,
) -> Result<Out, Err>
where
    Err: From<PyErr>,
    for<'py> Transform: FnOnce(&'py PyAny, Python<'py>) -> Result<Out, Err>,
{
    Python::with_gil(|py| {
        let main_module = token.module(py)?;
        let os = PyModule::import(py, "os")?;
        let cwd = os.getattr("getcwd")?.call0()?;
        let chdir = os.getattr("chdir")?;
        chdir.call1((execution_path.as_ref(),))?;
        let function = main_module.getattr(name)?;
        let res = function.call((), None)?;
        chdir.call1((cwd,))?;
        Ok(transform(res, py)?)
    })
}

#[cfg(test)]
mod tests {
    use super::{call_function, init};

    macro_rules! init_functions {
        ($code:expr, [$(($fun_name:expr, $fun_code:expr)), *]) => {{
            let mut code = $code.to_string();
            for (fun_name, fun_code) in [$(($fun_name, $fun_code)), *] {
                super::append_function(&mut code, fun_name, fun_code);
            }
            init(code.as_str())
        }};
    }

    #[test]
    fn create_empty_should_ok() {
        // arrange
        let code = "";

        // act + assert
        let _token = init_functions!(code, []).expect("Should be able to create empty module");
    }

    #[test]
    fn create_valid_should_ok() {
        // arrange
        let code = "x = 5\ny = x ** 2\nz = y / 10.0";

        // act + assert
        let _token =
            init_functions!(code, []).expect("Should be able to create module with valid code");
    }

    #[test]
    fn create_invalid_should_err() {
        // arrange
        let code = "x = 5;\ny = **5;\nfn a() {{\n    return &'_ 0;\n}}";

        // act + assert
        let _err = init_functions!(code, []).expect_err("Should deny bad code");
    }

    #[test]
    fn function_absent_should_err() {
        // arrange
        let code = "x = 5";
        let token = init_functions!(code, []).expect("Should be able to create module");

        // act + assert
        let _res = call_function(
            &token,
            "absent_function",
            |res, _| Ok::<_, super::PyErr>(res.str()?.to_string()),
            ".",
        )
        .expect_err("Should get error for non-existent function call");
    }

    #[test]
    fn function_present_should_ok() {
        // arrange
        let code = "x = 5";
        let token = init_functions!(code, [("ok_function", "return x")])
            .expect("Should be able to create module");

        // act
        let res = call_function(
            &token,
            "ok_function",
            |res, _| Ok::<_, super::PyErr>(res.str()?.to_string()),
            ".",
        );

        // assert
        assert_eq!(
            res.expect("Should be able to execute existing function"),
            "5"
        );
    }
}

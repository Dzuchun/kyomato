use std::{ffi::OsString, path::PathBuf, str::FromStr};

const CLEAR: &'static str = "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n" ;
const INPUT_DIR: &'static str = "./tests/input_files";
const LEX_DIR: &'static str = "./tests/lex_files";

fn input_files() -> impl Iterator<Item = (OsString, String)> {
    std::fs::read_dir(INPUT_DIR)
        .expect("Should be able to access input directory")
        .into_iter()
        .map(|file| {
            let file_path = file
                .expect("Should be able to access files in input directory")
                .path();
            let input =
                std::fs::read_to_string(&file_path).expect("Should be able to read input files");
            (OsString::from(file_path.file_name().unwrap()), input)
        })
}

/// Prints input file's lex into the stdout, for inspection
#[ignore = "manual"]
#[test]
fn show_lex() {
    for (_, input) in input_files() {
        let parsed = match kyomato::lex(input.as_str()) {
            Ok(ok) => ok,
            Err(err) => {
                panic!("Should successfully parse the input: {err:#?}");
            }
        };
        println!("{parsed:#?}");
        std::io::stdin()
            .read_line(&mut String::new())
            .expect("Should be able to read a line");
        println!("{}", CLEAR);
    }
}

// TODO rewrite to use serde-json after token rework
/// Generates representation of a parsed tokens, for automated testing to compare with
#[cfg(feature = "serde")]
#[ignore = "manual"]
#[test]
fn gen_lex_output() {
    use std::{fs::File, io::Write, os::unix::fs::FileExt, path::PathBuf, str::FromStr};
    {
        use std::io::Read;
        print!("Are you sure? This will rewrite previous test outputs: [y/N]: ");
        std::io::stdout()
            .flush()
            .expect("Should be able to flush the stdout");

        let mut buf = [0u8; 1];
        std::io::stdin()
            .read_exact(&mut buf)
            .expect("Should be able to read stdin");
        if buf[0] != b'y' {
            println!("\nAborting...");
            return;
        }
    }
    let mut output_path = PathBuf::from_str(LEX_DIR).unwrap();
    if !output_path.is_dir() {
        std::fs::create_dir(&output_path).expect("Failed to create directory");
    }
    for (filename, input) in input_files() {
        let (title_info, tokens) = match kyomato::lex(input.as_str()) {
            Ok(ok) => ok,
            Err(err) => {
                panic!("Input files should be successfully parsed: {err:#?}");
            }
        };
        // create representation of the parsed tokens
        // TODO this part should be replaced, once serde is supported
        let repr = format!("{:#?}\n{:#?}", title_info, tokens);
        // write the result to output directory
        let output_file = {
            output_path.push(filename);
            output_path.set_extension("txt");
            let output_file = if output_path.is_file() {
                File::options()
                    .write(true)
                    .truncate(true)
                    .open(&output_path)
            } else {
                File::create(&output_path)
            }
            .expect("Failed to create output file");
            output_path.pop();
            output_file
        };
        output_file
            .write_at(repr.as_bytes(), 0)
            .expect("Failed to write into output file");
    }
}

#[test]
fn test_lex() {
    let mut output_results = Vec::new();
    let mut output_path = PathBuf::from_str(LEX_DIR).unwrap();
    if !output_path.is_dir() {
        std::fs::create_dir(&output_path).expect("Failed to create directory");
    }
    for (filename, input) in input_files() {
        let (title_info, tokens) = match kyomato::lex(input.as_str()) {
            Ok(ok) => ok,
            Err(err) => {
                panic!("Should successfully parse the input: {err:#?}");
            }
        };
        // create representation of the parsed tokens
        // TODO this part should be replaced, once serde is supported
        let repr = format!("{:#?}\n{:#?}", title_info, tokens);
        // write the result to output directory
        let expected_output = {
            output_path.push(&filename);
            output_path.set_extension("txt");
            let output =
                std::fs::read_to_string(&output_path).expect("Failed to read expected output");
            output_path.pop();
            output
        };
        output_results.push((filename, repr, expected_output));
    }
    let total = output_results.len();
    let success = output_results
        .iter()
        .filter(|(_, real, expected)| real == expected)
        .count();
    let failed = total - success;
    if failed > 0 {
        let mut output_msg = String::new();
        output_msg.push_str(&format!("passed: ({success}/{total})\nFailures:\n"));
        for (filename, real, expected) in output_results {
            if real != expected {
                output_msg.push_str(&format!(
                    "\tFilename: {}\n\tActual: {real}\n\tExpected: {expected}\n\n",
                    filename.to_string_lossy(),
                ));
            }
        }
        panic!("{output_msg}");
    }
}

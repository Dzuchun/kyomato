use std::{ffi::OsString, path::PathBuf, str::FromStr};

const CLEAR: &'static str = "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n" ;
const INPUT_DIR: &'static str = "./tests/input_files";
const TRANSFORM_DIR: &'static str = "./tests/transformed_files";

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
fn show_transform() {
    for (_, input) in input_files() {
        let parsed = match kyomato::lex(input.as_str()) {
            Ok(ok) => ok,
            Err(err) => {
                panic!("Should successfully parse the input: {err:#?}");
            }
        };
        let repr = kyomato::gen_to_string(&parsed, None).expect("Should be able to transform tokens");
        println!("{repr}");
        std::io::stdin()
            .read_line(&mut String::new())
            .expect("Should be able to read a line");
        println!("{}", CLEAR);
    }
}

#[ignore = "manual"]
#[test]
fn gen_transform_output() {
    use std::{fs::File, io::Write, path::PathBuf, str::FromStr};
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
    let mut output_path = PathBuf::from_str(TRANSFORM_DIR).unwrap();
    if !output_path.is_dir() {
        std::fs::create_dir(&output_path).expect("Failed to create directory");
    }
    for (filename, input) in input_files() {
        let parsed = match kyomato::lex(input.as_str()) {
            Ok(ok) => ok,
            Err(err) => {
                panic!("Input files should be successfully parsed: {err:#?}");
            }
        };
        // open output file
        let mut output_file = {
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
        // write the result to output file
        kyomato::gen(&parsed, None, &mut output_file).expect("Failed to generate output");
    }
}

#[test]
fn test_transform() {
    let mut output_results = Vec::new();
    let mut output_path = PathBuf::from_str(TRANSFORM_DIR).unwrap();
    if !output_path.is_dir() {
        std::fs::create_dir(&output_path).expect("Failed to create directory");
    }
    for (filename, input) in input_files() {
        let parsed = match kyomato::lex(input.as_str()) {
            Ok(ok) => ok,
            Err(err) => {
                panic!("Should successfully parse the input: {err:#?}");
            }
        };
        let repr = kyomato::gen_to_string(&parsed, None).expect("Failed to generate output");
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

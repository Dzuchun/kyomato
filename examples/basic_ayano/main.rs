fn main() {
    //! This is ayano-involving example of programmatic output generation
    //!
    //! You might want to use CLI interface instead

    // that's a utility setup call; essentially, it reads input from one file, and opens output file for writing
    let (input, mut output) = examples_util::setup_io(
        "examples/basic_ayano/input.md",
        "examples/basic_ayano/output.tex",
    ).unwrap();

    // construct path engine
    // in this example, no external files are accessed (i.e. there are no images or special Ayano syntax)
    // so it's ok to pass in a blank primitive engine
    let path_engine = kyomato::path_engine::primitive_dbg();
    // _dbg stands for "informative error", just in case

    // parse input and write to output
    kyomato::LabaLatex::parse_and_write(path_engine, &input, &mut output)
        // this part is just for debug purposes
        .map_err(|err| (err, output))
        .unwrap()
}

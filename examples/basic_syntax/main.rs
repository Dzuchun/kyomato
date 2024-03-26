use std::path::PathBuf;

use kyomato::path_engine::PathEngineExt;

fn main() {
    //! This is a simple example of programmatic output generation
    //!
    //! You might want to use CLI interface instead

    // # Preparations
    // not mandatory; you may specify paths by hand everywhere.
    let mut workpath = PathBuf::from("examples/basic_syntax");

    // read the input
    workpath.push("input.md");
    let input = std::fs::read_to_string(&workpath).unwrap();
    // you must have a complete input to process it properly

    // open the output file
    workpath.set_file_name("output.tex");
    let output = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .truncate(true)
        .create(true)
        .open(&workpath)
        .unwrap();
    let mut output = kyomato::FmtToIo::new(output);

    // construct path engine
    // this thing is responsible for file path resolution in the generated document
    workpath.pop();
    workpath.push("images");
    let path_engine = kyomato::path_engine::base_gen()
        // in our case, there's a dedicated folder for images, so we inform engine about that
        .with_image_folder(&workpath)
        // this sort of call is required - with it, you specify an error type the engine will return
        // it's a no-op; so you may specify engine's type instead or th
        .with_error_type::<kyomato::path_engine::InformativeError>()
        // this adds file checking to the engine.
        // basically, this will ensure that any file engine happens to refer to, actually exists on the disk
        // you may omit this call, if you don't want that
        .with_check(kyomato::path_engine::fs_check);

    // parse input and write to output
    kyomato::LabaLatex::parse_and_write(path_engine, &input, &mut output)
        // this part is just for debug purposes
        .map_err(|err| (err, output))
        .unwrap()
}

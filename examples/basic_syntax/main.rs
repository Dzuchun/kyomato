use kyomato::path_engine::PathEngineExt;

fn main() {
    //! This is a simple example of programmatic output generation
    //!
    //! You might want to use CLI interface instead

    // that's a utility setup call; essentially, it reads input from one file, and opens output file for writing
    let (input, mut output) = examples_util::setup_io(
        "examples/basic_syntax/input.md",
        "examples/basic_syntax/output.tex",
    ).unwrap();

    // construct path engine
    // this thing is responsible for file path resolution in the generated document
    let path_engine = kyomato::path_engine::base_gen()
        // in our case, there's a dedicated folder for images, so we inform engine about that
        .with_image_folder("examples/basic_syntax/images")
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

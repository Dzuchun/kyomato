use kyomato::path_engine::PathEngineExt;

fn main() {
    //! This is ayano-involving example of programmatic output generation
    //!
    //! Some advanced setup is used here
    //!
    //! You might want to use CLI interface instead

    // we'll just assume that to demonstrate a certain thing, look it in the input
    std::env::set_current_dir("examples/advanced_ayano").unwrap();
    // that's a utility setup call; essentially, it reads input from one file, and opens output file for writing
    let (input, mut output) = examples_util::setup_io("source/input.md", "output.tex").unwrap();

    // construct path engine
    // there are a couple of parts of the path engine. you should specify them in the increasing priority order
    let path_engine = kyomato::path_engine::base_gen()
        // execution folder: you might want generator to check it, if all other options had failed
        .with_execution_folder(".")
        // source folder: you'll want to check it sometime. this is intended to be a folder where input markdown is located
        .with_source_folder("source")
        // image folder: a folder to exclusively check for images referred by the generator
        .with_image_folder("images")
        // this call will make sure that the engine actually respects ayano-insert directory supplied to it's methods
        .with_ayano()
        // this adds a check to the engine, that will ensure that returned path actually points to something in the system
        // without it, first proposed variant will be used (in this case, this turns out to be ayano-insert directory)
        .with_check(kyomato::path_engine::fs_check)
        // this last call is a no-op used to explicitly set error type. You may implement your own error too!
        // (the one actually used here is kinda bad, consider using `kyomato::path_engine::MarkerError` for more memory-efficiency)
        .with_error_type::<kyomato::path_engine::InformativeError>();
    // please note that most of the above is only used by actual latex engine you plan to use.
    // That is, take care of paths being the same for kyomato and the engine, or use some sort of absolute paths.

    // parse input and write to output
    kyomato::LabaLatex::parse_and_write(path_engine, &input, &mut output)
        // this part is just for debug purposes
        .map_err(|err| (err, output))
        .unwrap()
}

use std::path::Path;

pub fn setup_io(
    input_path: impl AsRef<Path>,
    output_path: impl AsRef<Path>,
) -> Result<(String, impl std::fmt::Write + std::fmt::Debug), std::io::Error> {
    let input = std::fs::read_to_string(input_path.as_ref())?;
    // you must have a complete input to process it properly

    // open the output file
    let output = std::fs::OpenOptions::new()
        .read(true)
        .write(true)
        .truncate(true)
        .create(true)
        .open(output_path.as_ref())?;
    let output = kyomato::FmtToIo::new(output);
    Ok((input, output))
}

use std::collections::HashMap;

#[derive(Debug, smart_default::SmartDefault)]
pub struct Context {
    pub encountered_footnotes: HashMap<String, usize>,
    pub inside_caption: bool,
    #[default(true)]
    pub allergic_to_newline: bool,
}

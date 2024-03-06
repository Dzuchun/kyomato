use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct Context {
    pub encountered_footnotes: HashMap<String, usize>,
}

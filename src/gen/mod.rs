mod ayano;
pub mod lab;

use std::fmt::Write;

use itertools::Itertools;

use crate::data::{Token, Tokens};

#[derive(Debug, derive_more::From, thiserror::Error)]
pub enum GenerationError<'source> {
    #[error("{}", .0)]
    IOs(std::fmt::Error),
    #[error("{}", .0)]
    Ayano(ayano::AyanoError<'source>),
    #[error("{}", .0)]
    Path(Box<dyn std::error::Error + 'source>),
}

pub type Res<'source> = Result<(), GenerationError<'source>>;

// TODO make this thing modular (probably use tower to handle writes or th)
pub trait OutputGenerator<'source, Meta, Context> {
    fn write_to<'meta, 'context, 'token, W: Write + ?Sized>(
        &self,
        output: &mut W,
        meta: &'meta Meta,
        context: &'context mut Context,
        token: &'token Token<'source>,
    ) -> Res<'source>
    where
        'source: 'meta + 'context + 'token,
        'token: 'context;

    fn write_tokens_to<'meta, 'context, 'token, W: Write + ?Sized>(
        &self,
        output: &mut W,
        meta: &'meta Meta,
        context: &'context mut Context,
        tokens: &'token Tokens<'source>,
    ) -> Res<'source>
    where
        'source: 'meta + 'context + 'token,
        'token: 'context,
    {
        tokens
            .into_iter()
            .map(|t| self.write_to(output, meta, context, t))
            .try_collect()
    }
}

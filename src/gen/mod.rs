mod ayano;
pub mod lab;

use std::fmt::Write;

use itertools::Itertools;

use crate::data::{Token, Tokens};

#[derive(Debug, thiserror::Error)]
pub enum GenerationError<'source> {
    #[error(transparent)]
    Write(std::fmt::Error),
    #[error(transparent)]
    Ayano(ayano::AyanoError<'source>),
    #[error(transparent)]
    Path(Box<dyn std::error::Error + 'source>),
    #[error(transparent)]
    Meta(Box<dyn std::error::Error + 'source>),
    #[error(transparent)]
    Python(pyo3::PyErr),
}

impl From<std::fmt::Error> for GenerationError<'_> {
    fn from(value: std::fmt::Error) -> Self {
        Self::Write(value)
    }
}

impl<'source> From<ayano::AyanoError<'source>> for GenerationError<'source> {
    fn from(value: ayano::AyanoError<'source>) -> Self {
        Self::Ayano(value)
    }
}

impl From<pyo3::PyErr> for GenerationError<'_> {
    fn from(value: pyo3::PyErr) -> Self {
        Self::Python(value)
    }
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
    fn write_preamble<'meta, W: Write + ?Sized>(&self, _: &mut W, _: &'meta Meta) -> Res<'source> {
        Ok(())
    }
    fn write_postamble<'meta, 'context, W: Write + ?Sized>(
        &self,
        _: &mut W,
        _: &'meta Meta,
        _: &'context mut Context,
    ) -> Res<'source> {
        Ok(())
    }
}

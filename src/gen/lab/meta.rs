use std::{
    borrow::{BorrowMut, Cow},
    collections::{HashMap, HashSet},
};

use itertools::Itertools;
use pyo3::PyErr;

use crate::{
    data::{TitleInfo, Token},
    path_engine::PathEngine,
};

use super::super::ayano::{self, AyanoBuilder, AyanoExecutor};

#[derive(Debug, thiserror::Error)]
pub enum MetaError<'l> {
    #[error("Failed to recognize Ayano syntax: {}", .0)]
    AyanoSyntax(ayano::SyntaxError<'l>),
}

#[derive(Debug, Default)]
pub struct SourceMeta<'source, Ayano> {
    pub footnotes: HashMap<Cow<'source, str>, Token<'source>>,
    pub refs: HashSet<Cow<'source, str>>,
    pub ayano: Ayano,
    pub title_info: TitleInfo<'source>,
}

impl<'source> SourceMeta<'source, AyanoBuilder> {
    fn empty() -> Self {
        Self::default()
    }

    pub fn collect<'meta, PEErr: std::error::Error + 'source, PE: PathEngine<PEErr>>(
        token: &'meta Token<'source>,
        path_engine: &PE,
    ) -> Result<Self, MetaError<'source>>
    where
        'source: 'meta,
    {
        let mut res = SourceMeta::empty();
        collect_single(token, &mut res, path_engine)?;
        Ok(res)
    }

    pub fn init_ayano(self) -> Result<SourceMeta<'source, AyanoExecutor>, PyErr> {
        Ok(SourceMeta {
            ayano: self.ayano.initialize()?,
            footnotes: self.footnotes,
            refs: self.refs,
            title_info: self.title_info,
        })
    }
}

fn collect_single<'meta, 'r, 'source, PEErr: std::error::Error + 'source, PE: PathEngine<PEErr>>(
    token: &'meta Token<'source>,
    dest: &'r mut SourceMeta<'source, AyanoBuilder>,
    path_engine: &PE,
) -> Result<(), MetaError<'source>>
where
    'source: 'meta,
    'meta: 'r,
{
    match token {
        Token::DisplayMath {
            ident: Some(ident), ..
        } => {
            dest.refs.borrow_mut().insert(ident.clone());
        }
        Token::Table {
            header,
            ident,
            cells,
            caption,
        } => {
            if let Some(ident) = ident {
                dest.refs.borrow_mut().insert(ident.clone());
            }
            collect_iterable(header, dest, path_engine)?;
            collect_iterable(cells, dest, path_engine)?;
            if let Some(caption) = caption {
                collect_single(caption, dest, path_engine)?;
            }
        }
        Token::Figure {
            ident: Some(ident),
            caption,
            src_name: _src_name,
            width: _width,
        } => {
            dest.refs.insert(ident.clone());
            if let Some(caption) = caption {
                collect_single(caption, dest, path_engine)?;
            }
        }
        Token::Multiple { tokens, .. } => {
            collect_iterable(tokens, dest, path_engine)?;
        }
        Token::FootnoteContent { content, ident } => {
            collect_single(content, dest, path_engine)?;
            dest.footnotes.insert(ident.clone(), content.borrow_ref());
        }
        Token::Ayano { data } => {
            dest.ayano
                .add_block(data, path_engine)
                .map_err(MetaError::AyanoSyntax)?;
        }
        Token::PageDiv
        | Token::Header { .. }
        | Token::Href { .. }
        | Token::List { .. }
        | Token::Paragraph { .. }
        | Token::InlineMath { .. }
        | Token::Reference { .. }
        | Token::FootnoteReference { .. }
        | Token::Figure { .. }
        | Token::DisplayMath { .. }
        | Token::CodeBlock { .. } => {}
        Token::Error { .. } => {}
    }
    Ok(())
}

fn collect_iterable<
    'r,
    'source: 'meta,
    'meta: 'r,
    I,
    PEErr: std::error::Error + 'source,
    PE: PathEngine<PEErr>,
>(
    tokens: I,
    dest: &'r mut SourceMeta<'source, AyanoBuilder>,
    path_engine: &PE,
) -> Result<(), MetaError<'source>>
where
    I: IntoIterator<Item = &'meta Token<'source>>,
{
    tokens
        .into_iter()
        .map(|token| collect_single(token, dest, path_engine))
        .try_collect()
}

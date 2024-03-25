use std::{
    borrow::BorrowMut,
    collections::{HashMap, HashSet},
};

use itertools::Itertools;
use pyo3::PyErr;

use crate::data::{TitleInfo, Token};

use super::super::ayano::{self, AyanoBuilder, AyanoExecutor};

#[derive(Debug, thiserror::Error)]
pub enum MetaError {
    #[error("Failed to recognize Ayano syntax: {}", .0)]
    AyanoSyntax(ayano::SyntaxError),
}

#[derive(Debug, Default)]
pub struct SourceMeta<'source, Ayano> {
    pub footnotes: HashMap<&'source str, Token<'source>>,
    pub refs: HashSet<&'source str>,
    pub ayano: Ayano,
    pub title_info: TitleInfo<'source>,
}

impl<'meta> SourceMeta<'meta, AyanoBuilder> {
    fn empty() -> Self {
        Self::default()
    }

    pub fn collect<'source>(token: &'meta Token<'source>) -> Result<Self, MetaError>
    where
        'source: 'meta,
    {
        let mut res: SourceMeta<'meta, _> = SourceMeta::empty();
        collect_single(token, &mut res)?;
        Ok(res)
    }

    pub fn init_ayano(self) -> Result<SourceMeta<'meta, AyanoExecutor>, PyErr> {
        Ok(SourceMeta {
            ayano: self.ayano.initialize()?,
            footnotes: self.footnotes,
            refs: self.refs,
            title_info: self.title_info,
        })
    }
}

fn collect_single<'meta, 'r, 'source>(
    token: &'meta Token<'source>,
    dest: &'r mut SourceMeta<'meta, AyanoBuilder>,
) -> Result<(), MetaError>
where
    'source: 'meta,
    'meta: 'r,
{
    match token {
        Token::DisplayMath {
            ident: Some(ident), ..
        } => {
            dest.refs.borrow_mut().insert(ident);
        }
        Token::Table {
            header,
            ident,
            cells,
            caption,
        } => {
            if let Some(ident) = ident {
                dest.refs.borrow_mut().insert(ident);
            }
            collect_iterable(header, dest)?;
            collect_iterable(cells, dest)?;
            if let Some(caption) = caption {
                collect_single(caption, dest)?;
            }
        }
        Token::Figure {
            ident: Some(ident),
            caption,
            src_name: _src_name,
            width: _width,
        } => {
            dest.refs.insert(ident);
            if let Some(caption) = caption {
                collect_single(caption, dest)?;
            }
        }
        Token::Multiple { tokens, .. } => {
            collect_iterable(tokens, dest)?;
        }
        Token::FootnoteContent { content, ident } => {
            collect_single(content, dest)?;
            dest.footnotes.insert(ident, content.borrow_ref());
        }
        Token::Ayano { data } => {
            dest.ayano.add_block(data).map_err(MetaError::AyanoSyntax)?;
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

fn collect_iterable<'r, 'source: 'meta, 'meta: 'r, I>(
    tokens: I,
    dest: &'r mut SourceMeta<'meta, AyanoBuilder>,
) -> Result<(), MetaError>
where
    I: IntoIterator<Item = &'meta Token<'source>>,
{
    tokens
        .into_iter()
        .map(|token| collect_single(token, dest))
        .try_collect()
}

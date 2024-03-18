use std::{
    borrow::{Borrow, Cow},
    ops::Deref,
    path::Path,
};

use url::Url;

// For whatever reason, std library Cow requires ToOwned for it's contained value to be cloned
// That makes NO SENSE in my case.
// I do not need a functionality to obtain owned value here,
// but rather be able to refer to the same data from multiple instances.

// Also, vectors of parsed tokens are obviously not intended to live longer than the source.
// In fact, for any possible appliance they can be assumed to life for as long as source does.
// Hope is for lifetime variance to help with lifetime casting, or th
#[derive(Debug, PartialEq)]
// #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Tokens<'source> {
    Owned(Box<[Token<'source>]>),
    Borrowed(&'source [Token<'source>]),
}

impl<'source> Deref for Tokens<'source> {
    type Target = [Token<'source>];

    fn deref(&self) -> &Self::Target {
        match &self {
            Tokens::Owned(b) => &b[..],
            Tokens::Borrowed(s) => s,
        }
    }
}

impl<'r, 'source: 'r> IntoIterator for &'r Tokens<'source> {
    type Item = &'r Token<'source>;

    type IntoIter = std::slice::Iter<'r, Token<'source>>;

    fn into_iter(self) -> Self::IntoIter {
        self.get_slice().into_iter()
    }
}

impl<'source> Tokens<'source> {
    pub fn new(value: impl Into<Box<[Token<'source>]>>) -> Self {
        Self::Owned(value.into())
    }

    pub fn get_slice<'r>(&'r self) -> &'r [Token<'source>]
    where
        'source: 'r,
    {
        match &self {
            Tokens::Owned(b) => &b[..],
            Tokens::Borrowed(s) => s,
        }
    }

    pub fn borrowed(&self) -> Tokens<'_> {
        Tokens::Borrowed(self.get_slice())
    }
}

#[derive(Debug, Clone, Hash, PartialEq)]
// #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct AyanoBlock<'code> {
    pub is_display: bool,
    pub is_static: bool,
    pub code: Tx<'code>,
    pub insert_path: Option<Cow<'code, Path>>,
}

// an abandoned idea. this would result in weird behavior, if underlying data happened to be moved.
// WARN! note this semantics: AyanoBlocks are deemed the same, if they are literally referencing the same data,
/*
impl PartialEq for AyanoBlock<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.is_display == other.is_display
            && self.is_static == other.is_static
            && self.code.as_ptr() == other.code.as_ptr()
            && self.code.len() == other.code.len()
            && self.insert_path == other.insert_path
    }
}
*/

#[derive(Debug, Clone, PartialEq)]
// #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ListType {
    Bullet,
    Num,
    Latin,
    Cyrillic,
    Roman,
}

#[derive(Debug, Clone, PartialEq)]
// #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Formatting {
    Bold,
    Italic,
    StrikeThrough,
}

#[derive(Debug, Clone, PartialEq)]
// #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Font {
    Normal,
    Caption,
}

type Tx<'source> = Cow<'source, str>;
type Pth<'source> = Cow<'source, Path>;

#[derive(Debug, PartialEq)]
// #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Token<'source> {
    PageDiv,
    Header {
        // 0..6
        order: usize,
        content: Tx<'source>,
    },
    Equation {
        content: Tx<'source>,
        ident: Option<Tx<'source>>,
    },
    Table {
        header: Tokens<'source>,
        cells: Tokens<'source>,
        caption: Option<Box<Token<'source>>>,
        ident: Option<Tx<'source>>,
    },
    Figure {
        src_name: Pth<'source>,
        caption: Option<Box<Token<'source>>>,
        ident: Option<Tx<'source>>,
        width: Option<f32>,
    },
    Href {
        // #[cfg_attr(feature = "serde", serde(with = "url_serde"))]
        url: Url,
        display: Tx<'source>,
    },
    Ayano {
        data: AyanoBlock<'source>,
    },
    List {
        list_type: ListType,
        content: Tokens<'source>,
    },
    #[deprecated = "`Font`-thing inside paragraph token does similar thing, so it would be wise to merge them"]
    Formatted(Formatting, Box<Token<'source>>),
    Text {
        tokens: Tokens<'source>,
    },
    // TODO move font and formatting exclusively to here
    // TODO add a flag to indicate, if this paragraph contains escaped chars (if it was parsed from caption, basically)
    Paragraph(Font, Tx<'source>),
    InlineMathmode {
        content: Tx<'source>,
    },
    Reference {
        ident: Tx<'source>,
    },
    FootNoteReference {
        ident: Tx<'source>,
    },
    FootNoteContent {
        content: Box<Token<'source>>,
        ident: Tx<'source>,
    },
    CodeBlock {
        code: Tx<'source>,
        language: Option<Tx<'source>>,
    },
    Error {
        message: String,
    },
}

trait ToStaticExt {
    type AsStatic;
    fn to_static(&self) -> Self::AsStatic;
}

impl ToStaticExt for Cow<'_, str> {
    type AsStatic = Cow<'static, str>;

    fn to_static(&self) -> Self::AsStatic {
        Cow::Owned(self.to_string())
    }
}

impl ToStaticExt for Cow<'_, Path> {
    type AsStatic = Cow<'static, Path>;

    fn to_static(&self) -> Self::AsStatic {
        Cow::Owned(self.to_path_buf())
    }
}

impl ToStaticExt for Token<'_> {
    type AsStatic = Token<'static>;

    fn to_static(&self) -> Self::AsStatic {
        self.to_static_token()
    }
}

impl ToStaticExt for Tokens<'_> {
    type AsStatic = Tokens<'static>;

    fn to_static(&self) -> Self::AsStatic {
        let slice = self.get_slice();
        Tokens::new(
            slice
                .into_iter()
                .map(|t| t.to_static_token())
                .collect::<Box<_>>(),
        )
    }
}

impl ToStaticExt for Box<Token<'_>> {
    type AsStatic = Box<Token<'static>>;

    fn to_static(&self) -> Self::AsStatic {
        Box::new((&**self).to_static())
    }
}

impl ToStaticExt for AyanoBlock<'_> {
    type AsStatic = AyanoBlock<'static>;

    fn to_static(&self) -> Self::AsStatic {
        let AyanoBlock {
            code,
            insert_path,
            is_display,
            is_static,
        } = self;
        AyanoBlock {
            code: code.to_static(),
            insert_path: insert_path.as_ref().map(Cow::to_static),
            is_static: *is_static,
            is_display: *is_display,
        }
    }
}

impl<'source> Token<'source> {
    pub fn text(text: impl Into<Cow<'source, str>>) -> Self {
        Self::Paragraph(Font::Normal, text.into())
    }

    pub fn borrow_ref<'r>(&'r self) -> Token<'r>
    where
        'source: 'r,
    {
        match self {
            Token::PageDiv => Token::PageDiv,
            Token::Header { order, content } => Token::Header {
                order: *order,
                content: Cow::Borrowed(content),
            },
            Token::Equation { content, ident } => Token::Equation {
                content: Cow::Borrowed(&content),
                ident: ident.as_ref().map(|s| Cow::<'r, str>::Borrowed(s)),
            },
            Token::Table {
                header,
                cells,
                caption,
                ident,
            } => Token::Table {
                header: header.borrowed(),
                cells: cells.borrowed(),
                caption: caption.as_ref().map(|t| Box::new(Token::borrow_ref(t))),
                ident: ident.as_ref().map(|s| Cow::<'r, str>::Borrowed(s)),
            },
            Token::Figure {
                src_name,
                caption,
                ident,
                width,
            } => Token::Figure {
                src_name: Cow::Borrowed(src_name),
                caption: caption.as_ref().map(|t| Box::new(Token::borrow_ref(t))),
                ident: ident.as_ref().map(|s| Cow::<'r, str>::Borrowed(s)),
                width: *width,
            },
            Token::Href { url, display } => Token::Href {
                url: url.clone(),
                display: Cow::Borrowed(&display),
            },
            Token::Ayano { data } => Token::Ayano { data: data.clone() },
            Token::List { list_type, content } => Token::List {
                list_type: list_type.clone(),
                content: Tokens::Borrowed(&content),
            },
            Token::Formatted(formatting, content) => {
                Token::Formatted(formatting.clone(), Box::new(content.borrow_ref()))
            }
            Token::Text { tokens } => Token::Text {
                tokens: Tokens::Borrowed(tokens),
            },
            Token::Paragraph(font, content) => {
                Token::Paragraph(font.clone(), Cow::Borrowed(content))
            }
            Token::InlineMathmode { content } => Token::InlineMathmode {
                content: Cow::Borrowed(&content),
            },
            Token::Reference { ident } => Token::Reference {
                ident: Cow::Borrowed(&ident),
            },
            Token::FootNoteReference { ident } => Token::FootNoteReference {
                ident: Cow::Borrowed(ident),
            },
            Token::FootNoteContent { content, ident } => Token::FootNoteContent {
                content: Box::new(content.borrow_ref()),
                ident: Cow::Borrowed(ident),
            },
            Token::CodeBlock { code, language } => Token::CodeBlock {
                code: Cow::Borrowed(&code),
                language: language.as_ref().map(|c| Cow::Borrowed(c.borrow())),
            },
            Token::Error { message } => Token::Error {
                message: message.clone(),
            },
        }
    }

    pub fn to_static_token<'r>(&'r self) -> Token<'static>
    where
        'source: 'r,
    {
        match self {
            Token::PageDiv => Token::PageDiv,
            Token::Header { order, content } => Token::Header {
                order: *order,
                content: content.to_static(),
            },
            Token::Equation { content, ident } => Token::Equation {
                content: content.to_static(),
                ident: ident.as_ref().map(Cow::to_static),
            },
            Token::Table {
                header,
                cells,
                caption,
                ident,
            } => Token::Table {
                header: header.to_static(),
                cells: cells.to_static(),
                caption: caption.as_ref().map(Box::to_static),
                ident: ident.as_ref().map(Cow::to_static),
            },
            Token::Figure {
                src_name,
                caption,
                ident,
                width,
            } => Token::Figure {
                src_name: src_name.to_static(),
                caption: caption.as_ref().map(Box::to_static),
                ident: ident.as_ref().map(Cow::to_static),
                width: *width,
            },
            Token::Href { url, display } => Token::Href {
                url: url.clone(),
                display: display.to_static(),
            },
            Token::Ayano { data } => Token::Ayano {
                data: data.to_static(),
            },
            Token::List { list_type, content } => Token::List {
                list_type: list_type.clone(),
                content: content.to_static(),
            },
            Token::Formatted(formatting, inner) => {
                Token::Formatted(formatting.clone(), inner.to_static())
            }
            Token::Text { tokens } => Token::Text {
                tokens: tokens.to_static(),
            },
            Token::Paragraph(font, inner) => Token::Paragraph(font.clone(), inner.to_static()),
            Token::InlineMathmode { content } => Token::InlineMathmode {
                content: content.to_static(),
            },
            Token::Reference { ident } => Token::Reference {
                ident: ident.to_static(),
            },
            Token::FootNoteReference { ident } => Token::FootNoteReference {
                ident: ident.to_static(),
            },
            Token::FootNoteContent { content, ident } => Token::FootNoteContent {
                content: content.to_static(),
                ident: ident.to_static(),
            },
            Token::CodeBlock { code, language } => Token::CodeBlock {
                code: code.to_static(),
                language: language.as_ref().map(Cow::to_static),
            },
            Token::Error { message } => Token::Error {
                message: message.clone(),
            },
        }
    }
}

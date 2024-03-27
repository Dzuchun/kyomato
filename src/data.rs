use std::{
    borrow::{Borrow, Cow},
    ops::Deref,
    path::Path,
    rc::Rc,
};

use url::Url;

use crate::util::StaticDebug;

#[derive(Debug, Default, PartialEq, Clone)]
pub struct TitleInfo<'source> {
    pub header_line1: Option<Tx<'source>>,
    pub header_line2: Option<Tx<'source>>,
    pub document_type: Option<Tx<'source>>,
    pub title_line1: Option<Tx<'source>>,
    pub title_line2: Option<Tx<'source>>,
    pub title_line3: Option<Tx<'source>>,
    pub title_line4: Option<Tx<'source>>,
    pub author_line1: Option<Tx<'source>>,
    pub author_line2: Option<Tx<'source>>,
    pub author_line3: Option<Tx<'source>>,
    pub date: Option<Tx<'source>>,
    pub prof: Option<Tx<'source>>,
    pub code_section_title: Option<Tx<'source>>,
}

// For whatever reason, std library Cow requires ToOwned for it's contained value to be cloned
// That makes NO SENSE in my case.
// I do not need a functionality to obtain owned value here,
// but rather be able to refer to the same data from multiple instances.

// Also, vectors of parsed tokens are obviously not intended to live longer than the source.
// In fact, for any possible appliance they can be assumed to life for as long as source does.
// Hope is for lifetime variance to help with lifetime casting, or th
#[derive(Debug, PartialEq, Clone)]
// #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Tokens<'source>(Rc<[Token<'source>]>);

impl<'source> Deref for Tokens<'source> {
    type Target = [Token<'source>];

    fn deref(&self) -> &Self::Target {
        &*self.0
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
        Self(Rc::from(value.into()))
    }

    pub fn get_slice<'r>(&'r self) -> &'r [Token<'source>]
    where
        'source: 'r,
    {
        self
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DisplayState<'code> {
    NotDisplayed,
    DisplayedNoCaption,
    Caption(Box<Token<'code>>),
}

impl<'code> DisplayState<'code> {
    fn to_static(&self) -> DisplayState<'static> {
        match self {
            DisplayState::NotDisplayed => DisplayState::NotDisplayed,
            DisplayState::DisplayedNoCaption => DisplayState::DisplayedNoCaption,
            DisplayState::Caption(caption) => {
                DisplayState::Caption(Box::new(caption.to_static_token()))
            }
        }
    }
}

#[derive(Debug, Clone)]
// #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct AyanoBlock<'code> {
    pub ident: StaticDebug<u64>,
    pub display_state: DisplayState<'code>,
    pub is_space_before: bool,
    pub is_static: bool,
    pub code: Tx<'code>,
    pub insert_path: Option<Cow<'code, Path>>,
}

impl PartialEq for AyanoBlock<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.display_state == other.display_state
            && self.is_space_before == other.is_space_before
            && self.is_static == other.is_static
            && self.code == other.code
            && self.insert_path == other.insert_path
    }
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

type Tx<'source> = Cow<'source, str>;
type Pth<'source> = Cow<'source, Path>;

#[derive(Debug, PartialEq, Clone)]
// #[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum Token<'source> {
    PageDiv,
    Header {
        // 0..6
        order: usize,
        content: Tx<'source>,
    },
    DisplayMath {
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
        space_before: bool,
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
    Multiple {
        tokens: Tokens<'source>,
    },
    // TODO move font and formatting exclusively to here
    // TODO add a flag to indicate, if this paragraph contains escaped chars (if it was parsed from caption, basically)
    Paragraph {
        is_newline: bool,
        space_before: bool,
        formatting: Option<Formatting>,
        content: Tx<'source>,
    },
    InlineMath {
        space_before: bool,
        content: Tx<'source>,
    },
    Reference {
        space_before: bool,
        ident: Tx<'source>,
    },
    FootnoteReference {
        space_before: bool,
        ident: Tx<'source>,
    },
    FootnoteContent {
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
            ident,
            code,
            insert_path,
            display_state,
            is_static,
            is_space_before,
        } = self;
        AyanoBlock {
            ident: ident.clone(),
            code: code.to_static(),
            insert_path: insert_path.as_ref().map(Cow::to_static),
            is_static: *is_static,
            display_state: display_state.to_static(),
            is_space_before: *is_space_before,
        }
    }
}

impl<'source> Token<'source> {
    pub fn text(text: impl Into<Cow<'source, str>>) -> Self {
        Self::Paragraph {
            space_before: false,
            is_newline: false,
            content: text.into(),
            formatting: None,
        }
    }

    pub fn borrow_ref(&self) -> Token<'source> {
        match self {
            Token::PageDiv => Token::PageDiv,
            Token::Header { order, content } => Token::Header {
                order: *order,
                content: content.clone(),
            },
            Token::DisplayMath { content, ident } => Token::DisplayMath {
                content: content.clone(),
                ident: ident.clone(),
            },
            Token::Table {
                header,
                cells,
                caption,
                ident,
            } => Token::Table {
                header: header.clone(),
                cells: cells.clone(),
                caption: caption.as_ref().map(|t| Box::new(Token::borrow_ref(t))),
                ident: ident.clone(),
            },
            Token::Figure {
                src_name,
                caption,
                ident,
                width,
            } => Token::Figure {
                src_name: src_name.clone(),
                caption: caption.clone(),
                ident: ident.clone(),
                width: width.clone(),
            },
            Token::Href {
                url,
                display,
                space_before,
            } => Token::Href {
                url: url.clone(),
                display: display.clone(),
                space_before: *space_before,
            },
            Token::Ayano { data } => Token::Ayano { data: data.clone() },
            Token::List { list_type, content } => Token::List {
                list_type: list_type.clone(),
                content: content.clone(),
            },
            Token::Multiple { tokens } => Token::Multiple {
                tokens: tokens.clone(),
            },
            Token::Paragraph {
                is_newline,
                formatting,
                content,
                space_before,
            } => Token::Paragraph {
                is_newline: *is_newline,
                formatting: formatting.clone(),
                content: content.clone(),
                space_before: *space_before,
            },
            Token::InlineMath {
                content,
                space_before,
            } => Token::InlineMath {
                content: content.clone(),
                space_before: *space_before,
            },
            Token::Reference {
                ident,
                space_before,
            } => Token::Reference {
                ident: ident.clone(),
                space_before: *space_before,
            },
            Token::FootnoteReference {
                ident,
                space_before,
            } => Token::FootnoteReference {
                ident: ident.clone(),
                space_before: *space_before,
            },
            Token::FootnoteContent { content, ident } => Token::FootnoteContent {
                content: content.clone(),
                ident: ident.clone(),
            },
            Token::CodeBlock { code, language } => Token::CodeBlock {
                code: code.clone(),
                language: language.clone(),
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
            Token::DisplayMath { content, ident } => Token::DisplayMath {
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
                width: width.clone(),
            },
            Token::Href {
                url,
                display,
                space_before,
            } => Token::Href {
                url: url.clone(),
                display: display.to_static(),
                space_before: *space_before,
            },
            Token::Ayano { data } => Token::Ayano {
                data: data.to_static(),
            },
            Token::List { list_type, content } => Token::List {
                list_type: list_type.clone(),
                content: content.to_static(),
            },
            Token::Multiple { tokens } => Token::Multiple {
                tokens: tokens.to_static(),
            },
            Token::Paragraph {
                is_newline,
                space_before,
                formatting,
                content,
            } => Token::Paragraph {
                is_newline: *is_newline,
                formatting: formatting.clone(),
                content: content.to_static(),
                space_before: *space_before,
            },
            Token::InlineMath {
                content,
                space_before,
            } => Token::InlineMath {
                content: content.to_static(),
                space_before: *space_before,
            },
            Token::Reference {
                ident,
                space_before,
            } => Token::Reference {
                ident: ident.to_static(),
                space_before: *space_before,
            },
            Token::FootnoteReference {
                ident,
                space_before,
            } => Token::FootnoteReference {
                ident: ident.to_static(),
                space_before: *space_before,
            },
            Token::FootnoteContent { content, ident } => Token::FootnoteContent {
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

    /// Defines if newline after this token will cause problems
    ///
    /// None means that it depends on previous tokens
    pub fn is_newline_allergic(&self) -> Option<bool> {
        match self {
            Token::Ayano {
                data: AyanoBlock {
                    is_static: false, ..
                },
            }
            | Token::Href { .. }
            | Token::Paragraph { .. }
            | Token::InlineMath { .. }
            | Token::Reference { .. }
            | Token::FootnoteReference { .. }
            | Token::FootnoteContent { .. }
            | Token::Error { .. } => Some(false),
            Token::PageDiv
            | Token::Header { .. }
            | Token::DisplayMath { .. }
            | Token::Table { .. }
            | Token::Figure { .. }
            | Token::List { .. }
            | Token::CodeBlock { .. } => Some(true),
            Token::Ayano {
                data: AyanoBlock {
                    is_static: true, ..
                },
            }
            | Token::Multiple { .. } => None,
        }
    }
}

/// # On token storage
///
/// Basically, tokens are not manipulated directly; Instead, they are aggregated into so-called "trees" to allow efficient data storage.
///
/// Tree consists of storage and a root, "pointing" somewhere inside it.
///
/// To create a tree, `Unsealed Storage` must be created first. While storage is unsealed, one can add a text, a token or a token slice (iterator of tokens)
/// for it to store. Once data was stored, `Unsealed Storage` will return a `Promise` to said data. Exact form of this `Promise` is up to storage to
/// decide, and that's exactly what allows token tree to only store stuff it needs, and the way it needs.
///
/// Once all tokens were added to `Unsealed Storage`, it can be transformed into a regular `Storage`. `Storage` allows to see text and token(s) with
/// the promises returned by `Unsealed Storage`.
///
/// ## Capabilities
/// If you happen to know, that all of your data is contained within a certain string, you may pass a reference to it, for storage to use.
/// At any time, if you decide to transfer your data from one storage to another - you can absolutely do it via dedicated method.
/// This also covers situations where you don't wanna hold onto some reference, and decide to copy over all of the data
///
/// There's also an ability to move the data into the storage, which can be useful for large text buffers.
/// Please note that this last operation is hard to define properly, so some of the methods used for it, are marked `unsafe` (since they break the invariant)
#[cfg(borrowing_concept)]
mod borrowing_concept {
    use std::{borrow::Cow, marker::PhantomData, ops::Range};

    struct ThingTree<Text, Leaf, Leaves, Storage> {
        root: Leaf,
        provider: Storage,
        _phantom: PhantomData<(Text, Leaves)>,
    }

    impl<Text, Leaf, Leaves, Storage> ThingTree<Text, Leaf, Leaves, Storage> {
        fn new<ToStorage>(thing: Thing<Text, Leaf, Leaves>, mut storage: ToStorage) -> Self
        where
            ToStorage: UnsealedStorage<Text, Leaf, Leaves, Sealed = Storage>,
        {
            let child = storage.register_leaf(thing);
            Self {
                root: child,
                provider: storage.seal(),
                _phantom: PhantomData,
            }
        }

        fn root(&self) -> &Leaf {
            &self.root
        }

        fn root_thing(&self) -> &Thing<Text, Leaf, Leaves>
        where
            Storage: SealedStorage<Text, Leaf, Leaves>,
        {
            self.provider.get_leaf(&self.root)
        }

        fn storage(&self) -> &Storage {
            &self.provider
        }

        fn move_to<OT, OL, OLS, ToStorage>(
            &self,
            other_storage: &mut ToStorage,
        ) -> Thing<OT, OL, OLS>
        where
            ToStorage: UnsealedStorage<OT, OL, OLS>,
            Storage: SealedStorage<Text, Leaf, Leaves>,
        {
            transfer_nodes(
                self.root_thing(),
                self.storage(),
                other_storage,
                &mut Vec::new(),
            )
        }

        fn write_to<W: std::fmt::Write + ?Sized>(&self, output: &mut W) -> std::fmt::Result
        where
            Storage: SealedStorage<Text, Leaf, Leaves>,
        {
            let thing = self.root_thing();
            thing.write_to(&self.provider, output)
        }
    }

    pub enum Thing<Text, Child, Children> {
        Text {
            text: Text,
        },
        Child {
            text: Text,
            child1: Child,
            child2: Child,
        },
        Multiple {
            children: Children,
        },
    }

    impl<Text, Leaf, Leaves> Thing<Text, Leaf, Leaves> {
        fn write_to<P, W>(&self, provider: &P, out: &mut W) -> std::fmt::Result
        where
            P: SealedStorage<Text, Leaf, Leaves>,
            W: std::fmt::Write + ?Sized,
        {
            match self {
                Thing::Text { text } => write!(out, "Text({})", provider.get_text(text)),
                Thing::Child {
                    text,
                    child1,
                    child2,
                } => {
                    write!(out, "Child({}, ", provider.get_text(text))?;
                    provider.get_leaf(child1).write_to(provider, out)?;
                    out.write_str(", ")?;
                    provider.get_leaf(child2).write_to(provider, out)?;
                    out.write_str(")")?;
                    Ok(())
                }
                Thing::Multiple { children } => {
                    out.write_str("Multiple[")?;
                    let children = provider.get_leaves(children);
                    if let Some(first) = children.first() {
                        first.write_to(provider, out)?;
                        for child in &children[1..] {
                            out.write_str(", ")?;
                            child.write_to(provider, out)?;
                        }
                    }
                    out.write_str("]")?;
                    Ok(())
                }
            }
        }
    }

    pub trait UnsealedStorage<Text, Leaf, Leaves> {
        type Sealed: SealedStorage<Text, Leaf, Leaves>;
        fn register_text(&mut self, text: &str) -> Text;
        fn register_leaf(&mut self, leaf: Thing<Text, Leaf, Leaves>) -> Leaf;
        fn register_leaves<'s>(
            &'s mut self,
            leaves: impl Iterator<Item = Thing<Text, Leaf, Leaves>>,
        ) -> Leaves;
        fn seal(self) -> Self::Sealed;
    }

    pub trait SealedStorage<Text, Leaf, Leaves> {
        fn get_text<'s>(&'s self, promise: &Text) -> &'s str;
        fn get_leaf<'s>(&'s self, promise: &Leaf) -> &'s Thing<Text, Leaf, Leaves>;
        fn get_leaves<'s>(&'s self, promise: &Leaves) -> &'s [Thing<Text, Leaf, Leaves>];
    }

    trait UnsealedStorageExt<Text, Leaf, Leaves>: UnsealedStorage<Text, Leaf, Leaves> {
        fn with_text_ref<'text>(
            self,
            text: &'text str,
        ) -> UnsealedTextRefStorage<'text, Text, Leaf, Leaves, Self>
        where
            Self: Sized,
        {
            UnsealedTextRefStorage::<'text, Text, Leaf, Leaves, Self> {
                text,
                inner: self,
                _phantom: PhantomData,
            }
        }

        fn with_text_find(
            self,
            text: impl Into<String>,
        ) -> UnsealedTextFindStorage<Text, Leaf, Leaves, Self>
        where
            Self: Sized,
        {
            UnsealedTextFindStorage {
                text: text.into(),
                inner: self,
                _phantom: PhantomData,
            }
        }

        fn text_owned(self) -> UnsealedTextOwnStorage<Text, Leaf, Leaves, Self>
        where
            Self: Sized,
        {
            UnsealedTextOwnStorage::<Text, Leaf, Leaves, Self> {
                text: String::new(),
                inner: self,
                _phantom: PhantomData,
            }
        }

        fn leaves_owned(self) -> UnsealedLeavesOwnStorage<Text, Leaf, Leaves, Self>
        where
            Self: Sized,
        {
            UnsealedLeavesOwnStorage::<Text, Leaf, Leaves, Self> {
                things: Vec::new(),
                inner: self,
            }
        }
    }
    impl<Text, Leaf, Leaves, S: UnsealedStorage<Text, Leaf, Leaves>>
        UnsealedStorageExt<Text, Leaf, Leaves> for S
    {
    }
    trait OptPromiseInfo<PromiseInfo, StorageType>: From<PromiseInfo> {
        fn info(&self) -> Option<&PromiseInfo>;
    }

    struct TextRefStorageType;
    pub struct UnsealedTextRefStorage<'text, Text, Leaf, Leaves, InnerStorage> {
        text: &'text str,
        inner: InnerStorage,
        _phantom: PhantomData<(Text, Leaf, Leaves)>,
    }

    fn try_find_pointer_match<'h, 'f>(haystack: &'h str, find: &'f str) -> Option<Range<usize>> {
        let referee_ptr = haystack.as_ptr() as usize;
        let text_ptr = find.as_ptr() as usize;
        if text_ptr < referee_ptr {
            return None;
        }
        let index_guess = text_ptr - referee_ptr;
        let self_len = haystack.len();
        if index_guess >= self_len {
            return None;
        }
        // yeah, seems like it
        // lastly, let's rule out partial overlap:
        let text_len = find.len();
        if text_len + index_guess > self_len {
            // buffers overlap partially, which is weird, probably should
            // TODO warn about that too
            return None;
        }
        // let's check that literally, for a good measure:
        // TODO probably add cfg to remove that in release build?
        if !haystack[index_guess..].starts_with(find) {
            // huh? that's something weird, probably must
            // TODO warn about it
            return None;
        }
        // ok, so it seems like we've found a match using nothing but pointer-things.
        // that's great, cause it's literally O(1) for any buffer size
        return Some(index_guess..index_guess + text_len);
    }

    impl<'text, Text, Leaf, Leaves, InnerStorage> UnsealedStorage<Text, Leaf, Leaves>
        for UnsealedTextRefStorage<'text, Text, Leaf, Leaves, InnerStorage>
    where
        InnerStorage: UnsealedStorage<Text, Leaf, Leaves>,
        Text: OptPromiseInfo<&'text str, TextRefStorageType>,
    {
        type Sealed = TextRefStorage<'text, Text, Leaf, Leaves, InnerStorage::Sealed>;

        fn register_text(&mut self, text: &str) -> Text {
            // first, check out if this text, as a pointer, points inside this provider's referee:
            if let Some(range) = try_find_pointer_match(self.text, text) {
                return self.text[range].into();
            }

            // then, let's try finding this sort of sequence in the entire referee:
            if let Some(ind) = self.text.find(text) {
                // well, return corresponding slice, then
                return self.text[ind..ind + text.len()].into();
            }

            // lastly, refer to inner provider
            self.inner.register_text(text)
        }

        fn register_leaf(&mut self, leaf: Thing<Text, Leaf, Leaves>) -> Leaf {
            self.inner.register_leaf(leaf)
        }

        fn register_leaves<'s>(
            &'s mut self,
            leaves: impl Iterator<Item = Thing<Text, Leaf, Leaves>>,
        ) -> Leaves {
            self.inner.register_leaves(leaves)
        }

        fn seal(self) -> Self::Sealed {
            Self::Sealed {
                inner: self.inner.seal(),
                _phantom: PhantomData,
            }
        }
    }

    pub struct TextRefStorage<'text, Text, Leaf, Leaves, InnerStorage> {
        inner: InnerStorage,
        _phantom: PhantomData<(&'text (), Text, Leaf, Leaves)>,
    }

    impl<'text, Text, Leaf, Leaves, InnerStorage> SealedStorage<Text, Leaf, Leaves>
        for TextRefStorage<'text, Text, Leaf, Leaves, InnerStorage>
    where
        InnerStorage: SealedStorage<Text, Leaf, Leaves>,
        Text: OptPromiseInfo<&'text str, TextRefStorageType>,
    {
        fn get_text<'s>(&'s self, promise: &Text) -> &'s str {
            if let Some(res) = promise.info() {
                res
            } else {
                self.inner.get_text(promise)
            }
        }

        fn get_leaf<'s>(&'s self, promise: &Leaf) -> &'s Thing<Text, Leaf, Leaves> {
            self.inner.get_leaf(promise)
        }

        fn get_leaves<'s>(&'s self, promise: &Leaves) -> &'s [Thing<Text, Leaf, Leaves>] {
            self.inner.get_leaves(promise)
        }
    }

    struct TextOwnStorageType;
    pub struct UnsealedTextOwnStorage<Text, Leaf, Leaves, InnerStorage> {
        text: String,
        inner: InnerStorage,
        _phantom: PhantomData<(Text, Leaf, Leaves)>,
    }

    impl<Text, Leaf, Leaves, InnerStorage> UnsealedStorage<Text, Leaf, Leaves>
        for UnsealedTextOwnStorage<Text, Leaf, Leaves, InnerStorage>
    where
        InnerStorage: UnsealedStorage<Text, Leaf, Leaves>,
        Text: OptPromiseInfo<Range<usize>, TextOwnStorageType>,
    {
        type Sealed = TextOwnStorage<Text, Leaf, Leaves, InnerStorage::Sealed>;

        fn register_text(&mut self, text: &str) -> Text {
            let range_start = self.text.len();
            self.text.push_str(text);
            let range_end = self.text.len();
            Text::from(range_start..range_end)
        }

        fn register_leaf(&mut self, leaf: Thing<Text, Leaf, Leaves>) -> Leaf {
            self.inner.register_leaf(leaf)
        }

        fn register_leaves<'s>(
            &'s mut self,
            leaves: impl Iterator<Item = Thing<Text, Leaf, Leaves>>,
        ) -> Leaves {
            self.inner.register_leaves(leaves)
        }

        fn seal(self) -> Self::Sealed {
            Self::Sealed {
                text: self.text.into_boxed_str(),
                inner: self.inner.seal(),
                _phantom: PhantomData,
            }
        }
    }

    pub struct TextOwnStorage<Text, Leaf, Leaves, InnerStorage> {
        text: Box<str>,
        inner: InnerStorage,
        _phantom: PhantomData<(Text, Leaf, Leaves)>,
    }

    impl<Text, Leaf, Leaves, InnerStorage> SealedStorage<Text, Leaf, Leaves>
        for TextOwnStorage<Text, Leaf, Leaves, InnerStorage>
    where
        InnerStorage: SealedStorage<Text, Leaf, Leaves>,
        Text: OptPromiseInfo<Range<usize>, TextOwnStorageType>,
    {
        fn get_text<'s>(&'s self, promise: &Text) -> &'s str {
            if let Some(range) = promise.info() {
                &self.text[range.clone()]
            } else {
                self.inner.get_text(promise)
            }
        }

        fn get_leaf<'s>(&'s self, promise: &Leaf) -> &'s Thing<Text, Leaf, Leaves> {
            self.inner.get_leaf(promise)
        }

        fn get_leaves<'s>(&'s self, promise: &Leaves) -> &'s [Thing<Text, Leaf, Leaves>] {
            self.inner.get_leaves(promise)
        }
    }

    struct TextFindStorageType;
    pub struct UnsealedTextFindStorage<Text, Leaf, Leaves, InnerStorage> {
        text: String,
        inner: InnerStorage,
        _phantom: PhantomData<(Text, Leaf, Leaves)>,
    }

    impl<Text, Leaf, Leaves, InnerStorage> UnsealedStorage<Text, Leaf, Leaves>
        for UnsealedTextFindStorage<Text, Leaf, Leaves, InnerStorage>
    where
        InnerStorage: UnsealedStorage<Text, Leaf, Leaves>,
        Text: OptPromiseInfo<Range<usize>, TextFindStorageType>,
    {
        type Sealed = TextFindStorage<Text, Leaf, Leaves, InnerStorage::Sealed>;

        fn register_text(&mut self, text: &str) -> Text {
            if let Some(ind) = self.text.find(text) {
                return Text::from(ind..ind + text.len());
            }
            let range_start = self.text.len();
            self.text.push_str(text);
            let range_end = self.text.len();
            Text::from(range_start..range_end)
        }

        fn register_leaf(&mut self, leaf: Thing<Text, Leaf, Leaves>) -> Leaf {
            self.inner.register_leaf(leaf)
        }

        fn register_leaves<'s>(
            &'s mut self,
            leaves: impl Iterator<Item = Thing<Text, Leaf, Leaves>>,
        ) -> Leaves {
            self.inner.register_leaves(leaves)
        }

        fn seal(self) -> Self::Sealed {
            Self::Sealed {
                text: self.text.into_boxed_str(),
                inner: self.inner.seal(),
                _phantom: PhantomData,
            }
        }
    }

    pub struct TextFindStorage<Text, Leaf, Leaves, InnerStorage> {
        text: Box<str>,
        inner: InnerStorage,
        _phantom: PhantomData<(Text, Leaf, Leaves)>,
    }

    impl<Text, Leaf, Leaves, InnerStorage> SealedStorage<Text, Leaf, Leaves>
        for TextFindStorage<Text, Leaf, Leaves, InnerStorage>
    where
        InnerStorage: SealedStorage<Text, Leaf, Leaves>,
        Text: OptPromiseInfo<Range<usize>, TextFindStorageType>,
    {
        fn get_text<'s>(&'s self, promise: &Text) -> &'s str {
            if let Some(range) = promise.info() {
                &self.text[range.clone()]
            } else {
                self.inner.get_text(promise)
            }
        }

        fn get_leaf<'s>(&'s self, promise: &Leaf) -> &'s Thing<Text, Leaf, Leaves> {
            self.inner.get_leaf(promise)
        }

        fn get_leaves<'s>(&'s self, promise: &Leaves) -> &'s [Thing<Text, Leaf, Leaves>] {
            self.inner.get_leaves(promise)
        }
    }

    struct LeavesOwnStorageType;
    pub struct UnsealedLeavesOwnStorage<Text, Leaf, Leaves, P> {
        things: Vec<Thing<Text, Leaf, Leaves>>,
        inner: P,
    }

    impl<Text, Leaf, Leaves, InnerStorage> UnsealedStorage<Text, Leaf, Leaves>
        for UnsealedLeavesOwnStorage<Text, Leaf, Leaves, InnerStorage>
    where
        InnerStorage: UnsealedStorage<Text, Leaf, Leaves>,
        Leaf: OptPromiseInfo<usize, LeavesOwnStorageType>,
        Leaves: OptPromiseInfo<Range<usize>, LeavesOwnStorageType>,
    {
        type Sealed = LeavesOwnStorage<Text, Leaf, Leaves, InnerStorage::Sealed>;

        fn register_text(&mut self, text: &str) -> Text {
            self.inner.register_text(text)
        }

        fn register_leaf(&mut self, leaf: Thing<Text, Leaf, Leaves>) -> Leaf {
            let ind = self.things.len();
            self.things.push(leaf);
            Leaf::from(ind)
        }

        fn register_leaves<'s>(
            &'s mut self,
            leaves: impl Iterator<Item = Thing<Text, Leaf, Leaves>>,
        ) -> Leaves {
            let range_start = self.things.len();
            self.things.extend(leaves);
            let range_end = self.things.len();
            Leaves::from(range_start..range_end)
        }

        fn seal(self) -> Self::Sealed {
            LeavesOwnStorage {
                things: self.things.into_boxed_slice(),
                inner: self.inner.seal(),
            }
        }
    }

    pub struct LeavesOwnStorage<Text, Leaf, Leaves, InnerStorage> {
        things: Box<[Thing<Text, Leaf, Leaves>]>,
        inner: InnerStorage,
    }

    impl<Text, Leaf, Leaves, InnerStorage> SealedStorage<Text, Leaf, Leaves>
        for LeavesOwnStorage<Text, Leaf, Leaves, InnerStorage>
    where
        InnerStorage: SealedStorage<Text, Leaf, Leaves>,
        Leaf: OptPromiseInfo<usize, LeavesOwnStorageType>,
        Leaves: OptPromiseInfo<Range<usize>, LeavesOwnStorageType>,
    {
        fn get_text<'s>(&'s self, promise: &Text) -> &'s str {
            self.inner.get_text(promise)
        }

        fn get_leaf<'s>(&'s self, promise: &Leaf) -> &'s Thing<Text, Leaf, Leaves> {
            if let Some(ind) = promise.info() {
                &self.things[*ind]
            } else {
                self.inner.get_leaf(promise)
            }
        }

        fn get_leaves<'s>(&'s self, promise: &Leaves) -> &'s [Thing<Text, Leaf, Leaves>] {
            if let Some(range) = promise.info() {
                &self.things[range.clone()]
            } else {
                self.inner.get_leaves(promise)
            }
        }
    }

    enum NeverText {}
    impl From<Range<usize>> for NeverText {
        fn from(_: Range<usize>) -> Self {
            unimplemented!("This data type is not supported")
        }
    }
    impl<'text> From<&'text str> for NeverText {
        fn from(_: &'text str) -> Self {
            unimplemented!("This data type is not supported")
        }
    }
    impl<T, StorageType> OptPromiseInfo<T, StorageType> for NeverText
    where
        NeverText: From<T>,
    {
        fn info(&self) -> Option<&T> {
            unimplemented!("This data type is not supported")
        }
    }
    enum NeverLeaf {}
    impl From<usize> for NeverLeaf {
        fn from(_: usize) -> Self {
            unimplemented!("This data type is not supported")
        }
    }
    impl<T, StorageType> OptPromiseInfo<T, StorageType> for NeverLeaf
    where
        NeverLeaf: From<T>,
    {
        fn info(&self) -> Option<&T> {
            unimplemented!("This data type is not supported")
        }
    }
    enum NeverLeaves {}
    impl From<Range<usize>> for NeverLeaves {
        fn from(_: Range<usize>) -> Self {
            unimplemented!("This data type is not supported")
        }
    }
    impl<T, StorageType> OptPromiseInfo<T, StorageType> for NeverLeaves
    where
        NeverLeaves: From<T>,
    {
        fn info(&self) -> Option<&T> {
            unimplemented!("This data type is not supported")
        }
    }

    #[derive(Debug, derive_more::From)]
    pub struct TextRef<'text>(&'text str);
    impl<'text, StorageType> OptPromiseInfo<&'text str, StorageType> for TextRef<'text> {
        fn info(&self) -> Option<&&'text str> {
            Some(&self.0)
        }
    }

    #[derive(Debug, derive_more::From)]
    pub struct TextOwn(Range<usize>);
    impl<StorageType> OptPromiseInfo<Range<usize>, StorageType> for TextOwn {
        fn info(&self) -> Option<&Range<usize>> {
            Some(&self.0)
        }
    }
    #[derive(Debug, derive_more::From)]
    pub enum TextMaybeOwn<'text> {
        Ref(&'text str),
        Own(Range<usize>),
    }
    impl<'text, StorageType> OptPromiseInfo<&'text str, StorageType> for TextMaybeOwn<'text> {
        fn info(&self) -> Option<&&'text str> {
            if let Self::Ref(s) = self {
                Some(s)
            } else {
                None
            }
        }
    }
    impl<StorageType> OptPromiseInfo<Range<usize>, StorageType> for TextMaybeOwn<'_> {
        fn info(&self) -> Option<&Range<usize>> {
            if let Self::Own(range) = self {
                Some(range)
            } else {
                None
            }
        }
    }

    #[derive(Debug, derive_more::From)]
    pub struct LeafInd(usize);
    impl<StorageType> OptPromiseInfo<usize, StorageType> for LeafInd {
        fn info(&self) -> Option<&usize> {
            Some(&self.0)
        }
    }

    #[derive(Debug, derive_more::From)]
    pub struct LeavesRange(Range<usize>);
    impl<StorageType> OptPromiseInfo<Range<usize>, StorageType> for LeavesRange {
        fn info(&self) -> Option<&Range<usize>> {
            Some(&self.0)
        }
    }

    /// Provider that holds nothing and panics at any call to it.
    struct UnsealedBottomStorage<Text = NeverText, Leaf = NeverLeaf, Leaves = NeverLeaves>(
        PhantomData<(Text, Leaf, Leaves)>,
    );
    impl<Text, Leaf, Leaves> UnsealedStorage<Text, Leaf, Leaves>
        for UnsealedBottomStorage<Text, Leaf, Leaves>
    {
        type Sealed = BottomStorage;
        fn register_text(&mut self, _: &str) -> Text {
            unimplemented!("This provider does not support text saving")
        }

        fn register_leaf(&mut self, _: Thing<Text, Leaf, Leaves>) -> Leaf {
            unimplemented!("This provider does not support child saving")
        }

        fn register_leaves<'s>(
            &'s mut self,
            _: impl Iterator<Item = Thing<Text, Leaf, Leaves>>,
        ) -> Leaves {
            unimplemented!("This provider does not support children saving")
        }

        fn seal(self) -> Self::Sealed {
            BottomStorage(PhantomData)
        }
    }
    impl<Text, Leaf, Leaves> UnsealedBottomStorage<Text, Leaf, Leaves> {
        fn new() -> Self {
            UnsealedBottomStorage(PhantomData)
        }
    }
    struct BottomStorage<Text = NeverText, Leaf = NeverLeaf, Leaves = NeverLeaves>(
        PhantomData<(Text, Leaf, Leaves)>,
    );
    impl<Text, Leaf, Leaves> SealedStorage<Text, Leaf, Leaves> for BottomStorage {
        fn get_text<'s>(&'s self, _: &Text) -> &'s str {
            unimplemented!("This provider does not provide text")
        }

        fn get_leaf<'s>(&'s self, _: &Leaf) -> &'s Thing<Text, Leaf, Leaves> {
            unimplemented!("This provider does not provide children")
        }

        fn get_leaves<'s>(&'s self, _: &Leaves) -> &'s [Thing<Text, Leaf, Leaves>] {
            unimplemented!("This provider does not provide children")
        }
    }

    pub fn text_ref_provider<'text>(
        text: &'text str,
    ) -> impl UnsealedStorage<TextRef<'text>, LeafInd, LeavesRange> {
        UnsealedBottomStorage::new()
            .with_text_ref(text)
            .leaves_owned()
    }

    pub fn text_own_provider() -> impl UnsealedStorage<TextOwn, LeafInd, LeavesRange> {
        UnsealedBottomStorage::new().text_owned().leaves_owned()
    }

    pub fn text_maybe_own_provider<'text>(
        ref_text: &'text str,
    ) -> impl UnsealedStorage<TextMaybeOwn<'text>, LeafInd, LeavesRange> {
        UnsealedBottomStorage::new()
            .text_owned()
            .with_text_ref(ref_text)
            .leaves_owned()
    }

    pub fn text_maybe_own_find_provider<'text>(
        ref_text: &'text str,
        own_text: impl Into<String>,
    ) -> impl UnsealedStorage<TextMaybeOwn<'text>, LeafInd, LeavesRange> {
        UnsealedBottomStorage::new()
            .with_text_find(own_text)
            .with_text_ref(ref_text)
            .leaves_owned()
    }

    pub fn text_own_find_provider(
        own_text: impl Into<String>,
    ) -> impl UnsealedStorage<TextOwn, LeafInd, LeavesRange> {
        UnsealedBottomStorage::new()
            .with_text_find(own_text)
            .leaves_owned()
    }

    fn transfer_nodes<
        FromText,
        FromChild,
        FromChildren,
        ToText,
        ToChild,
        ToChildren,
        FromProvider,
        ToProvider,
    >(
        root: &Thing<FromText, FromChild, FromChildren>,
        from_provider: &FromProvider,
        to_provider: &mut ToProvider,
        buf: &mut Vec<Thing<ToText, ToChild, ToChildren>>,
    ) -> Thing<ToText, ToChild, ToChildren>
    where
        FromProvider: SealedStorage<FromText, FromChild, FromChildren>,
        ToProvider: UnsealedStorage<ToText, ToChild, ToChildren>,
    {
        match root {
            Thing::Text { text } => {
                let s = from_provider.get_text(&text);
                let text = to_provider.register_text(s);
                Thing::Text { text }
            }
            Thing::Child {
                text,
                child1,
                child2,
            } => {
                let s = from_provider.get_text(text);
                let text = to_provider.register_text(s);
                let ch1 = from_provider.get_leaf(child1);
                let ch1 = transfer_nodes(ch1, from_provider, to_provider, buf);
                let child1 = to_provider.register_leaf(ch1);
                let ch2 = from_provider.get_leaf(child2);
                let ch2 = transfer_nodes(ch2, from_provider, to_provider, buf);
                let child2 = to_provider.register_leaf(ch2);
                Thing::Child {
                    text,
                    child1,
                    child2,
                }
            }
            Thing::Multiple { children } => {
                let ch = from_provider.get_leaves(children);
                let start_ind = buf.len();
                for child in ch {
                    let to_child = transfer_nodes(child, from_provider, to_provider, buf);
                    buf.push(to_child);
                }
                let children = to_provider.register_leaves(buf.drain(start_ind..));
                Thing::Multiple { children }
            }
        }
    }

    struct UnsealedFullOwner<'text> {
        yet_unowned_text: Cow<'text, str>,
        owned_text: String,
        things: Vec<Thing<TextOwn, LeafInd, LeavesRange>>,
    }

    impl<'text> UnsealedFullOwner<'text> {
        pub fn new(yet_unowned: &'text str) -> Self {
            Self {
                yet_unowned_text: yet_unowned.into(),
                owned_text: String::new(),
                things: Vec::new(),
            }
        }
        pub unsafe fn drop_unowned(self) -> UnsealedFullOwner<'static> {
            let Self {
                owned_text,
                things: children,
                ..
            } = self;
            UnsealedFullOwner {
                yet_unowned_text: "".into(),
                owned_text,
                things: children,
            }
        }
        // TODO add panic info
        pub unsafe fn provide(&mut self, data: impl Into<String>) {
            let s = data.into();
            // assert_eq!(self.yet_unowned_text, s, "Provided data must be the same!");
            self.yet_unowned_text = Cow::Owned(s)
        }
    }

    impl<'text> UnsealedStorage<TextOwn, LeafInd, LeavesRange> for UnsealedFullOwner<'text> {
        type Sealed = FullOwner;

        fn register_text(&mut self, text: &str) -> TextOwn {
            if let Some(range) = try_find_pointer_match(&self.yet_unowned_text, text) {
                return TextOwn(range);
            }
            if let Some(ind) = self.yet_unowned_text.find(text) {
                return TextOwn(ind..ind + text.len());
            }
            let len = self.yet_unowned_text.len();
            if let Some(mut ind) = self.owned_text.find(text) {
                ind += len;
                return TextOwn(ind..ind + text.len());
            }
            let start_ind = self.owned_text.len() + len;
            self.owned_text.push_str(text);
            let end_ind = self.owned_text.len() + len;
            TextOwn(start_ind..end_ind)
        }

        fn register_leaf(&mut self, child: Thing<TextOwn, LeafInd, LeavesRange>) -> LeafInd {
            let ind = self.things.len();
            self.things.push(child);
            LeafInd(ind)
        }

        fn register_leaves<'s>(
            &'s mut self,
            children: impl Iterator<Item = Thing<TextOwn, LeafInd, LeavesRange>>,
        ) -> LeavesRange {
            let start_ind = self.things.len();
            self.things.extend(children);
            let end_ind = self.things.len();
            LeavesRange(start_ind..end_ind)
        }

        fn seal(self) -> Self::Sealed {
            let Cow::Owned(s) = self.yet_unowned_text else {
                panic!("Full Owner cannot be baked until data is provided to it")
            };
            FullOwner {
                now_owned_text: s.into_boxed_str(),
                owned_text: self.owned_text.into_boxed_str(),
                things: self.things.into_boxed_slice(),
            }
        }
    }

    struct FullOwner {
        now_owned_text: Box<str>,
        owned_text: Box<str>,
        things: Box<[Thing<TextOwn, LeafInd, LeavesRange>]>,
    }

    impl SealedStorage<TextOwn, LeafInd, LeavesRange> for FullOwner {
        fn get_text<'s>(&'s self, TextOwn(range): &TextOwn) -> &'s str {
            let (start, end) = (range.start, range.end);
            let len = self.now_owned_text.len();
            if start >= len {
                return &self.owned_text[start - len..end - len];
            }
            &self.now_owned_text[start..end]
        }

        fn get_leaf<'s>(
            &'s self,
            &LeafInd(i): &LeafInd,
        ) -> &'s Thing<TextOwn, LeafInd, LeavesRange> {
            &self.things[i]
        }

        fn get_leaves<'s>(
            &'s self,
            LeavesRange(range): &LeavesRange,
        ) -> &'s [Thing<TextOwn, LeafInd, LeavesRange>] {
            &self.things[range.clone()]
        }
    }

    #[cfg(test)]
    mod tests {
        fn gen_text<R: rand::Rng + ?Sized>(rng: &mut R) -> String {
            // let it be of some length
            let l: usize = rng.gen_range(3..=10);
            let mut s = String::new();
            for _ in 0..l {
                s.push(if rng.gen_bool(0.5) {
                    rng.gen_range('a'..='z')
                } else if rng.gen_bool(0.4) {
                    rng.gen_range('а'..='я')
                } else {
                    [' ', ',', '.'][rng.gen_range(0..3)]
                })
            }
            s
        }

        fn gen_contained_text<'text, R: rand::Rng + ?Sized>(
            text: &'text str,
        ) -> impl FnMut(&mut R) -> &'text str {
            |rng| {
                // basically just select two random indices
                let mut ind1 = rng.gen_range(0..text.len());
                let mut ind2 = rng.gen_range(0..text.len());
                // swap them, if needed
                if ind1 > ind2 {
                    std::mem::swap(&mut ind1, &mut ind2);
                }
                &text[ind1..ind2]
            }
        }

        fn gen_maybe_contained_text<'text, R: rand::Rng + ?Sized>(
            text: &'text str,
        ) -> impl FnMut(&mut R) -> std::borrow::Cow<'text, str> {
            let mut contained = gen_contained_text(text);
            move |rng| {
                if rng.gen_bool(0.5) {
                    contained(rng).into()
                } else {
                    gen_text(rng).into()
                }
            }
        }

        fn generate_thing<
            'owner,
            R: rand::Rng + ?Sized,
            Text,
            Child,
            Children,
            P: UnsealedStorage<Text, Child, Children>,
            S: AsRef<str>,
        >(
            provider: &mut P,
            rng: &mut R,
            text_gen: &mut impl FnMut(&mut R) -> S,
            children_buf: &mut Vec<Thing<Text, Child, Children>>,
            nodes_left: &mut usize,
        ) -> Thing<Text, Child, Children> {
            *nodes_left = nodes_left.saturating_sub(1);
            let generated = if *nodes_left == 0 || rng.gen_bool(0.6) {
                // assume 60% or anything over specified number are text variants
                // we need some random text for it
                let text = text_gen(rng);
                Thing::Text {
                    text: provider.register_text(text.as_ref()),
                }
            } else if rng.gen_bool(0.9) {
                // say, 36% are child owners
                // we need some text and children for it
                let text = text_gen(rng);
                let child1 = generate_thing(provider, rng, text_gen, children_buf, nodes_left);
                let child2 = generate_thing(provider, rng, text_gen, children_buf, nodes_left);
                Thing::Child {
                    text: provider.register_text(text.as_ref()),
                    child1: provider.register_leaf(child1),
                    child2: provider.register_leaf(child2),
                }
            } else {
                // rest 4% own 1-5 children
                let buf_start = children_buf.len();
                for _ in 0..rng.gen_range(1..=5) {
                    let child = generate_thing(provider, rng, text_gen, children_buf, nodes_left);
                    children_buf.push(child);
                }
                let children = provider.register_leaves(children_buf.drain(buf_start..));
                Thing::Multiple { children }
            };
            // average coefficient is 0.6 * 0 + 0.36 * 2 + 2.5 * 0.04 = 0.82
            // meaning this thing is likely to die at some point (I hope for that, at least)
            generated
        }

        use crate::data::borrowing_concept::{UnsealedFullOwner, UnsealedStorage};

        use super::{
            text_maybe_own_provider, text_own_provider, text_ref_provider, Thing, ThingTree,
        };

        // TODO check these versions of fuzzes to actually run correctly
        #[test]
        fn ref_fuzz() {
            // arrange
            let rng = &mut rand::thread_rng();
            let text = "Lorem ipsum dolor sir ammet, concestesometghing idk never learned latin";
            for _ in 0..100_000 {
                let mut provider = text_ref_provider(text);

                // act
                let thing = generate_thing(
                    &mut provider,
                    rng,
                    &mut gen_contained_text(text),
                    &mut Vec::new(),
                    &mut 100,
                );
                let tree = ThingTree::new(thing, provider);

                // assert
                let mut output = String::new();
                tree.write_to(&mut output)
                    .expect("Should be able to write into string");
            }
        }
        #[test]
        fn owned_fuzz() {
            // arrange
            let rng = &mut rand::thread_rng();
            for _ in 0..100_000 {
                let mut provider = text_own_provider();

                // act
                let thing =
                    generate_thing(&mut provider, rng, &mut gen_text, &mut Vec::new(), &mut 100);
                let tree = ThingTree::new(thing, provider);

                // assert
                let mut output = String::new();
                tree.write_to(&mut output)
                    .expect("Should be able to write into string");
            }
        }

        #[test]
        fn maybe_owned_fuzz() {
            // arrange
            let rng = &mut rand::thread_rng();
            let text = "Lorem ipsum dolor sir ammet, concestesometghing idk never learned latin";
            for _ in 0..100_000 {
                let mut provider = text_maybe_own_provider(text);

                // act
                let thing = generate_thing(
                    &mut provider,
                    rng,
                    &mut gen_maybe_contained_text(text),
                    &mut Vec::new(),
                    &mut 100,
                );
                let tree = ThingTree::new(thing, provider);

                // assert
                let mut output = String::new();
                tree.write_to(&mut output)
                    .expect("Should be able to write into string");
            }
        }

        #[test]
        fn fuzz_transfer() {
            // arrange
            let rng = &mut rand::thread_rng();
            let text = "Lorem ipsum dolor sir ammet, concestesometghing idk never learned latin";
            for _ in 0..100_000 {
                let mut provider = text_maybe_own_provider(text);
                let thing = generate_thing(
                    &mut provider,
                    rng,
                    &mut gen_maybe_contained_text(text),
                    &mut Vec::new(),
                    &mut 100,
                );
                let original_tree = ThingTree::new(thing, provider);

                // act
                let mut moved_provider = text_own_provider();
                let moved_thing = original_tree.move_to(&mut moved_provider);
                let moved_tree = ThingTree::new(moved_thing, moved_provider);

                // assert
                let mut original = String::new();
                original_tree
                    .write_to(&mut original)
                    .expect("Should be able to write into string");
                let mut moved = String::new();
                moved_tree
                    .write_to(&mut moved)
                    .expect("Should be able to write into string");
                assert_eq!(
                    original, moved,
                    "Moved things should produce the same result"
                );
            }
        }

        #[test]
        fn fuzz_to_owned() {
            // arrange
            let rng = &mut rand::thread_rng();
            for _ in 0..100_000 {
                let text =
                    "Lorem ipsum dolor sir ammet, concestesometghing idk never learned latin"
                        .to_string()
                        .into_boxed_str();
                let mut provider = text_maybe_own_provider(&text);
                let thing = generate_thing(
                    &mut provider,
                    rng,
                    &mut gen_maybe_contained_text(&text),
                    &mut Vec::new(),
                    &mut 100,
                );
                let original_tree = ThingTree::new(thing, provider);

                // act
                let mut owning_provider = UnsealedFullOwner::new(&text);
                let owning_thing = original_tree.move_to(&mut owning_provider);
                let mut original = String::new();
                original_tree
                    .write_to(&mut original)
                    .expect("Should be able to write into string");
                drop(original_tree);
                // SAFETY:
                // data provided and data `OwnedProvider` referred to here is the same,
                // as it's contained in the same immutable variable
                let owning_provider = unsafe {
                    let mut owning_provider = owning_provider.drop_unowned();
                    owning_provider.provide(text);
                    owning_provider
                };
                let owning_tree = ThingTree::new(owning_thing, owning_provider);

                // assert
                let mut owned = String::new();
                owning_tree
                    .write_to(&mut owned)
                    .expect("Should be able to write into string");
                assert_eq!(
                    original, owned,
                    "Owned things should produce the same result"
                );
            }
        }
    }
}

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
        formatting: Option<Formatting>,
        content: Tx<'source>,
    },
    InlineMath {
        content: Tx<'source>,
    },
    Reference {
        ident: Tx<'source>,
    },
    FootnoteReference {
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
        Self::Paragraph {
            is_newline: false,
            content: text.into(),
            formatting: None,
        }
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
            Token::DisplayMath { content, ident } => Token::DisplayMath {
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
            Token::Multiple { tokens } => Token::Multiple {
                tokens: Tokens::Borrowed(tokens),
            },
            Token::Paragraph {
                is_newline,
                formatting,
                content,
            } => Token::Paragraph {
                is_newline: *is_newline,
                formatting: formatting.clone(),
                content: Cow::Borrowed(content),
            },
            Token::InlineMath { content } => Token::InlineMath {
                content: Cow::Borrowed(&content),
            },
            Token::Reference { ident } => Token::Reference {
                ident: Cow::Borrowed(&ident),
            },
            Token::FootnoteReference { ident } => Token::FootnoteReference {
                ident: Cow::Borrowed(ident),
            },
            Token::FootnoteContent { content, ident } => Token::FootnoteContent {
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
            Token::Multiple { tokens } => Token::Multiple {
                tokens: tokens.to_static(),
            },
            Token::Paragraph {
                is_newline,
                formatting,
                content,
            } => Token::Paragraph {
                is_newline: *is_newline,
                formatting: formatting.clone(),
                content: content.to_static(),
            },
            Token::InlineMath { content } => Token::InlineMath {
                content: content.to_static(),
            },
            Token::Reference { ident } => Token::Reference {
                ident: ident.to_static(),
            },
            Token::FootnoteReference { ident } => Token::FootnoteReference {
                ident: ident.to_static(),
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
}

/// This module contains, documents and helps developing my concept for refined token borrowing.
///
/// Main idea is to ALWAYS delegate token information storage. This is expected to minimize token size.
/// Problem with that idea - some tokens need to own their data, they need to be independent and live indefinitely.
///
/// To allow this, there would be a special token type - so-called `Owner` - holding text buffer for all their children to interpret and refer to.
/// A now-implemented with cloning `to_static` method would instead wrap token(s) into a new `Owner`, allowing them to exist indefinitely,
/// while in fact only cheap pointers were substituted.
///
/// The next step would be to realize that tokens don't even need to own their children - instead upper-standing owned could own them instead,
/// providing references to children tokens. This should **DRAMATICALLY** decrease number of heap allocations, as it would allow tokens for centralized data storage,
/// allocating only two memory arrays.
///
/// Now, here's a thing - vectors can be re-allocated in-memory, when their size reached their capacity. This presents another challenge: we should somehow guarantee,
/// that data referred by child tokens will not be moved out of the pointers (compiler will **force** us to do that, actually). I see two possible solutions on that matter:
///
/// - while token tree creation, `Owner` holds data in a regular vector, giving children only a *promise* to data (not a pointer).
/// once generation is complete, token tree is not supposed to be changed. at this point, `Owner` will allow children to *obtain* the pointers to now-immovable data.
/// - same as before, but children never get to see pointers themselves. this will allow `Owner` to freely mutate data, but will add an extra cost for some sort of
/// internal identifier resolution and a couple more function calls.
///
/// ~~Being a masochist I am, of course I went on with the first option. Following is a log of the development~~
///
/// # LOG
///
/// After brief prototyping, this looks really promising; New tokens type's size is only 40 (5 bytes). Paired with TWO allocations per tree this should be insane.
///
/// When I revised once again my design, It seems that there's no reason to make `Owner` a variant of token. In fact, we can call it a `TokenOwner` and call it a day.
/// Any place concerned about token's life would accept `TokenOwner`.
///
/// Since `TokenOwner` will own EVERYTHING about `Token`s inside it, ~~there's no need for lifetimes on it.~~
/// it's lifetime should not be used in any way, it's just a way to avoid `'static` lifetime, which can be used here, since,
/// IN FACT, data DOES NOT live to the end of the program. I would *probably ok* to use them, but I won't do that.
///
/// Seems like there's no crate achieving exactly what I want, meaning I'll have to result to **unsafe** myself.
///
/// New idiom: `Token` can't exist int a vacuum anymore, it's an internal implementation of `TokenTree`
///
/// `Owner` was renamed into `Provider`. Now inner token and text storage is generalized by a trait;
/// `Provider` can decide themselves, if they want to actually store the data, or keep as a reference to th.
///
/// There are now three traits related to provider concept:
/// - `UnbackedProvider` represents a provider that's ready to accept new things to be stored.
/// - `BackedProvider` represents a provider that's no longer able to accept new values, but instead can provide references to them
/// - `FixedProvider` represents a provider that's no longer able nor add nor provide values.
/// It can be as minified as possible, only storing data without any sort of bookkeeping.
///
/// Despite me being a masochist, I really do fear using `unsafe` in my code. There's just too many things to keep track of.
/// So after writing the entire module using self-borrowing stuff and `unsafe`, I'll resort to second option stated above: basically, ever-unbaked children.
///
/// A format of so-called "promise" that provider uses to return actual child/text can be different and it ultimately of to provider to decide:
/// - the simplest would be a provider that owns all of the data tokens refer to
/// - a provider could be relaying calls to some other provider to take care of
/// - a provider could refer to some plain text buffer, like &str to recognize and store it's data
///
/// All of that means, that both tokens and providers must be defined at generic over promise types.
/// This will allow provider to select in which way they would store and manipulate data.
///
/// This approach is less optimized then the `unsafe` one, but `miri` scared me away from pointer manipulation, so here I am, I guess
///
/// I see now, that providers can be composed into each other to build desired behavior step-by-step. That's really desireable too.
#[cfg(test)]
mod borrowing_concept {
    use std::{marker::PhantomData, ops::Range};

    #[derive(Debug, derive_more::From, derive_more::AsRef)]
    struct TextPromise(Range<usize>);
    #[derive(Debug, derive_more::From, derive_more::AsRef)]
    struct ChildPromise(usize);
    #[derive(Debug, derive_more::From, derive_more::AsRef)]
    struct ChildrenPromise(Range<usize>);

    enum Thing<Text, Child, Children> {
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

    impl<Text, Child, Children> Thing<Text, Child, Children> {
        fn write_to<P, W>(&self, provider: &P, out: &mut W) -> std::fmt::Result
        where
            P: BakedProvider<Text, Child, Children>,
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
                    provider.get_child(child1).write_to(provider, out)?;
                    out.write_str(", ")?;
                    provider.get_child(child2).write_to(provider, out)?;
                    out.write_str(")")?;
                    Ok(())
                }
                Thing::Multiple { children } => {
                    out.write_str("Multiple[")?;
                    let children = provider.get_children(children);
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

    trait UnbakedProvider<Text, Child, Children> {
        type Baked: BakedProvider<Text, Child, Children>;
        fn register_text(&mut self, text: &str) -> Text;
        fn register_child(&mut self, child: Thing<Text, Child, Children>) -> Child;
        fn register_children<'s>(
            &'s mut self,
            children: impl FnMut(&mut &'s mut Self) -> Option<Thing<Text, Child, Children>>,
        ) -> Children;
        fn bake(self) -> Self::Baked;
    }

    trait BakedProvider<Text, Child, Children> {
        fn get_text<'s>(&'s self, promise: &Text) -> &'s str;
        fn get_child<'s>(&'s self, promise: &Child) -> &'s Thing<Text, Child, Children>;
        fn get_children<'s>(&'s self, promise: &Children) -> &'s [Thing<Text, Child, Children>];
    }

    trait UnbakedProviderExt<Text, Child, Children>: UnbakedProvider<Text, Child, Children> {
        fn with_text_ref<'text>(
            self,
            text: &'text str,
        ) -> UnbakedTextRefProvider<'text, Text, Child, Children, Self> {
            UnbakedTextRefProvider { text, inner: self }
        }
    }
    impl<Text, Child, Children, P: UnbakedProvider<Text, Child, Children>>
        UnbakedProviderExt<Text, Child, Children> for P
    {
    }

    enum NeverText {}
    enum NeverChild {}
    enum NeverChildren {}

    /// Provider that holds nothing and panics at any call to it.
    struct UnbakedBottomProvider<Text = NeverText, Child = NeverChild, Children = NeverChildren>;
    impl<Text, Child, Children> UnbakedProvider<Text, Child, Children>
        for UnbakedBottomProvider<Text, Child, Children>
    {
        type Baked = BakedBottomProvider;
        fn register_text(&mut self, text: &str) -> Text {
            unimplemented!("This provider does not support text saving")
        }

        fn register_child(&mut self, child: Thing<Text, Child, Children>) -> Child {
            unimplemented!("This provider does not support child saving")
        }

        fn register_children<'s>(
            &'s mut self,
            children: impl FnMut(&mut &'s mut Self) -> Option<Thing<Text, Child, Children>>,
        ) -> Children {
            unimplemented!("This provider does not support children saving")
        }

        fn bake(self) -> Self::Baked {
            BakedBottomProvider
        }
    }
    struct BakedBottomProvider<Text = NeverText, Child = NeverChild, Children = NeverChildren>;
    impl<Text, Child, Children> BakedProvider<Text, Child, Children> for BakedBottomProvider {
        fn get_text<'s>(&'s self, promise: &Text) -> &'s str {
            unimplemented!("This provider does not provide text")
        }

        fn get_child<'s>(&'s self, promise: &Child) -> &'s Thing<Text, Child, Children> {
            unimplemented!("This provider does not provide children")
        }

        fn get_children<'s>(&'s self, promise: &Children) -> &'s [Thing<Text, Child, Children>] {
            unimplemented!("This provider does not provide children")
        }
    }

    trait MaybeTextRef<'text>: From<&'text str> {
        fn text_ref(&self) -> Option<&'text str>;
    }

    struct UnbakedTextRefProvider<'text, Text, Child, Children, P> {
        text: &'text str,
        inner: P,
    }

    impl<'text, Text, Child, Children, P> UnbakedProvider<Text, Child, Children>
        for UnbakedTextRefProvider<'text, Text, Child, Children, P>
    where
        P: UnbakedProvider<Text, Child, Children>,
    {
        type Baked = BakedTextRefProvider<'text, Text, Child, Children, P>;

        fn register_text(&mut self, text: &str) -> Text {
            // first, check out if this text, as a pointer, points inside this provider's referee:
            'nah: {
                let referee_ptr = self.text.as_ptr() as usize;
                let text_ptr = text.as_ptr() as usize;
                if text_ptr < referee_ptr {
                    break 'nah;
                }
                let index_guess = text_ptr - referee_ptr;
                let self_len = self.text.len();
                if index_guess >= self_len {
                    break 'nah;
                }
                // yeah, seems like it
                // lastly, lets rule out partial overlap:
                let text_len = text.len();
                if text_len + index_guess > self_len {
                    // buffers overlap partially, which is weird, probably should
                    // TODO warn about that too
                    break 'nah;
                }
                // let's check that literally, for a good measure:
                // TODO probably add cfg to remove that in release build?
                if !self.text[index_guess..].starts_with(text) {
                    // huh? that's something weird, probably must
                    // TODO warn about it
                    break 'nah;
                }
                // ok, so it seems like we've found a match using nothing but pointer-things.
                // that's great, cause it's literally O(1) for any buffer size
                return &self.text[index_guess..index_guess + text_len].into();
            }

            // then, let's try finding this sort of sequence in the entire referee:
            if let Some(ind) = self.text.find(text) {
                // well, return corresponding slice, then
                return TextRefPromise::Ref(&self.text[ind..ind + text.len()]);
            }

            // lastly, refer to inner provider
            self.inner.register_text(text)
        }

        fn register_child(&mut self, child: Thing<Text, Child, Children>) -> Child {
            self.inner.register_child(child)
        }

        fn register_children<'s>(
            &'s mut self,
            children: impl FnMut(&mut &'s mut Self) -> Option<Thing<Text, Child, Children>>,
        ) -> Children {
            self.inner.register_children(children)
        }

        fn bake(self) -> Self::Baked {
            BakedTextRefProvider { inner: self.inner }
        }
    }

    struct BakedTextRefProvider<'text, Text, Child, Children, P> {
        inner: P,
    }

    impl<'text, Text, Child, Children, P> BakedProvider<Text, Child, Children>
        for BakedTextRefProvider<'text, Text, Child, Children, P>
    where
        P: BakedProvider<Text, Child, Children>,
        Text: MaybeTextRef<'text>,
    {
        fn get_text<'s>(&'s self, promise: &Text) -> &'s str {
            if let Some(res) = promise.text_ref() {
                res
            } else {
                self.inner.get_text(promise)
            }
        }

        fn get_child<'s>(&'s self, promise: &Child) -> &'s Thing<Text, Child, Children> {
            self.inner.get_child(promise)
        }

        fn get_children<'s>(&'s self, promise: &Children) -> &'s [Thing<Text, Child, Children>] {
            self.inner.get_children(promise)
        }
    }

    trait MaybeTextOwnRange: From<Range<usize>> {
        fn own_range(&self) -> &Range<usize>;
    }

    struct UnbakedTextOwnProvider<Text, Child, Children, P> {
        text: String,
        inner: P,
    }

    impl<Text, Child, Children, P> UnbakedProvider<Text, Child, Children>
        for UnbakedTextOwnProvider<Text, Child, Children, P>
    where
        P: UnbakedProvider<Text, Child, Children>,
    {
        type Baked = BakedTextOwnProvider<Text, Child, Children, P>;

        fn register_text(&mut self, text: &str) -> Text {
            let range_start = self.text.len();
            self.text.push_str(text);
            let range_end = self.text.len();
            Text::from(range_start..range_end)
        }

        fn register_child(&mut self, child: Thing<Text, Child, Children>) -> Child {
            self.inner.register_child(child)
        }

        fn register_children<'s>(
            &'s mut self,
            children: impl FnMut(&mut &'s mut Self) -> Option<Thing<Text, Child, Children>>,
        ) -> Children {
            self.register_children(children)
        }

        fn bake(self) -> Self::Baked {
            BakedTextOwnProvider {
                text: self.text.into_boxed_str(),
                inner: self.inner.bake(),
            }
        }
    }

    struct BakedTextOwnProvider<Text, Child, Children, P> {
        text: Box<str>,
        inner: P,
    }

    impl<Text, Child, Children, P> BakedProvider<Text, Child, Children>
        for BakedTextOwnProvider<Text, Child, Children, P>
    where
        P: BakedProvider<Text, Child, Children>,
        Text: MaybeTextOwnRange,
    {
        fn get_text<'s>(&'s self, promise: &Text) -> &'s str {
            if let Some(range) = promise.own_range() {
                &self.text[range.clone()]
            } else {
                self.inner.get_text(promise)
            }
        }

        fn get_child<'s>(&'s self, promise: &Child) -> &'s Thing<Text, Child, Children> {
            self.inner.get_child(promise)
        }

        fn get_children<'s>(&'s self, promise: &Children) -> &'s [Thing<Text, Child, Children>] {
            self.inner.get_children(promise)
        }
    }

    trait MaybeChildInd: From<usize> {
        fn own_ind(&self) -> usize;
    }

    trait MaybeChildrenRange: From<Range<usize>> {
        fn own_range(&self) -> &Range<usize>;
    }

    struct UnbakedChildrenOwnProvider<Text, Child, Children, P> {
        children: Vec<Thing<Text, Child, Children>>,
        inner: P,
    }

    impl<Text, Child, Children, P> UnbakedProvider<Text, Child, Children>
        for UnbakedChildrenOwnProvider<Text, Child, Children, P>
    where
        P: UnbakedProvider<Text, Child, Children>,
    {
        type Baked = BakedChildrenOwnProvider<Text, Child, Children, P::Baked>;

        fn register_text(&mut self, text: &str) -> Text {
            self.inner.register_text(text)
        }

        fn register_child(&mut self, child: Thing<Text, Child, Children>) -> Child {
            let ind = self.things.len();
            self.things.push(child);
            ChildPromise(ind)
        }

        fn register_children<'s>(
            &'s mut self,
            children: impl FnMut(&mut &'s mut Self) -> Option<Thing<Text, Child, Children>>,
        ) -> Children {
            todo!()
        }

        fn bake(self) -> Self::Baked {
            BakedChildrenOwnProvider {
                children: self.children.into_boxed_slice(),
                inner: self.inner.bake(),
            }
        }
    }

    struct BakedChildrenOwnProvider<Text, Child, Children, P> {
        children: Box<[Thing<Text, Child, Children>]>,
        inner: P,
    }

    impl<Text, Child, Children, P> BakedProvider<Text, Child, Children>
        for BakedChildrenOwnProvider<Text, Child, Children, P>
    where
        P: BakedProvider<Text, Child, Children>,
        Child: MaybeChildInd,
        Children: MaybeChildrenRange,
    {
        fn get_text<'s>(&'s self, promise: &Text) -> &'s str {
            self.inner.get_text(promise)
        }

        fn get_child<'s>(&'s self, promise: &Child) -> &'s Thing<Text, Child, Children> {
            if let Some(ind) = promise.own_ind() {
                &self.children[ind]
            } else {
                self.inner.get_child(promise)
            }
        }

        fn get_children<'s>(&'s self, promise: &Children) -> &'s [Thing<Text, Child, Children>] {
            if let Some(range) = promise.own_range() {
                &self.children[range.clone()]
            } else {
                self.inner.get_children(promise)
            }
        }
    }

    struct UnbakedOwningProvider<Text, Child, Children> {
        text: String,
        things: Vec<OwnedThing>,
    }

    impl UnbakedOwningProvider {
        fn new() -> Self {
            Self {
                text: String::new(),
                things: Vec::new(),
            }
        }
    }

    impl UnbakedProvider for UnbakedOwningProvider {
        type Text = TextPromise;
        type Child = ChildPromise;
        type Children = ChildrenPromise;
        type Baked = BakedOwningProvider;

        fn register_text(&mut self, text: &str) -> TextPromise {
            let range_start = self.text.len();
            self.text.push_str(text);
            let range_end = self.text.len();
            TextPromise(range_start..range_end)
        }

        fn register_child(&mut self, child: OwnedThing) -> ChildPromise {
            let ind = self.things.len();
            self.things.push(child);
            ChildPromise(ind)
        }

        fn register_children<'s>(
            mut self: &'s mut Self,
            mut children: impl FnMut(&mut &'s mut Self) -> Option<OwnedThing>,
        ) -> ChildrenPromise {
            let range_start = self.things.len();
            while let Some(thing) = children(&mut self) {
                self.things.push(thing);
            }
            let range_end = self.things.len();
            ChildrenPromise(range_start..range_end)
        }

        fn bake(self) -> Self::Baked {
            BakedOwningProvider {
                text: self.text.into_boxed_str(),
                things: self.things.into_boxed_slice(),
            }
        }
    }

    enum TextRefPromise<'text> {
        Ref(&'text str),
        Owned(Range<usize>),
    }

    type RefThing<'text> = Thing<TextRefPromise<'text>, ChildPromise, ChildrenPromise>;

    struct UnbakedTextRefProvider<'text, P> {
        text: &'text str,
        inner: P,
    }

    impl<'text, P> UnbakedProvider for UnbakedTextRefProvider<'text, P>
    where
        P: UnbakedProvider<Text = Self::Text, Child = Self::Child, Children = Self::Children>,
    {
        type Text = TextRefPromise<'text>;
        type Child = ChildPromise;
        type Children = ChildrenPromise;
        type Baked = BakedTextRefProvider<'text, P::Baked>;

        fn register_text(&mut self, text: &str) -> Self::Text {
            // first, check out if this text, as a pointer, points inside this provider's referee:
            'nah: {
                let referee_ptr = self.text.as_ptr() as usize;
                let text_ptr = text.as_ptr() as usize;
                if text_ptr < referee_ptr {
                    break 'nah;
                }
                let index_guess = text_ptr - referee_ptr;
                let self_len = self.text.len();
                if index_guess >= self_len {
                    break 'nah;
                }
                // yeah, seems like it
                // lastly, lets rule out partial overlap:
                let text_len = text.len();
                if text_len + index_guess > self_len {
                    // buffers overlap partially, which is weird, probably should
                    // TODO warn about that too
                    break 'nah;
                }
                // let's check that literally, for a good measure:
                // TODO probably add cfg to remove that in release build?
                if !self.text[index_guess..].starts_with(text) {
                    // huh? that's something weird, probably must
                    // TODO warn about it
                    break 'nah;
                }
                // ok, so it seems like we've found a match using nothing but pointer-things.
                // that's great, cause it's literally O(1) for any buffer size
                return TextRefPromise::Ref(&self.text[index_guess..index_guess + text_len]);
            }

            // then, let's try finding this sort of sequence in the entire referee:
            if let Some(ind) = self.text.find(text) {
                // well, return corresponding slice, then
                return TextRefPromise::Ref(&self.text[ind..ind + text.len()]);
            }

            // lastly, refer to inner provider
            TextRefPromise::Owned(self.inner.register_text(text))
        }

        fn register_child(
            &mut self,
            child: Thing<Self::Text, Self::Child, Self::Children>,
        ) -> Self::Child {
            self.inner.register_child(child)
        }

        fn register_children<'s>(
            &'s mut self,
            children: impl FnMut(
                &mut &'s mut Self,
            ) -> Option<Thing<Self::Text, Self::Child, Self::Children>>,
        ) -> Self::Children {
            self.inner.register_children(children)
        }

        fn bake(self) -> Self::Baked {
            BakedTextRefProvider {
                inner: self.inner.bake(),
                owned_text: todo!(),
                _phantom: PhantomData,
            }
        }
    }

    struct BakedTextRefProvider<'text, P> {
        owned_text: Box<str>,
        inner: P,
        _phantom: PhantomData<&'text ()>,
    }

    impl<'text, P> BakedProvider for BakedTextRefProvider<'text, P>
    where
        P: BakedProvider<
            Text = TextRefPromise<'text>,
            Child = ChildPromise,
            Children = ChildrenPromise,
        >,
    {
        type Text = TextRefPromise<'text>;
        type Child = ChildPromise;
        type Children = ChildrenPromise;

        fn get_text<'s>(&'s self, promise: &Self::Text) -> &'s str {
            match promise {
                TextRefPromise::Ref(reference) => reference,
                TextRefPromise::Owned(owning) => &self.owned_text[owning.clone()],
            }
        }

        fn get_child<'s>(
            &'s self,
            ChildPromise(ind): &Self::Child,
        ) -> &'s Thing<Self::Text, Self::Child, Self::Children> {
            &self.things[*ind]
        }

        fn get_children<'s>(
            &'s self,
            ChildrenPromise(range): &Self::Children,
        ) -> &'s [Thing<Self::Text, Self::Child, Self::Children>] {
            &self.things[range.clone()]
        }
    }

    #[cfg(test)]
    mod tests {
        use crate::data::borrowing_concept::{OwnedThing, UnbakedOwningProvider, UnbakedProvider};

        #[test]
        fn owned_fuzz() {
            use crate::data::borrowing_concept::Thing;

            // arrange
            fn gen_thing<'owner, R: rand::Rng + ?Sized>(
                provider: &mut UnbakedOwningProvider,
                rng: &mut R,
                nodes_left: &mut usize,
            ) -> OwnedThing {
                *nodes_left = nodes_left.saturating_sub(1);
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
                let generated = if *nodes_left == 0 || rng.gen_bool(0.6) {
                    // assume 60% or anything over specified number are text variants
                    // we need some random text for it
                    let text = gen_text(rng);
                    Thing::Text {
                        text: provider.register_text(&text),
                    }
                } else if rng.gen_bool(0.9) {
                    // say, 36% are child owners
                    // we need some text and children for it
                    let text = gen_text(rng);
                    let child1 = gen_thing(provider, rng, nodes_left);
                    let child2 = gen_thing(provider, rng, nodes_left);
                    Thing::Child {
                        text: provider.register_text(&text),
                        child1: provider.register_child(child1),
                        child2: provider.register_child(child2),
                    }
                } else {
                    // rest 4% own 1-5 children
                    let mut inner = (0..rng.gen_range(1..=5)).into_iter();
                    let generator = move |provider: &mut &mut UnbakedOwningProvider| {
                        inner.next().map(|_| gen_thing(*provider, rng, nodes_left))
                    };
                    Thing::Multiple {
                        children: provider.register_children(generator),
                    }
                };
                // average coefficient is 0.6 * 0 + 0.36 * 2 + 2.5 * 0.04 = 0.82
                // meaning this thing is likely to die at some point (I hope for that, at least)
                generated
            }
            let rng = &mut rand::thread_rng();
            for _ in 0..100_000 {
                let mut provider = UnbakedOwningProvider::new();

                // act
                let thing = gen_thing(&mut provider, rng, &mut 100);
                let provider = provider.bake();

                // assert
                let mut output = String::new();
                thing
                    .write_to(&provider, &mut output)
                    .expect("Should be able to write into string");
            }
        }
    }
}

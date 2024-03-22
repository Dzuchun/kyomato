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
/// Being a masochist I am, of course I went on with the first option. Following is a log of the development
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
///
#[cfg(test)]
mod borrowing_concept {
    use std::marker::PhantomData;
    use std::mem::MaybeUninit;
    use std::ops::Range;
    use std::pin::Pin;

    use crate::util::Immutable;

    trait UnbakedProvider<'owner> {
        type TP;
        type CP;
        type MCP;
        type Baked: BakedProvider<'owner, TP = Self::TP, CP = Self::CP, MCP = Self::MCP>;
        fn register_text<'r>(&'r mut self, text: &str) -> Self::TP;
        fn register_child<'r>(
            &'r mut self,
            child: UnbakedThing<Self::TP, Self::CP, Self::MCP>,
        ) -> Self::CP;
        fn register_children<'r>(
            &'r mut self,
            children: impl IntoIterator<Item = UnbakedThing<Self::TP, Self::CP, Self::MCP>>,
        ) -> Self::MCP;
        fn bake(self) -> Self::Baked;
    }

    trait BakedProvider<'owner> {
        type TP;
        type CP;
        type MCP;
        type Fixed: FixedProvider<'owner>;
        fn get_text<'r>(&'r mut self, promise: Self::TP) -> &'owner str;
        fn get_child<'r>(&'r mut self, promise: Self::CP) -> &'owner Thing<'owner>;
        fn get_children<'r>(&'r mut self, promise: Self::MCP) -> &'owner [Thing<'owner>];
        fn fix(self) -> Self::Fixed;
    }

    trait FixedProvider<'owner> {
        type SP: FixedProvider<'static>;
        fn to_static(&self) -> Self::SP;
        fn inner(&self) -> &Thing<'owner>;
    }

    #[derive(Debug, Clone)]
    struct TextPromise(Range<usize>);
    #[derive(Debug, Clone)]
    struct ChildPromise(usize);
    #[derive(Debug, Clone)]
    struct ChildrenPromise(Range<usize>);

    struct UnbakedOwner<'owner> {
        text: String,
        children: Vec<UnbakedThing<TextPromise, ChildPromise, ChildrenPromise>>,
        _phantom: PhantomData<&'owner ()>,
    }

    impl UnbakedOwner<'_> {
        fn new() -> Self {
            Self {
                text: String::new(),
                children: Vec::new(),
                _phantom: PhantomData,
            }
        }
    }

    impl<'owner> UnbakedProvider<'owner> for UnbakedOwner<'owner> {
        type TP = TextPromise;
        type CP = ChildPromise;
        type MCP = ChildrenPromise;
        type Baked = BakedOwner<'owner>;

        fn register_text<'r>(&'r mut self, text: &str) -> Self::TP {
            let slice_start = self.text.len();
            self.text.push_str(text);
            let slice_end = self.text.len();
            TextPromise(slice_start..slice_end)
        }

        fn register_child<'r>(
            &'r mut self,
            child: UnbakedThing<TextPromise, ChildPromise, ChildrenPromise>,
        ) -> Self::CP {
            let ind = self.children.len();
            self.children.push(child);
            ChildPromise(ind)
        }

        fn register_children<'r>(
            &'r mut self,
            children: impl IntoIterator<Item = UnbakedThing<TextPromise, ChildPromise, ChildrenPromise>>,
        ) -> Self::MCP {
            let slice_start = self.children.len();
            self.children.extend(children);
            let slice_end = self.children.len();
            ChildrenPromise(slice_start..slice_end)
        }

        fn bake(self) -> Self::Baked {
            let children_num = self.children.len();
            let mut baked_children = Vec::with_capacity(children_num);
            for _ in 0..children_num {
                baked_children.push(MaybeUninit::uninit());
            }
            BakedOwner {
                text: Immutable::from(Pin::new(self.text.into_boxed_str())),
                unbaked_children: self.children.into_boxed_slice(),
                baked_children: Pin::new(baked_children.into_boxed_slice()),
                baked_children_mask: vec![false; children_num].into_boxed_slice(),
                _phantom: PhantomData,
            }
        }
    }

    struct BakedOwner<'owner> {
        text: Immutable<Pin<Box<str>>>,
        unbaked_children: Box<[UnbakedThing<TextPromise, ChildPromise, ChildrenPromise>]>,
        baked_children: Pin<Box<[MaybeUninit<Thing<'owner>>]>>,
        baked_children_mask: Box<[bool]>,
        _phantom: PhantomData<fn(&'owner ()) -> &'owner ()>,
    }

    impl<'owner> BakedOwner<'owner> {
        fn bake_a_child(&mut self, child_ind: usize) {
            let unbaked = self.unbaked_children[child_ind].clone();
            let baked = unbaked.bake(self);
            self.baked_children[child_ind].write(baked);
        }
    }

    impl<'owner> BakedProvider<'owner> for BakedOwner<'owner> {
        type TP = TextPromise;
        type CP = ChildPromise;
        type MCP = ChildrenPromise;
        type Fixed = FixedOwner<'owner>;

        fn get_text<'r>(&'r mut self, promise: Self::TP) -> &'owner str {
            // SAFETY:
            // This struct has a phantom field indicating that it lives exactly for `'owner`.
            // Along with rest of the fields being **pinned** **and immutable**, this means that returned
            // immutable reference will indeed live for `'owner`.
            unsafe { std::mem::transmute(&self.text[promise.0]) }
        }

        fn get_child<'r>(&'r mut self, ChildPromise(ind): Self::CP) -> &'owner Thing<'owner> {
            // first, check if child is init
            if !self.baked_children_mask[ind] {
                // it's not, make it init
                self.bake_a_child(ind);
            }
            // SAFETY:
            // we keep track of unbaked children separately, and we've just checked above that this exact token is, in fact, initialized
            let reference = unsafe { self.baked_children[ind].assume_init_ref() };
            // SAFETY:
            // This struct has a phantom field indicating that it lives exactly for `'owner`.
            // Along with rest of the fields being **pinned** **and immutable**, this means that returned
            // immutable reference will indeed live for `'owner`.
            unsafe { &*(reference as *const _) }
        }

        fn get_children<'r>(
            &'r mut self,
            ChildrenPromise(range): Self::MCP,
        ) -> &'owner [Thing<'owner>] {
            // first, check if all children in slice are init
            for ind in range.clone() {
                if !self.baked_children_mask[ind] {
                    // this child is not init, so init it
                    self.bake_a_child(ind);
                }
            }
            // SAFETY:
            // we keep track of unbaked children separately, and we've check that all of the tokens in slice to, in fact, exist
            let slice: &'r [Thing<'owner>] =
                unsafe { std::mem::transmute(&self.baked_children[range]) };
            // SAFETY:
            // This struct has a phantom field indicating that it lives exactly for `'owner`.
            // Along with rest of the fields being **pinned** **and immutable**, this means that returned
            // immutable reference will indeed live for `'owner`.
            unsafe { &*(slice as *const _) }
        }

        fn fix(self) -> Self::Fixed {
            let Self {
                text,
                baked_children_mask,
                baked_children,
                ..
            } = self;
            assert!(
                baked_children_mask.into_iter().all(|b| *b),
                "Not all children were initialized, so we cannot create output struct"
            );
            // SAFETY:
            // we keep track of unbaked children separately, and should've panicked above, if any of them were not properly init.
            let children: Immutable<Pin<Box<[Thing<'owner>]>>> =
                unsafe { std::mem::transmute(baked_children) };
            FixedOwner {
                _text: text,
                children,
                _phantom: PhantomData,
            }
        }
    }

    struct FixedOwner<'owner> {
        _text: Immutable<Pin<Box<str>>>,
        children: Immutable<Pin<Box<[Thing<'owner>]>>>,
        _phantom: PhantomData<fn(&'owner ()) -> &'owner ()>,
    }

    impl<'owner> FixedProvider<'owner> for FixedOwner<'owner> {
        type SP = FixedOwner<'static>;

        fn to_static(&self) -> Self::SP {
            todo!()
        }

        fn inner(&self) -> &'owner Thing<'owner> {
            // SAFETY:
            // This struct has a phantom field indicating that it lives exactly for `'owner`.
            // Along with rest of the fields being **pinned** **and immutable**, this means that returned
            // immutable reference will indeed live for `'owner`.
            let inner: &'owner Thing<'owner> = unsafe { &*(&self.children[0] as *const _) };
            inner
        }
    }

    #[derive(Debug, Clone)]
    enum UnbakedThing<TP, CP, MCP> {
        Text { text: TP },
        Child { text: TP, child1: CP, child2: CP },
        Multiple { children: MCP },
    }

    impl<'owner, TP, CP, MCP> UnbakedThing<TP, CP, MCP> {
        fn bake(
            self,
            provider: &mut impl BakedProvider<'owner, TP = TP, CP = CP, MCP = MCP>,
        ) -> Thing<'owner> {
            match self {
                UnbakedThing::Child {
                    text,
                    child1,
                    child2,
                } => Thing::Child {
                    text: provider.get_text(text),
                    child1: provider.get_child(child1),
                    child2: provider.get_child(child2),
                },
                UnbakedThing::Multiple { children } => Thing::Multiple {
                    children: provider.get_children(children),
                },
                UnbakedThing::Text { text } => Thing::Text {
                    text: provider.get_text(text),
                },
            }
        }
    }
    #[derive(Debug)]
    enum Thing<'source> {
        Text {
            text: &'source str,
        },
        Child {
            text: &'source str,
            child1: &'source Thing<'source>,
            child2: &'source Thing<'source>,
        },
        Multiple {
            children: &'source [Thing<'source>],
        },
    }

    #[cfg(test)]
    mod tests {
        use crate::data::borrowing_concept::{BakedProvider, UnbakedProvider, UnbakedThing};

        #[test]
        // #[cfg(miri)]
        fn miri_fuzz() {
            use crate::data::borrowing_concept::{
                ChildPromise, ChildrenPromise, TextPromise, Thing, UnbakedOwner,
            };

            // arrange
            fn gen_thing<'owner, R: rand::Rng + ?Sized>(
                provider: &mut UnbakedOwner<'owner>,
                rng: &mut R,
            ) -> (
                UnbakedThing<TextPromise, ChildPromise, ChildrenPromise>,
                Thing<'static>,
            ) {
                fn gen_text<R: rand::Rng + ?Sized>(rng: &mut R) -> &'static str {
                    // let it be of some length
                    let l: usize = rng.gen_range(3..=10);
                    let mut s = String::new();
                    for _ in 0..l {
                        s.push(rng.gen())
                    }
                    Box::leak(s.into_boxed_str())
                }
                let generated = if rng.gen_bool(0.6) {
                    // assume 60% are text variants
                    // we need some random text for it
                    let text = gen_text(rng);
                    (
                        UnbakedThing::Text {
                            text: provider.register_text(text),
                        },
                        Thing::Text { text },
                    )
                } else if rng.gen_bool(0.9) {
                    // say, 36% are child owners
                    // we need some text and children for it
                    let text = gen_text(rng);
                    let (c1_pr, ch1) = gen_thing(provider, rng);
                    let (c2_pr, ch2) = gen_thing(provider, rng);
                    (
                        UnbakedThing::Child {
                            text: provider.register_text(text),
                            child1: provider.register_child(c1_pr),
                            child2: provider.register_child(c2_pr),
                        },
                        Thing::Child {
                            text,
                            child1: Box::leak(Box::new(ch1)),
                            child2: Box::leak(Box::new(ch2)),
                        },
                    )
                } else {
                    // rest 4% own 1-5 children
                    let num_of_children = rng.gen_range(1..=5);
                    let mut children = Vec::with_capacity(num_of_children);
                    let mut children_pr = Vec::with_capacity(num_of_children);
                    for _ in 0..num_of_children {
                        let (ch_pr, child) = gen_thing(provider, rng);
                        children.push(child);
                        children_pr.push(ch_pr.clone());
                    }
                    let children_pr = provider.register_children(children_pr);
                    (
                        UnbakedThing::Multiple {
                            children: children_pr,
                        },
                        Thing::Multiple {
                            children: &*Box::leak(children.into_boxed_slice()),
                        },
                    )
                };
                // average coefficient is 0.6 * 0 + 0.36 * 2 + 2.5 * 0.04 = 0.82
                // meaning this thing is likely to die at some point (I hope for that, at least)
                generated
            }
            let mut provider = UnbakedOwner::new();

            // act
            let (promise, thing) = gen_thing(&mut provider, &mut rand::thread_rng());
            let mut provider = provider.bake();
            let baked_thing = promise.bake(&mut provider);

            // assert
            let thing_str = format!("{thing:?}");
            let baked_str = format!("{baked_thing:?}");
            assert_eq!(thing_str, baked_str, "Should produce identical strs");
        }
    }
}

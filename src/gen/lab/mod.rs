mod context;
mod meta;

pub use context::Context;
pub use meta::SourceMeta;

use itertools::Itertools;

use crate::{
    data::{Formatting, Token},
    path_engine::PathEngine,
};
use std::{borrow::Borrow, fmt::Write, marker::PhantomData};

use super::{ayano::AyanoExecutor, GenerationError, OutputGenerator, Res};

#[derive(Debug)]
pub struct LabaLatex<'source, PE, PEErr> {
    path_engine: PE,
    _phantom: PhantomData<(&'source (), PEErr)>,
}

impl<'source, PE: PathEngine<PEErr>, PEErr> LabaLatex<'source, PE, PEErr> {
    pub fn new(path_engine: PE) -> Self {
        Self {
            path_engine,
            _phantom: PhantomData,
        }
    }

    fn write_table_row<'meta, 'context, 'token: 'context, W: Write + ?Sized>(
        &self,
        output: &mut W,
        meta: &'meta SourceMeta<'source, AyanoExecutor>,
        context: &'context mut Context,
        cells: impl IntoIterator<Item = &'token Token<'source>>,
    ) -> Res<'source>
    where
        'source: 'meta + 'context + 'token,
        PE: PathEngine<PEErr>,
        PEErr: std::error::Error + 'source,
    {
        let mut cells = cells.into_iter();
        let Some(first_cell) = cells.next() else {
            return Ok(());
        };
        // first one must be written separately
        self.write_to(output, meta, context, first_cell)?;
        for cell in cells {
            output.write_str(" & ")?;
            self.write_to(output, meta, context, cell)?;
        }
        output.write_str("\\\\ \\hline\n")?;
        Ok(())
    }
}

impl<'source, PE, PEErr> OutputGenerator<'source, SourceMeta<'source, AyanoExecutor>, Context>
    for LabaLatex<'source, PE, PEErr>
where
    PE: PathEngine<PEErr>,
    PEErr: std::error::Error + 'source,
{
    fn write_to<'meta, 'context, 'token, W: Write + ?Sized>(
        &self,
        output: &mut W,
        meta: &'meta SourceMeta<'source, AyanoExecutor>,
        context: &'context mut Context,
        token: &'token Token<'source>,
    ) -> Res<'source>
    where
        'source: 'meta + 'context + 'token,
        'token: 'context,
    {
        match token {
            Token::PageDiv => {
                output.write_str("\n\\clearpage\n")?;
            }
            Token::Header { order, content } => {
                let command = match order {
                    0 => "section",
                    1 => "subsection",
                    2 => "subsubsection",
                    3 => "paragraph",
                    4 => "subparagraph",
                    _ => "textbf",
                };

                write!(output, "\n\\{command}{{{content}}}\n")?;
            }
            Token::DisplayMath { content, ident } => {
                let label = ident
                    .as_ref()
                    .map(|ident| format!("\\label{{eq:{}}}", ident))
                    .unwrap_or_else(String::new);
                write!(
                    output,
                    "\n\\begin{{equation}}\n{content}\n{label}\\end{{equation}}\n"
                )?;
            }
            Token::Table {
                header,
                cells,
                caption,
                ident,
            } => {
                let columns = header.len();
                let column_format =
                    itertools::intersperse(std::iter::repeat("c").take(columns), "|");
                write!(output, "\n\\begin{{table}}[h!]\n\\begin{{center}}\n")?;
                output.write_str("\\begin{tabular}{|")?;
                column_format
                    .map(|s| {
                        output.write_str(s)?;
                        Ok::<(), GenerationError>(())
                    })
                    .try_collect()?;
                output.write_str("|}\\hline\n")?;
                // write header line
                self.write_table_row(output, meta, context, header)?;
                output.write_str("\\hline\n")?;
                'cells: {
                    let mut rows = cells.chunks(columns).into_iter();
                    let Some(first_row) = rows.next() else {
                        break 'cells;
                    };
                    self.write_table_row(output, meta, context, first_row)?;
                    for row in rows {
                        self.write_table_row(output, meta, context, row)?;
                    }
                }
                output.write_str("\\end{tabular}\n\\stepcounter{tabnum}")?;
                if let Some(caption) = caption {
                    output.write_str("\n\\caption{\\capfnt ")?;
                    context.inside_caption = true;
                    self.write_to(output, meta, context, caption)?;
                    context.inside_caption = false;
                    output.write_str("}\n")?;
                }
                if let Some(ident) = ident.as_ref() {
                    write!(output, "\\label{{tab:{ident}}}\n")?;
                }
                output.write_str("\\end{center}\n\\end{table}\n")?;
            }
            Token::Figure {
                src_name,
                caption,
                ident,
                width,
            } => {
                output.write_str("\n\\begin{figure}[h!]\n\\centering\n\\includegraphics[width=")?;
                if let Some(width) = width.as_ref() {
                    output.write_str(&width.to_string())?
                } else {
                    output.write_str("0.9")?;
                }
                output.write_str("\\textwidth]{")?;
                let src_path = self
                    .path_engine
                    .image(src_name.as_ref(), None)
                    .map_err(|err| GenerationError::Path(Box::new(err)))?;
                output.write_str(src_path.to_string_lossy().borrow())?; // FIXME non-utf paths *might* cause an issue here
                output.write_str("}\n")?;
                if let Some(caption) = caption.as_ref() {
                    output.write_str("\\caption{\\capfnt ")?;
                    context.inside_caption = true;
                    self.write_to(output, meta, context, caption)?;
                    context.inside_caption = false;
                    output.write_str("}\n")?;
                }
                if let Some(ident) = ident.as_ref() {
                    write!(output, "\\label{{fig:{ident}}}\n")?;
                }
                output.write_str("\\end{figure}\n")?;
            }
            Token::Ayano { data } => {
                let token = meta.ayano.display_token(data)?;
                self.write_to(output, meta, context, &token)?;
            }
            Token::List { list_type, content } => {
                let start: &str = match list_type {
                    crate::data::ListType::Bullet => "\\begin{itemize}",
                    crate::data::ListType::Num => "\\begin{enumerate}",
                    crate::data::ListType::Latin => {
                        unimplemented!("Latin lists are not supported for now")
                    }
                    crate::data::ListType::Cyrillic => {
                        "\\begin{enumerate}[label=\\asbuk*), ref=\\asbuk*]"
                    }
                    crate::data::ListType::Roman => {
                        unimplemented!("Roman numeral lists are not supported for now")
                    }
                };
                let end: &str = match list_type {
                    crate::data::ListType::Bullet => "\\end{itemize}",
                    crate::data::ListType::Num | crate::data::ListType::Cyrillic => {
                        "\\end{enumerate}"
                    }
                    crate::data::ListType::Latin => {
                        unimplemented!("Latin lists are not supported for now")
                    }
                    crate::data::ListType::Roman => {
                        unimplemented!("Roman numeral lists are not supported for now")
                    }
                };
                output.write_str("\n")?;
                output.write_str(start)?;
                for item in content {
                    output.write_str("\n\\item ")?;
                    self.write_to(output, meta, context, item)?;
                }
                output.write_str("\n")?;
                output.write_str(end)?;
            }
            Token::InlineMath {
                content,
                space_before,
            } => {
                if *space_before {
                    output.write_char(' ')?;
                }
                write!(output, "${content}$")?;
            }
            Token::Reference {
                ident,
                space_before,
            } => {
                if *space_before {
                    output.write_char(' ')?;
                }
                write!(output, "\\ref{{{ident}}}")?;
            }
            Token::FootnoteReference {
                ident,
                space_before,
            } => {
                if *space_before {
                    output.write_char(' ')?;
                }
                let (encountered, i) = {
                    // TODO probably should move this to separate function
                    let encountered_map = &mut context.encountered_footnotes;
                    let encountered = encountered_map.get(ident.borrow() as &str).cloned();
                    let i = match encountered {
                        Some(i) => i,
                        None => {
                            let i: usize = encountered_map.len();
                            // let ident: &'context Cow<'source, str> = ident;
                            encountered_map.insert(ident.to_string(), i);
                            i
                        }
                    };
                    (encountered.is_some(), i)
                };
                // write out mark (always done)
                write!(output, "\\footnotemark[{}]", i + 1)?;
                if !encountered {
                    // write out content (done only on first occurrence)
                    if let Some(content) = meta.footnotes.get(&ident.borrow()) {
                        write!(output, "\\footnotetext[{}]{{", i + 1)?;
                        self.write_to(output, meta, context, content)?;
                        output.write_str("}")?;
                    }
                }
            }
            // might seem weird at first, but these are basically useless at this point, as their content was presumably collected into context
            Token::FootnoteContent { .. } => {}
            Token::Error { message } => {
                eprintln!("{message}");
                // TODO add representation in the document
            }
            Token::Paragraph {
                is_newline,
                formatting,
                content,
                space_before,
            } => {
                if *is_newline {
                    output.write_str("\n\\tbln ")?;
                } else if *space_before {
                    output.write_char(' ')?;
                }
                let (start, end) = match formatting {
                    None => ("", ""),
                    Some(Formatting::Bold) => ("\\textbf{", "}"),
                    Some(Formatting::Italic) => ("\\textit{", "}"),
                    Some(Formatting::StrikeThrough) => ("\\st{", "}"),
                };
                output.write_str(start)?;
                if context.inside_caption {
                    // We are generating text that was inside table/figure caption
                    // meaning, we must un-escape double quotes
                    let mut last_ind = 0;
                    for (ind, _) in content.match_indices("\\\"") {
                        output.write_str(&content[last_ind..ind])?;
                        output.write_char('\"')?;
                        last_ind = ind + 2;
                    }
                    output.write_str(&content[last_ind..])?;
                } else {
                    output.write_str(&content)?;
                }
                output.write_str(end)?;
            }
            Token::Href {
                url,
                display,
                space_before,
            } => {
                if *space_before {
                    output.write_char(' ')?;
                }
                write!(output, "\\href{{{url}}}{{{display}}}")?;
            }
            Token::Multiple { tokens } => self.write_tokens_to(output, meta, context, tokens)?,
            Token::CodeBlock { code, language } => {
                if let Some(language) = language {
                    writeln!(
                        output,
                        "\n\\begin{{minted}}{{{language}}}\n{code}\n\\end{{minted}}\n"
                    )?;
                } else {
                    writeln!(output, "\n\\begin{{minted}}\n{code}\n\\end{{minted}}\n")?;
                }
            }
        };
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use crate::data::{AyanoBlock, Formatting, ListType};

    use super::*;

    macro_rules! text {
        ($text:literal) => {
            Token::Paragraph {
                space_before: false,
                is_newline: false,
                formatting: None,
                content: $text.into(),
            }
        };
    }

    macro_rules! tex_sim {
        ($a:expr, $b:expr) => {
            let real = $a
                .chars()
                .filter(|c| !c.is_ascii_whitespace())
                .collect::<String>();
            let expected = $b
                .chars()
                .filter(|c| !c.is_ascii_whitespace())
                .collect::<String>();

            text_diff::assert_diff(real.as_str(), expected.as_str(), "", 0);
        };
    }

    macro_rules! tokens {
        [$($token:expr), *] => {
            $crate::data::Tokens::new([$($token), *])
        };
    }

    macro_rules! test {
        {$name:ident, $token:expr, $b:expr} => {
            #[test]
            fn $name() {
                use $crate::path_engine::primitive as primitive_engine;
                // arrange
                let token = $token;
                let meta = SourceMeta::collect(&token)
                    .expect("Should be able to collect")
                    .init_ayano()
                    .expect("Should be able to init Ayano");

                let generator = LabaLatex::new(primitive_engine());
                let mut output_target = String::new();

                // act
                generator.write_to(&mut output_target, &meta, &mut Context::default(), &token).expect("Should be able to write");

                // assert
                tex_sim!(output_target, $b);

            }
        };
    }

    // Page Divider
    test! {clearpage, Token::PageDiv, r"\clearpage"}

    // Headers
    test! {header0, Token::Header { order: 0, content: "HeaderText".into() }, r"\section{HeaderText}"}
    test! {header1, Token::Header { order: 1, content: "HeaderText".into() }, r"\subsection{HeaderText}"}
    test! {header2, Token::Header { order: 2, content: "HeaderText".into() }, r"\subsubsection{HeaderText}"}
    test! {header3, Token::Header { order: 3, content: "HeaderText".into() }, r"\paragraph{HeaderText}"}
    test! {header4, Token::Header { order: 4, content: "HeaderText".into() }, r"\subparagraph{HeaderText}"}
    test! {header5, Token::Header { order: 5, content: "HeaderText".into() }, r"\textbf{HeaderText}"}

    // Paragraphs
    test! {para1, Token::Paragraph{is_newline: false, formatting: None, content: "content, content!!!".into(), space_before: false}, "content, content!!!"}
    test! {para2, Token::Paragraph{is_newline: true, formatting: None, content: "content, content!!!".into(), space_before: false}, "\n\\tbln content, content!!!"}

    // Formatting
    test! {formatting1, Token::Paragraph{is_newline: false, formatting: Some(Formatting::Bold), content: "some text, idk".into(), space_before: false}, "\\textbf{some text, idk}"}
    test! {formatting2, Token::Paragraph{is_newline: false, formatting: Some(Formatting::Italic), content: "some text, idk".into(), space_before: false}, "\\textit{some text, idk}"}
    test! {formatting3, Token::Paragraph{is_newline: false, formatting: Some(Formatting::StrikeThrough), content: "some text, idk".into(), space_before: false}, "\\st{some text, idk}"}

    // Hrefs
    test! {href1, Token::Href { url: "https://www.github.com/Dzuchun".parse().unwrap(), display: "My gh page".into(), space_before: false }, r"\href{https://www.github.com/Dzuchun}{My gh page}"}
    test! {href2, Token::Href { url: "https://github.com/Dzuchun".parse().unwrap(), display: "Моя сторінка на гітхабі".into(), space_before: false }, r"\href{https://github.com/Dzuchun}{Моя сторінка на гітхабі}"}

    // Equations
    test! {equations_1, Token::DisplayMath { content: r"B(\alpha, \beta) = \int \limits_{0}^{1} x^{\alpha - 1} (1-x)^{\beta-1} dx".into(), ident: None },
    r"
    \begin{equation}
    B(\alpha, \beta) = \int \limits_{0}^{1} x^{\alpha - 1} (1-x)^{\beta-1} dx
    \end{equation}
    "}
    test! {equations_2, Token::DisplayMath { content: r"B(\alpha, \beta) = \int \limits_{0}^{1} x^{\alpha - 1} (1-x)^{\beta-1} dx".into(), ident: Some("beta_def".into()) },
    r"
    \begin{equation}
    B(\alpha, \beta) = \int \limits_{0}^{1} x^{\alpha - 1} (1-x)^{\beta-1} dx

    \label{eq:beta_def}
    \end{equation}
    "}

    // Inline math
    test! {inline_mathmode, Token::InlineMath{content:r"\dfrac{\partial x}{\partial t}".into(), space_before: false}, r"$ \dfrac{\partial x}{\partial t} $"}

    // Figures
    test! {fig1, Token::Figure { src_name: Path::new("apple.jpg").into(), caption: Some(Box::new(text!("a very realistic-looking apple"))), ident: None, width: None }, r"
    \begin{figure}[h!]
    \centering
    \includegraphics[width = 0.9 \textwidth]{apple.jpg}
    \caption{\capfnt a very realistic-looking apple}
    \end{figure}
    "}
    test! {fig2, Token::Figure { src_name: Path::new("schema1.jpg").into(), caption: Some(Box::new(text!("A schema providing a bunch of very important info on quantum refurbalidzer function"))), ident: Some("schema-1".into()), width: None }, r"
    \begin{figure}[h!]
    \centering
    \includegraphics[width = 0.9 \textwidth]{schema1.jpg}
    \caption{\capfnt A schema providing a bunch of very important info on quantum refurbalidzer function}
    \label{fig:schema-1}
    \end{figure}
    "}
    test! {fig3_width, Token::Figure { src_name: Path::new("schema1.jpg").into(), caption: Some(Box::new(text!("A schema providing a bunch of very important info on quantum refurbalidzer function"))), ident: Some("schema-1".into()), width: Some(0.25) }, r"
    \begin{figure}[h!]
    \centering
    \includegraphics[width = 0.25 \textwidth]{schema1.jpg}
    \caption{\capfnt A schema providing a bunch of very important info on quantum refurbalidzer function}
    \label{fig:schema-1}
    \end{figure}
    "}
    test! {fig4_width, Token::Figure { src_name: Path::new("schema1.jpg").into(), caption: Some(Box::new(text!("This caption contains \\\"double quotes\\\""))), ident: None, width: None }, r#"
    \begin{figure}[h!]
    \centering
    \includegraphics[width = 0.9 \textwidth]{schema1.jpg}
    \caption{\capfnt This caption contains "double quotes"}
    \end{figure}
    "#}
    // Tables
    test! {tab1, Token::Table {
    header:
    tokens![
        text!("header1"), text!("header2"), text!("header3")
    ],
    cells: tokens![
        text!("cell_11"),text!("cell_12"),text!("cell_13"),
        text!("cell_21"),text!("cell_22"),text!("cell_23")
    ],
    caption: Some(Box::new(text!("caption..."))),
    ident: Some("example".into()) },
    r"
    \begin{table}[h!]
    \begin{center}
    \begin{tabular}{|c|c|c|}\hline
header1 & header2 & header3 \\ \hline \hline
cell_11 & cell_12 & cell_13 \\ \hline
cell_21 & cell_22 & cell_23 \\ \hline
    \end{tabular}
    \stepcounter{tabnum}
    \caption{ \capfnt caption...}
    \label{tab:example}
    \end{center}
    \end{table}
    "}
    test! {tab_quotes, Token::Table {
    header:
    tokens![
        text!("header1"), text!("header2"), text!("header3")
    ],
    cells: tokens![
        text!("cell_11"),text!("cell_12"),text!("cell_13"),
        text!("cell_21"),text!("cell_22"),text!("cell_23")
    ],
    caption: Some(Box::new(text!("We like \\\"math\\\" so much!"))),
    ident: Some("example".into()) },
    r#"
    \begin{table}[h!]
    \begin{center}
    \begin{tabular}{|c|c|c|}\hline
header1 & header2 & header3 \\ \hline \hline
cell_11 & cell_12 & cell_13 \\ \hline
cell_21 & cell_22 & cell_23 \\ \hline
    \end{tabular}
    \stepcounter{tabnum}
    \caption{ \capfnt We like "math" so much!}
    \label{tab:example}
    \end{center}
    \end{table}
    "#}

    // Lists
    test! {list_bullets, Token::List { list_type: ListType::Bullet, content: tokens![
        text!("point1"),
        text!("point2"),
        text!("point3")
    ] }, r"
    \begin{itemize}
    \item point1
    \item point2
    \item point3
    \end{itemize}
    "}

    test! {list_numeric, Token::List { list_type: ListType::Num, content: tokens![
        text!("point1"),
        text!("point2"),
        text!("point3")
    ] }, r"
    \begin{enumerate}
    \item point1
    \item point2
    \item point3
    \end{enumerate}
    "}

    test! {list_cyrillic, Token::List { list_type: ListType::Cyrillic, content: tokens![
        text!("point1"),
        text!("point2"),
        text!("point3")
    ] }, r"
    \begin{enumerate}[label=\asbuk*), ref=\asbuk*]
    \item point1
    \item point2
    \item point3
    \end{enumerate}
    "}

    // Text
    test! {text, Token::Multiple{tokens:tokens![Token::PageDiv, Token::Header { order: 0, content: "Header".into() }, text!("Here's a little formula: "), Token::InlineMath{content:r"(a+b)^2 = a^2 + 2 \cdot a \cdot b + b^2".into(), space_before: false}, text!(". It's very useful, more useful than me!")]}, r"
    \clearpage
    \section{Header}
    Here's a little formula: $(a+b)^2 = a^2 + 2 \cdot a \cdot b + b^2$. It's very useful, more useful than me!
    "}

    // Footnotes
    test! {footnote_single, Token::Multiple{tokens:tokens![
        Token::FootnoteReference{ident:"explanation".into(), space_before: false},
        Token::FootnoteContent { content: Box::new(text!("42")), ident: "explanation".into() }
    ]}, r"
    \footnotemark[1]
    \footnotetext[1]{42}
    "}

    test! {footnote_multiple, Token::Multiple{tokens:tokens![
        text!("Here's a first note"),
        Token::FootnoteReference{ident:"explanation1".into(), space_before: false},
        text!(", there are also second"),
        Token::FootnoteReference{ident: "explanation2".into(), space_before: false},
        text!(", and the third"),
        Token::FootnoteReference{ident:"explanation3".into(), space_before: false},
        Token::FootnoteContent { content: Box::new(text!("There are things in this world that's you're not meant to see")), ident: "explanation1".into() },
        Token::FootnoteContent { content: Box::new(text!("now the voice of a deity permeates")), ident: "explanation2".into() },
        Token::FootnoteContent { content: Box::new(text!("see notes 1 and 2")), ident: "explanation3".into() },
        text!(". But notes will not be duplicated here!"),
        Token::FootnoteReference{ident:"explanation1".into(), space_before: false}

    ]}, r"
    Here's a first note
    \footnotemark[1]
    \footnotetext[1]{There are things in this world that's you're not meant to see}
    , there are also second
    \footnotemark[2]
    \footnotetext[2]{now the voice of a deity permeates}
    , and the third
    \footnotemark[3]
    \footnotetext[3]{see notes 1 and 2}
    . But notes will not be duplicated here!
    \footnotemark[1]
    "}

    test! {ayano_plain1, Token::Ayano{data: AyanoBlock{
        is_display: false,
        is_static: false,
        code: "1".into(),
        insert_path: None,
        is_space_before: false,
    }}, r"1"}
    test! {ayano_plain2, Token::Ayano{data: AyanoBlock{
        is_display: false,
        is_static: false,
        code: "1 + 2".into(),
        insert_path: None,
        is_space_before: false,
    }}, r"3"}
    test! {ayano_plain3, Token::Ayano{data: AyanoBlock{
        is_display: false,
        is_static: false,
        code: r"
from math import sqrt
x = 103
y = sqrt(x)
int(y)"
        .into(),
        insert_path: None,
        is_space_before: false,
    }}, r"10"}
    test! {ayano_plain4, Token::Ayano{data: AyanoBlock{
        is_display: false,
        is_static: false,
        code: r"
res = 0
for i in range(101):
    res += i
res"
        .into(),
        insert_path: None,
        is_space_before: false,
    }}, r"5050"}
    test! {ayano_err1, Token::Ayano{data: AyanoBlock{
        is_display: false,
        is_static: false,
        code: r"
    'err', 1.0, 0.1"
        .into(),
        insert_path: None,
        is_space_before: false,
    }}, r"1.00 $\pm$ 0.10"}
    test! {ayano_err2, Token::Ayano{data: AyanoBlock{
        is_display: false,
        is_static: false,
        code: r"
    'err', 1.0, 0.4"
        .into(),
        insert_path: None,
        is_space_before: false,
    }}, r"1.0 $\pm$ 0.4"}
    test! {ayano_err3, Token::Ayano{data: AyanoBlock{
            is_display: false,
            is_static: false,
            code: r"
x = 1.0
y = 0.4
@dev: x, y"
            .into(),
            insert_path: None,
            is_space_before: false,
        }}, r"1.0 $\pm$ 0.4"
    }

    test! {ayano_fig1, Token::Ayano{data: AyanoBlock{
        is_display: false,
        is_static: false,
        code: r"'fig', 'path/to/image.jpg', None, None, None".into(),
        insert_path: None,
        is_space_before: false,
    }}, r"
\begin{figure}[h!]
\centering
\includegraphics[width = 0.9 \textwidth]{path/to/image.jpg}
\end{figure}"
    }

    test! {ayano_fig2_width, Token::Ayano{data: AyanoBlock{
        is_display: false,
        is_static: false,
        code: r"'fig', 'path/to/image.jpg', None, None, 0.625".into(),
        insert_path: None,
        is_space_before: false,
    }}, r"
\begin{figure}[h!]
\centering
\includegraphics[width = 0.625 \textwidth]{path/to/image.jpg}
\end{figure}"
    }

    test! {ayano_fig2, Token::Ayano{data: AyanoBlock{is_display: false,is_static: false,code: r"@fig: src = 'path/to/image.jpg'".into(),insert_path: None,
                    is_space_before: false,}},
r"
\begin{figure}[h!]
\centering
\includegraphics[width = 0.9 \textwidth]{path/to/image.jpg}
\end{figure}"}
    test! {ayano_fig3, Token::Ayano{data: AyanoBlock{ is_display: false,is_static: false,code: r#"@fig: src = 'path/to/image.jpg', ident = "meow""#.into(),insert_path: None,
    is_space_before: false,
        }},
r#"
\begin{figure}[h!]
\centering
\includegraphics[width = 0.9 \textwidth]{path/to/image.jpg}
\label{fig:meow}
\end{figure}"#
    }
    // TODO add tests with captions

    test! {ayano_gen_tab1, Token::Ayano{data: AyanoBlock { is_display: false, is_static: false, code:
r#"
data = [[1, 2, 3], [4, 5, 6]]
@gen_table: lambda r,c: data[r][c]; rows=2, columns=3
"#.into(), insert_path: None,
    is_space_before: false, }},
r#"
\begin{table}[h!]
\begin{center}
\begin{tabular}{|c|c|c|}\hline
1 &  2 & 3 \\ \hline \hline
 4 &  5 & 6 \\ \hline
\end{tabular}
\stepcounter{tabnum}
\end{center}
\end{table}
"#
        }
    // TODO add tests with csv-tables

    // TODO add minted to preamble
    // TODO add -shell-escape to script util
    test! {cobe_block1, Token::CodeBlock { code: r"
def f():
    x = 5
    y = x * x
    return y".into(), language: Some("python".into()) },
r"
\begin{minted}{python}
def f():
    x = 5
    y = x * x
    return y
\end{minted}
"}
    // TODO probably add more tests for code blocks..?
}

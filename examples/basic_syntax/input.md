Here's a quick rundown of supported markdown syntax:

# Headers
###### Up to 6 deep

Inline math: $y = x^2\qquad \text{- is a parabola equation}$

Display math:
$$
\Gamma(z) = \int\limits_{0}^{+\infty} \exp(-t) t^{z-1} dt \qquad \text{- це Euler's Gamma-function definition}
$$
{ref = gamma_function}

Page breaks:

------

This text must appear on the next page

Figures (take note of the caption and ident):
![[loss_meme.png]]
{ref = loss_meme, caption = "These are all loss functions, technically making this image a loss meme", width = 0.4}

Tables:
| Number of appendages |     Name     |
| :------------------: | :----------: |
|          1           |  Monopedal   |
|          2           |   Bipedal    |
|          3           |   Tripedal   |
|          4           | Quadropedal  |
|          5           |  Pentapedal  |
|          6           |  Hexapedal   |
|          7           |  Heptopedal  |
|          8           |  Octopedal   |
|          9           |     why.     |
|         1001         | are you sure |
|      $\omega_0$      | are you ok?  |
|      $\zeta_1$       |   cracker    |
{ref = pedals, caption= "Source: trust **me**"}

Hyper references: [some website](https://www.somewebsite.com)

Footnotes: say, I need to explain this text, but not include the actual explanation right where I write it[^1]

Object references: this refers to table: [@tab:pedals], this refers to figure: [@fig:loss_meme], an this one to the equation at the beginning: [@eq:gamma_function].

Lists are supported too; Here's a bullet list:
- First item
- Second item
- Third item

Here's an enumeration:
1. First item
2. Second item
3. Third item

Here are alphabetic enumerations (latin and cyrillic are only supported, for now)
a. First item
b. Second item
c. Third item
а. First item
б. Second item
в. Third item

Cyrillic implementation detail: наразі у якості порядку елементів береться буквально 'а'..'я'. Тобто, згідно з [таблицею юнікоду](https://en.wikipedia.org/wiki/Cyrillic_(Unicode_block\)), тут поки що реалізовано терористичний алфавіт:
а. Перший елемент
б. Другий елемент
в. Третій елемент
г. Четвертий елемент
д. Шостий елемент (не вистачає ґ)
Це може змінитись у майбутньому, тож краще поки що утриматись від використання кириличних списків.

Finally, code blocks are supported:
```lua
print("Hello lua!")
function fact (n)
    if n == 0 or n == 1 then
        return 1
    else
        return n * fact(n-1)
    end
end

print(fact(7))
```
```rust
#[repr(transparent)]
#[derive(Debug, derive_more::From, derive_more::Deref, derive_more::DerefMut)]
pub struct Equivalent<T>(pub T);

impl<T> PartialEq for Equivalent<T> {
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl<T> Eq for Equivalent<T> {}

// This is a real type used in this project's codebase to allow error comparison for testing
// Although I might decide to remove it in the future
```
These blocks are powered by [minted](https://github.com/gpoore/minted), so to get full list of supported languages, check their docs out.

This is all of the markdown syntax supported for now. However, there's an *otherside* to `kyomato`'s capabilities - namely, `ayano` blocks.

------

[^1]: You can absolutely do this! *Note*, that despite this footnote definition is on a separate page at the end, actual document output includes the footnote in a more convenient spot.
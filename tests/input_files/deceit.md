This test is dedicated to attempts of parser deception, i.e. usage of special symbols in wrong places to see if parser recognizes them as control symbols. There we go:

# Page dividers

Page dividers are represented by ------

If the line above has dashes     ^^^^^^ here, it probably works alright.

Here's a genuine divider
------
Fun fact: there can be a paragraph right after it!
------ Some text, cause why not
But this **DOES** look cursed, don't do it.

# Headers
Headers are most recognizable by '#'-symbol. For it to actually represent a header, it needs to at the beginning of the line and there should be no more than 6 of them. **ANYTHING** up to the end of line will be included into a header, then.

Example:

###### This is a long, yet formally correct header. You can add $equations$ and *formatting* here, but I provide no guarantees as of the way these would display in final document.

Here's an example of incorrect header:

 ###### This header is considered incorrect for this application, and will be parsed as regular paragraph instead

Another example of incorrect header, featuring 7 hash-symbols:

####### This will be parsed as a regular paragraph

# Equations

There are two types of equations in markdown/latex: inline mathmode and display mathmode

Both of them are defined by $-symbols. However, they must be paired to actually represent these. Here are examples:

Inline: $y = x ^ 2$

Display:
$$
y = x ^ 2
$$

Note that display mathmode should ALWAYS start from a new line for this application.

Basically, there's not much to deceive for: you can do that:
$
$
(the above should not be mathmode), and that's it, basically :sad:

# Table
Tables are defined by |-symbol. But the actual table will happen only if they are the first thing on the line.

Due to current implementation, there's no cure apart from display mathmode and code:

(this should be display mathmode):
$$
|x|
$$

(this should be a code block):
```bash
head /dev/random
| cat
```

# Figure

Figure is defined by a ![[ sequence at a start of the line. Same points should be uphold here:

$$
![[
$$

```
![[
```

# References

These start with []

## Hyperrefs

These are followed by parentheses (). Nothing is allowed between them, thus

Here's a correct href: [my gh page](https://youtu.be/rickroll_link_lol)

Here are incorrect refs:

[my gh page] (https://youtu.be/rickroll_link_lol)

[my gh page]
(https://youtu.be/rickroll_link_lol)

## Footnote refs

These are started by [^.

Here are correct footnote refs: [^1], [^source]

Here are incorrect ones: [^1, ^1],

[^
1
]

So on

## Object refs

These refer to objects like figures, tables and equations.

Started by [@.

Here's a correct ref: [@   fig:experiment_schema]

Here's are bad ones: [@ fig:1, @  source3],

[@
eq2
]

**NOTE**: despite object types being prepended with 'eq', 'fig' or 'tab' in refs, you *can* use regular latex to define your custom refs. In fact, LaTeX doesn't care about all of these at all; I'm doing these transformations just for my own comfort, as I've been doing this manually.

# Formatting
This object's type is deprecated and will be removed soon.

It starts and ends with 
- * or _
- ** or __
- ~~

**WARN**: As of now, these *might* cause cause massive problems if encountered at the start of the line.

Basically, parsers for these tokens attempt parse everything up until they find a closing token. But if they are inserted in a middle of the text, then it's ok - they will be limited by current paragraph, and (most likely) will find no match inside it, if their use was unintended.

# Footnote content
These start with '[^ID]:' and proceed to the end of line.

Example of valid content:
[^source2]: Explanation of source2.

Example of invalid content:
[^source2: This is invalid content
[^source2] : This is invalid content
 [^source2]: This is invalid content

They also must start at the beginning of the line. If fact, this: [^source2]: Some content
Is expected to be a footnote ref with a colon.

# Code blocks

There are two fundamental types of code blocks: normal code block and **Ayano** code block.

## Normal code block

These start and end with '```' on a newline. There can be **practically anything** inside. Example:

```java
int x = 2;
int y = 4;
int[] arr = new int[2];
arr[0] = x;
arr[1] = 4;
```

There can be anything after code block had closed:
```lua(Idk why vscode detects this block closing as a valid lua syntax)
local x = 'matt'
local y = 'y'
local z = x .. y -- matty!!
``` Here's some text that's ok to be here

There can't be any spaces before delimiting ```s:

 ```python
 # This here is not really a python block
 x = 2
 print(x)
 ```

## Ayano code block

*because Ayano loves Kyoko*

This object looks and behaves just like normal code block, but is only limited to *python* language, can have spacial arguments in a language line, may leave something behind in the document. For example:

This Ayano block will leave plain "2" behind it:
```python, Ayano
2
```

Akin to normal blocks, it's ok to leave something trailing:
```ayano
2
``` This text is ok to be here

And these are invalid Ayano blocks:
```ayano
2
```
 ```ayano
2
 ```
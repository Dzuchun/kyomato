Here's a basic Ayano explanation:

Essentially, Ayano blocks are regular code blocks, having `python, Ayano` marked as their language. This sort of codeblocks are treated completely differently by the parser - these blocks are supposed to allow arbitrary python code execution for data generation and analysis.

For this example, we'll only consider examples that do not interact with the filesystem.

Say, you have some sort of dataset, you wish to find a mean a deviation of. Here's a way to do that with Ayano (without filesystem interaction):
```python, Ayano ! * "That's a static block, declaring initial dataset"
# first, you declare the dataset itself in a *static* Ayano block
# (note the exclamation mark above)
data = [1, 1, 2, 3, 2, 5, 5, 6, 2, 1, 4, 9]
# Static block essentially translated into a code initially executed by python, when the entire module it loaded.
# Keep that in mind: ALL changes introduced by static blocks will be visible by ALL function blocks.
```
You might notice, that there's actually nothing displayed above in the document. That's due to *static blocks* leaving nothing behind them. To actually display the dataset, we would use a regular, non-static Ayano block:


```python, Ayano
data
```
Above you should see a python's `str` representation of initial dataset.

Now, let's try calculating mean and the deviation. To achieve that, I'll use `statistics` module:

```python, Ayano * "mean and deviation calculations"
from statistics import stdev, mean
mean_val = mean(data)
deviation = stdev(data)
@dev: mean_val, deviation
```

You should see dataset's mean value and deviation above. You might notice, that's it's in a good-looking format. That's because of special format used for value-deviation sort-of-values, that's described in the docs. If you wish to circumvent it, you absolutely can:

```python, Ayano * "mean and deviation calculations, but without value-error formatting"
from statistics import stdev, mean
mean_val = mean(data)
deviation = stdev(data)
f"{mean_val} $\pm$ {deviation}"
```

Ayano can generate tables too! For example, let's generate a table of our dataset, but also add information on if it's greater or less than the mean value, it's square and absolute deviation from the mean:
```python, Ayano * "that chaotic table generation example"
# look at the last line first
# that's a table generation syntax - it involves a cell generator, number of rows, number of columns and optional ident and caption arguments
# you could use python's function as generator too (a general requirement is for the thing to be callable with two integer args)
from statistics import mean
from math import sqrt
header = ["initial data", "cmp to mean", "square", "absdev"]
mean_val = mean(data)
cells = list([[("err", val, sqrt(val)), "greater" if val >= mean_val else "less", val * val, abs(val - mean_val)] for val in data])
@gen_table: lambda r,c: header[c] if r == 0 else cells[r-1][c]; rows = 13, columns = 4, ident="crazy_table", caption = "A crazy table containing a bunch of stuff"
```

Above you can see that crazy table, and you also can refer to it ([@tab:crazy_table]).

That's pretty much it for Ayano blocks that do not access the filesystem.
This is an example of advanced `Ayano` features.

First, there are insert blocks. These can load some code from specified python script, and insert it at the start:


```python, Ayano ! ~ scripts/calculate_e.py * "That's a way to calculate pi"
not_pi = e
```

Note that their displayed version at the end actually includes the loaded script. These blocks can be used for display too:


```python, Ayano ~ scripts/calculate_e.py * "Ooops, that actually turns out to be a formula for e :("
from math import pi
f"e = {e}, pi = {pi}, pi != {not_pi}"
```

Sometimes you wish to only include part of the script, and do something else after some point. To achieve that, you can include `# TOSHINO KYOKO!` comment (note the space and casing; you can append anything after it at that line). Once this comment is seen by `Ayano`, it stops inserting the file, and appends the actual code you've provided in the block (possibly none). Comment itself is not included:

```python, Ayano ~ scripts/toshino_kyoko.py * "the all-mighty **2**!!!"
# you'll see this comment, and the 2
x
```

Ayano blocks can actually interact with the filesystem themselves. The caveat is - where exactly python thinks it's current directory is.
Well, the rule of thumb turns out to be quite simple:

- if there is an insertion declared for a block, it's execution will be in the directory that file is in
- if there is no insertion declared, python will continue it's execution at `kyomato`'s process directory
```python, Ayano ! ~ data/read3.py * "Here you can see an empty static block displayed as inserted script"
```
```python, Ayano ! * "You can clearly see that this block has no insert statement, yet it knows **exactly** where to find the file"
two = None
with open("2", "r") as f:
    two = int(f.readline())
```


```python, Ayano * "This block here uses variables prior-defined in static blocks."
two + three
```

Ayano can be used to insert csv tables (possibly generated at runtime too). Here's a special syntax for it:
```python, Ayano * "table insertion example"
@csv_table: src="data/20240327230336_8798.csv", caption="cred: [randat](http://randat.com/)", ident = "random_table1"
```

This can be combined with insertion blocks to achieve generated table insertion; you can specify a path relative to the inserted file,
```python, Ayano * "An example of runtime-generated table insertion" ~ data/doing_stuff.py
@csv_table: src="calculations_table.csv"
```

... but you've not forced to do that; you may do execution-relative path too:
```python, Ayano * "An example of runtime-generated table insertion with execution-relative path" ~ data/doing_stuff.py
@csv_table: src="data/calculations_table.csv"
```

An of course, once data was already generated (it happened twice here, twice, in fact), you can insert it without any script:
```python, Ayano * "An example of runtime-generated table insertion with execution-relative path"
@csv_table: src="data/calculations_table.csv"
```

You can also restrict the columns and rows you want, for example:
```python, Ayano * "This is only a part of the table above, due to specified column and row restrictions"
@csv_table: src="data/calculations_table.csv", rows = 3..4, columns = ["name"], caption = "That's a-bit-cut table!"
```

There's also a special columns syntax to display some columns in value-error format:
```python, Ayano * "You can see values having error right beside them, and formatted accordingly!"
@csv_table: src="data/calculations_table.csv", columns = ["name", ("value", "err"), "%"], caption = "That's a table with value-error formatting!"
```

That's pretty much it, regarding tables.

Ayano also allows you to include dynamically-generated figures. You can go two ways about that:

### 1st way

Generate a figure in a static block, and save it somewhere. After that, include it as an image:
```python, Ayano ! * "some figure generation"
import numpy as np
import matplotlib.pyplot as plt
from math import pi
x = np.arange(-pi, pi, 0.01)
y = np.sin(x)
plt.plot(x, y)
plt.savefig("sine_graph.png")
```
![[sine_graph.png]]
{caption = "that's a nice little sine graph!", ref = sine}

### 2nd way (more idiomatic, in my opinion)

Generate a figure in function block, and insert it with a special syntax:
```python, Ayano * "that's a figure insertion syntax, all-in-one!"
import numpy as np
import matplotlib.pyplot as plt
from math import pi
x = np.arange(-pi, pi, 0.01)
y = np.sin(x)
plt.plot(x, y)
plt.savefig("sine_graph.png")
@fig: src = "sine_graph.png", ident = "other_sine_graph", caption = "That's another way to include a figure from Ayano!", width = 0.9
```
The only real upside I can see here - you can generate-and-include figures with prior-unknown names or locations

Oh, and you also can specify width of the figure this way, if it matters to you, that is

And of course you can generate a figure with inserted block too!
```python, Ayano * "That's a third way to do the same thing!" ~ scripts/generate_sine.py
@fig: src = "other_sine_graph.png", ident = "yet_another_sine_graph", caption = "The same sine. Again.", width = 0.2
```

Note, that in the last example, script actually saves sine into a folder near input markdown fine, and yet `Kyomato` will be able to find it (if configured properly, that is)
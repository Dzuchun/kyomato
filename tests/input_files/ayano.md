This is a test of ayano-generated stuff.

This is supposed to be a generated figure:
```python, Ayano
@fig: src="circle.png", ident="circle", caption="Some caption for a line drawing. This is parsed as a *regular document text*, so you may [@eq:line] and $f(x) = A \cdot x^{1} + B \cdot x^{0}$ here, for example"
```

Here's a table with specified width:
```python, Ayano
width = 0.2
@fig: src = "graph.png", width = width
```

This is expected to be a generated table:
```python, Ayano
data = [
		["x", "y"],
		[1.0, 3.0],
		[3.0, 4.0],
		[-1.0, -5743234.0],
]
@gen_table: lambda r,c: data[r][c]; rows=4, columns=2, caption="Caption for a table 1, *because* why not", ident="table1"
```
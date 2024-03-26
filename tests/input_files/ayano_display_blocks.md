---
code-section-title: Код
---

# Display blocks

This feature allows you to insert certain `Ayano` blocks to the end of the output document, to document your exact actions for displayed results.

Here's an example:

```python, Ayano *
# You should be able to see this comment in the output doc
42
```

Display blocks can have descriptions too! Here's one:

```python, Ayano * "me, fooling around"
# This code block has description
# Might as well add some python syntax to check out minted's syntax highlighting:
for i in range(100):
    s = str(i)
    s = s[::-1]
@dev: -2.034e4, 145.64
```
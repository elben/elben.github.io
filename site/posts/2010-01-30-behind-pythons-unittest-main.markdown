<!--
{
"layout": "post",
"postTitle": "Behind Python's unittest.main()",
"date": "2010-01-30 09:08:00"
"categories": ["blog"],
"tags": ["python"]
}
-->

Is Python magical? Consider the trivial unit test below:

```python
import unittest

class TestSomething(unittest.TestCase):
  def test1(self):
    self.assert_(True)

if __name__ == '__main__':
  unittest.main()
```

Unless you're a Python guru, I'm sure you've wondered how `unittest.main()`
found and ran `TestSomething` and `TestSomething.test1`. If you look at the
`unittest` source code, you find a nice trick:

```python
# unittest.py

class TestProgram:
  def __init__(...):
    ...
    self.runTests()

main = TestProgram
```

Ah, `unittest.main` isn't a function, it's a class! So `unittest.main()` creates
a new `TestProgram` object, whose initializer then goes ahead and runs the unit
tests for you. But how does `TestProgram` know to load the `TestSomething` class
and run `TestSomething.test1`?

Most of this work is done by the `unittest.TestLoader` class. `TestLoader` will look
at the current module (i.e. your current file), examine each class in it, and
load classes that inherit `unittest.TestCase`. Then, `TestLoader` will look inside
these classes for methods that have `test` as their prefix.

After this, `TestProgram` will know what tests it needs to run.

Of course, `unittest` is more complicated than this, but this describes the main
flow of `unittest`. I suggest that you download the Python source code and read
`unittest.py` yourself. It's a lot of fun, really. Plus, reading source code is
a great way to improve and enrich your coding, and Python's source is one of the
best treasures around.

### Archived comments; imported from Posterous

3530 views and 1 response

Oct 15 2011, 1:52 PM

Steffen Hoffmann responded:

Thanks for sharing this - exactly what I was looking for (before looking into the code itself). As soon as one has got a clue about Python in general, I agree strongly that the code, commented or not, is a valuable resource.

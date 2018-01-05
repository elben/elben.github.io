<!-- PREAMBLE
{
"postTitle": "Behind Python's unittest.main()",
"date": "2010-01-30",
"tags": ["python", "second element", "third"]
}
-->

${postTitle}

${date}

My own URL is ${this.url} which links to <a href="${this.url}">here</a>

${tags}

${unknown variable}

Is Python magical? Consider the trivial unit test below:

```python
import unittest

class TestSomething(unittest.TestCase):
  def test1(self):
    self.assert_(True)
```

Unless you're a Python *guru*, I'm sure you've wondered how `unittest.main()`
found and ran `TestSomething` and `TestSomething.test1`.

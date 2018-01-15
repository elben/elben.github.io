<!--PREAMBLE
{
"postTitle": "Singleton Pattern in Python",
"date": "2010-04-16",
"tags": ["python"]
}
-->

Sometimes, you find that you only want one instance of a class to be created.
Ever. This way of programming is often referred to as the singleton pattern.

In languages like Java, the singleton pattern is often implemented by using a
class static variable as the single object:

```java
class OneOnly {
  private static final OneOnly the_one = new OneOnly();

  private OneOnly() { ... }

  public static get() {
    return the_one;
  }

  ...
}
```

In the snippet above, the default constructor is set as private so to prevent
the user from instantiating a `OneOnly` object. Instead, the user must use
`OneOnly.get()` to get access to the one and only `OneOnly` instance.

This way of executing the singleton pattern is impossible in Python due to the
fact that Python does not have an equivalent to Java’s `final` keyword.

A well-defined singleton pattern, however, is possible in Python. The Wikipedia
entry on the singleton pattern [1] presents a clever way of doing it in Python.
I reproduced Wikipedia’s Python example below and added some more code to
explain what’s going on under the hood.

```{.python .numberLines}
def singleton(cls):
    instances = {}
    def getinstance():
        if cls not in instances:
            instances[cls] = cls()
        return instances[cls]
    return getinstance

class Counter:
    def __init__(self):
        self.count = 0
    def inc(self):
        self.count += 1

print type(Counter)    # <type 'classobj'>
Counter = singleton(Counter)
print type(Counter)    # <type 'function'>
```

In the snippet above, `singleton()` is a function that takes a class and returns a
function. I created a simple `Counter` class. Line 15 shows that `Counter` is a
class. I then set `Counter` to the return value of `singleton(Counter)`. `Counter` is
now a function, as line 17 shows. This is awesome. Now, whenever the user tries
to “instantiate” a `Counter`, the user is really just calling a function, namely
`getinstance()`. And `getinstance()` is built so that it will only build one
instance of `Counter,` on line 5. If an instance of `Counter` already exists,
`getinstance()` will simply return that instance.

## The Power of Closures

But why does `getinstance()` work? The most amazing part is that the `getinstance()`
that is set to `Counter` is a specific `getinstance()` that “closes over” the
variables `cls` and `instances`. That is, when we define `getinstance()` inside of
singleton(), `getinstance()` still has access to `cls` and `instances,` even after we
exit singleton(). Thus, we don’t have to create a new `instances` dictionary every
time. This only happens once, at line 2. We get to line 2 through the call at
line 16, which only happens once (for `Counter`).

Why does `instances` have to be a dictionary? Well, it doesn’t. The only thing
that matters is that we don’t set the `instances` reference to a new object
somewhere inside `getinstance()`. If we do, we lose the closure over the
`instances` defined on line 2. Thus, `instances` should be a mutable object such
as a dictionary or a list.

Let us test our singleton object:

```{.python}
>>> print Counter() is Counter()
True
```

As you can see, every call to `Counter()` returns the same `Counter` instance.
Great! We now have a very powerful construct to build classes that adhere to the
singleton pattern. If we want to create another one-instance-only class named
`Puppeteer`, we simply set `Puppeteer = singleton(Puppeteer)` after the class
definition. `Puppeteer` will then get its own `getinstance()` function and its own
`instances` dictionary, separate from `Counter`’s.

## Decorating

Python has something called decorators. A decorator is just syntactic sugar [2]
for what we did when we wrote `Counter = singleton(Counter)`, which can look a bit
ugly. We can prettify this by adding a `@singleton` before our `Counter` class
definition:

```python
@singleton
class Counter:
    def __init__(self):
        self.count = 0
    def inc(self):
        self.count += 1

print type(Counter) # <type 'function'>
```

If you haven’t seen decorators before, you should now realize that a decorator
`@singleton` will simply set the class name (in our case, `Counter`) to the
value returned by `singleton(Counter)`.

## Conclusion

So yes, Python’s open nature is sometimes unwanted. But with closures (e.g.
`getinstance()` closing over `cls` and `instances`) and first-class functions
and classes (i.e. passing around classes and functions as ordinary objects),
Python allows us to device beautiful, clever constructs.

## References

[1] http://en.wikipedia.org/wiki/Singleton_pattern

[2] Python Decorators Don’t Have to be (that) Scary. http://www.siafoo.net/article/68

### Archived comments; imported from Posterous

4492 views and 3 responses

Apr 16 2010, 10:53 PM

Paddy3118 responded:

The pythonic way of solving the problem that the singleton addresses in other
languages is to use the Borg pattern:
http://code.activestate.com/recipes/66531-singleton-we-dont-need-no-stinkin-s...

Apr 17 2010, 1:57 PM

Elben Shira responded:

Paddy, great advice. Sometimes, though, I think we want to make sure that
Counter() is Counter() (the Borg pattern article has comments that addresses
this). Is there a reason as to why enforcing the singleton pattern is
unpythonic?

Apr 18 2010, 6:58 AM

Paddy3118 responded:

The argument is that what most people want is the effect, and that the Borg
pattern works in most situations and is much easier to code and understand,
hence more "Pythonic".

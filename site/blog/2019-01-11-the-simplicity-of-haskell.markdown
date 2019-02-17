<!--PREAMBLE
postTitle: "The Simplicity of Haskell"
date: 2019-01-11
tags:
  - programming-languages
  - haskell
-->

What makes Haskell difficult to learn? The building blocks of Haskell are the same as any other language: primitive values, containers, and functions. When we list them out like that, it’s quite simple. But Haskell combines these elements—specifically, functions—in unfamiliar ways.

Many of us are now unfazed when we see functions passed around like values:

```
function f(g, n) {
  return g(n) + g(n+1)
}

function triple(x) {
  return x * 3
}

f(triple, 5)
```

But when we first saw something like this, we probably traced these “complicated” functions into their fundamental values. “So `f` is a function that takes `g`, which is any function that the user has chosen. We call that user-given function twice, passing in the user-given `n` and then `n+1`. So when we call `f(triple, 5)`, what’s happening is that we call `triple` twice. We first feed `5` to `triple`, and then `6`.” We were playing interpreter.

After a while, we got comfortable with treating functions as values and we didn’t have to play out the interpreter as often.

Likewise, Haskell composes functions in ways we're not familiar with. Things can feel foreign and confusing, and we may wonder why things have to be so complicated. The truth is that we are *forced* to do things this way because of the restrictions Haskell has placed on itself—it is the byproduct of its purity. Whereas other languages "cheat" and use mutable variables or impure functions when things get complicated, Haskell does not.

So if Haskell is just function composition, what we need to do is decompose them, over and over. Manually break out dense code into its “raw” form. Figure out how your values are manipulated. It’s a slow process, and you’ll get mixed up and frustrated many times. It's OK—that's normal. But the better we are at translating between the different layers of abstraction, the better our understanding. The practice of going up-and-down between these different layers is what produces deep, intuitive knowledge.

This is why I don’t really buy into the idea that “Haskell programmers think in terms of types.” Types describe the underlying values or behavior. But when you don’t understand what the values are doing to begin with, the focus on types is misplaced. So when the types confuse you, ignore the types and start looking at the values instead.

## Example: Dependency Injection

Let's practice this together by looking at common problem: dependency injection.

Let's say we have these two functions:

```haskell
foo :: Int -> Int
foo n = n + 10

bar :: Int -> Int
bar n = n * 2
```

That we combine and use like this:

```
>>> bar (foo 2)
24
```

But what if we want to make the hard-coded values `10` and `2` configurable? Say we have a `Config` data structure that contains configuration loaded from a YAML file.

```
data Config = Config {
    getIncr :: Int
  , getMult :: Int
}

config :: Config
config = Config { getIncr = 10, getMult = 2 }
```

We could re-write `foo` and `bar` to accept a `Config`.

```haskell
foo :: Int -> (Config -> Int)
foo n = \c -> n + getIncr c

bar :: Int -> (Config -> Int)
bar n = \c -> n * getMult c
```


```
>>> (bar (foo 2 config) config)
24
```

Note that we could have placed the `Config` argument either first or second in the argument list, but I've chosen to place it in the second position (the last argument). Placing `Config` as the last argument will allow us to compose these functions in a better way, which we will see below.

We can write a function `(>>==)` (let's pronounce it "pipe") that combines two `Config`-awaiting functions together.

```haskell
(>>==) :: (Config -> a)
       -> (a -> (Config -> b))
       -> (Config -> b)
(>>==) g f = \c -> f (g c) c
```

And now we can combine the functions like this:

```
>>> (foo 2 >>== bar) config
24
```

Compare the above to what we had previously, `(bar (foo 2 config) config)`. With the `>>==` version, we only had to pass in `config` once, at the end. Check out its type:

```haskell
>>> :type (foo 2 >>== bar)
(foo 2 >>== bar) :: Config -> Int
```

A longer pipeline also works:

```haskell
>>> :type (foo 2 >>== bar >>== foo >>== bar)
(foo 2 >>== bar >>== foo >>== bar) :: Config -> Int

>>> (foo 2 >>== bar >>== foo >>== bar) config
68
```

Using `>>==` makes sense visually, but how does it work exactly? How was `>>==` able to combine different functions that all expected a `Config` in such a way that we only needed to pass `Config` once, at the end?

Let's play interpreter and break `foo 2 >>== bar` down. First, let's substitute `>>==` with its definition:

```haskell
-- Starting with
foo 2 >>== bar

-- Where
g >>== f = \c -> f (g c) c

-- Becomes
\c -> bar ((foo 2) c) c
```

You can see already that `>>==` sets up the left and right functions to accept the passed-in `Config`, and that the left side gets evaluated before the right.

What about `foo 2 >>== bar >>== foo >>== bar`?

```
-- Starting with
foo 2 >>== bar >>== foo >>== bar

-- Where
g >>== f = \c -> f (g c) c

-- Replacing the first >>== with its definition.
(\c -> bar ((foo 2) c) c) >>== foo >>== bar
```

That looks a bit confusing, but let's continue and replace the other two `>>==`. Note that we call the *new* `Config` argument `c2` because it's a different variable than the original `c`.

```
-- Starting with
(\c -> bar ((foo 2) c) c) >>== foo >>== bar

-- Replacing the next >>== with its definition.
(\c2 -> foo ((\c -> bar ((foo 2) c) c') c2) c2) >>= bar
```

And the last `>>==`:

```
-- Starting with
(\c2 -> foo ((\c -> bar ((foo 2) c) c') c2) c2) >>= bar

-- Replacing >>== with its definition.
(\c3 -> bar ((\c2 -> foo ((\c -> bar ((foo 2) c) c) c2) c2) c3) c3)
```

We can clean this up a bit by doing some function application:

```haskell
-- Starting with
(\c3 -> bar ((\c2 -> foo ((\c -> bar ((foo 2) c) c) c2) c2) c3) c3)

-- Substituting c, c2 and c3.
(\c3 -> bar (foo (bar ((foo 2) c3) c3) c3) c3)
```


Piping a bunch of `Config`-awaiting functions together with `>>==` resulted in a giant, nested function that is *also* waiting for a `Config`. Once you give it a `Config`, this mess will "unwind" and run inside-out, starting with `(foo 2) c3` (the inner-most expression). So once `(foo 2) c3` is evaluated to a value, that value **and the same `Config`** will be passed into the next method in line, `bar`.

`foo 2 >>== bar >>== foo >>== bar` may look like there's magic happening, but it's quite simple when you discover what's underneath.

So when things get confusing, remember that it's just primitive values, boxes of stuff, and functions at the end of the day. Start digging, and perhaps you'll find that things aren't as complicated as they first seem.

## Appendix 1: The Reader Monad

What we did above is a well-known pattern, known as `Reader`. The `Reader` data type is just a box with a function inside, which is exactly what we were doing.

```haskell
newtype Reader r a = Reader (r -> a)
```

In our example, the type variable `r` was our `Config`, and the type variable `a` was an `Int`.

`Reader` comes with a helper method to unbox the function:

```haskell
runReader :: Reader r a -> (r -> a)
runReader (Reader f) = f
```

`Reader` also defines an instance of `Monad`:

```haskell
instance Monad (Reader r) where
  -- return a :: a -> Reader r a
  return a = Reader (\r -> a)

  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  m >>= f = Reader (\r -> runReader (f ((runReader m) r)) r)
```

And a helper method `ask` to get the dependency-injected value (`r`):

```haskell
ask :: Reader r r
ask = Reader $ \r -> r
```

The `Reader` version of `foo` and `bar` looks like:

```
fooR :: Int -> Reader Config Int
fooR n = ask >>= (\c -> n + return (getIncr c))

barR :: Int -> Reader Config Int
barR n = ask >>= (\c -> n * return (getMult c))
```

Using them:

```
>>> runReader (fooR 2 >>= barR) config
24
```

## Appendix 2: Unboxing `fooR`

Let's try to unbox `fooR` to its primitive values. Taking `fooR`, let's put `(>>=)` in front:

```
fooRa n = (>>=) ask (\c -> return (n + getIncr c))
```

Substituting `(>>=) m f` with its definition `Reader $ \r -> runReader (f ((runReader m) r)) r`:

```haskell
fooRb n = Reader $ \r -> runReader ((\c -> return (n + getIncr c)) ((runReader ask) r)) r
```

Substituting `ask` with `(Reader $ \r2 -> r2)`:

```haskell
fooRc n = Reader $ \r -> runReader ((\c -> return (n + getIncr c)) ((runReader (Reader $ \r2 -> r2)) r)) r
```

All of `((runReader (Reader $ \r2 -> r2)) r)` can be simplified to just `r`:

```haskell
fooRd n = Reader $ \r -> runReader ((\c -> return (n + getIncr c)) r) r
```

Now let's substitute `return (n + getIncr c)` with `(Reader (\r3 -> (n + getIncr c)))`:

```haskell
fooRe n = Reader $ \r -> runReader ((\c -> (Reader (\r3 -> (n + getIncr c)))) r) r
```

Simplifying `((\c -> (Reader (\r3 -> (n + getIncr c)))) r)` to `(Reader (\r3 -> (n + getIncr r)))`:

```haskell
fooRf n = Reader $ \r -> runReader (Reader (\r3 -> (n + getIncr r))) r
```

Simplifying `runReader (Reader (\r3 -> (n + getIncr r))) r` to `(n + getIncr r)`:

```haskell
fooRg n = Reader $ \r -> (n + getIncr r)
```

So at the end of the day, when we defined `fooR`, we got a bunch of functions that can be reduced down to the above,
in `fooRg`.

```
>>> runReader (fooRg 2) config
12
```

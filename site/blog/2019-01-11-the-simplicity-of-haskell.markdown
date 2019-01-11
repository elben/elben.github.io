<!--PREAMBLE
postTitle: "The Simplicity of Haskell"
date: 2019-01-11
tags:
  - programming-languages
  - haskell
-->

What makes Haskell difficult to learn? The building blocks of Haskell are the same as any other language: primitive values, containers, and functions. When we list them out like that, it’s quite simple. But Haskell combines these elements—specifically, functions—at a higher level of abstraction than we are used.

Remember when you first realized that functions can be passed around like values? A lot of us are now unfazed when we see something like this:

```
function f(g, n) {
  return g(n) + g(n+1)
}

function triple(x) {
  return x * 3
}

f(triple, 5)
```

But when we first saw something like this, we probably traced these “complicated” functions into its fundamental values. “So `f` takes `g`, which is any function that the user has chosen. We call that user-given function twice, passing in the user-given `n` and then `n+1`. So when we call `f(triple, 5)`, what’s happening is that we call `triple` twice. We first feed `5` to `triple`, and then `6`.” We were playing interpreter.

After a while, we got comfortable with treating functions as values and we didn’t have to play out the interpreter as often.

Again, when we're writing Haskell we're just manipulating a bunch of primitive values, container-looking things and functions using, well, functions. That’s it. We could describe Haskell as an attempt at figuring out how we would write programs without side-effects (that is, with immutable data structures and pure functions). All of the complicated-sounding stuff is the byproduct of this restriction.

So if Haskell is just function composition, what we need to do is decompose them, over and over. Manually break out dense code into its “raw” form, and figure out how your values are manipulated. It’s a slow process, and you’ll get mixed up many times. But the better we are at translating between the different layers of abstraction, the better we understand. The practice of going up-and-down between these different layers is what produces deep, intuitive knowledge.

This is why I don’t really buy into the idea that “Haskell programmers think in terms of types.” Types describe the underlying values or behavior. But when you don’t understand what the values are doing to begin with, the focus on types is misplaced. So when the types confuse you, ignore the types and start looking at the values instead.


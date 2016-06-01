---
title: Smarter Help: Our Programming Tools
categories: blog
---

In [The End of Dynamic Languages](http://elbenshira.com/blog/the-end-of-dynamic-languages/), I argued that one of the gains of statically-typed languages is superior tooling.

Can the same be said for dynamic languages? It seems that the correct answer here is: it can improve.

I’ve been using [rubocop](https://github.com/bbatsov/rubocop) and HoundCI. They are the wrong tools. For example, here are some of the errors Hound threw at me for a recent PR:

<img src="/images/smarter-help/hound-useless-0.png" title="Hound">

<img src="/images/smarter-help/hound-useless-1.png" title="Hound">

<img src="/images/smarter-help/hound-useless-2.png" title="Hound">

<img src="/images/smarter-help/hound-useless-3.png" title="Hound">

<img src="/images/smarter-help/hound-useless-4.png" title="Hound">

None of these are useful! They contribute nothing whatsoever to the quality of the code. It is candy for stylistic pedants—nothing more.

Well you can just disable those rules, you argue. Yes, you can, just muck with this [1000+ lines of configuration](https://github.com/bbatsov/rubocop/blob/master/config/default.yml) so that you can create strings with `'`s instead of `"`s.

This is a false sense of quality and accomplishment.

The fact that it checks in your PR worsens the problem. We need systems to help us at the code *writing* stage, not before merging into master.

# What we need

The closer we can get our tools to work *with* us instead of *against* us, the better we will be.

Below is an example how [hlint](https://hackage.haskell.org/package/hlint) helps me refactor as I write code. First, I write this function that adds up the values of the list:

<img src="/images/smarter-help/haskell-1.png" title="Haskell">

But `hlint` finds a reduction:

<img src="/images/smarter-help/haskell-2.png" title="Haskell">

And another:

<img src="/images/smarter-help/haskell-3.png" title="Haskell">

Turns out `sum` already exists:

<img src="/images/smarter-help/haskell-4.png" title="Haskell">

What we need are tools that, more and more, understand intent.

And because Haskell is statically typed, you can search for things via the type. For example, if I need a function that flattens `[[a]]` (a list of lists of `a`s) into `[a]`, I can search for that on Hoogle ([`[[a]] -> [a]`](https://www.haskell.org/hoogle/?hoogle=%5B%5Ba%5D%5D%20-%3E%20%5Ba%5D)) and find the function [`concat :: [[a]] -> [a]`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html#v:concat).

These tools today mirror the first batch of AI research: it’s mostly (if not all) symbolic reasoning.

Of course some work is being done outside of symbolic reasoning.  We can use more than just types. [Kite](https://kite.com/), for example, is interesting because clearly it’s parsing the code, but it also leverages and organizes the ocean of programming knowledge available online.

It’s clear that more we assist our tools, the more our tools can assist us. Clojure’s [`spec`](https://clojure.org/about/spec) can be a huge win because it adds symbolic metadata that a program can read and manipulate. Property-based testing (ala [QuickCheck](https://www.youtube.com/watch?v=zi0rHwfiX1Q)) also works together with types to convey information that types cannot; namely, the invariants (or property) of our functions.

But I don’t know how far you can take this without types. If you give me an untyped AST, there is not much I can do, other than perhaps memorize or regex for some known modules and their functions (aka auto-complete). But it won’t be able to do what `hlint` can do, because it can’t manipulate that untyped AST in a sure way.

Underlying all of this is a realization that **the programmer and the tools affect each other**. If you experience, for example, the power of property-based testing, you will begin to write your programs in a property-testable way. If your type system guides you to writing correct programs, then you will accept its assistant. If they complain about you using `'` instead of `"` for strings, then, well, you should uninstall that tool.

The next generation of tools should combine symbolic, statistical and data mining techniques. We should take as much help as we can, from types and specifications and properties and data mining. The point is, we need help. And we need smarter help. 

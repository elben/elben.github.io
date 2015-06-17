---
layout: post
title:  The Best Introduction to Haskell
categories: blog
tags: haskell
---

When I mention that I’m learning Haskell, many people reference Learn You a Haskell. It’s a fine book, but I found it too slow for an experienced programmer. And the cute cartoons and verbose style got in the way.

Instead, I recommend UPenn’s [CS 194: Introduction to Haskell](http://www.seas.upenn.edu/~cis194/spring13/) course. The materials are available online and were created by Brent Yorgey of Typeclassopedia fame.

I’m only six weeks in, but I’ve found it much more enjoyable than any other introduction to Haskell attempt. Because if you’re like me, you learn best by doing, not by reading books. In that regard, the course provides assignments, which make up the bulk of the course. Granted, they are tailored to undergraduate CS students—the Tower of Hanoi and the Fibonacci sequence make an appearance—but they are to-the-point, useful and (most importantly) fun.

## So far

Perhaps this is the mirage of a neophyte, but Haskell *feels* like a scripting language. I think this is due to its powerful type inferencer and `ghci`, the interactive environment.

With Haskell’s type inferencer, I can convert thought to code quickly; the type checker doesn’t get in the way. This is unlike Scala, where I often feel more frustrated than helped by types. Library design, of course, also plays an important part. But so far, **Haskell’s type system is like pair programming with a precise and careful expert.**

## Tools

[GhcMod](https://hackage.haskell.org/package/ghc-mod) is essential. It turns Vim/Emacs/Sublime into IntelliJ, without the IntelliJ. With GhcMod you can instantly see the type of any statement.

For example, in the image below, I check the type of `v`, which is a String:

<div class="some-padding"><img src="{{ site.url }}/images/the-best-introduction-to-haskell/type-check-0.png"/></div>

Then I check the type of `mapping`, which is a map of Strings to Integers:

<div class="some-padding"><img src="{{ site.url }}/images/the-best-introduction-to-haskell/type-check-1.png"/></div>

I then view the type of the `lookup` function, which reveals the argument order:

<div class="some-padding"><img src="{{ site.url }}/images/the-best-introduction-to-haskell/type-check-2.png"/></div>

And finally, I can verify the whole expression:

<div class="some-padding"><img src="{{ site.url }}/images/the-best-introduction-to-haskell/type-check-3.png"/></div>

Even though the code is terse, there’s a lot of metadata and context to help you out. They’re called types!
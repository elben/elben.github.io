<!--PREAMBLE
postTitle: "A programming language for the laboratory"
date: 2018-10-30
tags:
  - programming-languages
-->

I may be a [proponent of static typing](http://elbenshira.com/blog/the-end-of-dynamic-languages/), but I haven’t found good research that I trust or would cite as evidence of my intuition that static typing helps you write programs with less bugs, greater re-use and easier to change. And while debating programming language features and designs is fun, most of our arguments are backed with little data; it's mostly personal experiences or intuitions.

The hesitation I have with existing studies<sup>[1](#footnote1)</sup>
is that they try to compare different languages against each other. They get a bunch of students, make them implement some stuff in a variety of languages, then count the number of bugs. But there are way too many loose variables, like their familiarity of one language over another. And is it really fair to compare Python to Scala? Some languages are better than others at solving a particular problem.

## A controlled environment

I'm not a researcher or scientist, but I would like to see a programming language designed for the laboratory, so that we can compare various language features against one another in a controlled manner.

Such a language should support multiple paradigms, but with the ability to toggle features on-and-off. We should be able to turn off the static type checker, or toggle between mutable and immutable data structures, or enable object-oriented features, or sum types, and so on.

The development environment should also be controlled. Imagine an editor and REPL environment that we can track the subject's actions. Every time the subject clicks `Run`, we'd be able to take a snapshot of the code, run tests in the background to check for correctness, do whatever.

The programming tasks should aim to replicate real-world tasks like reading wonky data from the database or refactoring some crappy code. You can even design different versions of a library. Provide one JSON library in a pure functional style and another that is more imperative.

Such an environment would allow us to control one variable at a time, and may help us see precisely how certain language features help programmers write less bugs, or ease refactoring. It could help see what features actually hurt programmers.

Lively debate around programming language features will continue until the end of time. But it sure would be more interesting if we had studies like this. And definitely more fun.

<a name="footnote1">1</a>: [http://danluu.com/empirical-pl/](http://danluu.com/empirical-pl/)



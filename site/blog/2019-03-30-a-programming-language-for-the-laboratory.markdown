<!--PREAMBLE
postTitle: "A Programming Language for the Laboratory"
date: 2019-03-30
tags:
  - programming-languages
-->

In the perhaps arduous debate of static versus dynamic typing, I fall quite easily in the [camp of static typing](http://elbenshira.com/blog/the-end-of-dynamic-languages/). For what it’s worth, there is [some clinical evidence](http://danluu.com/empirical-pl/) of its benefits. A [recent study](https://pleiad.cl/papers/2012/kleinschmagerAl-icpc2012.pdf) showed that types help developers learn new classes and fix type errors, but showed no benefit against semantic errors. In [another experiment](https://ieeexplore.ieee.org/document/7503719/), the authors played Devil’s advocate and polled for scenarios in which they thought dynamic languages would be superior to typed languages in terms of development time. They found that a typed language helped two of the four tasks that were supposedly better in a dynamic context, suggesting that types do help.

But I had several hesitations with these experiments. The number of subjects were usually low, and the researchers used the same subject to accomplish both the typed and untyped tasks, potentially injecting bias. And the tasks assigned were often trivial and not indicative of the complexity found in real software.

When I brought these objections up to Stefan Hanenberg, a co-author on both papers, he pointed out that many CS departments (and thus their students) are not well-versed in research methods. What we may perceive as a low sample size or a naive experiment (i.e. not realistic enough) may be either unnecessary or even harmful to the experiment.

I admit that I don’t have the background to judge these things soundly. But one thing I do wish for is more control of the programming language used in the experiment. The two papers I referenced, for example, pit Java against a subset of Groovy (a dynamic language) to make it as close to Java as possible. Other papers try to compare wildly different languages against each other.

But we know that type systems vary greatly across languages. Haskell programmers use algebraic data types and type classes, but Java programmers use interfaces and inheritance. And other factors influence productivity and maintainability of software outside of the type system. Clojure programmers write immutable functions because immutable collections are the default, but Ruby programmers don’t have the same encouragement. Java programmers have access to advanced IDEs, and JavaScript programmers can easily open a REPL and debugger in production. How do we even begin to compare, in a controlled setting, the benefits of one feature over another? Or figure out *why* certain features help, while others hurt?

## A controlled environment

I'm not a researcher or scientist, but I would like to see a programming language designed for the laboratory. Such a language should support multiple paradigms, but with the ability to toggle features on-and-off. We should be able to turn off the static type checker, or toggle between mutable and immutable data structures, or enable object-oriented features or algebraic data types.

The development environment should also be controlled. Imagine an editor and REPL environment that can track the subject's actions. We can record their code over time, run automated tests in the background. Heck, we could even measure blood pressure over time and see that thing spike when we force them to hunt down a null pointer exception in production.

Such an environment would allow us to control one variable at a time, and may help us see precisely how certain language features help programmers write less bugs, or ease refactoring. It could help see what features actually hurt programmers. Maybe we could even begin to understand *why* certain features affect us a certain way. Imagine if we had this kind of clinical attitude in 1965, when Tony Hoare's itchy fingers implemented the null reference. What if null was tested and *rejected* in the laboratory? How many programmers would we have saved from existential dread? How many angry `git-blame`s would we have prevented?

Granted, this would be a huge undertaking. Isn't exactly the most-exciting or praise-generating kind of research, and it probably wouldn’t have prevented the release of the `null` virus. But we are in a programming language renaissance, and I think research like this is something our community longs for, whether we know it or not. Sure, debate around programming languages will continue until the end of time. But it sure would be more interesting if we had studies like this. And definitely more fun.

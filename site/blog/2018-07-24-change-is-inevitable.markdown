<!--PREAMBLE
postTitle: "Change is Inevitable: Designing Software in a Chaotic World"
date: 2018-07-24
tags:
  - software-design
-->

When I was a young programmer, I believed that good code required a well-thought-out architectural construct. Measure twice, cut once—or something like that—was the mantra. But during my internship at Big Java Shop I witnessed a codebase burn to the ground from an architectural mad man. If only they had thought _correctly_ about their architecture, I thought, things would have gone fine.

But we know how that goes.

To pick up our tools of abstraction and find patterns is an impulse rooted in our psyche. It is our need for order, our desire for control. Extracting commonalities to abstract classes and interfaces feels good. We want to grab chaos and wrangle it into submission. But why do the results often fail us?

## Bad patterns

One reason is that we choose bad patterns. How can you tell if a pattern is good or bad? There’s a lot to be said about experience, but here’s a couple of good heuristics:

- The ones you made up yourself are generally bad.
- The ones found across languages and libraries are generally good.

This is because the patterns you think of is often too specific. It may serve you well today, but who knows about tomorrow. You may come back to it months from now and realize that you overthought half of it and was too careless in the other half. But an abstraction or generalization that spans libraries, languages and communities is an indicator of its value and reliability.

The monad pattern, for example, is a pattern of computation found everywhere. It is a beautiful shield of safety that coos your data type into a lavender-scented blanket of comfort. And by giving that pattern a name we are empowered to write code that works. It is *because* of its limitations that we are able to write something good. You’ll find this pattern in languages like Scala, Rust, and Haskell. Heck even Java 8’s `Optional` is monadic-y, and so are most futures libraries because of the inherit need of callback function composition.

Now let’s compare the monad with, say, all those interfaces and abstract classes you created for your first big-boy Java project. You know what kind of mess you made for yourself. This is because they were careless abstractions made without much thought. They didn’t empower you in any way, didn’t simplify anything, didn’t prevent bugs. You thought you were doing something useful, something productive. But the end result, judged by the harsh realities of time, proved otherwise.

So be careful about what patterns you employ. Learn and adopt good patterns that span languages and libraries, not the ones you made up.

## Things change

You can choose good abstractions, but the reality is that things change. What is suitable today won’t be suitable tomorrow. We’re all making predictions about the future.

This doesn’t mean that we abandon thinking about the future. Instead, **we need to account for inevitable change. Choose the simplest thing that is easiest to change.**

This was a breakthrough in my thinking. Don’t abstract the class. Don’t pull out the pattern. Don’t code for the worst-case scenario, or for some potential feature a product manager may want six months from now. At least not yet. Think: what do I gain from doing it, and what do I lose? Maybe you’re sacrificing directness, simplicity, debuggability. Maybe you’re sacrificing ease of change. Those are precious things. Is it worth sacrificing for your clever pattern?

**The best thing you can do is to do nothing.** Wait until you actually need to implement that feature, until you find yourself doing the same thing three times. Then take out the scalpel. The end result will be simpler and more powerful at the same time.

> “Two children make a happy home. But three make good abstractions.” — Ancient Lithuanians proverb

These are simple ideas, but I’ve seen cities burn down due to premature architecting, and I’ve seen projects reap the benefits of simplicity. Change is inevitable, but we can plan for the inevitable. We can design for chaos.

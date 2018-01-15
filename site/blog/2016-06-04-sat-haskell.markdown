<!--PREAMBLE
{
"postTitle": "SAT in Haskell",
"date": "2016-06-04",
"draft": true,
"tags": ["programming-languages"]
}
-->

*Inspired by Felienne Hermans’ [Quarto](https://github.com/Felienne/Quarto) talk at LambdaConf 2016.*

Every gathering of people have, for better or worse, sets of cliques. Even in my company, I’m sure there are cliques that I don’t even know exists.

Today I will show you how we can uncover these cliques by putting my own company, Spredfast, under the microscope.

How will we do this?

## Cliques

First, we need to define what a clique is.

In an undirected graph, a clique is subgraph where each node is connected to every other node. For example, in this image below, nodes 1, 2 and 5 are part of a clique of size 3.

https://upload.wikimedia.org/wikipedia/commons/thumb/8/86/6n-graf-clique.svg/220px-6n-graf-clique.svg.png

What I’m going to do is find the largest clique in my company, based of who my co-workers follow on Twitter.

We can get everyone’s Twitter handle off our Pingboard directory. From there, I’ll have to hit the Twitter API to grab everyone’s follower/followee lists, which will make the graph.

Then comes the fun part: writing an algorithm to discover cliques.

The interesting part is we’re going to use an SAT solver to do this.

## SAT what?

In our day-to-day programming, we’re used to seeing logical propositions like `user.valid? && so_on`. This is part of the problem space known as the satisfiability problem, or SAT for short. The problem is this: given a boolean proposition like `user.valid? && so_on` or, in fancy notation, %%p \\lor q%% (read this as "p or q"), can we choose a set of values for each variable such that the proposition is true?

In our example, the answer is yes we can, with three possible solutions (%%p%% is true, %%q%% is true, or %%p%% and %%q%% are true).

Now imagine a very complicated proposition with thousands of variables and clauses, like (notation-wise, %%\\lor%% means "or", %%\\land%% means "and", %%\\lnot%% means "not"):

$$
p \lor (q \land \lnot a) \land (b \lor \lnot (c \land d \land \lnot q)) \land \ldots
$$

It would take you a while to figure out what values to set each variable such that the proposition is true.

In fact, this is an NP-complete problem!

Even though it’s NP-hard, there are lots of SAT solvers out there because solving these equations prove to be very useful. In Felienne’s talk, she an SAT solver to check whether or not the game Quarto can end in a tie (it can), which would have taken much more time if it was brute-forced.

## Converting the problem to SAT

The amazing part about NP-complete problems like SAT is that any other NP-hard problem can be *converted* into any NP-complete problem.

Let me illustrate.

Say you’re in China, and you don’t speak the langauge. And you need a bathroom, fast, and for some reason they’re not understanding your frantic act of charades. But you do have a pocket electronic dictionary, where you can type English words and it spits out the Chinese characters. And they can type in Chinese, and it spits out English.

Well, now all you have to do is translate your problem from English to Chinese using this black box. They answer in Chinese, and the black box translates it back to English.

The same is true for NP-complete problems. We have have a SAT solver (and we do), then if we can somehow convert problems into the SAT problem, then we can find the answer.

## Cliques


<script type="text/x-mathjax-config">
MathJax.Hub.Config({
  tex2jax: {
    inlineMath: [['$','$'], ['\\(','\\)'], ['%%', '%%']],
    displayMath: [['$$','$$'], ["\\[","\\]"], ['%%%', '%%%']]
  }
});
</script>
<script type="text/javascript" async src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-MML-AM_CHTML">
</script>

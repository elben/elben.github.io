<!--PREAMBLE
{
"postTitle": "The End of Dynamic Languages",
"date": "2015-11-22",
"tags": ["programming-languages", "recommended"]
}
-->

For the past several months, I’ve been programming almost exclusively in Scala (for work) and Haskell (for pleasure). But this week, I was also saturated in Ruby (for work) and Clojure (for pleasure).

Ruby frustrated me at once. Working in Ruby is fine if you’re just *adding* a feature on top of the pile of features. All you have to do is add some unit tests, make sure the old ones pass, and run away. But anything else is impossibly difficult.

But my brand-new, clean-slate weekend project in Clojure… Ah, Clojure! A breath of fresh air! A land green with libraries rolling in composible functions, immutable data structures, and kind people. How beautiful is your syntax, and how wise are your sensibilities! Your middleware is just a function that takes a map and returns a map. And so is your SQL generator and DB migrator and HTML parser and URL router—a psychedelic circle of maps passed to-and-fro to the beat of the CPU, tick-tocking like a well-tuned Swiss watch.

To be back in Clojure after such a long time away was to feel, instantly, back home. It restored my programmer’s soul.

But then crept in me a peculiar emotion I did not expect: **uncertainty**.

It went something like this:

> Thank you for this immutable `request` map, O elegant [Ring](https://github.com/ring-clojure/ring/). But may I ask: what exactly is in it?

— *Is it not obvious? It is the HTTP request*.

> Yes, of course, but *what* is that? What are the keys, and what are the values?

— *Is it not obvious? It is the HTTP request*.

> Yes, you are right. I will read thy source and seek the truth.

— *Yes, read and understand*.

> So I’m reading it all right, but what exactly is this local variable `attrs`, and `f`? And when I use `wrap-params`, what keys are added to my map?

— *Is it not obvious?*

> Never mind, I’ll just add a `println` or two.

This uncertainty wrecks productivity. Every library function you call, unless you have it memorized, requires hunting down the source or test files for examples on how to use it. Every map you get back requires a `println` to know what it contains.

Yes, Clojure is powerful. But power without support, without someone showing us how to wield that power, can only corrupt. I’m not talking about human philosophy here—just code. Which one of us has not suffered from Ruby’s metaprogramming, or Clojure’s maps? We are both inflictor and victim.

### An example: do DSLs empower or confuse?
Let’s talk about the corrupting power of Clojure DSLs. Clojurians love DSLs, and Clojure data structures lend itself well to be used in that way.

But I think there is something wrong with this.

[Hiccup](https://github.com/weavejester/hiccup), for example, generates HTML. So instead of writing:

```html
<span class="foo">bar</span>
```

You write:

```clojure
[:span {:class "foo"} "bar"]
```

But HTML is the perfect DSL for writing HTML—why replace it for another DSL with your own set of rules and restrictions, *and* lose the decades of tooling and know-how of every designer on the face of the planet?

(Thankfully, there is [enlive](https://github.com/cgrand/enlive) and [yesql](https://github.com/krisajenkins/yesql)).

But a larger problem with using data structures as DSL is that there is no way for me to know if I’m conforming to your DSL until I get a confusing runtime error.

Let’s pick on [bidi](https://github.com/juxt/bidi), a lovely little URL routing library. It’s fun to use, but there is a big, gaping problem with these DSLs.

Say we want to route `GET /notes`. In bidi, you would define your route like this:

```
(def my-routes
  ["/notes" :index-handler])
```

We can test this handler:

```
(bidi/match-route my-routes "/notes")
;; {:handler :index-handler}
;; Success!
```

Easy enough. But what if I want a bunch more routes?

```
GET  /
GET  /notes
GET  /notes/:id
POST /notes
POST /notes/:id
```

After several `Ctrl-F`s in the README, a close reading of the source code, lots of REPL guesswork, and an uncountable number of `nil`s and exceptions thrown in my face, I find the magic incantation:

```
(def my-routes
  ["" {"/" :home-page-handler
       "/notes"
         {:get  {"" :index-handler}
          :post {"" :create-handler}
          ["/" :id] {:get  {"" :show-handler}
                     :post {"" :update-handler}}}}])
```

I’m sure there’s a pattern in there somewhere, but do you see it? Will you remember to type it exactly right, or will you resort to REPLing our routes to death?

I have a feeling that REPL is going to get a *lot* of use.

## The problem with uncertainty
Now, don’t get me wrong, negative feelings do arise when writing in typed languages like Haskell and Scala. Confusion, frustration, hopelessness. But uncertainty is perhaps worse than all the others. You can resolve all the others by sitting down and learning the thing. But how does one resolve *uncertainty*? Only with more certainty. But what if the language does not provide a way to make certain of the uncertain?

Let’s examine some current attempts.

### Gradual typing
There is a frantic rush to bolt-on a type system to every dynamic language out there. Typed Racket, Typed Clojure, TypeScript, Typed Lua. Even Python has “type hints”.

This is a good thing, and a long time coming. It shows that people are hungry for certainty.

Unfortunately I have a suspicion that gradual typing won’t be enough. First, it requires a lot of energy to type all the libraries. Can the Clojure community be disciplined enough to add the required annotations in a majority of libraries? How many Clojure libraries provide `core.typed` annotations today?

Currently, I do not see such an effort succeeding.

We can strive for a culture of care and ownership, and we can demand for one until we are blue in the face (you know when your PR comments turn more snippy than helpful). But why is it so hard?

The fundamental problem, you see, is that a programming language is not just about code. Implied in the community is a school of thought, a philosophy. And *that* is difficult to change.

### It is not enough

What about unit tests, property tests, linters, runtime contracts, pair programming and PR reviews? What a plethora of tools! Surely they are sufficient.

I’m afraid they are not. Unit and property tests are of most value when they test functionality, not when they are used to test whether or not the shape of the result conforms to your expectation. Runtime contracts can diverge from the actual intent and are still runtime problems, and pair programming and PRs still have the fallible human in the picture.

Linters for dynamic languages are crippled to the point of being more about vanity and less about quality. If your argument for using your linter is that it finds whitespace issues, I’m not sure that’s solving important problems.

Using Haskell’s `hlint`, however, is like programming in the future. Because Haskell is a typed language, the linter knows a lot more about your program than just syntax. It can find *structural* problems too, like:

- When two equivalent anonymous functions that can be extracted out
- When a library method already exists for an expression
- When you fail to match every possible result in a case statement

We talk about strong AI. What if we start with tools that help us prevent runtime mistakes and guide us to write better code.

Does that sound like something you want?

## A short story

OK, enough ranting. Let’s pause. Here is a story of how we used our *programming language* to help us eradicate a certain class of bugs from a production code base.

Last week, I was frustrated at a particular Scala code base because it was so easy to write bugs that wrote wrong values into a Solr search cluster. For example, Solr silently allows `null` to be written to a boolean field. I spent a week refactoring that messy, bug-prone monolith into small, composible chunks that passed around immutable data structures ala Clojure. At first I thought it was great work. The new code was nice-to-read and well-tested. Yet I found myself writing the same runtime bugs I was writing before. One even involved breaking our porn filter (quick, what do you do when `isItPorn` returns `null`?).

So my co-worker Adam and I set a goal: **turn all these runtime bugs into compiler errors.** We wanted a big, fat FAILED TO COMPILE message if we tried to insert `null` into a boolean field.

It took us two days to figure out how to wrangle the Scala type system to do what we wanted. We almost pulled in [shapeless](https://github.com/milessabin/shapeless) (conjecture: all Scala programmers converge to shapeless).

But then we found a simpler system that worked. We could convert all of our old runtime errors into type errors. Adam refactored the entire code base to use our new design in just a few hours.

Today, I updated one of my client projects to use this refactored library. <span style="border-bottom: 2px dotted #ff0000;">A red line appeared</span>. It was a bug, caught by the compiler, never noticed until now.

Such power will never be available in a language like Ruby, and likely not Clojure either. But such power does exist. And you can have it too.

Let’s be clear: I am not suggesting that you and your team will start writing better, more-certain code the day you move away from Ruby. In fact, there will be a noticable dip in productivity and a noticable rise in audible profanity. But that is a growing pain, and it is only expected when learning anything new. What we should look forward to, however, is the inevitable improvement and confidence to come.

## The end of an era

This is my bet: **the age of dynamic languages is over**. There will be no new successful ones. Indeed we have learned a lot from them. We’ve learned that library code should be extendable by the programmer (mixins and meta-programming), that we want to control the structure (macros), that we disdain verbosity. And above all, we’ve learned that we want our languages to be enjoyable.

But it’s time to move on. We will see a flourishing of languages that *feel* like you’re writing in a Clojure, but typed. Included will be a suite of powerful tools that we’ve never seen before, tools so convincing that only ascetics will ignore.

To do this, we need to feed our tools the information they need to help us; a psychologist cannot help the silent patient. We begin by adding types, by restricting the space of possibilities to free ourselves from the self-made burden of uncertainty. New languages like Elm and Crystal are on the right track, and of course established ones like Haskell and Scala. We need more languages like these.

We think we have seen everything under the sun, that there are no new ideas worth learning. Worse yet, we refuse to learn anything unfamiliar. (Yuck, look at that syntax!) But remember how *hard* it was when we first learned to code? Was it not worth the effort? We should not be afraid to embark on that familiar journey once again.

The Christian theologian Gerhardus Vos described the tension of this life as "already but not yet." That is, a Christian is at once made into a new creation by their faith, but at the same time still carries the burdens and sorrows of this life.

What an apt description of where we are today. From a distance, we can envision what productive, empowering and confident languages would look and feel like. Many that exist today is on its way, but we are not there yet. Surely we can do better.

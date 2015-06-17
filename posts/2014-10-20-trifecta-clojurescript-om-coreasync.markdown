---
layout: post
title: The Trifecta of ClojureScript, Om and core.async
categories: blog
tags: clojure
---

*This post is an adaptation of my lighting talk given at Austin JS and Austin Clojure Meetup.*

- Introduction
- [Taking back control of your state](#taking-back-control)
- [Om and React](#om-and-react)
- [How to communicate](#how-to-communicate)

It's too easy to build a front-end house of straw. Somehow the JavaScript and DOM manipulations we write turn into a fragile state machine glued together with cryptic callbacks. We start with good intentions and write modular, understandable code with clean lines of separation between our views and models. But as our app becomes more complex, and as more people touch the code, the lines often begin to blur.

Our application state becomes difficult to reason about; they are barraged by asynchronous modifications, passed to and fro between different models, and are irresponsibly stored in our views as CSS classes. Our DOM crumbles, dependent on programmer incantation to wrangle it to the shape we want. And our code grows uncontrollably complex like wildfire, oblivious to the scorched state machine it leaves in its wake.

But who is to blame? Naturally, ourselves. Laziness, ignorance, hurriedness, insipidness. With enough dedication, skill and discipline we can learn to build graceful systems. But I don’t think the fault is completely ours. Given the complexity of asynchronous user interfaces and the need to orchestrate state between many components, it may be that tools we have are unfit for the job. Perhaps we need better tools.

Is there something better? Can we take back control of our state, erase the fragility of DOM manipulation and restrain ad-hoc, asynchronous messages? 

I think we can, through these means:

- Prevent wanton state changes.
- Give up control of DOM manipulation.
- Remove state management in callbacks.

With ClojureScript, Om and core.async, we can do all three.

I am not prophetic enough to declare that what I’m presenting here is *the* solution; it is never that easy. But I do think that the ideas brought by Clojure, Om (via React) and core.async are game changers. Their ideas are so good that it would be a shame for anyone to miss out on it.

<a id="taking-back-control"></a>

## Taking back control of your state

*If you're not familiar with Clojure syntax, you may want to read [this quick primer](/p/clojure-primer-js/).*

Clojure (and ClojureScript) data structures are immutable. We prefer, for example, that the number `0` always stay the number `0`. This is because `0` is considered a *value*, and values cannot change. Clojure extends this idea to data structures like maps. You cannot, for example, modify a Clojure map by adding a new element to it; you must make a “copy”.

To understand why this is important, let's see how mutable JavaScript data structures behave. There is no good sense of object equality:

```javascript
o1 = {foo: 0};
o2 = {foo: 0};
o1 == o2;
// false
```

And if you pass your object to some function, you have no idea what that other function may potentially do to your object.

```javascript
o = {foo: 0};
g(o);  // Your co-worker wrote this

// What is o?
```

You can try cloning your object before passing it to `g`, but now you have to decide if you should do a shallow clone or a deep clone. And just how deep do we need to go? One level? Two? All the way until you hit primitives?

Clojure's immutable data structures, on the other hand, gives us meaningful equality:

```clojure
(= {:foo 0 :bar {:car "far"}}
   {:foo 0 :bar {:car "far"}})
;; ⇒ true

;; Corresponding JavaScript object:
;; {foo: 0, bar: {car: "far"}}
```

And there is no need to clone:

```clojure
(def o {:foo 0})
(g o) ; Corresponding JavaScript: g(o)

;; o is still o
(= o {:foo 0})
;; ⇒ true
```

Immutability seems restrictive at first, but just as an artist thrives off the constraints of a canvas, you'll find that immutability frees you.

Immutability frees us to treat objects like *values*, because they are. We all prefer that `0 == 0` and `"far" == "far"`. So why do we put up with `{foo: 0} != {foo: 0}`?

Immutability also encourages modularity. When you can't modify your data and override your local variables, you unwittingly focus less on state manipulation and more on writing smaller, more manageable functions.

And lastly, immutability prevents you from building a spaghetti state monster. The Clojure map `{:name "Ayo", :email "ayo@example.com"}` is not a stateful object that anyone can add to, remove from or modify. That map is just a value, and the value is `{:name "Ayo", :email "ayo@example.com"}`.

Mutable objects, on the other hand, allow undiscerning programmers to modify state at inopportune times. Consider, for example, the User model below:

```javascript
var User = function (name, email) {
  this.name = name;
  this.email = email;

  this.isValid = function () {
    if (this.name.length > 100 ||
        this.email.indexOf("@") === -1) {
      return false;
    }
    return true;
  };

  this.resetEmail = function () {
    this.email = "";
  };

  this.magic = function () {
    // lots of code here…
    this.resetEmail();
    // lots of code here…
  };

  this.save = function () { /* … */ };

  return this;
};
```

And now let's use it:

```javascript
var u = User("Ayo", "ayo@example.com")
if (u.isValid()) {
  // …
  if (somePredicate) { u.magic(); }
  // …
  u.save(); // Oops, saved invalid email.
}
```

Obviously this is a contrived example, but in a large code base with many collaborators, mistakes like this are common.

And yes, we can do a number of things to mitigate disasters like the one above. We can emphasize good coding styles. We can enforce code reads that will hopefully catch such errors. These things are possible, but hard to do. For example, what if the commit was adding the single expression `u.magic()`? Is the code reader expected to know what `magic()` does through and through? But with immutable data structures, the data structures and compiler enforces all of this for you, for free.

The conclusion is simple:

1. Immutability prevents you from doing stupid things.
2. Immutability frees you to do the correct thing.

<a id="om-and-react"></a>

## Om and React

Given a page number of a book, you know that the contents will be the same *every single time*. Books are immutable. If you take the page number as your input, and your fingers flipping to the page as your function, you get:

$$\mathit{fingers}(\mathit{page}) \Rightarrow \mathit{contents}$$

Can we do the same with our programs? Can we pass in a state and get the UI at that given state?

With Om, you can.

Om is a ClojureScript library for React. While there are a lot of good [articles](http://blog.getprismatic.com/om-sweet-om-high-functional-frontend-engineering-with-clojurescript-and-react/) and [videos](https://www.youtube.com/watch?v=DMtwq3QtddY) that explain [Om](http://swannodette.github.io/2013/12/17/the-future-of-javascript-mvcs/) and [React](http://2013.jsconf.eu/speakers/pete-hunt-react-rethinking-best-practices.html), I like to think of an Om application as a function $f$ that takes your global application state $s$ and returns a $DOM$.

$$f(s) \Rightarrow DOM$$

Your application state $s$ is one giant, immutable map that contains all the necessary data for your UI to render. But naturally our UI needs to transition state at some point (e.g. when a user clicks something, or when an AJAX request comes back). To do this, we enclose our immutable map in a Clojure `atom`, like this:

```clojure
(def my-state (atom {:my-data {} :users [] :foo 0}))
```

An atom is like a box.  You can replace the thing inside the box, but you can't ever modify the content itself (because it's immutable). To get the value inside of the box, you must deference it using `@`. To “modify” an atom, use `swap!` or `reset!`. Like this:

```clojure
@my-state
;; ⇒ {:my-data {} :users [] :foo 0}

(reset! my-state {:bar 1})
@my-state
;; ⇒ {:bar 1}

(swap! my-state merge {:far 2})
@my-state
;; ⇒ {:bar 1 :far 2}
```

### Virtual DOM

In Om (and React), instead of writing raw HTML, you build virtual DOM elements. For example:

```clojure
;; Renders "Hello `name`", where `name` is retrieved
;; from the global state.
(dom/div
  #js {:className "greeting"}
  (str "Hello" (:name my-state)))
```

In React JSX, this may look like this:

```
<div className="greeting">Hello {this.props.name}</div>;

// Compiled JS:
// React.DOM.div({className: "greeting"}, "Hello ", this.props.name);
```

The virtual DOM is just a bunch of JavaScript objects—objects controlled by the React framework. At every predefined tick, React will take your virtual DOM, check which elements have changed, and render only those elements. React's [Reconciliation](http://facebook.github.io/react/docs/reconciliation.html) page describes in detail the optimizations made under the hood to make these renders fast. In short, React tries its best to only render changed components.

But Om has one distinct advantage against React: ClojureScript data structures are immutable. The rocket fuel, as David Nolen, creator of Om, calls it.

Let's see how this rocket fuel works. Say you have a custom component that renders a list of users and their names. Our state may look something like this:

```clojure
@my-state
;; ⇒ {:users [{:name "Ayo"} {:name "Zoey"}]}
```

The virtual DOM may look something like this (simplified for brevity):

```clojure
(ul
  (li "Ayo")
  (li "Zoey"))
```

At the next render phase, React will ask our component, “should I re-render you?” If our state were mutable objects, we would have to self-reconcile our state between two render phases, via the usual prescription of a recursive equality check or some hand-tuned mechanism. But because our state is immutable, we have only have to do a reference equality check, via Om [cursors](https://github.com/swannodette/om/wiki/Cursors).

David Nolen's [article](http://swannodette.github.io/2013/12/17/the-future-of-javascript-mvcs/) goes in depth.

### State over time

As your program's state changes over time, Om automatically re-renders the components that depend on the changed state, producing the DOM represented by your state. We can think of it like this:

$$f(s\_0) \Rightarrow \mathit{DOM}\_0$$

$$f(s\_1) \Rightarrow \mathit{DOM}\_1$$

$$f(s\_2) \Rightarrow \mathit{DOM}\_2$$

What a refreshing way of thinking!

Other than a marvelous elegance, you also get features like snapshots for free. The famous example is this: say we save a snapshot of our states over time, $[s\_0, s\_1, s\_2, …]$. Then choose any state, pass that into your program, and *bam* your UI is rendered as expected.

This makes [time travel](http://swannodette.github.io/2013/12/31/time-travel/) (e.g. undo/redo) easy. [Planjure](http://elbenshira.com/p/planjure/), Om [TodoMVC](http://swannodette.github.io/todomvc/labs/architecture-examples/om-undo/index.html) and [Goya](http://jackschaedler.github.io/goya/) are three example apps that implement undo/redo, all with just a few lines of code.

With Om, the complexity of state management, writing error-prone code to make sure that component A knows that component B knows that component C got a mouse click and is waiting for an AJAX response—all of that goes away. You find yourself astonished, really, at how simple everything can be.

I don't mean, of course, that your programs will be bug or frustration free. But I do think that Om obliterates the state monster and leads you towards the better path.

<a id="how-to-communicate"></a>

## How to communicate

JavaScript environments are asynchronous—we use callbacks to handle events. Let's say we're building a document editor and we want to save the document either when the user clicks `[Save]` or `[Exit]`. Saving a document may take several seconds, so we should ignore new save requests if the current one is in-flight (e.g. the user clicks `[Save]` and `[Exit]` consecutively).

If we were writing this in Backbone.js, we may have components that look like this

```javascript
var SaveButtonView = Backbone.View.Extend({
  events: {
    'click .save-button': 'save'
  },
  save: function () { this.model.save(); }
});

var ExitButtonView = Backbone.View.Extend({
  events: {
    'click .exit-button': 'save'
  },
  save: function () { this.model.save(); }
});

var SaveButtonModel = Backbone.Model.Extend({
  save: function () { this.document.save(); }
});

var ExitButtonModel = Backbone.Model.Extend({
  save: function () { this.document.save(); }
});

var DocumentModel = Backbone.Model.Extend({
  save: function () { /* complex logic */ }
});
```

But now let's say we want *two* GET requests to succeed before we print `Done!`. Furthermore, print `Failed!` on failure if one of the requests fail. The naive approach is

```javascript
$.get(opts1, function () {
	$.get(opts2, function () {
    console.log("Done!");
  }, function () {
    console.log("Failed!");
  });
}, function () {
  console.log("Failed!");
});
```

Even from the code above, and from many of our experiences, we realize that callbacks become unwieldy and cumbersome as our logic increases in complexity. Callback hell, it's often called. Many go to *promises* to ease the pain. But as the name implies, given a promise, you must know how to fulfill your promise. Why must this be?

Your boss may promise to give you a raise, but she may not actually know *how* to put more money in your bank account. Most likely, she will send an email to someone else who knows how to put the money into your bank account. Notice that email is a communication channel.

Our code works in the same way—if some module A gets a promise, why should we assume that module A knows how handle this? And let’s not just pass the promise to some module B that knows how to handle it. Why should A even know about B? Your boss does not walk over to accounting, interrupt someone, tell them to change your salary, and watch them type out the number on the screen. No, your boss just sends an asynchronous email and continues with her work.

In the document app example, we said that our app should save the document whenever we click [Save] or [Exit]. Let’s introduce a new method, Ctrl+S. Why should both the keyboard handler and the mouse handler have to hold a reference to the save module which does some complicated logic?

[core.async](https://github.com/clojure/core.async) erases this problem by introducing channels. Your app now holds a save channel. Any handler or module that wants to save the document simply puts a signal into the channel (any value would work). The save module, then, saves the document whenever a new value is found in the channel. Any module, including not-yet-written modules, may produce and consume from the channel. Just like email.

In essence, core.async simplifies cross-module communication by putting channels between them. [Rich](https://www.youtube.com/watch?v=dGVqrGmwOAw) and [Timothy](https://www.youtube.com/watch?v=enwIIGzhahw) goes in depth.

## Moving forward

In the beginning, I asserted that we could improve our applications by removing complex state modifications and by abstracting away the DOM. We saw the power of immutable data structures and of thinking about the DOM as an output to a function, instead of a stateful object to be modified. ClojureScript, Om and core.async are a trifecta of tools that help us realize these goals.

But If you’re not ready to take the plunge into Clojure, try out Facebook’s [immutable-js](https://github.com/facebook/immutable-js) and [React](http://facebook.github.io/react) instead.

At the end, we know that UI development is messy, and often times broken. We want to solve this, but it will require us to try out new ideas and new ways of thought. Both the JavaScript and Clojure communities, it seems, have benefited and learned from each other. So if the ideas presented here seem too foreign, don't balk. If they seem too radical, don't dismiss it. Let these ideas simmer. And if you want, try it out; play with it. But I think you’ll find these tools to be brilliant, powerful and, frankly, a lot of fun.

# References

[David Nolen: The Final Frontier](https://www.youtube.com/watch?v=DMtwq3QtddY)

[David Nolen: The future of JavaScript MVCs](http://swannodette.github.io/2013/12/17/the-future-of-javascript-mvcs/)

[David Nolen: Time Travel](http://swannodette.github.io/2013/12/31/time-travel/)

[Facebook: Immutable Data Structures](https://github.com/facebook/immutable-js)

[Facebook: React's Reconciliation](http://facebook.github.io/react/docs/reconciliation.html)

[Prismatic: Om sweet Om](http://blog.getprismatic.com/om-sweet-om-high-functional-frontend-engineering-with-clojurescript-and-react/)

[Rich Hickey: Clojure Concurrency (video)](https://www.youtube.com/watch?v=dGVqrGmwOAw)

[Rich Hickey: Clojure core.async (video)](http://www.infoq.com/presentations/clojure-core-async)

[Timothy Baldridge: Core.Async (video)](https://www.youtube.com/watch?v=enwIIGzhahw)

<script type="text/x-mathjax-config">
MathJax.Hub.Config({
  tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
});
</script>
<script type="text/javascript" src="//cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>

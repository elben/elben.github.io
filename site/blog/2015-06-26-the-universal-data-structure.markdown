<!--PREAMBLE
{
"postTitle": "The Universal Data Structure",
"date": "2015-06-26",
"tags": ["bad-theory", "recommended"]
}
-->

Whether you write compilers, web services or Django admin panels, you probably use **hash maps** more than all other data structures combined. In fact, you may not remember the last time you used anything else. This is not a coincidence: **any conceivable data structure can be implemented with a hash map; therefore, there is no need to use any other data structure**.

Specifically, the only hash you need is this one:

`UniversalHash = Hash[String|Int, UniversalHash|String]`

(If you’re not one of those Haskell or Scalaz guys, the complicated expression above tells us that a "universal hash" is a hash where the keys are either strings and integers, and the values are either universal hashes or strings.)

The universal hash obsoletes not just all other hash structures, but all other data structures. In this article, I’ll show you why this is a good thing, and why you’ll want to use the universal hash everywhere.

## Common patterns

We all admit that a hash is the *lingua franca* of programming. What is a User, after all, but just a hash:

```
{
  "name": "Alice",
  "email": "alice@example.com"
}
```

And we can’t forget our favorite JavaScript interview question of all time: If you only had twenty-four hours to implement arrays in JavaScript, how would you do it? Answer: with a hash!

```
{
  0: "First",
  1: "Second",
  2: "Third",
  …
}
```

And then on the third day, a collection of users:

```
{
  0: { "name": …, "email": …},
  1: { "name": …, "email": …},
}
```

Or, to specify relationships of managers to drones:

```
{
  "id": "1",
  "name": "Bob",
  "is_manager": "true"
}
```

```
{
  "id": "2",
  "name": "Adrian",
  "is_manager": "false",
  "manager_id": "1"
}
```

In fact, this pattern is so common that this year vulture capitalists have poured over $30 billion on JSON-backed database technologies. That is, hash-backed techonologies.

## Why this is a good thing

Given the abysmal state of of today’s software engineering, I believe that a full embrace of the universal hash will result in better, simpler programs.

First of all, a hash is simple to understand. JSON, for example, is the most-famous of all hash-based DSLs. And anyone can read JSON—even your product manager!

Second, hashes are fast. Hashes are always O(1) reads, inserts and writes. Why try to understand logarithms when "1" is not only much *easier* but also *faster*. (Note: there’s something about amortized complexity, but that’s a rare anomaly over billions of hits).

Third, every modern language (e.g. Ruby, CoffeeScript) comes with a good hash implementation and an easy-to-type syntax. There ain’t nothing easier than typing `{}` to initialize a database. I mean, show me a language with easy-to-type, first-class support for, God-forbid, finger trees. Can you even do that in ASCII? Thankfully, progressive language designers learned something post-Java: save curly braces for hashes.

Finally, hashes are malleable. Just as languages that support macros (e.g. LISP, Scalaz) can define control structures that macro-less languages can only dream about, using hashes allow a malleability that other data structures just don’t have. If you choose a tree to represent a file system, for example, you’re stuck with a tree-looking thing. An array is even less flexible—you have to use integers as your keys. So use hashes to give you that wiggle room you need. Because it’s not *if* your schema changes, but *when*. And *when* my schema changes, I’d rather be transforming a hash than a tree. (Plus, tree transformations are slow since those algorithms are recursive).

## Academic stuff

OK, you’re all-in on the idea that a hash is the most useful data structure man or woman will ever invent (and let’s face it, you may have read [en.wikipedia.org/wiki/Finger_tree](https://en.wikipedia.org/wiki/Finger_tree) five times but you’ll never used it). But how can you *know* for certain that a hash is all you’ll ever need? For this, we need a proof.

I usually try to avoid the rigorous thinking type of articles because why should we water down good analogies with rigor? Why spend hours working through difficult exercises when I can just show you a comic of a caterpillar eating a spider to describe some abstract concept? Let’s all admit that most of the time, the caterpillar analogies are sufficient, and we render LaTeX only to play Mathematician.

But I’m going to risk some academic stuff because I think it's important enough to delve into the theory. And for once, it's not too much work. It’s a proof for the common programmer, not just those ivy tower theorists.

The proof is so easy it’s a snore.

**Theorem**. Any conceivable data structure can be implemented with a hash of type `UniversalHash = Hash[String|Int, UniversalHash|String]`.

**Proof**.

1. Every conceivable data structure must exist in main memory.
2. Main memory is a mapping of addresses to values, where addresses are integers, and values are bytes (i.e. strings). This is equivalent to the type `Hash[Int, String]`.
3. Any `Hash[Int, String]` is a sub-type of `Hash[String|Int, UniversalHash|String]`.
4. Therefore, every conceivable data structure can be implemented using only `Hash[String|Int, UniversalHash|String]`.
5. QED.

You may ask, why use `UniversalHash` when you can just use `Hash[Int, String]`, which has the equivalent flexibility of main memory? The answer is simple: simplification. If we only had `Hash[Int, String]`, then our programs would have to mimic the low-level operations of languages like ASM, C and Rust. We would have to learn about stuff like bit shifting, XOR and maybe even virtual memory (since Moore’s Law is failing us). By *complexing* to `UniversalHash`, we are actually *simplexing*. This is one of those brilliant Hickey moments; something complex was made simple via a proper dose of complex.

OK, let’s QED this part and move on.

## Implementing other data structures on the UniversalHash

The theorem above proved that every data structure is just a hash. In this section, I'll show some examples of how different data structures can be implemented on top of a universal hash.

(By the way, a *theorem* is academic lingo for a truth backed by a *proof*, which is academic lingo for an irrefutable argument using our God-given logic, other proven theorems and a couple of God-given *axioms*, which is academic lingo for a *truth* we accept purely by our God-given logic, like: if something is not true then it is false, something cannot exist in an empty set, something logical is logical.)

### Array

As seen before, an array is just a hash where the keys are indices to the array.

Most famous is JavaScript’s implementation of arrays, which erases a litany of bugs by  returning `undefined` if the provided index is out of bounds. Universal values like `undefined` and `null` are useful values to return if you want to indicate system failure, out of memory error, user error, invalid memory location, timeout, nothing, something, anything.

### Linked list

Linked lists suffer an O(n) look up. I don’t know why you would ever use this over an array (or a hash), but you can implement it via a `UniversalHash`. Here’s a linked list of two elements:

```
{
  "head": "objectid1",
  "objectid1": {
    "value": …,
    "next": "objectid2"
  },
  "objectid2": {
    "value": …,
    "next": null
  }
}
```

### Tree

A tree is a node, where a node is just a value paired with an array of nodes.

```
{
  "value": …,
  "children": {
    0: {
      "value": …,
      "children": { … }
    },
    1: …,
  }
}
```

### Postgres

I am so sick and tired of this question: "why don’t you just use Postgres?" The answer is the same chorus: because everything ends up being a hash anyways!

Remember when everyone used tables to lay out their HTML? Well, that proved to be a horrible way to do things, because tables are inherently inflexible. It’s a strictly *geometric* constraint. Now we all use `div`s and CSS, because we get a much-more flexible CSS engine to define our layout. Postgres and her SQL friends are all table based, just like the `<table>` of 1999. Don’t be 1999.

### JSON

JSON support things like booleans, floating points, integers, etc. How can we support these types if we only have a `UniversalHash = Hash[String|Int, UniversalHash|String]`? There’s a simple solution that the bookmarking company [del.icio.us](https://delicious.com/) heralded: just tag stuff. For example, say our JSON looks like this:

```json
{
  "id": 1,
  "name": "Bob",
  "is_manager": true
}
```

We can represent this like this:

```
{
  "id":         {"value": "1",    "tag": "integer"},
  "name":       {"value": "Bob",  "tag": "string"},
  "is_manager": {"value": "true", "tag": "boolean"}
}
```

Now, our program can just read the `tag` and know what to do with the string value. In fact, this is what compiler writers have to do, since everything is just a bunch of bytes at the end of the day.

And to blow your minds even more, note that we’re implementing hashes with hashes.

(Some of you may question how we got the first implementation of a hash to begin with. When you type `{}`, the compiler surely puts that in a hash. But where did the *compiler’s* hash come from? This is where *mathematical induction* comes in. It’s a big math word that basically means that if you can prove that the bottom rung of your ladder exists, and you can get from rung *n* to rung *(n-1)*, then you'll eventually get off the ladder. Well, we know main memory is really just a `Hash[Int, String]`. And since we can implement hashes with hashes, this means that a hash can be implemented.)

## Practical implications

Unlike most academic work that has little to no practical implications, I think blindly following the stuff here will prove to be incalculably beneficial for you.

When a co-worker or interviewee stuffs some data into a hash even though it looks suspiciously like a tree, remind yourself that it’s OK—everything is just a hash. Plus, hashes are faster.

And when you’re picking database technology, I recommend that you choose databases that are flexible by nature. That is, databases that use the *hash* as the standard unit of storage, instead of rows or what-not.

I hope you’re convinced. A hash is simple. A hash is fast. A hash is all you need.

# Tutorial 1: Getting Started

This is $${title}

This tutorial walks you through your first Pencil website. At the end of this
tutorial, you'll have the beginnings of a website that you'll be able to use for
your own.

TODO DELETE THIS: Pencil version ${pencilVersion}

You may find it useful to also have [Pencil's Haddock page](https://hackage.haskell.org/package/pencil-${pencilVersion}/docs/Pencil.html) open as a reference.

We'll be using [stack](http://haskellstack.org) to quickly get going, so make
sure you have it installed. Once installed, let's create our project:

```sh
stack new my-website simple
cd my-website
```

Open the file `my-website.cabal` and look for the `executable my-website-exe` section. Add `pencil` into the `build-depends` section. It should look something like this:

```
executable my-website
  hs-source-dirs:      src
  main-is:             Main.hs
  default-language:    Haskell2010
  build-depends:       base >= 4.7 && < 5
                     , pencil
```

Now, we're going to add some files. First, let's make a new directory called
`site`, that will contain all of our website's HTML, Markdown and CSS files.

```
mkdir site
```

Your `my-website` folder should have a directory structure that looks something
like this (ignoring some files):

```
my-website.cabal
src/
  Main.hs
site/
```

Let's create a new file, in the `site` directory, called `layout.html`. This
will become our website's structural template. Copy-and-paste this into
`layout.html`:

```html
<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8" />
    <title>$${title}</title>
    <link rel="stylesheet" type="text/css" href="style.css"/>
  </head>
<body>$${body}</body>
</html>
```

Notice that `layout.html` contains strings that look like `$${title}` and
`$${body}`. These are variables, and they are what allows us to dynamically
generate content and share common templates like this `layout.html` file.

Let's also create a stylesheet. Create a new file in `site` called
`stylesheet.scss`, with this content:

```
$fontcolor: #333;

body {
  color: $fontcolor;
}
```

Notice that we're using the `.scss` extension, and we have that weird
`$fontcolor` thing, which you may not have seen before. This is because we're
going to use [Sass/Scss](http://sass-lang.com) for our styling. I like Scss
because it's a super set of CSS, so you can write plain-old CSS but "add on" the
Scss parts when you need it.

The final source file we'll add is `index.markdown`. This will contain our index
page's content, but in Markdown. You'll see how easy it is convert Markdown to
HTML, and inject it into our HTML-based layout.

`index.markdown` contains:

```
Welcome to my *awesome* [website](http://example.com)!
```

## Writing some Haskell

OK, let's write some Haskell! Fill `app/Main.hs` with this:

```haskell
module Main where

import Pencil

website :: PencilApp ()
website = do
  index <- load toHtml "index.markdown"
  render index

main :: IO ()
  main = run website defaultConfig
```

Currently this will only render the index page, but let's build our project and
try it out.

```
stack build
stack exec my-website-exe
```

This should create a `out` directory with a lousy `index.html` file in, with
your Markdown rendered as HTML. It's basic stuff, but we're getting somewhere.

`PencilApp` is Pencil's monad transformer Don't worry if you aren't familiar
with monad transformers.

In simple terms, `PencilApp` is a function that takes a `Config` and does a bunch
of stuff under the `IO` monad (e.g. reading your source files, converting
Markdown to HTML, and writing HTML files).

This is why we have to "run" our `website` function inside `main`; we have to
"give" the `PencilApp` function a `Config`. Passing in `defaultConfig`, which is
provided by Pencil, is sufficient for now.

## Rendering Pages

Let's look at what's happening inside `website`. The first thing you see is
`index <- load toHtml "index.markdown"`. The `load` function is the primary way
we load source files in Pencil. `toHtml` is a function that has the type
`String -> String`. In the documentation, you'll see its type as `FilePath ->
FilePath`, but `FilePath` is just an alias for `String`. In essence, `toHtml`
tells load to rename the file from `.markdown` to `.html`. That's all.

`load` will load the given file and convert it (if neccessary) to HTML. This is
done under the `IO` monad (because it's reading a file), so it's not a pure
function. This is why we save the result to `index` using `<-` inside a `do` block.

`index` is a `Page`. A Page is just a wrapper around the contents of the file.
Unlike a simple string, Pages know about things like those `$${title}` and
`$${body}` variables we saw in `layout.html`.

And finally we _render_ the Page into an actual HTML file by calling `render
index`.

As discussed before, we tell our program to do all of this via `run website
defaultConfig`.

## Structuring our Pages

Modify `Main.app` to look like this:

```haskell
module Main where

import Pencil

website :: PencilApp ()
website = do
  layout <- load toHtml "layout.html"
  index <- load toHtml "index.markdown"
  render (layout <|| index)

  renderCss "style.scss"

main :: IO ()
main = run website config
```

The call to `renderCss` loads and compiles your Scss file into `style.css` in
your output directory. Look at the source code of
[`renderCss`](https://hackage.haskell.org/package/pencil-${pencilVersion}/docs/Pencil.html#v:renderCss).
It's just a call to `load toCss` with a `render` at the end.

`layout <- load toHtml "layout.html"`, as we've seen before, loads our layout
into a Page. But what about `(layout <|| index)`?

We often will want to share some template across many pages. Specifically, often
we just want the contents of some page to be injected into another outer page.
In this case, we want the contents of `index.markdown` inside the `$${body}`
position of `layout.html`.

To do this, Pencil provides the concept of a `Structure`. A `Structure` is a
list of `Page`s, defining a nesting order. Think of them like [Russian nesting
dolls](https://en.wikipedia.org/wiki/Matryoshka_doll) The first element defines
the outer-most container, and subsequent elements are _inside_ the previous
element.

So when you have two Pages, you can combine them into a Structure using `(<||)`.
This is what we're doing with `(layout <|| index)`. This tells Pencil to insert
the contents of `index` into the `$${body}` variable of `layout`. This is why
our `layout.html` had `$${body}`; we want whatever Page is combined with
`layout.html` to inject it's content at that location.

You can read more about `Structure`s
[here](https://hackage.haskell.org/package/pencil-${pencilVersion}/docs/Pencil.html#g:3).

# Done

To genrate and serve your website, run the following
commands:

```
stack build
stack exec my-website-exe
cd out && python -m SimpleHTTPServer 8000
```

And go to [http://localhost:8000](http://localhost:8000). Note that we're using
Python's HTTP server to serve our HTML files so that our relative URLs (when we
add them) work correctly.

You're now on your way to using Pencil! To continue learning, check out
[Tutorial 2: Template Directives](/pencil/tutorials/02-template-directives) or
[Tutorial 3: Blogging](/pencil/tutorials/03-blogging).

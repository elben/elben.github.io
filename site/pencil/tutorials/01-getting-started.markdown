# Tutorial 1: Getting Started

This tutorial walks you through your first Pencil website. At the end of this
tutorial, you'll have the beginnings of a website that you'll be able to use for
your own.

You may find it useful to also have [Pencil's Haddock page](https://hackage.haskell.org/package/pencil) open as a reference. Once you choose the right version, click into the top-level "Pencil" module. That's where most of our functions will come from. This [link](https://hackage.haskell.org/package/pencil-0.1.1/docs/Pencil.html) takes you to that page for Pencil version 0.1.1.

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
    <title>${title}</title>
    <link rel="stylesheet" type="text/css" href="style.css"/>
  </head>
<body>${body}</body>
</html>
```

Notice that `layout.html` contains strings that look like `${title}` and
`${body}`. These are variables, and they are what allows us to dynamically
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

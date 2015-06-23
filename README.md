See [elbenshira.com](http://elbenshira.com).

# Development

Currently operating on GHC 7.8.3 and Hakyll 4.7.0.0 on a Cabal sandbox.

To install:

```bash
cabal install hakyll
```

To build:

```bash
cabal build && dist/build/site/site clean
dist/build/site/site watch

cabal repl
```

# Deploying

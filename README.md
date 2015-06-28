See [elbenshira.com](http://elbenshira.com).

# Development

Currently operating on GHC 7.8.4 and Stackage lts-2.15.

To install, download stack from here: https://www.stackage.org/

```bash
cd ~/code/elben.github.io
cabal install --only-dep
```

To build:

```bash
cabal build && dist/build/site/site clean
dist/build/site/site watch

cabal repl
```


# Deploying

Hakyll code lives in the `source` branch. When `source` is pushed to GitHub,
CircleCI builds the source and pushes the resulting static website into the
`master` branch, which then is published by GitHub pages.


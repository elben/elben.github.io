See [elbenshira.com](http://elbenshira.com).

# Development

To install, download stack from here: https://www.stackage.org/

```bash
cd ~/code/elben.github.io
cabal install --only-dep
```

To build:

```bash
stack build
stack exec site clean
stack exec site watch

stack ghci
```


# Deploying

Hakyll code lives in the `source` branch. When `source` is pushed to GitHub,
CircleCI builds the source and pushes the resulting static website into the
`master` branch, which then is published by GitHub pages.

[CircleCI Build](https://circleci.com/gh/elben/elben.github.io)

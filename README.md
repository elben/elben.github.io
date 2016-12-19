See [elbenshira.com](http://elbenshira.com).

# Development

To install, download stack from here: https://www.stackage.org/

```bash
cd ~/code/elben.github.io
cabal install --only-dep

# Install SCSS compiler
# Set the version in circle.yml to be the same
gem install sass --version=3.4.22
```

To build:

```bash
stack build
stack exec site clean
stack exec site watch

stack ghci
```

# Drafts

To set a post as a draft, add this to its metadata:

```
---
postTitle: The Post Title
categories: blog
draft: true
---
```

By default this is published in dev mode, but not in production. To *not* publish drafts locally:

```
stack exec site clean
LOAD_DRAFTS=false stack exec site watch
```

# Deploying

Hakyll code lives in the `source` branch. When `source` is pushed to GitHub,
CircleCI builds the source and pushes the resulting static website into the
`master` branch, which then is published by GitHub pages.

[CircleCI Build](https://circleci.com/gh/elben/elben.github.io)

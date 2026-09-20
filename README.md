# elbenshira.com

My personal website. Statically generated using
[Pencil](https://hackage.haskell.org/package/pencil).

# Development

Install GHC and Cabal with [ghcup](https://www.haskell.org/ghcup/):

```bash
ghcup install ghc 9.6.7
ghcup install cabal latest
```

Build and generate the site:

```bash
cabal build
cabal run elbenshiracom-exe
```

Or, using the Makefile:

```bash
make          # build and generate into out/
make serve    # generate, then serve out/ on http://localhost:8000
```

# Deployment

Deployment is handled by GitHub Actions
([`.github/workflows/deploy.yml`](.github/workflows/deploy.yml)). On every push
to the `source` branch it builds the site and deploys `out/` to GitHub Pages.
The workflow can also be triggered manually from the Actions tab.

The repository's Pages source must be set to **GitHub Actions**:

```bash
gh api -X PUT repos/elben/elben.github.io/pages -f build_type=workflow
```

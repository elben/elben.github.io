# elbenshira.com

My personal website. Statically generated using
[Pencil](https://hackage.haskell.org/package/pencil).

# Development

First, install `nix`:

```bash
curl https://nixos.org/nix/install | sh

nix-channel --add https://nixos.org/channels/nixos-18.09 nixpkgs
nix-channel --update
```

Building a new `elbenshiracom.nix`:

```bash
# If cabal2nix is not installed:
nix-env -i cabal2nix

rm -f elbenshiracom.nix && cabal2nix . > elbenshiracom.nix
```

Build using Nix:

```bash
nix-build
```

Calling `nix-build` everytime can be slow, however. When developing, it'll be
faster to get into a nix shell and use cabal to build incrementally:

```bash
nix-shell

cabal build
cabal run elbenshiracom-exe
```


```bash
make
cd out && python -m SimpleHTTPServer 8000
open localhost:8000
```

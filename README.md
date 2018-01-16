# Pencil

# Generate

```bash
make
```

# Development

```bash
stack install hindent

stack build
stack test
stack exec doctest src/

stack build && stack exec pencil-exe
cd out && python -m SimpleHTTPServer 8000
open localhost:8000
```

# Notes

To find differences, use:

```
/usr/bin/diff -qr elben.github.io/_site/ pencil/out/
```

- Partial vs Structured layouts. Partials are dumb; they do not carry an
  environment, nor can they declare PREAMBLE variables. Think of partials as
  just static copy-and-paste jobs; we literally just paste the text of the
  partial into the parent page. Whereas structured layouts describe the
  application structure of environment variables, and can be evaluated against.

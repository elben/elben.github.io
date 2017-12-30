# Pencil

# Development

```bash
stack install hindent

stack build
stack test

stack build && stack exec pencil-exe
cd out && python -m SimpleHTTPServer 8000
open localhost:8000
```

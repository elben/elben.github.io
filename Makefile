# "@" means don't echo this command
all: generate
	@true

build:
	cabal build

generate: build clean
	@mkdir -p out
	cabal run elbenshiracom-exe

clean:
	rm -rf out/*

# Preview the generated site at http://localhost:8000
serve: generate
	python3 -m http.server 8000 --directory out

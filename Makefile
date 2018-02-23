REPO := git@github.com:elben/elben.github.io.git

EXE := stack

all: generate
	@true

build:
	stack build --pedantic
	@mkdir -p out

generate: build clean
	stack exec elbenshiracom-exe

clean:
	rm -rf out/*

# Deploy generated out/ folder to Github Pages
deploy:
	rm -rf out/.git
	mkdir out/.circleci
	cp .circleci/noop.yml out/.circleci/config.yml
	cd out && git init && git add .
	cd out && git config user.email "nobody@circleci.com"
	cd out && git config user.name CircleCI
	cd out && git commit -m "Generated on `date`"
	cd out && git remote add origin ${REPO}
	cd out && git push -f origin master:master


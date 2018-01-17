CNAME := elbenshira.com
REPO := git@github.com:elben/elben.github.io.git

EXE := stack

all: generate
	@true

build: app/Main.hs
	stack build
	@mkdir -p out

generate: build clean
	stack exec pencil-exe

clean:
	rm -rf out/*

# Deploy generated out/ folder to Github Pages
#
# Write a circle.yml file that tells CircleCI to not build master.
deploy:
	rm -rf out/.git
	cd out && echo "general:\n  branches:\n    ignore:\n      - master" > circle.yml
	cd out && git init && git add .
	cd out && git config user.email "nobody@circleci.com"
	cd out && git config user.name CircleCI
	cd out && git commit -m "Generated on `date`"
	cd out && git remote add origin ${REPO}
	cd out && git push -f origin master:master


CNAME := elbenshira.com
REPO := git@github.com:elben/elben.github.io.git

EXE := stack exec site

all:	build
	@true

${EXE}:	site.hs
	stack build
	${EXE} clean

build:	${EXE}
	${EXE} build

clean:
	${EXE} clean

run:	build
	${EXE} watch

# Deploy _site to Github Pages
#
# Write a circle.yml file that tells CircleCI to not build master.
deploy:
	echo ${CNAME} > _site/CNAME
	rm -rf _site/.git
	cd _site && echo "general:\n  branches:\n    ignore:\n      - master" > circle.yml
	cd _site && git init && git add .
	cd _site && git config user.email "nobody@circleci.com"
	cd _site && git config user.name CircleCI
	cd _site && git commit -m "Generated on `date`"
	cd _site && git remote add origin ${REPO}
	cd _site && git push -f origin master:master

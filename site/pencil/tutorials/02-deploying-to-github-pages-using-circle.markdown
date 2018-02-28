# Tutorial 2: Deploying to GitHub Pages using Circle

[GitHub Pages](https://pages.github.com) is a great place to deploy static
websites. You can even set up a [custom domain
name](https://help.github.com/articles/using-a-custom-domain-with-github-pages/).
This tutorial uses the "user or organization site" style of GitHub Pages. This
means that your GitHub repository name for your website is expected to be
`yourusername.github.io`. You can also choose the "project site" style if you
want, though it is not covered in this tutorial.

We'll setup a [CircleCI](https://circleci.com) to automatically build
and deploy our website whenever we push our source.

First, make sure that your CircleCI account is set up to read your website's repository on GitHub.

Now we're going to use the `master` branch for our generated sources, so we'll
need to move our code into a different branch, `source`:

```
git checkout source
git push origin
```

Our build and deploy scripts will automatically generate commits to `master`, so
you should no longer use that. You may want to set `source` as the default
branch in the GitHub repository settings.

## Makefile

Next, let's write a simple `Makefile` for our project. This is not neccessary,
but makes things a bit simpler. Start with this
[tutorial](https://matthias-endler.de/2017/makefiles/) if `Makefiles` seem
daunting.

In your project's directory, create a `Makefile` file with (make sure you
replace `myusername` and `my-website-exe` accordingly):

```makefile
REPO := git@github.com:myusername/myusername.github.io.git

EXE := stack

all: generate
	@true

build:
	stack build --pedantic
	@mkdir -p out

generate: build clean
	stack exec my-website-exe

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
```

With this `Makefile`, you can now just type `make` to build all your
dependencies and generate your website. We'll also use this in the CircleCI
build.

# CircleCI Configuration

In your project's directory:

```bash
mkdir .circleci
touch .circleci/config.yml
```

Then open `config.yml` and fill it with (replace `myusername` and `my-website.cabal` accordingly):

```yaml
version: 2

jobs:
  build:
    docker:
      - image: fpco/stack-build
    steps:
      - checkout
      - restore_cache:
          keys:
            - v1-myusername.github.io-{{ checksum "my-website.cabal" }}-{{ checksum "stack.yaml" }}
            - v1-myusername.github.io-
      - run:
          name: Upgrade stack
          command: stack upgrade
      - run:
          name: Setup GHC
          command: stack setup
      - run:
          name: Build dependencies
          command: stack build --pedantic
      - save_cache:
          key: v1-myusername.github.io-{{ checksum "my-website.cabal" }}-{{ checksum "stack.yaml" }}
          paths:
            - ~/.stack
      - run:
          name: Generate website
          command: make all
      - save_cache:
          # Save the built files to be deployed later
          key: v1-myusername.github.io-build-{{ .Branch }}-{{ .Revision }}
          paths:
            - out
  deploy:
    docker:
      - image: buildpack-deps:xenial
    steps:
      - checkout
      - restore_cache:
          keys:
            - v1-myusername.github.io-build-{{ .Branch }}-{{ .Revision }}
            - v1-myusername.github.io-build
      - run:
          name: Deploy website
          command: make deploy

workflows:
  version: 2
  build-deploy:
    jobs:
      - build:
          filters:
            # Don't build master since that's for GitHub pages
            branches:
              ignore: master
      - deploy:
          requires:
            - build
          filters:
            branches:
              # Only deploy website files from the `source` branch
              only: source
```

This CircleCI build plan will build all branches (except `master`, which is
reserved for the actual generated files). It will, however, only "deploy" the
`source` branch. A "deploy" in this case means generate a git commit in the
`master` branch of your static files.

You can read more about the CircleCi configuration [here](https://circleci.com/docs/2.0/configuration-reference/).

We also will need one more YAML file:

```bash
touch .circleci/noop.yml
```

Edit that file and fill it with:

```yaml
version: 2

jobs:
  build:
    branches:
      ignore:
        - master
```

The `Makefile`'s `deploy` command makes `noop.yml` be the `config.yml` when it
"deploys" to the `master` branch. This is because we don't need CircleCI to do
anything with the static source files in `master`; GitHub Pages will deploy it for us.

## Putting it all together

With that, commit the files, push to `source`, and checkout your project's build
status in [CircleCI's dashboard](https://circleci.com/dashboard). Once the build
is done (it may take a while at first as it's downloading and caching all the
Haskell dependencies), you should be able to access your website on
`myusername.github.io`.

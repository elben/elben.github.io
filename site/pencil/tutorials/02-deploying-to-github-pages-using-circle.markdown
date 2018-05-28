# Tutorial 2: Deploying to GitHub Pages using Circle

In this tutorial, we'll deploy our website that we started in Tutorial 1 to
[GitHub Pages](https://pages.github.com) using [CircleCI](https://circleci.com).
By the end of this tutorial, you'll have:

- Your source code in the `source` branch.
- CircleCI building your website's generated files from `source` branch.
- CircleCI automatically commiting your generated files into `master`.
- GitHub hosting the files in `master`.

## Configuring GitHub

In your GitHub repository's settings page (Settings -> Options -> GitHub Pages),
set the "Source" branch to `master` for GitHub Pages. This tells GitHub that
we'll want to deploy whatever is in `master` as a static website to
http://myusername.github.io/my-website.

![GitHub Settings: Set pages source to `master` branch](../images/tutorial-1-github-settings.png)

Now that we're using the `master` branch for our generated files, we need to
move our code into a different branch (e.g. `source`):

```
git checkout -b source
git push origin
```

Our build and deploy scripts will automatically generate commits to `master`, so
you should no longer use that. You'll want to set `source` as the default
branch in the GitHub repository settings (Settings -> Branches -> Default branch).

![GitHub Settings: Set base branch to `source`](../images/tutorial-1-github-base-branch.png)

## Makefile

Next, let's write a simple `Makefile` for our project. This is not necessary,
but makes things a bit simpler. Start with this
[tutorial](https://matthias-endler.de/2017/makefiles/) if `Makefiles` seem
daunting.

In your project's directory, create a `Makefile` file with (make sure you
replace `myusername` and `my-website` accordingly):

```makefile
REPO := git@github.com:myusername/myusername.github.io.git

EXE := stack

all: generate
	@true

build:
	stack build --pedantic
	@mkdir -p out

generate: build clean
	stack exec my-website

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

## CircleCI Configuration

We'll also setup a [CircleCI](https://circleci.com) to automatically build and
deploy our website whenever we push our source.

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
anything with the files in `master`; GitHub Pages will deploy it for us.

## Putting it all together

With that, lets commit and push the files to the remote `source` branch:

```
git add .circleci Makefile
git commit -m "Add Makefile and .circleci files"
git push
```

Now we need to set up our project in CircleCI:

1. Go to the [CircleCI dashboard](https://circleci.com/dashboard).
2. Click on "Add Projects".
3. Add your website repo by clicking "Set Up Project".
4. In the project setup page, just click on "Start building", since we've
   already created our `.circleci/config.yml` file.

At this point, Circle should start building your website. Wait for the build to
finish, and Circle should automatically push your generated website to the
`master` branch.

If CircleCI fails to push to GitHub, you may need to set up your SSH keys to
have write access to the GitHub repo. Follow the instructions in your project's
"Checkout SSH keys" settings page to set up either a user key or a deploy key
with write access (circleci.com/gh/your-username/your-project/edit#checkout).

Once the build is done (it may take a while at first as it's downloading and
caching all the Haskell dependencies), GitHub will deploy your website to GitHub
will then deploy your website to
[myusername.github.io/my-website](http://myusername.github.io/my-website)

And that's it! You've successfully set up continuous integration. Any new pushes
to `source` will now kick-off a build and deploy.

In this tutorial, we'll deploy our website that we started in Tutorial 1 to
[GitHub Pages](https://pages.github.com) using [CircleCI](https://circleci.com).
By the end of this tutorial, you'll have:

- Your source code in the `source` branch.
- CircleCI building your website's generated files from `source` branch.
- CircleCI automatically committing your generated files into `master`.
- GitHub hosting the files in `master`.

## Configuring GitHub

In your GitHub repository's settings page (Settings -> Options -> GitHub Pages),
set the "Source" branch to `master` for GitHub Pages. This tells GitHub that
we'll want to deploy whatever is in `master` as a static website to
http://myusername.github.io/my-website.

![GitHub Settings: Set pages source to `master` branch](../images/tutorial-1-github-settings.png)

Now that we're using the `master` branch for our generated files, we need to
move our code into a different branch (e.g. `source`):

```
git checkout -b source
git push origin
```

Our build and deploy scripts will automatically generate commits to `master`, so
you should no longer use that. You'll want to set `source` as the default
branch in the GitHub repository settings (Settings -> Branches -> Default branch).

![GitHub Settings: Set base branch to `source`](../images/tutorial-1-github-base-branch.png)

## Makefile

Next, let's write a simple `Makefile` for our project. This is not necessary,
but makes things a bit simpler. Start with this
[tutorial](https://matthias-endler.de/2017/makefiles/) if `Makefiles` seem
daunting.

In your project's directory, create a `Makefile` file with (make sure you
replace `myusername` and `my-website` accordingly):

```makefile
REPO := git@github.com:myusername/myusername.github.io.git

EXE := stack

all: generate
	@true

build:
	stack build --pedantic
	@mkdir -p out

generate: build clean
	stack exec my-website

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

## CircleCI Configuration

We'll also setup a [CircleCI](https://circleci.com) to automatically build and
deploy our website whenever we push our source.

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
anything with the files in `master`; GitHub Pages will deploy it for us.

## Putting it all together

With that, lets commit and push the files to the remote `source` branch:

```
git add .circleci Makefile
git commit -m "Add Makefile and .circleci files"
git push
```

Now we need to set up our project in CircleCI:

1. Go to the [CircleCI dashboard](https://circleci.com/dashboard).
2. Click on "Add Projects".
3. Add your website repo by clicking "Set Up Project".
4. In the project setup page, just click on "Start building", since we've
   already created our `.circleci/config.yml` file.

At this point, Circle should start building your website. Wait for the build to
finish, and Circle should automatically push your generated website to the
`master` branch.

If CircleCI fails to push to GitHub, you may need to set up your SSH keys to
have write access to the GitHub repo. Follow the instructions in your project's
"Checkout SSH keys" settings page to set up either a user key or a deploy key
with write access (circleci.com/gh/your-username/your-project/edit#checkout).

Once the build is done (it may take a while at first as it's downloading and
caching all the Haskell dependencies), GitHub will deploy your website to GitHub
will then deploy your website to
[myusername.github.io/my-website](http://myusername.github.io/my-website)

And that's it! You've successfully set up continuous integration. Any new pushes
to `source` will now kick-off a build and deploy.

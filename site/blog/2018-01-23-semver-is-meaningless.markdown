<!--PREAMBLE
postTitle: SemVer is Meaningless
date: 2018-01-23
-->

Version numbers try to solve two problems, and thus solve neither very well.

Version numbers are suppose to aid computers in figuring out which packages are compatible with each other. This is why SemVer is strict. But this clashes directly with the other problem it’s trying to solve: for humans to gauge the amount of work required to upgrade to a newer version.

Rails programmers know, for example, that `patch` version bumps require minimal effort. `minor` bumps may introduce a new feature or a few changes here and there—maybe a couple of hours or days of work. But every Rails programmer know that a `major` version bump can take either a few weeks, a few months, or—if you didn’t maintain your code and its dependencies very well—is not worth the effort at all. Rails programmers are familiar with this release cadence, and we’ve learned to map the version bump to amount of work required.

But in SemVer land, the change between version `77` and `78` may be so minute as to not be worth discussing, but `78` to `79` may have ground-breaking changes. It may be an entire re-write of the package. But most of the time, a `major` is a small “backward incompatible” change. Our eyes glaze over when we see that some dependency was on version `14` last week and is now on `17`. We have to now read CHANGELOGs and READMEs to figure out how much work we have in front of us. In this sense the SemVer-ed version number has lost all meaning to the programmer.

Maybe package maintainers jump from `78` to `100` to convey big changes. But my point is that in making the `major` part be *any* breaking change, we’ve lost that information of what is *truly* breaking from the programmer’s point-of-view, and what is a minor nuisance.

So in trying to use a single string to convey two meanings, we fail to both well.

## The Solution

I don’t have a solution; dependency management is a long-standing problem. But I suspect that a better solution lies in  taking advantage of the type information provided by the packages, plus strong expectations that if the type of a function has not changed, then its computation has not either. This is of course easier to enforce in functional environments. Parametricity, for example, tells us that that a function of type `(a, b) -> a` has exactly one implementation. It must be the function that returns the left side of the pair, since we have no idea what `a` or `b` are. ([This is a good talk on parametricity](https://www.youtube.com/watch?v=qBvFsA3dglk)).

If we know that some package `vacuum-1.1.0` uses a couple of methods in `left-pad-2.0.4`, then perhaps we could statically analyze which newer versions of `left-pad` we can use, given our *current* usage of `left-pad`. If `left-pad-2.1.0` changed some function we never used, then it won’t affect us. We can build systems that auto-upgrade packages, and run our test suites against them. In essence this is what Haskell’s [Stackage](https://www.stackage.org) does, which provides a global snapshot of packages that are all compatible with each other. Maybe this is the way of the future—it’s easier to depend on one, global version than on many. But a public global snapshot does not solve private dependencies.

Ultimately, this kind of system would solve the programmatic problem that version numbers try to solve, and we can allow version numbers to speak more for humans than machines.

SemVer was a good attempt at trying to solve a big problem. But version numbers are fundamentally not well-suited to convey both its compatibility in relation to other packages, and the amount of work required by a programmer to upgrade. And when we try to use it to solve both, what we get is something rather muddled.

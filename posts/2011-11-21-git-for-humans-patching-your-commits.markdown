---
layout: post
title: Git for Humans: Patching Your Commits
date:   2011-11-21 11:16:00
categories: blog
tags: git
---

Ideally, every git commit would contain one logical work. But we don't work this
way. We often get side-tracked from our current task. In the middle of a bug
fix, we may see a method vulnerable for refactoring. So we refactor it.

 And now we have to commit a bug fix and a method refactor, and they're both in
 the same file. The lazy way out would be to commit the file with a nice
 message: "fixed bug #183 and refactored method_name".


But can we do better? Yes. With the `--patch` option, we can precisely choose
which hunks of code we want to add into the staging environment:

{% highlight bash %}
$ git add -p
diff --git a/README b/README
index 9938af1..88f41c4 100644
--- a/README
+++ b/README
@@ -1,3 +1,4 @@
+Version 1.2
-Version 1.1
Stage this hunk [y,n,q,a,d,/,e,?]? 
{% endhighlight %}

Not surprisingly, the `--patch` option works for other git commands like git-checkout, which would allow us to precisely choose which hunks of code we want to discard from our working directory.
So use `--patch` and make your commits precise.
 
### Archived comments; imported from Posterous

2143 views and 4 responses

Dec 23 2010, 8:45 AM

vipaca responded:

I've found this mechanism useful as you say in terms of committing logical units
of work, but like most things in git it can be risky. That is when committing
partial change sets of the working local copy you could be committing incomplete
or broken versions to the repository, an undesirable side effect.

Dec 23 2010, 9:09 AM

Elben Shira responded:

I think it's fine to commit non-working code. And I think it's fine to push
non-working code to a remote repository, as long as it's not the production
branch.

If you wait until everything works, then why are you using version control?

Dec 23 2010, 1:03 PM

vipaca responded:

One of the principle reasons for using version control is to facilitate
simultaneous development on a single project. Alleviating the headache
associated with version tracking and file synchronization.  However if you are
committing broken code then your throwing a lock back in the scenario and
blocking development for others. Not a productive approach to software
development.

Dec 23 2010, 1:54 PM

Elben Shira responded:

I agree. I think all code that is committed to the remote repository and visible
to others should pass all tests and not be broken.  But I think it is OK to make
local (and remote, as long as its your own personal remote branch) commits.

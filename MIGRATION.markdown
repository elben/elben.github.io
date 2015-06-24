
# TODO

- [ ] Fix highlighting-kate's Clojure highlighting of keywords like :foo.

      My issue here: https://github.com/jgm/highlighting-kate/issues/73

      From the
      [source](https://github.com/jgm/highlighting-kate/blob/master/xml/clojure.xml#L765),
      it seems like they intended for :keyword to be a Modifier2 type, which gets
      turned to a dsKeyword type. This is probably because keywords are in fact
      functions. So maybe they don't want to change it.

      https://github.com/jgm/highlighting-kate/blob/master/xml/ruby.xml#L881

      https://github.com/jgm/highlighting-kate/blob/master/xml/ruby.xml#L280-L284


      https://github.com/jgm/highlighting-kate/blob/82fe7a913af1b4c79376df12bc739adabfe60c9e/ParseSyntaxFiles.hs#L191

      Need to have a new itemData with style dsString, probably.

# DONE

- [x] Look at [hakyll-convert](https://hackage.haskell.org/package/hakyll-convert),

- [x] Post URLs should be exact same as before (SEO).

      Like:
      /blog/utf8-for-jruby-and-mysql/

      Not:
      /posts/2014-07-22-utf8-for-jruby-and-mysql.html

      Or, use /posts (but fix file name) and set up re-direct files (via head) from /blog.

      - [x] Figure out how to make like /blog/utf8-for-jruby-and-mysql/
      - [x] Links to blog posts should NOT include the .html at the end.

- [x] Tags

- [x] clojure atom feed equivalent.

- [x] Does mathjax work?


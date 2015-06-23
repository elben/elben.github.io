---
layout: pages
title: "Clojure Primer"
h1_class: small
---

Clojure examples compared to JavaScript counterparts.

Defining vars:

```clojure
(def age 100)
```

```javascript
// JavaScript
var age = 100;
```

Creating maps:

```clojure
{:foo 0 :bar {:car "far"}}
```

```javascript
// JavaScript
{foo: 0, bar: {car: "far"}}
```

Maps are functions:

```clojure
({:foo 0 :bar {:car "far"}} :foo)
;; ⇒ 0
```

```javascript
// JavaScript
var o = {foo: 0, bar: {car: "far"}};
o.foo
// ⇒ 0
```

Functions are values:

```clojure
(map inc [1 2 3])
;; ⇒ [2 3 4]
```

```javascript
// With Underscore.js
_.map([1, 2, 3], function (n) { return n + 1; });
// ⇒ [2 3 4]
```

Defining and using functions:

```clojure
(defn square [x] (* x x))

(square 3)
;; ⇒ 9
```

```javascript
// JavaScript
function square(x) { return x * x; }

square(3);
// ⇒ 9
```

Defining and using anonymous functions:

```clojure
((fn [x] (* x x)) 3)
;; ⇒ 9

;; % represents the first argument to the anonymous function.
;; We could have also used %1 to get at the first argument.
;; This function is the same as function above.
(#(* % %) 3)
;; ⇒ 9
```

```javascript
// JavaScript
(function (x) { return x * x; })(3);
// ⇒ 9
```

Clojure sequences are lazy:

```clojure
;; Take the first ten even numbers from a list of size one billion.
;; List is lazily built, so it never builds all billion elements.
(take 10 (filter even? (range 1000000000)))
;; ⇒ (0 2 4 6 8 10 12 14 16 18)
```

The JavaScript counterpart may crash your browser:

```javascript
// With Underscore.js
_.chain(_.range(1000000000))
 .filter(function (n) { return n % 2 == 0; })
 .take(10)
 .value();
// ⇒ ?
```

Defining and using functions, with a destructured map in function argument:

```clojure
(defn my-function
  [{:keys [name occupation]}]
  (println name "is a" occupation))

(my-function {:name  "Elben"
              :phone "5555555"
              :occupation "Farmer"})
;; ⇒ Elben is a Farmer
```

JavaScript doesn't support destructuring:

```javascript
var myFunction = function(map) {
  console.log(map.name + " is a " + map.occupation);
};

myFunction({name:  "Elben",
            phone: "5555555",
            occupation: "Farmer"});
// ⇒ Elben is a Farmer
```

If you're interested in learning more, I recommend [Joy of Clojure](http://joyofclojure.com/).


# Rich Comment Tests (RCT)

RCT turns rich comment forms into tests.

RCT is a version of the excellent [hyperfiddle/rcf](https://github.com/hyperfiddle/rcf)
that uses [rewrite-clj](https://github.com/clj-commons/rewrite-clj)
to evaluate `comment` blocks and match the result of each sexpr against
`;=> result` comments.

[CHANGELOG](#changes) | Uses [Break Versioning](https://github.com/ptaoussanis/encore/blob/master/BREAK-VERSIONING.md)
```clojure
com.mjdowney/rich-comment-tests {:git/tag "v0.0.1"
                                 :git/sha "..."}
```

Inspired by the discussion in [hyperfiddle/rcf/issues/49](https://github.com/hyperfiddle/rcf/issues/49).
Further discussion / feature requests welcome.

## Usage

Write rich `comment` forms as you normally would, and tag some of them with 
`{:rct/test true}`.

```clojure 
(defn some-function [x] (+ x 1))

^:rct/test
(comment
  ;; The function increments a number
  (some-function 1) ;=> 2

  ; Did I make a typo here?
  (+ 1 1)
  ;=> 3
  )

(require '[com.mjdowney.rich-comment-tests :as rct])
(rct/run-ns-tests! *ns*)
; FAIL in () (scratch.clj:12)
; Did I make a typo here?
;
; expected: (= (+ 1 1) 3)
;   actual: (not (= 2 3))
```

I have an editor shortcut that reloads the current namespace and sends 
`(com.mjdowney.rich-comment-tests/run-ns-tests! *ns*)` to the REPL.

It's kind of nice that you don't even have to include this library in the 
project itself, just in the development environment. 

Of course if you'd like to integrate with `clojure.test`, you'll need this as a 
test dependency, but none of this code need be packaged with production builds.

## Use with clojure.test / CI

The test assertions play nicely with `clojure.test`, so you can call tests
from inside a `deftest`:

```clojure
(ns some-ns-test
  (:require [clojure.test :refer :all]
            [com.mjdowney.rich-comment-tests :as rct]
            [some-ns]))

(deftest rich-comment-tests
  (rct/run-ns-tests! 'some-ns))
```

This is what I'm doing right now to integrate these kinds of tests with normal
Clojure unit tests, since I don't think they necessarily replace typical unit 
testing. 

## Roadmap
- [x] Use rewrite-clj to emit clojure.test forms from rich comment blocks
- [ ] Support multi-line result comments
  ```clojure 
  (map inc [1 2 3])
  ;=> [4
  ;    5
  ;    6]
  ```
- [ ] Better integration with clojure.test. Maybe some reporting when running 
  tests in a namespace, support for automatically discovering all tests in a 
  directory instead of proxying via `clojure.test/deftest`.
- [ ] Enable https://github.com/HealthSamurai/matcho patterns in addition to 
  literal `=` assertions?

## Changes

v0.0.1 
- Initial working version.

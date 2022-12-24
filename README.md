# Rich Comment Tests (RCT)

RCT turns rich comment forms into tests.

RCT is a version of the excellent [hyperfiddle/rcf](https://github.com/hyperfiddle/rcf)
that uses [rewrite-clj](https://github.com/clj-commons/rewrite-clj)
to evaluate `comment` blocks and match the result of each sexpr against
`;=> result` comments.

[CHANGELOG](#changes) | Uses [Break Versioning](https://github.com/ptaoussanis/encore/blob/master/BREAK-VERSIONING.md)
```clojure
io.github.matthewdowney/rich-comment-tests {:git/tag "v0.0.3" :git/sha "dc4e412"}
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
; Testing com.mjdowney.scratch
;
; FAIL in () (scratch.clj:11)
; ; Did I make a typo here?
;
; expected: (= (+ 1 1) 3)
;   actual: (not (= 2 3))
;
; Ran 1 tests containing 2 assertions.
; 1 failures, 0 errors.
```

I have an editor shortcut that reloads the current namespace and sends 
`(com.mjdowney.rich-comment-tests/run-ns-tests! *ns*)` to the REPL.

It's kind of nice that you don't even have to include this library in the 
project itself, just in the development environment. 

Of course if you'd like to integrate with `clojure.test`, you'll need this as a 
test dependency, but none of this code need be packaged with production builds.

## Assertions

RCT supports two kinds of assertions:
- `=>` asserts literal equality
- `=>>` asserts a [matcho](https://github.com/HealthSamurai/matcho) pattern 
  (plus [ellipses](https://github.com/matthewdowney/rich-comment-tests/issues/1))

```clojure 

^:rct/test
(comment
  ;; Literal assertions with =>
  (range 3) ;=> (0 1 2)
  (+ 5 5) ;; => 10
  (System/getProperty "java.version.date") ;=> "2022-09-20"


  ;; Pattern matching assertions with =>>
  (range 3) ;=>> '(0 1 ...)
  (+ 5 5) ;=>> int?

  (into {} (System/getProperties))
  ;=>> {"java.version.date" #"\d{4}-\d{2}-\d{2}"}

  (def response {:status 200 :body "ok"})
  response
  ;=>> {:status #(< % 300)
  ;     :body   not-empty}

  ;; Or with spec
  (require '[clojure.spec.alpha :as s])
  (into {} (System/getProperties)) ;=>> (s/map-of string? string?)
  )
```

## Use with clojure.test / CI

RCT is designed to hook in nicely with `clojure.test` reporting / assertion 
counting, and to be easy to run from an idiomatic Clojure CI workflow.

### Option 1: Use a single `deftest` to run all rich comment tests in the source tree

This works well for the following scenario: 
- you have a `test/` directory with source files that use `clojure.test` + `deftest`, 
- plus a `src` directory with rich comment tests sprinkled throughout,
- ... and you want all your tests to run at once.

```clojure 
(ns some-test-ns
  (:require [clojure.test :refer :all]
    [com.mjdowney.rich-comment-tests.test-runner :as test-runner]))

(deftest rct-tests
  (test-runner/run-tests-in-file-tree! :dirs #{"src"}))
```

Now when you run `clojure.test`, rich comment tests are also included. 

This is what happens when you run this project with:

    clj -X:test

### Option 2: Only run rich comment tests

For a project that only uses rich comment tests, you can add an alias to 
`deps.edn`:

```clojure 
{:aliases
 {:test {:exec-fn com.mjdowney.rich-comment-tests.test-runner/run-tests-in-file-tree!
         :exec-args {:dirs #{"src"}}}}}
```

## Changes
v0.0.3
- (@lilactown) Automatically quote result when used with => [#5](https://github.com/matthewdowney/rich-comment-tests/issues/5)
- (@lilactown) Allow whitespace between semicolon and arrow (;; =>) [#3](https://github.com/matthewdowney/rich-comment-tests/issues/3)
- Support matcho assertions with `;=>>` [#2](https://github.com/matthewdowney/rich-comment-tests/issues/2)
- Allow expectation string ellipses like ;=>> {:a :b ...} [#1](https://github.com/matthewdowney/rich-comment-tests/issues/1)

v0.0.2
- Add integration with `clojure.test` reporting + way to run RCT alongside it
- Improve resolution of source file path from Clojure namespace
- Add support for multi-line result comments

v0.0.1 
- Initial working version.

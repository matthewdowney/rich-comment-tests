[![clojure CI](https://github.com/matthewdowney/rich-comment-tests/actions/workflows/clojure.yml/badge.svg)](https://github.com/matthewdowney/rich-comment-tests/actions/workflows/clojure.yml)
[![bb compatible](https://raw.githubusercontent.com/babashka/babashka/master/logo/badge.svg)](https://babashka.org)

# Rich Comment Tests (RCT)

RCT turns rich comment forms into tests.

```clojure
^:rct/test
(comment
  (+ 1 1) ;=> 2
  (+ 1 1) ;=> 3
  )

(com.mjdowney.rich-comment-tests/run-ns-tests! *ns*)
; Testing com.mjdowney.rich-comment-tests.example
;
; FAIL in () (example.clj:4)
; expected: (= (+ 1 1) 3)
;   actual: (not (= 2 3))
;
; Ran 1 tests containing 2 assertions.
; 1 failures, 0 errors.
;=> {:test 1, :pass 1, :fail 1, :error 0}
```

## Coordinates
[CHANGELOG](#changes) | Uses [Break Versioning](https://github.com/ptaoussanis/encore/blob/master/BREAK-VERSIONING.md)
```clojure
io.github.matthewdowney/rich-comment-tests {:git/tag "v1.0.1" :git/sha "0f401d8"}
```

## Introduction

RCT is a version of the excellent [hyperfiddle/rcf](https://github.com/hyperfiddle/rcf)
that uses [rewrite-clj](https://github.com/clj-commons/rewrite-clj)
to evaluate `comment` blocks and match the result of each sexpr against
`;=> result` comments.

It was inspired by the discussion in [hyperfiddle/rcf/issues/49](https://github.com/hyperfiddle/rcf/issues/49).
Further discussion / feature requests welcome.

Its **goals** are to encourage writing rich comment forms in the most natural 
way possible, using normal `(comment ...)` forms and Clojure comments, and to 
integrate nicely with both REPL and clojure.test workflows.

**Non-goals** include providing advanced unit test features and syntax (the 
original hyperfiddle/rcf is much better for this!) or completely replacing 
clojure.test. I see this as a complementary tool to aid in REPL development,
help keep examples from `comment` forms up to date with CI, and encourage 
writing small tests alongside the function under test.

## Usage

1. Write rich `comment` forms as you normally would, and tag some of them with 
   `{:rct/test true}`. 
2. Run these tests during development by sending `run-ns-tests!` to the REPL (I 
   have an editor shortcut that reloads the namespace and then does this). 
3. [Configure your test runner](#use-with-clojuretest) to run all the rct tests 
   in your source tree (see also: [Use with Babashka Tasks](#use-with-babashka-tasks)).

## Assertions

RCT supports two kinds of assertions:
- `=>` asserts literal equality
- `=>>` asserts a [matcho](https://github.com/HealthSamurai/matcho) pattern 
  (and allows [... to indicate a partial pattern](https://github.com/matthewdowney/rich-comment-tests/issues/1))

Assertions are either part of the comment or follow it directly.

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

  ;; Or using a blank ;=> line to match against the next form
  response
  ;=>
  {:status 200
   :body "ok"}
  )
```

## Use with clojure.test

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

## Use with Babashka tasks

Sample bb.edn file to run RCTs via `bb test`:
```clojure
{:paths ["src"]
 :deps  {}
 :tasks {test
         {:docs "Run unit tests."
          :extra-deps {io.github.matthewdowney/rich-comment-tests {...}}
          :requires ([com.mjdowney.rich-comment-tests.test-runner :as rct])
          :task (rct/run-tests-in-file-tree! {:dirs #{"src"}})}}}
```

See also: [Running tests](https://book.babashka.org/#_running_tests) from the 
Babashka book. 

## Changes
v1.0.2 (2023-02-09)
- Update for Babashka test/*report-counters* is a ref instead of an atom for bb >= 1.1.171 [#18](https://github.com/matthewdowney/rich-comment-tests/issues/18)

v1.0.1 (2023-02-07)
- Fix: Babashka run-tests-in-file-tree! throws: No implementation of method: :getName of protocol: #'sci.impl.types/HasName [#17](https://github.com/matthewdowney/rich-comment-tests/issues/17)
- Make it easier to find the line from which test exceptions are thrown

v1.0.0 — no breaking changes, API is now stable 🎉 (2023-01-21) 
- Fix: `;=> nil` assertions ignored [#16](https://github.com/matthewdowney/rich-comment-tests/issues/16)
- Add stdout capture helper [#15](https://github.com/matthewdowney/rich-comment-tests/issues/15)

v0.0.4 (2023-01-04)
- Add Babashka support [#14](https://github.com/matthewdowney/rich-comment-tests/pull/14)
- Allow non-commented expectation strings following blank ;=> line [#13](https://github.com/matthewdowney/rich-comment-tests/issues/13)
- Make context string parsing smarter [#8](https://github.com/matthewdowney/rich-comment-tests/issues/8)
- Add CI for PRs / pushes to main [#10](https://github.com/matthewdowney/rich-comment-tests/issues/10)

v0.0.3 (2022-12-11)
- (@lilactown) Automatically quote result when used with => [#5](https://github.com/matthewdowney/rich-comment-tests/issues/5)
- (@lilactown) Allow whitespace between semicolon and arrow (;; =>) [#3](https://github.com/matthewdowney/rich-comment-tests/issues/3)
- Support matcho assertions with `;=>>` [#2](https://github.com/matthewdowney/rich-comment-tests/issues/2)
- Allow expectation string ellipses like ;=>> {:a :b ...} [#1](https://github.com/matthewdowney/rich-comment-tests/issues/1)

v0.0.2 (2022-12-07)
- Add integration with `clojure.test` reporting + way to run RCT alongside it
- Improve resolution of source file path from Clojure namespace
- Add support for multi-line result comments

v0.0.1 (2022-12-06)
- Initial working version.

(ns com.mjdowney.rich-comment-tests
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [com.mjdowney.rich-comment-tests.emit-tests :as tests]
            [rewrite-clj.zip :as z])
  (:import (clojure.lang Namespace)))

;;; Some example code that I'd like to be able to run this on

^:rct/test
(comment
  ;; For example, let's add two numbers.
  ;; We can see that the result should be '2'.
  (+ 1 1) ;=> 2

  ; `foo` and inc are equivalent
  (let [foo inc]
    (= (foo 1) (inc 1))) ;=> true

  ; This comment shouldn't be attached to anything

  (map inc
       [1 2 3])
  ;;=> [2 3 4]

  ; This form is run, but it's not an assertion, since the following comment
  ; doesn't start with =>
  (apply assoc {} (repeatedly 2 rand))
  ; {0.10999946790750348 0.4352718677404722}
  )

;;; End example code

;;; Nomenclature
;
; ^:rct/test
; (comment            ; <- This form is the "rich comment test", rct
;   ; Some text       ; <- Comments here are the "context strings"
;   (+ 1 1)           ; <- the test-sexpr
;   ;=> 2             ; <- the expectation-str
; )
;
; The parsing code reads the rich comment test and generates 'test forms':
; (clojure.test/is (clojure.core/= (+ 1 1) 2) "Some text")

(defn rct?
  "Is the node at the `zloc` a ^:rct/test (comment ...) block?"
  [zloc]
  (when-let [sexpr (when (z/sexpr-able? zloc) (z/sexpr zloc))]
    (and (= (first sexpr) 'comment) (:rct/test (meta sexpr)))))

(defn iterate1
  "Like `clojure.core/iterate`, but stops at the first `nil` element."
  [f x]
  (lazy-seq
    (cons x
          (when-some [nxt (f x)]
            (iterate1 f nxt)))))

(defn rct-zlocs
  "Given the root zloc for a source file, return a series of rct zlocs."
  [source-file-root-zloc]
  (->> source-file-root-zloc
       (iterate1 z/right)
       (filter rct?)))

(defn test-sexpr-zlocs
  "All sexpr-able nodes inside a rct form."
  [rct-zloc]
  (->>
    (iterate1
      z/right
      (-> rct-zloc
          z/down
          z/right
          z/down
          z/right))
    (filter z/sexpr-able?)))

(defn context-strings
  "A series of string comments preceding the test sexpr (until a line break)."
  [test-sexpr-zloc]
  (let [nodes-preceding-assertion (rest (iterate z/left* test-sexpr-zloc))]
    (reverse
      (sequence
        (comp
          ; Look for *comments* preceding the assertion, but stop searching if
          ; we hit a line break
          (take-while
            #(and (z/whitespace-or-comment? %) (not (z/linebreak? %))))

          ; Convert to strings and remove any empties
          (map z/string)
          (remove (comp empty? string/trim)))
        nodes-preceding-assertion))))

(defn some1
  "Like `clojure.core/some`, but returns the matching element, not the
  predicate result."
  [pred coll]
  (reduce (fn [_ x] (when (pred x) (reduced x))) nil coll))

(defn expectation-string
  "A string representing the expectation for a test expression (or nil if none).

  The expected result is designated by a =>-prefixed comment, either directly
  after or on a line following the test expression.

  For example:

    (+ 1 1) ;=> 2

    (+ 1 1)
    ;;=> 2"
  [test-sexpr-zloc]
  (let [nodes-following-assertion (rest (iterate z/right* test-sexpr-zloc))]
    (when-let [next-comment
               (->> nodes-following-assertion
                    (take-while z/whitespace-or-comment?)
                    (some1 #(= (z/tag %) :comment))
                    z/string)]
      (when-let [[_ expected] (re-matches #"\s*;+=>\s*(.*)\n" next-comment)]
        expected))))

(defn rct-data-seq
  "Take an rct zloc and return a series of maps with information about
  tests to run."
  [rct-zloc]
  (for [zloc (test-sexpr-zlocs rct-zloc)]
    {:context-strings (context-strings zloc)
     :test-sexpr (z/sexpr zloc)
     :expectation-string (expectation-string zloc)
     :location (z/position zloc)}))

^:rct/test
(comment
  ;; For example, run some tests on this source file.

  ;; Get the individual test assertions to run
  (->> (z/of-file *file* {:track-position? true})
       rct-zlocs
       (mapcat rct-data-seq))
  ; [{:test (+ 1 1) :expected "2" :location [13 3]} ...]

  ;; Select the very first test in the first test comment block (from line 14)
  (->> (z/of-file *file* {:track-position? true})
       rct-zlocs
       first
       rct-data-seq
       first)
  ; {...}

  (select-keys *1 [:test-sexpr :expectation-string :location])
  ;=> {:test-sexpr '(+ 1 1) :expectation-string "2" :location [14 3]}

  (-> *2 :context-strings first)
  ;=> ";; For example, let's add two numbers.\n"
  )

(defn run-tests*
  "Take a `rewrite-clj` zipper pointed at the root of a file and run all rich
  comment tests.

  *ns* and *file* should be bound before calling this."
  [zloc]
  {:pre [(:position zloc)]}
  (->> zloc
       rct-zlocs
       (mapcat rct-data-seq)
       (run! (comp eval tests/emit-test-form))))

(defn run-file-tests!
  "Take a file path and the namespace it corresponds to, and run tests."
  [file ns]
  {:pre [(string? file) (instance? Namespace ns)]}
  (binding [*ns* ns
            *file* file]
    (run-tests* (z/of-file *file* {:track-position? true}))))

(defn require-file-for-ns
  "Given a Namespace, attempt to find a corresponding source file, or throw an
  exception if this isn't possible."
  [ns]
  ; A bit of a hack: attempt to get the source file for a namespace from the
  ; meta data of one of its public vars
  (or
    (when-let [ns-file-from-metadata (->> (ns-publics ns)
                                          vals
                                          (keep (comp :file meta))
                                          first)]
      (let [nsf (io/file ns-file-from-metadata)]
        (when (.isFile nsf)
          (.getPath nsf))))
    (throw
      (ex-info (str "Failed to resolve source file for ns " ns)
               {:ns ns}))))

(defn run-ns-tests!
  "Take a namespace or namespace symbol, attempt to find the corresponding
  source file, and run tests in the namespace."
  [ns]
  {:pre [(or (instance? Namespace ns) (symbol? ns))]}
  (let [ns (if (symbol? ns)
             (doto (find-ns ns)
               (assert (str "Namespace exists for symbol: " ns)))
             ns)]
    (run-file-tests! (require-file-for-ns ns) ns)))


(defmacro capture-clojure-test-out
  "Capture any string output fom clojure.test while invoking `body`, and
  isolate test state."
  [& body]
  `(let [sw# (java.io.StringWriter.)]
     (binding [clojure.test/*test-out* sw#
               *out* sw#
               ; Isolate this test that we expect to fail, in case this snippet
               ; is being run from clojure.test, so that its failure isn't
               ; counted with other test failures.
               clojure.test/*report-counters*
               (ref clojure.test/*initial-report-counters*)]
       (do ~@body)
       (string/trim (.toString sw#)))))


^:rct/test
(comment
  ; RCT to read directly as a string
  (def form-that-fails
    "^:rct/test
    (comment
      ;; When asserting impossibilities...
      ;; The test fails
      (+ 1 1) ;=> 3
    )")

  ; Call *run-tests* on the string, and capture any clojure.test output
  (capture-clojure-test-out
    (run-tests* (z/of-string form-that-fails {:track-position? true})))

  ; There is output for the failed test
  (string/starts-with? *1 "FAIL in") ;=> true
  (string/ends-with? *2 "(not (= 2 3))") ;=> true
  )

(comment ;; For example...
  (run-ns-tests! *ns*))

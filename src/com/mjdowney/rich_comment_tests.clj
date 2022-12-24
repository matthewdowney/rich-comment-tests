(ns com.mjdowney.rich-comment-tests
  "RCT turns rich comment forms into tests."
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :as test]
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

  ;; Allows space between comment and arrow
  (+ 1 1)
  ;; => 2

  ; This comment isn't attached to anything

  ; Test results can be multi-line
  (map inc
       [1 2 3])
  ;; => (2
  ;;     3
  ;;     4)
  ;; More comments can follow

  ; This form is run, but it's not an assertion bc there is no => or =>>
  (apply assoc {} (repeatedly 2 rand))
  ; {0.10999946790750348 0.4352718677404722}

  ; Use use '=>>' for pattern matching with https://github.com/HealthSamurai/matcho
  (let [this-file (slurp *file*)]
    {:contents this-file
     :characters (count this-file)})
  ;=>> {:contents string?
  ;     :characters int?}

  ; '=>>' also allows ellipses before ), }, or ] to indicate more elements
  (range 10) ;;=>> [0 1 2 ...]
  (range 10) ;;=>> '(0 1 2 ...)
  (apply assoc {} (range 20)) ;=>> {0 1, 2 3 ...}
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

(defn result-comment?
  "A string like \";=> _\" or \";=>> _\" or \";; => _\""
  [s]
  (re-matches #"\s*;+\s?=>{1,2}.+\n" s))

(defn pairs
  "Transducer from [a b c ... z] => [[a b] [b c] ... [z nil]]."
  []
  (fn [rf]
    (let [left (volatile! nil)]
      (fn
        ([] rf)
        ([xs x]
         (let [lft @left]
           (vreset! left x)
           (if lft
             (rf xs [lft x])
             xs)))
        ([xs]
         (rf
           (if-let [lft @left]
             (unreduced (rf xs [lft nil]))
             xs)))))))

(defn rsequence
  "Like `(reverse (sequence xform coll))`."
  [xform coll]
  (transduce xform (completing #(cons %2 %1)) (list) coll))

(defn context-strings
  "A series of string comments preceding the test sexpr."
  [test-sexpr-zloc]
  (let [nodes-preceding-assertion (rest (iterate z/left* test-sexpr-zloc))]
    (letfn [(form-expstr-pair? [[a b]]
              (and a b ; is a pair
                   (z/sexpr-able? b) ; starting with a form
                   (= (z/tag a) :comment) ; then an expectation string comment
                   (result-comment? (z/string a))))]
      (rsequence
        (comp
          ; take up to the next completely blank line, ignoring nodes that are
          ; only whitespace
          (take-while (complement z/linebreak?))
          (remove z/whitespace?)

          ; drop any form + expectation string comment pairs
          (pairs)
          (remove form-expstr-pair?)
          (map first)

          ; take all other comments and remove empties
          (filter z/whitespace-or-comment?)
          (map z/string)
          (remove (comp empty? string/trim)))
        nodes-preceding-assertion))))

(defn expectation-data
  "Parse a string representing the expectation for a test expression and an
  expression type, returning a vector of `[type str]` (or nil if none).

  The expected result is designated by a =>-prefixed comment, either directly
  after or on a line following the test expression.

  For example:

    (+ 1 1) ;=> 2

    (+ 1 1)
    ;;=> 2

    (+ 1 1)
    ;; => 2"
  [test-sexpr-zloc]
  (let [nodes-following-assertion (rest (iterate z/right* test-sexpr-zloc))]
    (when-let [[fst-line & rest]
               (->> nodes-following-assertion
                    (take-while z/whitespace-or-comment?)
                    (map z/string)
                    (drop-while (complement result-comment?))
                    ; stop searching at the first double line break
                    (take-while (complement #{"\n"}))
                    ; strip leading ;s from comments
                    (map #(string/replace-first % #"^\s*;+\s?" ""))
                    seq)]
      (let [[_ type' fst-line] (re-matches #"(?s)(=>{1,2})(.+)" fst-line)]
        [(symbol type')
         (string/trim (apply str fst-line rest))]))))

(defn rct-data-seq
  "Take an rct zloc and return a series of maps with information about
  tests to run."
  [rct-zloc]
  (for [zloc (test-sexpr-zlocs rct-zloc)
        :let [[et es] (expectation-data zloc)]]
    {:context-strings (context-strings zloc)
     :test-sexpr (z/sexpr zloc)
     :expectation-string es
     :expectation-type et
     :location (z/position zloc)}))

^:rct/test
(comment
  ;; For example, run some tests on this source file.

  ;; Get the individual test assertions to run
  (->> (z/of-file *file* {:track-position? true})
       rct-zlocs
       (mapcat rct-data-seq))
  ;=>> [{:test-sexpr '(+ 1 1) :expectation-string "2" ...} ...]

  ;; Select the very first test in the first test comment block
  (->> (z/of-file *file* {:track-position? true})
       rct-zlocs
       first
       rct-data-seq
       first)
  ;=>> {:test-sexpr '(+ 1 1)
  ;     :expectation-string "2"
  ;     :expectation-type '=>
  ;     :location [int? int?]} ; <- these are line and column numbers

  (-> *1 :context-strings first)
  ;=> ";; For example, let's add two numbers.\n"

  ;; Select the sixth test in the first comment block, which uses a different
  ;; expectation type.
  (->> (z/of-file *file* {:track-position? true})
       rct-zlocs
       (mapcat rct-data-seq)
       (drop 5)
       first
       :expectation-type)
  ;=> =>>
  )

(defn clojure-test-reporting-active? [] (some? test/*report-counters*))

(defmacro with-clojure-test-reporting
  "Run `body` with clojure.test reporting, unless it is already active."
  [& body]
  `(if (clojure-test-reporting-active?)
     (do ~@body)
     (binding [test/*report-counters* (ref test/*initial-report-counters*)]
       (test/do-report {:type :begin-test-ns :ns *ns*})
       (let [ret# (do ~@body)]
         (test/do-report {:type :end-test-ns   :ns *ns*})
         (test/do-report (assoc @test/*report-counters* :type :summary))
         ret#))))

(defn run-tests*
  "Take a `rewrite-clj` zipper pointed at the root of a file and run all rich
  comment tests.

  The zipper must have be configured with {:track-position? true}.

  *ns* and *file* should be bound before calling this."
  [zloc]
  {:pre [(:position zloc)]}
  (with-clojure-test-reporting
    (doseq [rct-zloc (rct-zlocs zloc)]
      (test/inc-report-counter :test) ; treat each rct as a separate 'test'
      (try
        ; Build and eval test assertions for each test-sexpr in the rct
        (run! (comp eval tests/emit-test-form) (rct-data-seq rct-zloc))

        ; Copy clojure.test behavior in case of uncaught exception
        (catch Throwable e
          (test/do-report
            {:type :error
             :message "Uncaught exception, not in assertion."
             :expected nil
             :actual e}))))))

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
     (binding [test/*test-out* sw#
               *out* sw#
               ; Isolate this test that we expect to fail, in case this snippet
               ; is being run from clojure.test, so that its failure isn't
               ; counted with other test failures.
               test/*report-counters* (ref test/*initial-report-counters*)]
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

  (capture-clojure-test-out
    (run-tests* (z/of-string form-that-fails {:track-position? true})))

  ; The test fails and outputs a failure report with the line number, context
  ; strings, etc.
  (string/includes?
    *1
    "(rich_comment_tests.clj:5)
;; When asserting impossibilities...
;; The test fails

expected: (= (+ 1 1) 3)
  actual: (not (= 2 3))")
  ;=> true
  )

^:rct/test
(comment
  (def matcho-assert-that-fails
    "^:rct/test
    (comment
      ; Pattern match the types of the map values
      {:status 200 :body \"foo\"}
      ;=>> {:status int?
      ;     :body   int?}
    )")

  (capture-clojure-test-out
    (run-tests* (z/of-string matcho-assert-that-fails {:track-position? true})))

  (string/includes? *1 "(rich_comment_tests.clj:4)") ;=> true
  (string/includes? *2 "Matcho pattern mismatch:") ;=> true

  ;; Same assertion, but using matcho itself
  (capture-clojure-test-out
    (run-tests* (z/of-string matcho-assert-that-fails {:track-position? true})))
  ;=>> #".*FAIL in \(.*\) \(rich_comment_tests.clj:4\).*"
  )

;; Demo some kinds of test assertions
^:rct/test
(comment
  ;; Literal assertions with =>
  (range 3) ;=> (0 1 2)
  (+ 5 5) ;; => 10
  ; (System/getProperty "java.version.date") ;=> "2022-09-20"


  ;; Pattern matching assertions with =>>
  (range 3) ;=>> '(0 1 ...)
  (+ 5 5) ;=>> int?

  ; (into {} (System/getProperties)) ;=>> {"java.version.date" #"\d{4}-\d{2}-\d{2}"}

  (def response {:status 200 :body "ok"})
  response
  ;=>> {:status #(< % 300)
  ;     :body   not-empty}

  ;; Or with spec
  (require '[clojure.spec.alpha :as s])
  (into {} (System/getProperties)) ;=>> (s/map-of string? string?)
  )

(comment ;; For example...
  (run-ns-tests! *ns*))

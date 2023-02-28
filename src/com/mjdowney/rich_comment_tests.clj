(ns com.mjdowney.rich-comment-tests
  "RCT turns rich comment forms into tests."
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as string]
            [clojure.test :as test]
            [com.mjdowney.rich-comment-tests.emit-tests :as tests]
            [com.mjdowney.rich-comment-tests.impl :as impl]
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

  ; 'throws=>' tests exceptions
  (throw (Exception. "none")) ;throws=> Exception
  ; 'throws=>' can match a message
  (throw (Exception. "none")) ;throws=> #"none"
  ; if throws an ex-info, its ex-data can be matched using a matcho pattern
  (throw (ex-info "none" {:number 3})) ;throws=> {:number odd?}
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

(defn rct-zlocs
  "Given the root zloc for a source file, return a series of rct zlocs."
  [source-file-root-zloc]
  (->> source-file-root-zloc
       (impl/iterate1 z/right)
       (filter impl/rct?)))

(defn rct-data-seq
  "Take an rct zloc and return a series of maps with information about
  tests to run."
  [rct-zloc]
  (for [zloc (impl/test-sexpr-zlocs rct-zloc)
        :let [[et es] (impl/expectation-data zloc)]]
    {:context-strings (impl/context-strings zloc)
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
        (run!
          (fn [data]
            (let [tf (tests/emit-test-form data)]
              (try
                (eval tf)
                (catch Exception e
                  (throw
                    (ex-info
                      (str "Got " (type e) " evaluating form:\n"
                           (with-out-str (pprint/pprint (:test-sexpr data))))
                      {::eval-error true}
                      e))))))
          (rct-data-seq rct-zloc))

        ; Copy clojure.test behavior in case of uncaught exception
        (catch Throwable e
          (test/do-report
            {:type :error
             :message (if (::eval-error (ex-data e))
                        (ex-message e)
                        "Uncaught exception, not in assertion.")
             :expected nil
             :actual (if (::eval-error (ex-data e)) (ex-cause e) e)}))))
    @test/*report-counters*))

(defn run-file-tests!
  "Take a file path and the namespace it corresponds to, and run tests."
  [file ns]
  {:pre [(string? file) (instance? Namespace ns)]}
  (binding [*ns* ns
            *file* file]
    (let [resolver (fn [alias] (or (get {:current *ns*} alias) alias))]
      (run-tests* (z/of-file *file* {:track-position? true :auto-resolve resolver})))))

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

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defmacro with-printlns
  "Rebind `println` during the execution of body to print as normal, PLUS
  save each line of output, and return a vector of printed lines."
  [& body]
  `(let [println# clojure.core/println
         printed# (atom [])]
     (with-redefs [println (fn [& args#]
                             (apply println# args#)
                             (binding [*print-readably* nil]
                               (swap! printed# conj (apply pr-str args#)))
                             nil)]
       ~@body
       @printed#)))

^:rct/test
(comment
  ;; Tests for things which should fail

  ;; These tests need a helper to run a test from a string and capture the
  ;; output
  (defn rctstr [s]
    (capture-clojure-test-out
      (run-tests*
        (z/of-string
          (str "^:rct/test\n (comment\n" s "\n)")
          {:track-position? true}))))

  (string/split-lines ; Test failure shows file + line number
    (rctstr
      ";; When asserting impossibilities
       ;; The test fails
       (+ 1 1) ;=> 3"))
  ;=>> [#"FAIL in .*rich_comment_tests.clj:5\)"
  ;      ";; When asserting impossibilities"
  ;      ";; The test fails"
  ;      ""
  ;      "expected: (= (+ 1 1) 3)"
  ;      "  actual: (not (= 2 3))"]

  (rctstr
    "; Pattern match the types of the map values
     {:status 200 :body \"foo\"}
     ;=>> {:status int?
     ;     :body   int?}")

  (string/includes? *1 "(rich_comment_tests.clj:4)") ;=> true
  (string/includes? *2 "Matcho pattern mismatch:") ;=> true

  ;; Same assertion, but using matcho itself
  *3
  ;=>> #".*FAIL in \(.*\) \(rich_comment_tests.clj:4\).*"

  (defn fails? [s] (string/includes? (rctstr s) "FAIL in"))

  ;; Assertion of `nil` fails when the result is not nil
  (fails? "(+ 1 1) ;=>> nil?") ;=> true
  (fails? "(+ 1 1) ;=>> nil")  ;=> true
  (fails? "(+ 1 1) ;=> nil") ;=> true
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
  #_{:clj-kondo/ignore [:unused-namespace]}
  (require '[clojure.spec.alpha :as s])
  (into {} (System/getProperties)) ;=>> (s/map-of string? string?)
  
  ;; Or using a blank ;=> line to match against the next form
  response
  ;=>
  {:status 200
   :body "ok"}
  
  ;;auto resolve current ns keyword
  ::ok ;=> ::ok
  )

(comment ;; For example...
  (run-ns-tests! *ns*))

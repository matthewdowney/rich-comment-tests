;; TODO: *1 in test evaluations
;; TODO: Handle #_#_:rct/test (comment ...)
(ns com.mjdowney.rich-comment-tests
  (:require [clojure.string :as string]
            [clojure.test :as test]
            [rewrite-clj.zip :as z]))

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
  ; [{:test (+ 1 1) :expected "2" :location [11 3]} ...]

  ;; Select the very first test in the first test comment block (from line 11)
  (->> (z/of-file *file* {:track-position? true})
       rct-zlocs
       first
       rct-data-seq
       first)
  ; {...}

  (select-keys *1 [:test :expected :location])
  ;=> {:test '(+ 1 1) :expected "2" :location [11 3]}

  (-> *2 :assertion-string first)
  ;=> ";; For example, let's add two numbers.\n"
  )

(defn is
  "Like clojure.test/is, but allows passing an explicit line number and file."
  [form message line-number file]
  (let [args (rest form)
        pred (first form)
        pred1 (if (= pred `clojure.core/=) '= pred)
        form1 (if (= (first form) `clojure.core/=)
                (cons '= (rest form))
                form)]
    `(let [values# (list ~@args)
           result# (apply ~pred values#)]
       (if result#
         (test/do-report
           {:type :pass,
            :message ~message
            :expected '~form1,
            :actual (cons '~pred1 values#)
            :line ~line-number
            :file ~file})
         (test/do-report
           {:type :fail,
            :message ~message
            :expected '~form1,
            :actual (list '~'not (cons '~pred1 values#))
            :line ~line-number
            :file ~file}))
       result#)))

(defn throw-bad-expectation-string
  [{:keys [context-strings test-sexpr expectation-string location] :as data}]
  (throw
    (ex-info
      (format
        "Error reading expected return value %s on line %s for test: %s"
        (pr-str expectation-string) (first location) test-sexpr)
      data)))

(defn emit-test-form
  [{:keys [context-strings test-sexpr expectation-string location] :as data}]
  (let [expectation-form (try
                           (read-string expectation-string)
                           (catch Exception _
                             (throw-bad-expectation-string data)))
        form
        (if expectation-string
          (is `(= ~test-sexpr ~expectation-form)
              (last context-strings)
              (first location)
              *file*)
          test-sexpr)]
    (if-some [ctx (butlast context-strings)]
      `(test/testing ~(string/trim (apply str ctx)) ~form)
      form)))

#_(defn attempt-find-file-for-ns [for-ns]
  (-> (ns-publics for-ns)
      vals
      first
      meta
      :file))

#_(defn run-ns-tests! [for-ns]
  (if-let [file (attempt-find-file-for-ns for-ns)]
    (binding [*ns* for-ns
              *file* file]
      (->> (z/of-file *file* {:track-position? true})
           rct-zlocs
           (map rct-data-seq)
           (map #(build-test-assertions (rct-data-seq %) file))
           (run! (fn [test-code] (eval test-code)))))
    (throw
      (ex-info
        (str "Failed to resolve source file for namespace " for-ns)
        {:namespace for-ns}))))


(comment
  (def one-test
    (->> (z/of-file *file* {:track-position? true})
         rct-zlocs
         (mapcat rct-data-seq)
         (map emit-test-form)
         (run! eval)))

  one-test
  (eval (emit-test-form (assoc one-test :expectation-string "[1 2\n")))

  (map emit-test-form
       (->> (z/of-file *file* {:track-position? true})
            rct-zlocs
            first
            rct-data-seq))

  (run-ns-tests! *ns*)
  )

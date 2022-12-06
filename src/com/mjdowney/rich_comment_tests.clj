;; TODO: *1 in test evaluations
;; TODO: Pretty error handling for failed rct parsing
;; TODO: Nomenclature (lots of things call test/assertion/etc)
;; TODO: Handle #_#_:rct/test (comment ...)
(ns com.mjdowney.rich-comment-tests
  (:require [clojure.string :as string]
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

(defn is-test-comment?
  "Is the node at the `zloc` a (comment ...) block with metadata passing
  `pred`?"
  [zloc pred]
  (when-let [sexpr (when (z/sexpr-able? zloc) (z/sexpr zloc))]
    (and (= (first sexpr) 'comment) (pred (meta sexpr)))))

(defn iterate1
  "Like `clojure.core/iterate`, but stops at the first `nil` element."
  [f x]
  (lazy-seq
    (cons x
          (when-some [nxt (f x)]
            (iterate1 f nxt)))))

(defn test-comments
  "Given the root zloc for a source file, return a series of test comment
  zlocs."
  [source-file-root-zloc]
  (->> source-file-root-zloc
       (iterate1 z/right)
       (filter #(is-test-comment? % :rct/test))))

(defn test-comment-sexprs
  "All sexpr-able nodes inside a ^:rct/test (comment ...) block."
  [test-comment-zloc]
  (->>
    (iterate1
      z/right
      (-> test-comment-zloc
          z/down
          z/right
          z/down
          z/right))
    (filter z/sexpr-able?)))

(defn assertion-strings
  "A series of string comments preceding the test expr until a line break."
  [test-zloc]
  (let [nodes-preceding-assertion (rest (iterate z/left* test-zloc))]
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

(defn test-expects
  "A string representing the expectation for a test expression.

  The expected result is designated by a =>-prefixed comment, either directly
  after or on a line following the test expression.

  For example:

    (+ 1 1) ;=> 2

    (+ 1 1)
    ;;=> 2"
  [test-zloc]
  (let [nodes-following-assertion (rest (iterate z/right* test-zloc))]
    (when-let [next-comment
               (->> nodes-following-assertion
                    (take-while z/whitespace-or-comment?)
                    (some1 #(= (z/tag %) :comment))
                    z/string)]
      (when-let [[_ expected] (re-matches #"\s*;+=>\s*(.*)\n" next-comment)]
        expected))))

(defn test-comment-data
  "Parse a test comment and return a series of maps with information about
  tests to run."
  [comment-zloc]
  (for [zloc (test-comment-sexprs comment-zloc)]
    {:assertion-string (assertion-strings zloc)
     :test (z/sexpr zloc)
     :expected (test-expects zloc)
     :location (z/position zloc)}))

#_#_^:rct/test
(comment
  ;; For example, run some tests on this source file.

  ;; Get the individual test assertions to run
  (->> (z/of-file *file* {:track-position? true})
       test-comments
       (mapcat test-comment-data))
  ; [{:test (+ 1 1) :expected "2" :location [11 3]} ...]

  ;; Select the very first test in the first test comment block (from line 11)
  (->> (z/of-file *file* {:track-position? true})
       test-comments
       first
       test-comment-data
       first)
  ; {...}

  (select-keys *1 [:test :expected :location])
  ;=> {:test '(+ 1 1) :expected "2" :location [11 3]}

  (-> *2 :assertion-string first)
  ;=> ";; For example, let's add two numbers.\n"
  )

(defn assert-expr
  "Like clojure.test/is, but allows passing an explicit line number and file."
  [form line-number file]
  (let [args (rest form)
        pred (first form)]
    `(let [values# (list ~@args)
           result# (apply ~pred values#)]
       (if result#
         (clojure.test/do-report
           {:type :pass,
            :message nil,
            :expected '~form,
            :actual (cons '~pred values#)
            :line ~line-number
            :file ~file})
         (clojure.test/do-report
           {:type :fail,
            :message nil,
            :expected '~form,
            :actual (list '~'not (cons '~pred values#))
            :line ~line-number
            :file ~file}))
       result#)))

(defn build-test-assertions
  "Return test assertion code from rich comment tests in the given file path."
  [test-comment-data file]
  (cons
    `do
    (for [{:keys [assertion-string test expected location]} test-comment-data]
      `(clojure.test/testing ~(if (seq assertion-string)
                                (apply str assertion-string)
                                "")
         ~(if expected
            (assert-expr
              `(= ~test ~(read-string expected))
              (first location)
              file)
            test)))))

(defn attempt-find-file-for-ns [for-ns]
  (-> (ns-publics for-ns)
      vals
      first
      meta
      :file))

(defn run-ns-tests! [for-ns]
  (if-let [file (attempt-find-file-for-ns for-ns)]
    (binding [*ns* for-ns
              *file* file]
      (->> (z/of-file *file* {:track-position? true})
           test-comments
           (map test-comment-data)
           (map #(build-test-assertions (test-comment-data %) file))
           (run! (fn [test-code] (eval test-code)))))
    (throw
      (ex-info
        (str "Failed to resolve source file for namespace " for-ns)
        {:namespace for-ns}))))


(comment
  (->> (z/of-file *file* {:track-position? true})
       test-comments
       (map test-comment-data))

  (run-ns-tests! *ns*)
  )

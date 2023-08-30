(ns com.mjdowney.rich-comment-tests.emit-tests
  "Generate test code from parsed RCT data."
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :as test]
            [matcho.core :as m]
            [rewrite-clj.zip :as z]))

(defn throw-evaluation-error [test-form line-number file cause]
  (test/with-test-out
    (println "ERROR at " (str file ":" line-number) "-" (type cause)))
  (throw
    (ex-info
      (format
        "Exception during eval of %s at %s:%s" test-form line-number file)
      {:test-form test-form :line-number line-number :file file}
      cause)))

(defn try-bind-repl-vars
  "Write code which tries to evaluate `form`, and binds *1 *2 *3 and *e
  appropriately."
  [form line-number file]
  `(let [form-result#
         (try ~form
              (catch Exception e#
                (when (thread-bound? #'*e) (set! *e e#))
                (throw-evaluation-error '~form ~line-number ~file e#)))]
     (when (thread-bound? #'*3) (set! *3 *2))
     (when (thread-bound? #'*2) (set! *2 *1))
     (when (thread-bound? #'*1) (set! *1 form-result#))
     form-result#))

;; Add color to the exception if the supplied expectation string is malformed
;; E.g. in the case of ;=> "Oops, the quotes aren't balanced...
(defn throw-bad-expectation-string
  [{:keys [test-sexpr expectation-string location] :as data}]
  (throw
    (ex-info
      (format
        "Error reading expected return value %s on line %s for test: %s"
        (pr-str expectation-string) (first location) test-sexpr)
      data)))

(defmulti emit-assertion
  "Build test assertion code given the rct data and an expectation-form, which
  is a `read-string`'d version of expectation-string."
  #_{:clj-kondo/ignore [:unused-binding]}
  (fn [rct-data expectation-form]
    (:expectation-type rct-data)))

(defmulti read-expectation-form
  "Return a read-string'd expectation form from an expectation string."
  (fn [rct-data]
    (:expectation-type rct-data)))

(defn emit-test-form
  "Take parsed rct data and emit test code compatible with clojure.test."
  [{:keys [context-strings test-sexpr location] :as data}]
  (let [expectation-form (read-expectation-form data)
        form (if (= expectation-form :none)
               (try-bind-repl-vars test-sexpr (first location) *file*)
               (emit-assertion data expectation-form))]
    (if-some [ctx (butlast context-strings)]
      `(test/testing ~(string/trim (apply str ctx)) ~form)
      form)))

(defn default-read-expectation 
  [{:keys [expectation-string] :as data}]
  (if expectation-string
    (try
      (read-string expectation-string)
      (catch Exception _
        (throw-bad-expectation-string data)))
    :none))

; By default, just try to read-string it, if present
(defmethod read-expectation-form :default
  [data]
  (default-read-expectation data))

(defn elide-ellipses-in-expectation-string
  "Allow writing \"...\" before end brackets / parens in maps, vectors, and
  lists.

  Take a string which includes such ellipses and remove them."
  [expectation-string]
  (-> expectation-string
      z/of-string
      (z/postwalk
        ; Select all nodes which are maps / vectors / lists which end with a
        ; literal `...`
        (fn map-or-vector-with-ellipses? [zloc]
          (and
            (or (z/map? zloc) (z/vector? zloc) (z/list? zloc))
            (= (-> zloc z/down z/rightmost z/string) "...")))
        ; Remove the rightmost item of the collection, which is the `...`
        (fn remove-ellipses [zloc]
          (-> zloc z/down z/rightmost z/remove z/up)))
      z/string))

^:rct/test
(comment
  ;; E.g. updating this expectation string to omit the ellipses
  (elide-ellipses-in-expectation-string
    "[{:foo :bar
       :bar \"{:this :is :a :string ...}\" ; inside a str, so no removal
       :baz {:a :b
             :c [{:d :e ...} :f ...]}
       :list (1 2 3 ...)}
      ...]")
  ;=> "[{:foo :bar
  ;      :bar \"{:this :is :a :string ...}\" ; inside a str, so no removal
  ;      :baz {:a :b
  ;            :c [{:d :e} :f]}
  ;      :list (1 2 3)}]"
  )

; In matcho strings, allow ellipses
(defmethod read-expectation-form '=>>
  [data]
  (-> data ; update the expectation string then run default read logic
      (update :expectation-string elide-ellipses-in-expectation-string) 
      default-read-expectation))

; Workaround for Babashka where the *file* is not available from test code
(defmacro -*file* [] `(if *file* (.getName (io/file *file*)) "unknown_file"))

(defmethod emit-assertion :default
  [{:keys [expectation-type location] :as data} _expectation-form]
  (let [err (format "Unknown expectation string type ;%s at %s:%s"
                    expectation-type
                    (-*file*)
                    (first location))]
    (throw (ex-info err data))))

; Kind of like clojure.test/is, but hard-coded for (is (= _ _))
(defmethod emit-assertion '=>
  [{:keys [context-strings test-sexpr location]} expectation-form]
  (let [message (last context-strings)
        line-number (first location)
        fname (-*file*)
        test-form (list '= test-sexpr expectation-form)]
    `(let [form-result# ~(try-bind-repl-vars test-sexpr line-number *file*)
           test-result# (= form-result# '~expectation-form)]
       (clojure.test/do-report
        {:type (if test-result# :pass :fail),
         :message ~message
         :expected '~test-form
         :actual (if test-result# '~test-form (list '~'not (list '~'= form-result# '~expectation-form)))
         :line ~line-number
         :file ~fname}))))

(defn ?enclose [enclosing-form sexpr]
  (if enclosing-form
    (concat enclosing-form (list sexpr))
    sexpr))

; Assertion with matcho
(defmethod emit-assertion '=>>
  [{:keys [context-strings test-sexpr location]} expectation-form]
  (let [message (last context-strings)
        line (first location)
        fname (-*file*)]
    (?enclose
      (when message `(clojure.test/testing ~(str \newline message)))
      `(let [form-result# ~(try-bind-repl-vars test-sexpr line *file*)
             ; Rebind do-report to include the file name and line number, since
             ; matcho doesn't expose a way for us to set these
             -do-report# clojure.test/do-report
             dr# (fn [m#] (-do-report# (assoc m# :line ~line :file ~fname)))]
         (with-redefs [clojure.test/do-report dr#]
           (m/assert ~expectation-form form-result#))))))

(defn error-datafy 
  "datafy a Throwable `ex`"
  [ex]
  (letfn [(conj-when [m kv] (cond-> m (some? (second kv)) (conj kv)))]
    (-> {:error/class (class ex)}
        (conj-when [:error/message (ex-message ex)])
        (conj-when [:error/cause (ex-cause ex)])
        (conj-when [:error/data (ex-data ex)]))))

(defmethod emit-assertion 'throws=>>
  [{:keys [context-strings test-sexpr location]} expectation-form]
  (let [message (last context-strings)
        line-number (first location)
        fname (-*file*)]
    `(try
       ~test-sexpr 
       (clojure.test/do-report {:type :fail, :message ~message, :expected ~expectation-form, :file ~fname, :line ~line-number})
       (catch Throwable e#
         (m/assert ~expectation-form (error-datafy e#))))))

(comment
  (emit-assertion {:expectation-type 'throws=>> :test-sexpr '(throw (Exception. "")) :location [20]} 'Exception) 
  )

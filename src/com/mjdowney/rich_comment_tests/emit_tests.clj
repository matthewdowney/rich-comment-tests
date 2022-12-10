(ns com.mjdowney.rich-comment-tests.emit-tests
  "Generate test code from parsed RCT data."
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :as test]
            [matcho.core :as m]))

(defn throw-evaluation-error [test-form line-number file cause]
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
                (set! *e e#)
                (throw-evaluation-error '~form ~line-number ~file e#)))]
     (set! *3 *2)
     (set! *2 *1)
     (set! *1 form-result#)))

;; Add color to the exception if the supplied expectation string is malformed
;; E.g. in the case of ;=> "Oops, the quotes aren't balanced...
(defn throw-bad-expectation-string
  [{:keys [context-strings test-sexpr expectation-string location] :as data}]
  (throw
    (ex-info
      (format
        "Error reading expected return value %s on line %s for test: %s"
        (pr-str expectation-string) (first location) test-sexpr)
      data)))

(defmulti emit-assertion
  "Build test assertion code given the rct data and an expectation-form, which
  is a `read-string`'d version of expectation-string."
  (fn [rct-data expectation-form]
    (:expectation-type rct-data)))

(defn emit-test-form
  "Take parsed rct data and emit test code compatible with clojure.test."
  [{:keys [context-strings test-sexpr expectation-string expectation-type location]
    :as data}]
  (let [expectation-form (when expectation-string
                           (try
                             (read-string expectation-string)
                             (catch Exception _
                               (throw-bad-expectation-string data))))
        form (if expectation-form
               (emit-assertion data expectation-form)
               (try-bind-repl-vars test-sexpr (first location) *file*))]
    (if-some [ctx (butlast context-strings)]
      `(test/testing ~(string/trim (apply str ctx)) ~form)
      form)))

(defmethod emit-assertion :default
  [{:keys [expectation-type location] :as data} expectation-form]
  (let [err (format "Unknown expectation string type ;%s at %s:%s"
                    expectation-type
                    (.getName (io/file *file*))
                    (first location))]
    (throw (ex-info err data))))

; Kind of like clojure.test/is, but hard-coded for (is (= _ _))
(defmethod emit-assertion '=>
  [{:keys [context-strings test-sexpr location]} expectation-form]
  (let [message (last context-strings)
        line-number (first location)
        fname (.getName (io/file *file*))
        test-form (list '= test-sexpr expectation-form)]
    `(let [form-result# ~(try-bind-repl-vars test-sexpr line-number *file*)
           test-result# (= form-result# '~expectation-form)]

       (if test-result#
         (test/do-report
           {:type :pass,
            :message ~message
            :expected '~test-form
            :actual '~test-form
            :line ~line-number
            :file ~fname})
         (test/do-report
           {:type     :fail,
            :message  ~message
            :expected '~test-form
            :actual   (list '~'not (list '~'= form-result# '~expectation-form))
            :line     ~line-number
            :file     ~fname})))))

(defn ?enclose [enclosing-form sexpr]
  (if enclosing-form
    (concat enclosing-form (list sexpr))
    sexpr))

; Assertion with matcho
(defmethod emit-assertion '=>>
  [{:keys [context-strings test-sexpr location]} expectation-form]
  (let [message (last context-strings)
        line (first location)
        fname (.getName (io/file *file*))]
    (?enclose
      (when message `(test/testing ~(str \newline message)))
      `(let [form-result# ~(try-bind-repl-vars test-sexpr line *file*)
             ; Rebind do-report to include the file name and line number, since
             ; matcho doesn't expose a way for us to set these
             -do-report# clojure.test/do-report
             dr# (fn [m#] (-do-report# (assoc m# :line ~line :file ~fname)))]
         (with-redefs [clojure.test/do-report dr#]
           (m/assert ~expectation-form form-result#))))))

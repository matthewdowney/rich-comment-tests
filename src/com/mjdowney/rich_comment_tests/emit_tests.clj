(ns com.mjdowney.rich-comment-tests.emit-tests
  "Generate test code from parsed RCT data."
  (:require [clojure.string :as string]
            [clojure.test :as test]))

(defn throw-evaluation-error [test-form line-number file cause]
  (throw
    (ex-info
      (format
        "Exception during eval of %s at %s:%s" test-form line-number file)
      {:test-form test-form :line-number line-number :file file}
      cause)))

(defn try-eval-form-and-bind-repl-dynamics
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

(defn assert-equal
  "Kind of like clojure.test/is, but hard-coded for (is (= _ _))."
  [form expectation-form message line-number file]
  (let [test-form (list '= form expectation-form)]
    `(let [form-result# ~(try-eval-form-and-bind-repl-dynamics
                           form line-number file)
           test-result# (= form-result# ~expectation-form)]

       (if test-result#
         (test/do-report
           {:type :pass,
            :message ~message
            :expected '~test-form
            :actual '~test-form
            :line ~line-number
            :file ~file})
         (test/do-report
           {:type :fail,
            :message ~message
            :expected '~test-form
            :actual (list '~'not (list '~'= form-result# '~expectation-form))
            :line ~line-number
            :file ~file})))))

;; Add color to the exception if the supplied expectation string is malformed
(defn throw-bad-expectation-string
  [{:keys [context-strings test-sexpr expectation-string location] :as data}]
  (throw
    (ex-info
      (format
        "Error reading expected return value %s on line %s for test: %s"
        (pr-str expectation-string) (first location) test-sexpr)
      data)))

;; E.g. in the case of
(comment
  (+ 1 1)
  ;=> "Oops, the quotes aren't balanced...
  )

(defn emit-test-form
  "Take parsed rct data and emit test code compatible with clojure.test."
  [{:keys [context-strings test-sexpr expectation-string location] :as data}]
  (let [expectation-form (when expectation-string
                           (try
                             (read-string expectation-string)
                             (catch Exception _
                               (throw-bad-expectation-string data))))
        form
        (if expectation-form
          (assert-equal
            test-sexpr
            expectation-form
            (last context-strings)
            (first location)
            *file*)
          (try-eval-form-and-bind-repl-dynamics
            test-sexpr (first location) *file*))]
    (if-some [ctx (butlast context-strings)]
      `(test/testing ~(string/trim (apply str ctx)) ~form)
      form)))

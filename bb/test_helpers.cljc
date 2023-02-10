(ns test-helpers
  (:require
    [clojure.string :as str]
    [clojure.test :refer [is] :as test]
    [com.mjdowney.rich-comment-tests :as rct]
    [com.mjdowney.rich-comment-tests.emit-tests :as tests]
    [rewrite-clj.zip :as z]))

(defn rct
  "Stick a single RCT assertion string into a comment block and run it,
  returning any test output."
  [s]
  (let [rct-data
        (->> (z/of-string
               (str "^:rct/test\n (comment\n" s "\n)")
               {:track-position? true})
             rct/rct-zlocs
             (mapcat rct/rct-data-seq))]
    (is (= (count rct-data) 1) "parses one rct")

    (let [form (tests/emit-test-form rct-data)]
      (is (some? form) "emits a sexpr")
      (with-out-str
        (binding [test/*test-out* *out*
                  test/*report-counters* (ref test/*initial-report-counters*)]
          (eval form))))))

(defn quote-str [s]
  (->> (str/split-lines s)
       (map #(str \tab ">" \tab %))
       (str/join \newline)))

(defmacro passes? [s]
  (let [rsym (gensym "result")
        qsym (gensym "quoted-result")]
    `(let [~rsym (rct ~s)
           ~qsym (quote-str ~rsym)]
       (clojure.test/is
         (= ~rsym "")
         (str "no test failures running the form, but have\n" ~qsym)))))

(defmacro fails? [s & contains-failure-msgs]
  (let [rsym (gensym "result")
        qsym (gensym "quoted-result")]
    `(let [~rsym (rct ~s)
           ~qsym (quote-str ~rsym)]
       ~@(map
           (fn [msg]
             `(clojure.test/is
                (.contains ~rsym ~msg)
                (str "test fails with: " ~msg "\nhave:\n" ~qsym)))
           contains-failure-msgs))))

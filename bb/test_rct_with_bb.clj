(ns test-rct-with-bb
  "Test that RCT runs on Babashka, too."
  (:require 
   [clojure.test :as test :refer [deftest is testing]]
   [com.mjdowney.rich-comment-tests :as rct]
   [test-helpers :as t]))

(deftest rct-tests
  (testing "simple assertions"
    (t/passes? "(range 3) ;=> (0 1 2)")
    (t/fails? "(range 3) ;=> (0 2 2)" "actual: (not (= (0 2 2) (0 1 2)))")

    (t/passes?
      "(+ 5 5)
      ; => 10")

    (t/fails?
      "; 5 times 5
      (* 5 5) ;=> 35"

      "; 5 times 5" ; includes the context
      "actual: (not (= 35 25))"))

  (testing "reflective tests"
    (testing "when evaluating a :rct/test comment form in a .bb file"
      (let [ns-tests-result
            (binding [test/*report-counters* (ref test/*initial-report-counters*)]
              (rct/run-ns-tests! 'test-rct-with-bb))]
        (is (= ns-tests-result {:test 1 :pass 6 :fail 0 :error 0})
            "tests passing"))))

  (testing "matcho assertions"
    (t/passes? "(+ 5 5) ;;=>> int?")
    (t/fails? "(+ 5 5) ;;=>> string?" "Matcho pattern mismatch:")

    (t/passes? "(range 3) ;;=>> '(0 1 2)")
    (t/passes? "(assoc {} :foo :bar :bar :baz) ;;=>> {:foo :bar ...}")
    (t/passes? "(+ 5 5) ;=>> 10"))

  #_(testing "includes file names and line numbers in assertions"
      (is (str/starts-with? (t/rct "(+ 1 1) ;=> 3")
                            "\nFAIL in () (rct_babashka_test.bb:2)\n"))))

^:rct/test
(comment
  ;; Literal assertions with =>
  (range 3) ;=> (0 1 2)
  (+ 5 5) ;; => 10

  ;; Pattern matching assertions with =>>
  (range 3) ;=>> '(0 1 ...)
  (+ 5 5) ;=>> int?

  (def response {:status 200 :body "ok"})
  response
  ;=>> {:status #(< % 300)
  ;     :body   not-empty}

  ;; Or with spec
  #_{:clj-kondo/ignore [:unused-namespace]}
  (require '[clojure.spec.alpha :as s])
  (into {} (System/getProperties)) ;=>> (s/map-of string? string?)
  )

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn main []
  (let [{:keys [fail error] :as results}
        (clojure.test/test-ns 'test-rct-with-bb)]
    (println results)
    (when (pos? (+ fail error))
      (System/exit 1))))

(comment
  (rct/run-file-tests!
    "src/com/mjdowney/rich_comment_tests.clj"
    (find-ns 'com.mjdowney.rich-comment-tests))
  )

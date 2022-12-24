(ns com.mjdowney.rich-comment-tests-test
  (:require [clojure.string :as string]
            [clojure.test :refer :all]
            [com.mjdowney.rich-comment-tests :as rct]
            [com.mjdowney.rich-comment-tests.test-runner :as test-runner]
            [matcho.core :as m]
            [rewrite-clj.zip :as z]))

(deftest rct-tests
  (test-runner/run-tests-in-file-tree! :dirs #{"src"}))

(defn ctx-strings [comment-body]
  (let [form (str "^:rct/test\n(comment\n" comment-body "\n)")
        >str (comp string/trim string/join)]
    (->> (z/of-string form {:track-position? true})
         rct/rct-zlocs
         (mapcat rct/rct-data-seq)
         (map (juxt :test-sexpr (comp >str :context-strings)))
         (into {}))))

(deftest context-strings-test
  (m/assert
    '{(* 0 0) ""
      (+ 1 1) ";; Test for\n;; addition"
      (+ 2 2) ";; Test for\n;; addition"
      (* 1 1) ""
      (* 2 3) ""
      (* 2 2) ";; Squares\n;; and such"
      (* 3 3) ";; Squares\n;; and such\n; 3 squared"}
    (ctx-strings
      "(* 0 0) ;=> 0

       ;; Test for
       ;; addition
       (+ 1 1) ;=> 2
       (+ 2 2) ;=> 4

       (* 1 1) ;=> 1
       (* 2 3) ;=> 6

       ;; Squares
       ;; and such
       (* 2 2) ;=> 4
       ; 3 squared
       (* 3 3) ;=> 9")))

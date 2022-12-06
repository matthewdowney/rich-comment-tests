(ns com.mjdowney.rich-comment-tests-test
  (:require [clojure.test :refer :all]
            [com.mjdowney.rich-comment-tests :refer :all]))

(deftest namespace-tests
  (run-ns-tests! 'com.mjdowney.rich-comment-tests))

(ns com.mjdowney.rich-comment-tests.test-runner
  "Find source files, run any tests with rct, and report with clojure.test."
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.test :as test]
            [clojure.tools.namespace.file :as file]
            [clojure.tools.namespace.parse :as parse]
            [com.mjdowney.rich-comment-tests :as rct])
  (:import (java.io File)))

; from (private) macro clojure.tools.namespace.find/ignore-reader-exception
(defn try-get-namespace-from-file [file]
  (try
    (parse/name-from-ns-decl (file/read-file-ns-decl file (:read-opts nil)))
    (catch Exception e
      (when (not= :reader-exception (:type (ex-data e)))
        (throw e)))))

(defn file-ending-with?
  "Build a predicate matching files that end with any of the strings in the
  given set of extensions."
  [extension-set]
  (fn [^File f]
    (let [fname (.getName f)]
      (some
        (fn [ext] (string/ends-with? fname ext))
        extension-set))))

(defn ns->files
  "Take a series of `dirs` (e.g. `[\"src\"]`) and return a map of
  namespace symbol to series of source file paths.

  Typically, there is a single source file per namespace, but the relationship
  is potentially 1 : many."
  [dirs file-pred]
  (let [root-dirs->files (comp
                           (map io/file)
                           (mapcat file-seq)
                           (filter file-pred))]
    (transduce
      root-dirs->files ; get a seq of files
      (completing ; and reduce into a map of {namespace [file]}
        (fn [ns->files ^File file]
          (if-let [ns (try-get-namespace-from-file file)]
            (update ns->files ns conj (.getPath file))
            ns->files)))
      {}
      dirs)))

(defn run-tests-in-file-tree!
  "Find all files in the set of `dirs` matching `file-pred` and execute any
  RCT tests."
  [& {:keys [dirs file-pred]
      :or   {file-pred (file-ending-with? #{".clj" ".cljc"})}}]
  {:pre [(and (set? dirs) (every? string? dirs))]}
  ; Get the namespaces to test and the files to read tests from
  (let [ns->fs (sort-by key (ns->files dirs file-pred))]
    (println "\nRunning rich-comment-tests in" dirs)
    (run! (comp require key) ns->fs)

    ; Set up the clojure.test state
    (let [called-from-clojure-test-runner? (some? test/*report-counters*)]
      (binding [test/*report-counters* (if called-from-clojure-test-runner?
                                         test/*report-counters*
                                         (ref test/*initial-report-counters*))]

        ; Run tests for the namespace by calling `run-file-tests!` with each
        ; associated file
        (doseq [[ns files] ns->fs]
          (test/do-report {:type :begin-test-ns :ns ns})
          (let [n-tests (:test @test/*report-counters*)
                _ (doseq [file files] (rct/run-file-tests! file (find-ns ns)))
                n-tests (- (:test @test/*report-counters*) n-tests)]

            (if (zero? n-tests)
              (println "No tests.")
              (println "Ran" n-tests "tests."))

            (test/do-report {:type :end-test-ns :ns ns})))

        ; Finish clojure.test state
        (when-not called-from-clojure-test-runner?
          (test/do-report (assoc @test/*report-counters* :type :summary)))
        @test/*report-counters*))))

#_(run-tests-in-file-tree! :dirs #{"src"})

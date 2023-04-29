(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.string :as str]
            [org.corfield.build :as bb]
            [clojure.java.shell :refer [sh]]))

(defn git-tag []
  (let [tag (->> (sh "git" "tag") :out str/split-lines peek)]
    (if (seq tag) tag "0.1-SNAPSHOT")))

(def lib 'io.github.matthewdowney/rich-comment-tests)
(def main 'com.mjdowney.rich-comment-tests)
(def version (git-tag))
(def class-dir "target/classes")
(def basis (b/create-basis {:project "deps.edn"}))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn clean [_]
  (b/delete {:path "target"}))

(defn jar [_]
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :scm {:url "https://github.com/matthewdowney/rich-comment-tests"
                      :connection "scm:git:git://github.com/matthewdowney/rich-comment-tests.git"
                      :developerConnection "scm:git:ssh://git@github.com/matthewdowney/rich-comment-tests.git"}
                :src-dirs ["src"]})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn deploy "Deploy the JAR to Clojars." [{:as opts}]
  (-> opts
      (assoc :lib lib
             :version version
             :main main)
      (bb/deploy)))

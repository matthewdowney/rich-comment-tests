(ns com.mjdowney.rich-comment-tests.impl 
  (:require [clojure.string :as string]
            [rewrite-clj.zip :as z]))

(defn rct?
  "Is the node at the `zloc` a ^:rct/test (comment ...) block?"
  [zloc]
  (when-let [sexpr (when (z/sexpr-able? zloc) (z/sexpr zloc))]
    (and (seqable? sexpr) (= (first sexpr) 'comment) (:rct/test (meta sexpr)))))

^:rct/test
(comment
  (rct? (z/of-string "3")) ;=> false
  (rct? (z/of-string "^:rct/test (comment )")) ;=> true
  )

(defn iterate1
  "Like `clojure.core/iterate`, but stops at the first `nil` element."
  [f x]
  (lazy-seq
   (cons x
         (when-some [nxt (f x)]
           (iterate1 f nxt)))))

(defn test-sexpr-zlocs
  "All sexpr-able nodes inside a rct form."
  [rct-zloc]
  (->>
   (iterate1
    z/right
    (-> rct-zloc
        z/down
        z/right
        z/down
        z/right))
   (filter z/sexpr-able?)))

(defn pairs
  "Transducer from [a b c ... z] => [[a b] [b c] ... [z nil]]."
  []
  (fn [rf]
    (let [left (volatile! nil)]
      (fn
        ([] rf)
        ([xs x]
         (let [lft @left]
           (vreset! left x)
           (if lft
             (rf xs [lft x])
             xs)))
        ([xs]
         (rf
          (if-let [lft @left]
            (unreduced (rf xs [lft nil]))
            xs)))))))

(defn rsequence
  "Like `(reverse (sequence xform coll))`."
  [xform coll]
  (transduce xform (completing #(cons %2 %1)) (list) coll))

(let [operator "\\w*=>{1,2}"
      ptn-comment (re-pattern (str "\\s*;+\\s*?" operator ".*\\n"))
      ptn-op-type (re-pattern (str "(?s);+\\s*(" operator ").+"))
      ptn-expectation (re-pattern (str "(?s)" operator "(.+)"))]
  (defn result-comment?
    [s]
    (boolean (re-matches ptn-comment s)))

  (defn result-comment-type [s]
    (-> (re-matches ptn-op-type s) (second) (symbol)))

  (defn expectation-str [fst-line]
    (-> (re-matches ptn-expectation fst-line) second)))

^:rct/test
(comment
  (result-comment? "  ;;  throws=> 3\n") ;=> true
  (result-comment-type ";;   =>\n") ;=> =>
  (expectation-str "=>>:ok") ;=> ":ok"
  )

(defn context-strings
  "A series of string comments preceding the test sexpr."
  [test-sexpr-zloc]
  (let [nodes-preceding-assertion (rest (iterate z/left* test-sexpr-zloc))]
    (letfn [(form-expstr-pair? [[a b]]
              (and a b ; is a pair
                   (z/sexpr-able? b) ; starting with a form
                   (= (z/tag a) :comment) ; then an expectation string comment
                   (result-comment? (z/string a))))]
      (rsequence
       (comp
          ; take up to the next completely blank line, ignoring nodes that are
          ; only whitespace
        (take-while (complement z/linebreak?))
        (remove z/whitespace?)

          ; drop any form + expectation string comment pairs
        (pairs)
        (remove form-expstr-pair?)
        (map first)

          ; take all other comments and remove empties
        (filter z/whitespace-or-comment?)
        (map z/string)
        (remove (comp empty? string/trim)))
       nodes-preceding-assertion))))

(defn advance [zloc & {:keys [to through] :as ops}]
  {:pre [to through]}
  (let [zloc (z/right* zloc)]
    (cond
      (to zloc) zloc
      (through zloc) (recur zloc ops)
      :else nil)))

(defn tag? [x] #(= (z/tag %) x))

(defn ?result-comment-zloc [z]
  ; Find the next comment after the form, breaking if a node is not whitespace
  (when-let [z (advance z :to (tag? :comment) :through z/whitespace?)]
    ; Return the zloc if the comment is a result comment
    (when (result-comment? (z/string z))
      z)))

(defn ?comment-expectation-string [z]
  (let [nodes-following-assertion (iterate z/right* z)
        ?sequence (comp seq sequence)]
    (when-let [[fst-line & rest]
               (?sequence
                (comp
                 (take-while z/whitespace-or-comment?)
                 (map z/string)
                 (drop-while (complement result-comment?))
                   ; stop searching at the first double line break
                 (take-while (complement #{"\n"}))
                   ; strip leading ;s from comments
                 (map #(string/replace-first % #"^\s*;+\s?" "")))
                nodes-following-assertion)]
      (let [s (string/trim (apply str (expectation-str fst-line) rest))]
        (when (seq s)
          s)))))

(defn expectation-data
  "Parse a string representing the expectation for a test expression and an
  expression type, returning a vector of `[type str]` (or nil if none).

  The expected result is designated by a =>-prefixed comment, either directly
  after or on a line following the test expression.

  For example:

    (+ 1 1) ;=> 2

    (+ 1 1)
    ;;=> 2

    (+ 1 1)
    ;; => 2"
  [test-sexpr-zloc]
  ; Get the zloc for a result comment node ("; => ...") directly following
  ; the test sexpr, if one exists
  (when-let [rcz (?result-comment-zloc test-sexpr-zloc)]
    ; Get the string following the => part of the comment (including to
    ; following lines) if one exists that isn't empty
    (if-let [ces (?comment-expectation-string rcz)]
      [(result-comment-type (z/string rcz)) ces]

      ; Otherwise, check if there's a sexpr directly following the empty
      ; result comment
      (when-let [sexpr (advance rcz
                                :to z/sexpr-able?
                                :through (tag? :whitespace))]
        [(result-comment-type (z/string rcz)) (z/string sexpr)]))))

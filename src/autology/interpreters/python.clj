(ns autology.interpreters.python
  (:require [clojure.string :as s]
            [autology.interpreters.common :refer [clean-str]]))

(declare evaluate)

(def libraries
  {"numpy" {'array identity
            'prod (partial apply *)}})

(defn eval-import
  [expr env]
  (let [[_import lib _as lname] (s/split expr #" ")]
    (reduce (fn [acc-env [fname f]]
              (assoc acc-env
                     (keyword (str lname "." fname))
                     f))
            env
            (get libraries lib))))

(defn eval-assignment
  [expr env]
  (let [[sym r] (map clean-str (s/split expr #"="))
        val (evaluate r env)]
    (assoc env (keyword sym) val)))

(defn eval-array
  [expr env]
  (let [stripped (apply str (rest (butlast (clean-str expr))))
        elements (map clean-str (s/split stripped #","))]
    (vec (map #(evaluate % env) elements))))

(defn eval-funcall
  [expr env]
  (let [[_ fname args] (re-find #"(\S*)\((.*)\)" expr)
        f (get env (keyword fname))]
    (f (evaluate args env))))

(defn eval-return
  [expr env]
  (let [ret-expr (clean-str (s/replace expr #"return" ""))]
    (evaluate ret-expr env)))

(defn evaluate
  [expr env]
  ;; Start by splitting based on lines. Really we should split by
  ;; indentation blocks, but we're not doing anything so fancy.
  (let [sub-exprs (map clean-str (remove s/blank? (s/split-lines expr)))]
    ;; Reduce evaluating the sub-expressions over the env.
    (reduce (fn [acc sub-expr]
              (cond
                ;; Add new functions to the environment from
                ;; a "library".
                (s/starts-with? sub-expr "import")
                (eval-import sub-expr acc)

                ;; If the expression contains an `=` then we're doing
                ;; an assignment, so we should add/update a variable
                ;; in our env.
                (re-find #"=" sub-expr)
                (eval-assignment sub-expr acc)

                ;; If the expression is just a number we can
                ;; read-string it.
                (re-matches #"\d+\.?\d*" sub-expr)
                (read-string sub-expr)

                ;; If the expression is a symbol, look it up in the
                ;; env.
                (re-matches #"[a-zA-Z]*" sub-expr)
                (get env (keyword sub-expr))

                ;; If the expression is a Python array we can just map
                ;; evaluate across it's elements.
                (re-matches #"\[.*\]" sub-expr)
                (eval-array sub-expr acc)

                ;; A function name followed by parens is a function
                ;; call.
                (re-matches #"\S*\(.*\)" sub-expr)
                (eval-funcall sub-expr acc)

                ;; If the expression starts with `return` then we
                ;; know we're at the end and should return the
                ;; resulting value.
                (s/starts-with? sub-expr "return")
                (eval-return sub-expr acc)))
            env
            sub-exprs)))

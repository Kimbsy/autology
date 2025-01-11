(ns autology.interpreters.c
  (:require [clojure.string :as s]
            [autology.interpreters.c :as c]))

(declare evaluate)

(def example
  "printf(\"Hello World!\n\");

int x = 0;
for (int i = 13; i < 16; i++) {
  x = x + i;
}

return x;")

(def clean-str (comp s/trim #(apply str %)))

(defn split-for-expression
  [expr]
  (loop [acc-chars []
         open-braces 0
         [h & tail] expr]
    (if (#{\}} h)
      (if (= 1 open-braces)
        [(clean-str (conj acc-chars h)) (clean-str tail)]
        (recur (conj acc-chars h) (dec open-braces) tail))
      (if (#{\{} h)
        (recur (conj acc-chars h) (inc open-braces) tail)
        (recur (conj acc-chars h) open-braces tail)))))

(defn c-split
  [init-expr]
  (loop [subs []
         expr init-expr]
    (if (s/blank? expr)
      subs
      (if (not (s/starts-with? expr "for"))
        (let [sub-expr (clean-str (take-while (complement #{\;}) expr))
              remaining (clean-str (drop 1 (drop-while (complement #{\;}) expr)))]
          (recur (conj subs sub-expr) remaining))
        (let [[sub-expr remaining] (split-for-expression expr)]
          (recur (conj subs sub-expr) remaining))))))

(defn eval-arithmetic
  [state expr]
  ;; only allows 2 variables, good enough
  (let [[a op b] (map read-string (s/split expr #" "))
        a-val (if (symbol? a)
                (get state (keyword a))
                a)
        b-val (if (symbol? b)
                (get state (keyword b))
                b)]
    ((eval op) a-val b-val)))

(defn eval-for
  [state expr]
  (let [[_ pre body] (re-find #"(?s)for \((.*?)\) \{(.*?)\}" expr)
        ;; assuming step is always ++
        [init condition _step] (map clean-str (s/split pre #";"))
        [sym-str val-str] (map clean-str (s/split init #"="))
        sym (keyword (last (s/split sym-str #" ")))
        init-val (read-string val-str)
        init-state (assoc state sym init-val)
        sentinel-val (read-string (last (s/split condition #" ")))
        vals (range init-val sentinel-val)]

    (reduce (fn [acc-state i-val]
              (evaluate (assoc acc-state sym i-val) body))
            init-state
            vals)))

(defn eval-printf
  [state expr]
  (let [content (last (re-find #"(?s)printf\(\"(.*)\"\)" expr))]
    (print content)
    state))

(defn eval-assignment
  [state expr]
  (let [[l r] (map clean-str (s/split expr #"="))
        sym (last (s/split l #" "))
        val (if (re-matches #"\d+\.?\d*" r)
              (read-string r)
              (eval-arithmetic state r))]
    (assoc state (keyword sym) val)))

(defn eval-return
  [state expr]
  (let [ret-expr (clean-str (s/replace expr #"return" ""))]
    (if (= 1 (count (s/split ret-expr #" ")))
      (if (re-matches #"\d+\.?\d*" ret-expr)
        (read-string ret-expr)
        (get state (keyword ret-expr)))
      (eval-arithmetic state ret-expr))))

(defn evaluate
  [state expr]
  ;; Start by splitting it up into chunks. We probably want to do
  ;; this greedily. Select up to the next semicolon unless the line
  ;; starts with `for` in which case select up to the closing brace.
  (let [sub-exprs (c-split expr)]
    ;; then we want to have some initial state, and reduce evaluating
    ;; the sub-expressions over the state.
    (reduce (fn [acc sub-expr]
              (cond
                ;; if the expression starts with `for` then we're
                ;; gonna need to deconstrut the for loop and do some
                ;; recursion probably. We'll handle this first as it
                ;; will likely also trigger other cond cases here.
                (s/starts-with? sub-expr "for")
                (eval-for acc sub-expr)

                ;; if the expression starts with `printf` then we
                ;; need to do some printing.
                (s/starts-with? sub-expr "printf")
                (eval-printf acc sub-expr)

                ;; if the expression contains an `=` then we're doing
                ;; an assignment, so we should add/update a variable
                ;; in our state.
                (re-find #"=" sub-expr)
                (eval-assignment acc sub-expr)

                ;; if the expression starts with `return` then we
                ;; know we're at the end and should return the
                ;; resulting value.
                (s/starts-with? sub-expr "return")
                (eval-return acc sub-expr)))
            state
            sub-exprs)))

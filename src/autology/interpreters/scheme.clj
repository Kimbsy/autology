(ns autology.interpreters.scheme)

(declare evaluate*)

(defn self-evaluating?
  [expr]
  (or (number? expr)
      (string? expr)))

(def variable? symbol?)

(defn lookup-variable-value
  [sym env]
  (get env sym))

(defn tagged-list?
  [expr tag]
  (and (list? expr)
       (= tag (first expr))))

(defn quoted?
  [expr]
  (tagged-list? expr 'quote))

(defn text-of-quotation
  [expr]
  (rest expr))

(defn assignment?
  [expr]
  (tagged-list? expr 'set!))

(defn eval-assignment
  [expr env]
  (let [[_set! sym sub-expr] expr]
    (assoc env sym (evaluate* sub-expr env))))

(defn definition?
  [expr]
  (tagged-list? expr 'define))

(defn eval-variable-definition
  [expr env]
  (let [[_define sym sub-expr] expr]
    (assoc env sym (evaluate* sub-expr env))))

(defn extend-env
  [env syms values]
  (reduce (fn [acc-env [s v]]
            (assoc acc-env s v))
          env
          (zipmap syms values)))

(defn make-procedure
  [params body env]
  (fn [& values]
    (evaluate* body (extend-env env params values))))

(defn eval-function-definition
  [expr env]
  (let [[_define [sym & params] body] expr]
    (assoc env sym (make-procedure params body env))))

(defn eval-definition
  [expr env]
  (if (symbol? (second expr))
    (eval-variable-definition expr env)
    (eval-function-definition expr env)))

(defn if?
  [expr]
  (tagged-list? expr 'if))

(defn eval-if
  [expr env]
  (let [[_if predicate consequent alternative] expr]
    (if (evaluate* predicate env)
      (evaluate* consequent env)
      (evaluate* alternative env))))

(defn lambda?
  [expr]
  (tagged-list? expr 'lambda))

(defn lambda-parameters
  [expr]
  (second expr))

(defn lambda-body
  [expr]
  (last expr))

(defn begin?
  [expr]
  (tagged-list? expr 'begin))

(defn eval-sequence
  [exprs env]
  (reduce (fn [acc-env expr]
            (evaluate* expr acc-env))
          env
          exprs))

(defn begin-actions
  [expr]
  (rest expr))

(defn cond?
  [expr]
  (tagged-list? expr 'cond))

(defn clauses->if
  [[[predicate consequent] & clauses]]
  (if (= 'else predicate)
    consequent
    (concat (list 'if predicate consequent)
            (list (when (seq clauses) (clauses->if clauses))))))

(defn cond->if
  [expr]
  (let [clauses (rest expr)]
    (clauses->if clauses)))

(defn application?
  [expr]
  (list? expr))

(defn operator
  [expr]
  (first expr))

(defn operands
  [expr]
  (rest expr))

(defn list-of-values
  [exprs env]
  (map #(evaluate* % env) exprs))

(defn evaluate*
  "This is inspired by the `eval` function taken straight from SICP,
  Chapter 4.1, 'The Metacircular Evaluator'.

  The implementation of the individual functions differs as we're
  writing this in Clojure, not Scheme, but it's nice to show that we
  can have the language fully specified in the same way."
  [expr env]
  (cond
    (self-evaluating? expr) expr
    (variable? expr) (lookup-variable-value expr env)
    (quoted? expr) (text-of-quotation expr)
    (assignment? expr) (eval-assignment expr env)
    (definition? expr) (eval-definition expr env)
    (if? expr) (eval-if expr env)
    (lambda? expr) (make-procedure (lambda-parameters expr)
                                   (lambda-body expr)
                                   env)
    (begin? expr) (eval-sequence (begin-actions expr) env)
    (cond? expr) (evaluate* (cond->if expr) env)
    (application? expr) (apply (evaluate* (operator expr) env)
                               (list-of-values (operands expr)
                                               env))))

(defn evaluate
  "Our Scheme program may be multiple separate expressions so we wrap it
  in a list and reduce over evaluating them."
  [expr-str env]
  (let [exprs (read-string (str "(" expr-str ")"))]
    (eval-sequence exprs env)))

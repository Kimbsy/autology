(ns autology.core
  (:gen-class)
  (:require [autology.interpreters.c :as c]
            [autology.interpreters.debug :as debug]
            [autology.interpreters.python :as python]
            [autology.interpreters.scheme :as scheme]
            [clojure.string :as s]
            [clojure.walk :refer [postwalk]]))

(defn wrap-unsafe
  "Takes an autology program as a string.

  Returns a version where the potentially unsafe bodies of any
  `with-*i*` expressions are wrapped in strings. This version should
  be safe to `clojure.core/read-string`."
  [input-expr]
  (loop [out []
         [h & tail :as expr] input-expr]
    (if (s/blank? expr)
      (apply str out)
      (if (s/starts-with? expr "(with-*i*")
        ;; we know out expression starts with a `(`, need to take chars
        ;; till we get to balanced parens. Then return a clojure list of
        ;; with-*i*, the interpreter symbol and a string of the body.
        (let [[with-i remaining]
              (loop [with-i-expr []
                     open-parens 0
                     [h & tail] expr]
                (if (#{\)} h)
                  (if (= 1 open-parens)
                    ;; done
                    [(apply str (conj with-i-expr h))
                     (apply str tail)]
                    ;; closing nested paren
                    (recur (conj with-i-expr h) (dec open-parens) tail))
                  (if (#{\(} h)
                    ;; opening new nested paren
                    (recur (conj with-i-expr h) (inc open-parens) tail)
                    ;; non-paren char
                    (recur (conj with-i-expr h) open-parens tail))))
              [_with-i-sym interpreter body] (re-find #"(?s)\(with-\*i\*\s+(\S+)\s+((?:.|\n)*)\)" with-i)]
          (recur (conj out (str "(with-*i* " interpreter " \"" (s/escape body {\" "\\\""}) "\")"))
                 (apply str (drop (count with-i) expr))))
        ;; otherwise, take a safe char
        (recur (conj out h) (apply str tail))))))

;; Uses of `evaluate` in this definition refer to the function
;; `autology.core/evaluate` defined below which will get the
;; interpreter from the execution environment.
;;
;; We mark each section with an `:atl/foo` qualified keyword, this
;; allows us to easily replace sections in our program (including
;; inserting new markers for subsequent modification).
;;
;; We want to keep this interpreter fully defined in this one
;; expression, no named functions, only lambdas, that way it's easy to
;; modify from within an autology program.
(def initial-interpreter
  '(:atl/all
    (fn [e env]
      (:atl/list-switch
       (if-not (list? e)
         ;; evaluate an atom
         (:atl/eval-atom
          (if (symbol? e)
            (get env e)
            (:atl/literal e)))
         ;; evaluate a list
         (:atl/eval-list
          (case (first e)
            ;; we need our own symbol for quote so the code we're
            ;; writing isn't seen as the clojure quote special form.
            qu (:atl/quote (second e))

            ;; @TODO: need the func special form so we can write
            ;; meaningful programs.

            ;; Rebind the special *i* symbol to a predefined
            ;; interpreter, then evaluate the body.
            with-*i* (:atl/with-i
                      (let [[interpreter body] (rest e)]
                        ;; The `body` is a string at this point, the
                        ;; interpreter is expected to handle this in an
                        ;; appropriate way.
                        (autology.core/evaluate body (assoc env '*i* (get env interpreter)))))

            bind (:atl/bind
                  (let [bindings (partition 2 (second e))]
                    (autology.core/evaluate
                     (nth e 2)
                     (reduce (fn [acc-env [n v]]
                               (assoc acc-env n (autology.core/evaluate v acc-env)))
                             env
                             bindings))))
            
            ;; default to function application
            (:atl/function-application
             (apply (autology.core/evaluate (first e) env)
                    (map (fn [arg] (autology.core/evaluate arg env))
                         (rest e)))))))))))

(defn strip-markers
  "Remove all the `:atl/foo` markers from a interpreter data
  structure."
  [e]
  (if (list? e)
    (if (and (keyword? (first e))
             (= "atl" (namespace (first e))))
      (first (map strip-markers (rest e)))
      (map strip-markers e))
    e))

(defn get-marker
  "Get the expression wrapped by the specified `:atl/foo` marker."
  [expr marker]
  (if (list? expr)
    (if (= marker (first expr))
      (second expr)
      (first (keep #(get-marker % marker) (rest expr))))
    nil))

(defn replace-marker
  "Replace the contents of a marked expression in an interpreter with a
  new expression.

  The new expression will not be wrapped in the existing marker, since
  that marker might want to be renamed or even removed."
  [interpreter marker new-expression]
  (postwalk
   (fn [expr]
     (if (and (list? expr)
              (= marker (first expr)))
       new-expression
       expr))
   interpreter))

(def initial-env
  {'+ +
   '* *
   '= =
   'prn prn
   'first first
   'last last
   'nth nth
   'drop-last drop-last
   'reverse reverse
   'rest rest
   'conj conj
   'list list
   'concat concat

   ;; Interpreter-editing utils
   'strip-markers strip-markers
   'get-marker get-marker
   'replace-marker replace-marker

   ;; The Autology interpreter
   '*i* initial-interpreter

   ;; Other available interpreters
   '*debug* debug/evaluate
   '*c* c/evaluate
   '*python* python/evaluate
   '*scheme* scheme/evaluate
   })

(defn evaluate
  "Grab the interpreter out of the execution environment, strip all
  the markers out, evaluate it (as a Clojure function) and use it to
  evaluate the expression."
  ([e]
   (autology.core/evaluate e initial-env))
  ([e env]
   ((eval (strip-markers (get env '*i*))) e env)))

(defn eval-string
  [s]
  (-> s
      wrap-unsafe
      read-string
      evaluate))

(defn eval-file
  [filename]
  (eval-string (slurp filename)))

(defn repl
  []
  (while true
    (newline)
    (print "> ")
    (flush)
    ;; @TODO: could loop the `read-line` till we have balanced parens to allow multi-line expressions.
    (prn (eval-string (read-line)))))

(defn -main
  [& args]
  ;; We bind `*ns*` to be `autology.core` so we ensure we're able to
  ;; access it regardless of how autology is run (REPL, script, jar
  ;; etc).
  (binding [*ns* (the-ns 'autology.core)]
    (if args
      (prn (eval-file (first args)))
      (do
        (print "repl:\n")
        (flush)
        (repl)))))

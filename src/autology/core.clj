(ns autology.core
  (:gen-class)
  (:require [clojure.walk :refer [postwalk]]))



;; @TODO: we want some easy `with-*i*` function/special form to allow us to switch between existing interpreters.




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

            ;; @TODO: need the func special form so we can start saving
            ;; our interpreter modification functions into the
            ;; environment

            bind (:atl/bind
                  (let [bindings (partition 2 (second e))]
                    (evaluate
                     (nth e 2)
                     (reduce (fn [acc-env [n v]]
                               (assoc acc-env n (evaluate v acc-env)))
                             env
                             bindings))))
            
            ;; default to function application
            (:atl/function-application
             (apply (evaluate (first e))
                    (map (fn [arg] (evaluate arg env))
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
   'prn prn
   'nth nth
   'last last
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

   ;; the autology interpreter
   '*i*
   initial-interpreter})

(defn evaluate
  "Grab the interpreter out of the execution environment, strip all
  the markers out, evaluate it (as a Clojure function) and use it to
  evaluate the expression."
  ([e]
   (evaluate e initial-env))
  ([e env]
   ((eval (strip-markers (get env '*i*))) e env)))

(def eval-string (comp evaluate read-string))

(defn eval-file
  [filename]
  (eval-string (slurp filename)))

(defn repl
  []
  (while true
    (newline)
    (print "> ")
    (flush)
    (prn (evaluate (read-string (read-line))))))

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

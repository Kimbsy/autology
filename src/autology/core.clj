(ns autology.core
  (:gen-class))

;; Uses of `evaluate` in this definition refer to the function
;; `autology.core/evaluate` defined below which will get the
;; interpreter from the execution environment.
;;
;; We want to keep this interpreter fully defined in this one
;; expression, no named functions, that way it's easy to modify from
;; within an autology program.
(def initial-interpreter
  '(fn [e env]
     (if-not (coll? e)
       ;; evaluate an atom
       (if (symbol? e)
         (get env e)
         e)
       ;; evaluate a list
       (case (first e)
         ;; we need our own symbol for quote so the code we're writing isn't seen as the clojure quote special form.
         qu (second e)

         ;; @TODO: need the func special form so we can start saving
         ;; our interpreter modification functions into the
         ;; environment

         bind (let [bindings (partition 2 (second e))]
                (evaluate (nth e 2)
                          (reduce (fn [acc-env [n v]]
                                    (assoc acc-env n (evaluate v acc-env)))
                                  env
                                  bindings)))

         ;; default to function application
         (apply (evaluate (first e))
                (map (fn [arg] (evaluate arg env))
                     (rest e)))))))

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

   ;; the autology interpreter
   '*i*
   initial-interpreter})

(defn evaluate
  ([e]
   (evaluate e initial-env))
  ([e env]
   ;; grab the interpreter out of the execution environment ad use it
   ;; to evaluate the expression.
   ((eval (get env '*i*)) e env)))

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

;; @TODO: this doesn't seem to be working properly when passing in a filename
(defn -main
  [& args]
  (prn args)
  (if args
    (prn (eval-file (first args)))

    (do
      (print "repl:\n")
      (flush)
      (repl))))

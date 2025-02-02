(ns autology.interpreters.debug)

(declare evaluate*)

;; These are the keys in the `autology.core/initial-env` map.
;; @TODO: keep this up to date
(def initial-env-keys
  '(* *c* *debug* *i* *python* *scheme* + = butlast concat conj first get-marker last list nth prn replace-marker rest reverse strip-markers))

(defn print-locals
  [env]
  (println "locals:\n")
  (println (reduce dissoc env initial-env-keys))
  (newline)
  (newline))

(defn eval-expr
  [env]
  (println "evaluating in current lexical scope:")
  (let [expr (read-line)]
    (prn (evaluate* (read-string expr) env false)))
  (newline)
  (newline))

(defn evaluate*
  ([e env]
   (evaluate* e env true))
  ([e env debug?]
   (when debug?
     (println "=> " e)
     (println "[DEBUG] press `enter` to continue,
        `l` to print locals,
        `e` to eval expression in current environment\n")
     (flush)
     (let [command (read-line)]
       (case command
         "l" (print-locals env)
         "e" (eval-expr env)
         "" (newline)
         (prn "unrecognised option, continuing"))))
   (if-not (list? e)
     (if (symbol? e)
       (get env e)
       e)
     (case (first e)
       qu (second e)
       with-*i* (let [[interpreter body] (rest e)]
                  (evaluate* body (assoc env '*i* (eval interpreter)) debug?))

       bind (let [bindings (partition 2 (second e))]
              (evaluate*
               (nth e 2)
               (reduce (fn [acc-env [n v]]
                         (assoc acc-env n (evaluate* v acc-env)))
                       env
                       bindings)
               debug?))
       (apply (evaluate* (first e) env debug?)
              (map (fn [arg] (evaluate* arg env debug?))
                   (rest e)))))))

(defn evaluate
  "Since we know the body is valid Clojure code, we can just use
  `read-string`."
  [e-str env]
  (evaluate* (read-string e-str) env))

(ns autology.interpreters.python)

(def evaluate
  '(fn [e env]
     (let [initial-fns {'print (fn [args env]
                                 (prn args)
                                 env)
                        'insert (fn [xs args env]
                                  (let [[_ idx-str value-str] (re-find #"(.*?)\, (.*)" args)
                                        idx (read-string idx-str)
                                        value (autology.core/evaluate value-str env)]
                                    (concat (take idx xs)
                                            [(map symbol value)]
                                            (drop idx xs))))}
           libraries {"numpy" {'array (fn [a env] a)
                               'prod (fn foo [args env] (apply * args))}}]
       ;; We need to define all functions inline so we can quote the whole
       ;; interpreter form.
       (letfn [(clean-str [s]
                 (clojure.string/trim (apply str s)))
               (eval-import [expr env]
                 (let [[_import lib _as lname] (clojure.string/split expr #" ")]
                   (reduce (fn [acc-env [fname f]]
                             (assoc acc-env
                                    (symbol (str lname "." fname))
                                    f))
                           env
                           (get libraries lib))))

               (eval-assignment [expr env]
                 (let [[sym r] (map clean-str (clojure.string/split expr #"="))
                       val (autology.core/evaluate r env)]
                   (assoc env (symbol sym) val)))

               (eval-array [expr env]
                 (let [stripped (apply str (rest (butlast (clean-str expr))))
                       elements (map clean-str (clojure.string/split stripped #","))]
                   (vec (map #(autology.core/evaluate % env) elements))))

               (eval-funcall [expr env]
                 (let [fenv (merge env initial-fns)
                       [_ fname args] (re-find #"(\S*)\((.*)\)" expr)]
                   (if (re-matches #"qu\(.*\)" expr)
                     (read-string (second (re-find #"qu\((.*)\)" expr)))
                     (if (re-matches #".*\.insert\(.*\)" expr)
                       (let [[_ x] (re-find #"(.*)\.insert.*" expr)
                             new ((get fenv 'insert)
                                  (get fenv (symbol x))
                                  args
                                  env)]
                         (assoc env (symbol x) new))
                       (let [f (get fenv (symbol fname))]
                         (f (autology.core/evaluate args env) env))))))

               (eval-return [expr env]
                 (let [ret-expr (clean-str (clojure.string/replace expr #"return" ""))]
                   (autology.core/evaluate ret-expr env)))]    

         ;; Start by splitting based on lines. Really we should split by
         ;; indentation blocks, but we're not doing anything so fancy.
         (let [sub-exprs (map clean-str (remove clojure.string/blank? (clojure.string/split-lines e)))]
           ;; Reduce evaluating the sub-expressions over the env.
           (reduce (fn [acc sub-expr]
                     (cond
                       ;; Add new functions to the environment from
                       ;; a "library".
                       (clojure.string/starts-with? sub-expr "import")
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
                       (re-matches #"[a-zA-Z\*]*" sub-expr)
                       (get env (symbol sub-expr))

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
                       (clojure.string/starts-with? sub-expr "return")
                       (eval-return sub-expr acc)))
                   env
                   sub-exprs))))))

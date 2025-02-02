(ns autology.core-test
  (:require [autology.core :refer [eval-file eval-string evaluate]]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(deftest basic-evaluation-test
  (testing "data literals are evaluated correctly"
    (is (= 42 (evaluate 42)))
    (is (= "foo" (evaluate "foo")))
    (is (= :bar (evaluate :bar)))
    (is (= nil (evaluate nil)))

    (is (= 42 (eval-string "42")))
    (is (= "foo" (eval-string "\"foo\"")))
    (is (= :bar (eval-string ":bar")))
    (is (= nil (eval-string "nil"))))

  (testing "simple function application is evaluated correctly"
    (is (= 2 (evaluate '(+ 1 1))))
    (is (= 15 (evaluate '(+ 1 2 3 4 5))))
    (is (= '(:a :b :c) (evaluate '(list :a :b :c))))

    (is (= 2 (eval-string "(+ 1 1)")))
    (is (= 15 (eval-string "(+ 1 2 3 4 5)")))
    (is (= '(:a :b :c) (eval-string "(list :a :b :c)"))))

  (testing "nested function application is evaluate correctly"
    (is (= 20 (evaluate '(+ 5 (+ 10 (+ 2 3))))))
    (is (= :a (evaluate '(first (list :a :b :c)))))

    (is (= 20 (eval-string "(+ 5 (+ 10 (+ 2 3)))")))
    (is (= :a (eval-string "(first (list :a :b :c))")))))

(deftest quoting-test
  (testing "the `qu` special form quotes expressions correctly"
    (is (= 42 (evaluate '(qu 42))))
    (is (= '(:a :b :c) (evaluate '(qu (:a :b :c)))))
    (is (= '(+ 1 1) (evaluate '(qu (+ 1 1)))))
    (is (= '+ (evaluate '(first (qu (+ 1 1))))))
    (is (= '(qu (+ 1 1)) (evaluate '(qu (qu (+ 1 1))))))
    (is (= '(qu (qu (+ 1 1))) (evaluate '(qu (qu (qu (+ 1 1)))))))

    (is (= 42 (eval-string "(qu 42)")))
    (is (= '(:a :b :c) (eval-string "(qu (:a :b :c))")))
    (is (= '(+ 1 1) (eval-string "(qu (+ 1 1))")))
    (is (= '+ (eval-string "(first (qu (+ 1 1)))")))
    (is (= '(qu (+ 1 1)) (eval-string "(qu (qu (+ 1 1)))")))
    (is (= '(qu (qu (+ 1 1))) (eval-string "(qu (qu (qu (+ 1 1))))")))))

(def simple-bind
  '(bind (a 10)
         a))

(def expression-bind
  '(bind (a (+ 5 5))
         a))

(def multi-variable-bind
  '(bind (a 10
          b 20)
         (list a b)))

(def nested-bind
  '(bind (a 10)
         (bind (b 20)
               (list a b))))

(def dependant-variable-bind
  '(bind (a 10
          b (+ a 10))
         (list a b)))

(def shadowing-bind
  '(bind (a 10
          b 20)
         (list a
               b
               (bind (a 11
                      b 22)
                     (list a b)))))

(deftest binding-test
  (testing "the `bind` special form binds values to symbols in the environment correctly"
    (is (= 10 (evaluate simple-bind)))
    (is (= 10 (evaluate expression-bind)))
    (is (= '(10 20) (evaluate multi-variable-bind)))
    (is (= '(10 20) (evaluate nested-bind)))
    (is (= '(10 20) (evaluate dependant-variable-bind)))
    (is (= '(10 20 (11 22)) (evaluate shadowing-bind)))

    (is (= 10 (eval-string (str simple-bind))))
    (is (= 10 (eval-string (str expression-bind))))
    (is (= '(10 20) (eval-string (str multi-variable-bind))))
    (is (= '(10 20) (eval-string (str nested-bind))))
    (is (= '(10 20) (eval-string (str dependant-variable-bind))))
    (is (= '(10 20 (11 22)) (eval-string (str shadowing-bind))))))

(deftest eval-file-test
  (testing "programs can be read from files and evaluated correctly"
    (testing "normal evaluation order"
        (let [out
              (with-out-str
                (is (= 42 (eval-file (io/resource "examples/eval-order-1.atl")))))]
          (is (= "1\n2\n39\n" out))))
    
    (testing "reverse evaluation order"
      (let [out
            (with-out-str
              (is (= 42 (eval-file (io/resource "examples/eval-order-1.atl")))))]
        (is (= "1\n2\n39\n" out))))))

(deftest feature-add-test
  (testing "we can add significantly complex features to the language"
    (is (= 42.0 (eval-file (io/resource "examples/functions.atl"))))))

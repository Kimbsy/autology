(ns autology.interpreters.inter-lingual-test
  (:require [autology.core :refer [eval-file]]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(deftest scheme-python-c-interpreter-test
  (let [out (with-out-str
              (is (= 126 (eval-file (io/resource "examples/inter-lingual-4.atl")))))]
    (is (= "Hello World!\n" out))))

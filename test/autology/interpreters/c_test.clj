(ns autology.interpreters.c-test
  (:require [autology.core :refer [eval-file]]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(deftest c-interpreter-test
  (let [out (with-out-str
              (is (= 42 (eval-file (io/resource "inter-lingual-3.atl")))))]
    (is (= "Hello World!\n" out))))

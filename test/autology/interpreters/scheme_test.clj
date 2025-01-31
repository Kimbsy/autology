(ns autology.interpreters.scheme-test
  (:require [autology.core :refer [eval-file]]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(deftest scheme-interpreter-test
  (is (= 42 (eval-file (io/resource "examples/inter-lingual-1.atl")))))

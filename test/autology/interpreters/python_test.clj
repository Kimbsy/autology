(ns autology.interpreters.python-test
  (:require [autology.core :refer [eval-file]]
            [clojure.java.io :as io]
            [clojure.test :refer :all]))

(deftest python-interpreter-test
  (is (= 42 (eval-file (io/resource "examples/inter-lingual-2.atl")))))

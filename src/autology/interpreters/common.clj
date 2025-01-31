(ns autology.interpreters.common
  (:require [clojure.string :refer [trim]]))

(def clean-str (comp trim #(apply str %)))

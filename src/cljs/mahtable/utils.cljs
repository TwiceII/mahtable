(ns mahtable.utils
  (:require
    [clojure.string :as cstr]))

(defn value-or-empty-str
  "Либо какое-то значение либо пустая строка"
  [v]
  (if v
    v
    ""))


(defn nil-or-empty? [x]
  (or (nil? x) (empty? x)))

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


(defn index-of-pred
  "Получить индекс элемента в списке coll, удовлетворяющий условию pred"
  [pred coll]
  (first (keep-indexed (fn [idx x]
                         (when (pred x)
                           idx))
                       coll)))

(defn random-from-coll
  "Получить рандомное значение из вектора элементов coll"
  [coll]
  (get coll (rand-int (count coll))))


(defn get-number-with-decimals-str
  "Вывести число с разделениями, запятой и т.д"
  ([input] (get-number-with-decimals-str input false))
  ([input show-dash-if-zero?]
   (if (nil? input)
     ""
     (if (and show-dash-if-zero? (= input 0))
      "-"
       (let [js-reg (js/RegExp. "\\B(?=(\\d{3})+(?!\\d))" "g")
            n (.replace (.toString input) js-reg " ")]
        n)))))

(defn money-str
  "Основной способ отображения денег (для поступления денег)
  с '-' вместо 0"
  [input]
  (get-number-with-decimals-str (js/Math.round input)
                                true))

(defn money-str-with-zero
  "Отображение денег с 0"
  [input]
  (get-number-with-decimals-str (js/Math.round input)
                                false))

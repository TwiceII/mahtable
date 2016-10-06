(ns mahtable.dom-utils
  (:require
    [clojure.string :as str]))

(defn q [selector]
  (js/document.querySelector selector))

(defn rcomp->js$
  [react-comp]
  (js/$ (.findDOMNode js/ReactDOM react-comp)))

(defn find-in-rcomp [react-comp sel-str]
  (let [elem (.find (rcomp->js$ react-comp) sel-str)]
    (aget elem 0)))


(defn set-value! [el value]
  (set! (.-value el) value))

(defn value [el]
  (when-not (nil? el)
    (let [val (.-value el)]
      (when-not (str/blank? val)
        (str/trim val)))))


(defn target-value [e]
  "Получить значение инпута через e.target.value"
  (-> e
      .-target
      value))


(defn date-value [el]
  (when-let [val (value el)]
    (let [val (js/Date.parse val)]
      (when-not (js/isNaN val)
        (js/Date. val)))))

(defn array-value [el]
  (when-let [val (value el)]
    (str/split val #"\s+")))


(defn value-from-dom
  [selector]
  (value (q selector)))



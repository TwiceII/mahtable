(ns mahtable.core
  (:require [rum.core :as rum]))

(enable-console-print!)

(println "hey hey")

(defonce app-state (atom
                     {:columns [{:name "Бренд" :width 10
                                 :field-name :brand}
                                {:name "Товар" :width 15
                                 :field-name :good}
                                {:name "Продажи (тг)" :width 13
                                 :field-name :salesSum}
                                {:name "Продажи (шт)" :width 13
                                 :field-name :salesAmount}
                                {:name "Маржа (тг)" :width 12
                                 :field-name :margin}
                                {:name "ТМЗ (тг)" :width 12
                                 :field-name :tmzSales}
                                {:name "ТМЗ (шт)" :width 10
                                 :field-name :tmzAmount}
                                {:name "% от общей прибыли" :width 15
                                 :field-name :salesPercent}]
                      :rows []}))


(defn random-row
  []
  {:brand (str "масло " (rand-int 100))
   :good (str "масло Sunar" (rand-int 100))
   :salesSum (rand-int 10000000)
   :salesAmount (rand-int 10000)
   :margin (rand-int 8000000)
   :tmzSales (rand-int 80000) :tmzAmount (rand-int 800)
   :salesPercent (rand 100)})

(rum/defc header-th-view
  [column]
  [:th (:name column)])


(rum/defc header-filter-th-view
  [column]
  [:th "="])


(rum/defc row-td-view
  [row field-name]
  [:td (get row field-name)])


(rum/defc row-view
  [row columns]
  [:tr
   (map #(row-td-view row (:field-name %))
        columns)])


(rum/defc body-part-view
  [columns rows]
  [:div.table-body-div
   [:table {:cellSpacing 0}
    [:colgroup
     (map #(-> [:col {:style {:width (str (:width %) "%")}}])
          columns)]
    [:tbody
     (map #(row-view % columns) rows)]]])


(rum/defc footer-part-view
  [columns]
  [:div.table-footer-div
   [:table {:cellSpacing 0}
    [:colgroup
     (map #(-> [:col {:style {:width (str (:width %) "%")}}])
          columns)]
    [:tbody
     [:tr
      [:td "Итого"]
      [:td]
      [:td 0]
      [:td 0]
      [:td]
      [:td]
      [:td]
      [:td]]]]])


(rum/defc header-part-view
  [columns]
  [:div.table-header-div
   [:table {:cellSpacing 0}
    [:colgroup
     (map #(-> [:col {:style {:width (str (:width %) "%")}}])
          columns)]
    [:thead
     [:tr
      (map header-th-view columns)]
     [:tr
      (map header-filter-th-view columns)]]]])


(rum/defc mahtable-view
  [columns rows]
  [:div#table-div

  (header-part-view columns)

  (body-part-view columns rows)

  (footer-part-view columns)])


(defn init
  []
  (let [rand-rows (for [x (range 100)]
                    (random-row))]
    (reset! (rum/cursor-in app-state [:rows]) rand-rows)
    (rum/mount (mahtable-view (:columns @app-state)
                          (:rows @app-state))
           (.getElementById js/document "table-div"))))


(init)















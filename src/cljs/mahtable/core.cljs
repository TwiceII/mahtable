(ns mahtable.core
  (:require [rum.core :as rum]))

(enable-console-print!)

(println "hey hey")

(def default-sort-type :asc)

(defn column->col-filter
  [column]
  (case (:field-type column)
    :text {:filter-type :text
           :search-str nil}
    :numeric {:filter-type :numeric
              :search-str nil
              :filter-cond nil}))

(def columns [{:name "Бренд" :width 10
               :field-name :brand
               :field-type :text }
              {:name "Товар" :width 15
               :field-name :good
               :field-type :text }
              {:name "Продажи (тг)" :width 13
               :field-name :salesSum
               :field-type :numeric }
              {:name "Продажи (шт)" :width 13
               :field-name :salesAmount
               :field-type :numeric }
              {:name "Маржа (тг)" :width 12
               :field-name :margin
               :field-type :numeric }
              {:name "ТМЗ (тг)" :width 12
               :field-name :tmzSales
               :field-type :numeric }
              {:name "ТМЗ (шт)" :width 10
               :field-name :tmzAmount
               :field-type :numeric }
              {:name "% от общей прибыли" :width 15
               :field-name :salesPercent
               :field-type :numeric }])

(defonce app-state (atom
                     {:columns columns
                      :rows []

                      :sort-params {:sort-column nil
                                    :sort-type default-sort-type}

                      :col-filters (map column->col-filter columns)
                      }))





(defn get-sorted-rows
  [rows field-name field-type sort-type]
  (let [sort-func (if (= field-type :numeric)
                    (if (= sort-type :asc)
                      > <)
                    (if (= sort-type :asc)
                      #(compare %1 %2) #(compare %2 %1)))]
    (sort-by field-name sort-func rows)))


(defn sort-rows!
  [sort-params]
  (println "sort-rows!")
  (let [rows-cursor (rum/cursor-in app-state [:rows])
        rows @rows-cursor
        column (:sort-column sort-params)
        sorted-rows (get-sorted-rows rows
                                     (:field-name column)
                                     (:field-type column)
                                     (:sort-type sort-params))]
    (reset! rows-cursor sorted-rows)))


(add-watch (rum/cursor-in app-state [:sort-params])
           :sort-watcher
           (fn [k a old-s new-s]
             (println "watch sort-params")
             (println old-s)
             (println new-s)
             (sort-rows! new-s)))

(defn random-row
  []
  {:brand (str "масло " (rand-int 100))
   :good (str "масло Sunar" (rand-int 100))
   :salesSum (rand-int 10000000)
   :salesAmount (rand-int 10000)
   :margin (rand-int 8000000)
   :tmzSales (rand-int 80000) :tmzAmount (rand-int 800)
   :salesPercent (rand 100)})

;; (let [rows (for [x (range 5)]
;;              (random-row))
;;       sorted-rows (get-sorted-rows rows
;;                                    :brand
;;                                    :numeric
;;                                    :desc)]
;;   (println rows)
;;   (println sorted-rows))



(defn toggle-sort-type
  [sort-type]
  (if (= sort-type :asc) :desc :asc))


(defn make-column-sorted
  [column]
  (let [sort-cursor (rum/cursor-in app-state [:sort-params])
        prev-sort-params @sort-cursor
        new-sort-type (if (= column
                             (:sort-column prev-sort-params))
                        (toggle-sort-type (:sort-type prev-sort-params))
                        default-sort-type)]
    (reset! sort-cursor {:sort-column column
                         :sort-type new-sort-type})))

(rum/defc header-th-view
  [column sort? sort-type]
  [:th
   {:on-click #(make-column-sorted column)}
   (when sort?
     [:i {:style {:float "right"}
          :class (str "icon sort "
                      (if (= sort-type :asc) "ascending " "descending ")
                      (if (= (:field-type column)
                             :numeric) "numeric" "alphabet"))}])
   (:name column)
   ])


(rum/defc header-filter-th-view
  "Строка для фильтров в заголовке"
  [col-filter]
  [:th.filter-th
;;    (case (:filter-type col-filter)
;;      :numeric "="
;;      :text "<>")
   [:div.ui.fluid.labeled.input
    [:div.ui.basic.label.filter-btn
    (case (:filter-type col-filter)
     :numeric "="
     :text "<>")]
    [:input {:type "text"
             :class "filter-input"
             :style {:text-align "right"}
             }]]])


(rum/defc row-td-view
  "Ячейка строки"
  [row column]
  [:td
   {:style {:text-align (if (= (:field-type column) :numeric)
                          "right" "left")}}
   (get row (:field-name column))])


(rum/defc row-view
  [row columns]
  [:tr
   (map #(row-td-view row %)
        columns)])


(rum/defc body-part-view < {:after-render (fn[state]
                                            (js/moveBodyScrollUp)
                                            state) }
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
  "Заголовок таблицы с названиями столбцов,
   сортировкой и фильтрами"
  [columns col-filters sort-params]
  [:div.table-header-div
   [:table {:cellSpacing 0}
    [:colgroup
     (map #(-> [:col {:style {:width (str (:width %) "%")}}])
          columns)]
    [:thead
     ;; заголовок с сортировкой
     [:tr
      (map #(header-th-view %
                            (= % (:sort-column sort-params))
                            (:sort-type sort-params))
           columns)]
     ;; фильтры
     [:tr
      (map header-filter-th-view col-filters)]]]])


(rum/defc mahtable-view < rum/reactive {:after-render (fn[state]
                                                        (js/alignHeaderWidths)
                                                        state)}
  [appstate]
  (let [columns (:columns (rum/react appstate))
        rows (:rows (rum/react appstate))
        sort-params (:sort-params (rum/react appstate))
        col-filters (:col-filters (rum/react appstate))]
    [:div#table-div

    (header-part-view columns col-filters sort-params)

    (body-part-view columns rows)

    (footer-part-view columns)]))


(defn init
  []
  (let [rand-rows (for [x (range 100)]
                    (random-row))]
    (reset! (rum/cursor-in app-state [:rows]) rand-rows)
    (rum/mount (mahtable-view app-state)
               (.getElementById js/document "table-div"))))


(init)















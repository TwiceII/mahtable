(ns mahtable.core
  (:require [mahtable.utils :as u]
            [mahtable.dom-utils :as dom]
            [cljs.core.async :as a]
            [rum.core :as rum])
  (:require-macros [cljs.core.async.macros :as am]))

(enable-console-print!)

(println "hey hey")

(def default-sort-type :asc)

(defn column->init-col-filter
  "Получить начальные параметры фильтра для столбца"
  [column]
  (case (:field-type column)
    :text {:col-index (:index column)
           :field-name (:field-name column)
           :filter-type :text
           :search-str nil}
    :numeric {:col-index (:index column)
              :field-name (:field-name column)
              :filter-type :numeric
              :search-str nil
              :filter-cond nil}))

;;; настройки столбцов
(def columns [{:index 1
               :name "Бренд" :width 10
               :field-name :brand
               :field-type :text }
              {:index 2
               :name "Товар" :width 15
               :field-name :good
               :field-type :text }
              {:index 3
               :name "Продажи (тг)" :width 13
               :field-name :salesSum
               :field-type :numeric }
              {:index 4
               :name "Продажи (шт)" :width 13
               :field-name :salesAmount
               :field-type :numeric }
              {:index 5
               :name "Маржа (тг)" :width 12
               :field-name :margin
               :field-type :numeric }
              {:index 6
               :name "ТМЗ (тг)" :width 12
               :field-name :tmzSales
               :field-type :numeric }
              {:index 7
               :name "ТМЗ (шт)" :width 10
               :field-name :tmzAmount
               :field-type :numeric }
              {:index 8
               :name "% от общей прибыли" :width 15
               :field-name :salesPercent
               :field-type :numeric }])

;;; изначальный список строк
(def init-rows (atom []))



;;; состояние всей таблицы
(defonce app-state (atom
                     {:columns columns
                      ;; показываемый список строк (с фильтрами, сортировкой и т.д)
                      :rows []

                      ;; список настроек по сортировкам
                      :sort-params {:sort-column nil
                                    :sort-type default-sort-type}

                      ;; параметры по включенным фильтрам для столбцов
                      :active-col-filters {}

                      ;; признак загрузки
                      :loading? false

                      ;; id выбранной строки
                      :selected-row-id nil
                      }))

(defn app-cursor
  [& path]
  (rum/cursor-in app-state (into [] path)))



(defn loading-on!
  []
  (println "loading-on!")
  (reset! (app-cursor :loading?) true))


(defn loading-off!
  []
  (println "loading-off!")
  (reset! (app-cursor :loading?) false))


(def load-ch (a/chan))
(am/go (while true
         (let [table-ready? (a/<! load-ch)]
           (println "load-ch incoming!")
           (when (and table-ready?
                      (= true @(app-cursor :loading?)))
             (loading-off!)))))


(defn notify-loaded
  []
  (println "!!! notify-loaded !!!")
  (am/go (a/>! load-ch true)))


(defn select-row
  "Выделить строку по id"
  [row-id]
  (println "SELECTED ROW: " row-id)
  (reset! (app-cursor :selected-row-id) row-id))

(defn deselect-row
  "Отменить выделение строки"
  []
  (println "DESELECTED ROW")
  (reset! (app-cursor :selected-row-id) nil))


(defn get-sorted-rows
  [rows field-name field-type sort-type]
  ;; если есть все поля
  (if (and field-name
           field-type
           sort-type)
    (let [sort-func (if (= field-type :numeric)
                      (if (= sort-type :asc)
                        > <)
                      (if (= sort-type :asc)
                        #(compare %1 %2) #(compare %2 %1)))]
      (sort-by field-name sort-func rows))
    ;; иначе не сортируем
    rows))


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


(defn filter-rows-numeric
  [rows filter-cond field-name value]
  (filter #((case filter-cond ; получаем оператор сравнения
              :gt >
              :lt <)
            (get % field-name) ; зн-ие поля
            value)
          rows))


(defn filter-rows!
  [filter-params]
  (println "filter-rows!")
  (let [init-rows @init-rows
        filtered-rows (if (u/nil-or-empty? filter-params)
                        init-rows
                        (reduce-kv (fn [rows flt-index flt-params]
                                     (-> rows
                                         (#(if (:search-str flt-params)
                                             (filter-rows-numeric %
                                                                  (:filter-cond flt-params)
                                                                  (:field-name flt-params)
                                                                  (:search-str flt-params))
                                             %))))
                                   init-rows
                                   filter-params))]
    (when (= filtered-rows init-rows) (loading-off!))
    (reset! (rum/cursor-in app-state [:rows]) filtered-rows)))



(def redraw-table-ch (a/chan))

(am/go (while true
         (let [redraw-type (a/<! redraw-table-ch)]
           (println ">>> redraw table")
           (case redraw-type
             :sort-redraw (sort-rows! @(app-cursor :sort-params))
             :filter-redraw (do
                              (filter-rows! @(app-cursor :active-col-filters))
                              (sort-rows! @(app-cursor :sort-params)))))))

(defn start-redrawing-table
  [redraw-type]
  (loading-on!)
  (deselect-row)
  (js/moveBodyScrollUp)
  (am/go (a/>! redraw-table-ch redraw-type)))

;;; для сортировок
(add-watch (rum/cursor-in app-state [:sort-params])
           :sort-watcher
           (fn [k a old-s new-s]
             (start-redrawing-table :sort-redraw)
             ;(loading-off!)
             ))

;;; для фильтров
(add-watch (rum/cursor-in app-state [:active-col-filters])
           :filters-watcher
           (fn [k a old-s new-s]
             (println ":filters-watcher")
             (println new-s)
             (when (or (some #(not (nil? (get-in % [1 :search-str]))) new-s)
                       (u/nil-or-empty? new-s))
               (start-redrawing-table :filter-redraw))
;;                ;; фильтруем
;;                (filter-rows! new-s)
;;                ;; заодно сортируем
;;                (sort-rows! @(rum/cursor-in app-state [:sort-params]))
;;                ;(loading-off!)
             ))


(defn random-row
  [row-number]
  {:id row-number
   :brand (str "масло " (rand-int 100))
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


(defn click-numeric-filter
  "При щелчке по фильтру для числовых значений"
  [f-cond column prev-filter]
  (swap! (rum/cursor-in app-state [:active-col-filters])
         ;; если фильтр какой-то уже стоит на столбце
         #(if prev-filter
            ;; изменяем только одно поле
            (assoc-in % [(:index column) :filter-cond] f-cond)
            ;; иначе создаем первоначальную инфу по фильтру
            (assoc % (:index column)
              (assoc (column->init-col-filter column)
                                     :filter-cond f-cond))))
;;   (println @(rum/cursor-in app-state [:active-col-filters]))
  )

(def click-gt-filter (partial click-numeric-filter :gt))
(def click-lt-filter (partial click-numeric-filter :lt))


(defn click-off-filter
  "Отключить фильтр для столбца"
  [column]
  (swap! (rum/cursor-in app-state [:active-col-filters])
         #(dissoc % (:index column)))
;;   (println @(rum/cursor-in app-state [:active-col-filters]))
  )


(defn change-col-filter-search-str
  [column value]
  (println "change-col-filter-search-str " value)
  (swap! (rum/cursor-in app-state [:active-col-filters])
         #(assoc-in % [(:index column) :search-str] value))
;;   (println @(rum/cursor-in app-state [:active-col-filters]))
  )

(defn show-filter-popup
  [evt]
  (.popup (.popup (js/$. (.-target evt))
                  (js-obj "popup" (js/$. ".ui.popup")
                          "hoverable" true
                          "on" "manual"
                          "hideOnScroll" false
                          "exclusive" true
                          "position" "bottom left"))
          "show"))

;;; =======================================
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


(rum/defc numeric-filter-view
  "Фильтр для числовых значений"
  [column active-col-filter]
  [:div.ui.fluid.labeled.input
   [:div.ui.dropdown.icon.label.filter-btn
    (if active-col-filter
      (if (= (:filter-cond active-col-filter) :gt) ">" "<")
      [:i.filter.icon])
    [:div.menu
     [:div.item {:class (if (= (:filter-cond active-col-filter) :gt) "active" "")
                 :on-click #(click-gt-filter column active-col-filter)}
      "больше чем"]
     [:div.item {:class (if (= (:filter-cond active-col-filter) :lt) "active" "")
                 :on-click #(click-lt-filter column active-col-filter)}
      "меньше чем"]
     [:div.item {:class (if-not active-col-filter "active")
                 :on-click #(click-off-filter column)}
      "отключить"]]]

   [:input {:type "text"
            :class "filter-input"
            :disabled (nil? active-col-filter)
            :style {:text-align "right"}
            :value (u/value-or-empty-str (:search-str active-col-filter))
            :on-change #(change-col-filter-search-str column
                                                      (dom/target-value %))
            }]])

(rum/defc text-filter-view
  "Фильтр для текстовых значений"
  [column active-col-filter]
  [:div.ui.fluid.labeled.input
   [:div.ui.icon.label.filter-btn
    {:on-click #(show-filter-popup %)}
    [:i.filter.icon]]
   [:input {:type "text"
            :class "filter-input"
            :value (u/value-or-empty-str (:search-str active-col-filter))
            :on-change #(println "text " (dom/target-value %))
            }]])


(rum/defc header-filter-th-view < rum/reactive
  "Строка для фильтров в заголовке"
  [column active-col-filter]
  [:th.filter-th
   (case (:field-type column)
     :numeric (numeric-filter-view column active-col-filter)
     :text (text-filter-view column active-col-filter))])


(rum/defc row-td-view
  "Ячейка строки"
  [row column]
  [:td
   {:style {:text-align (if (= (:field-type column) :numeric)
                          "right" "left")}}
   (get row (:field-name column))])


(defn from-args-state
  [state index]
  (as-> state a
        (:rum/args a)
        (into [] a)
        (get a index)))


(rum/defcs row-view < {:should-update (fn [old-st new-st]
                                       (let [old-row (from-args-state old-st 0)
                                             new-row (from-args-state new-st 0)
                                             old-sel? (from-args-state old-st 2)
                                             new-sel? (from-args-state new-st 2)]
                                         (or (not= old-row new-row)
                                             (not= old-sel? new-sel?))))}
  [state row columns selected?]
  [:tr {:class (if selected? "selected-row" "")
        :on-click #(select-row (:id row))}
   (map #(rum/with-key (row-td-view row %) (:index %))
        columns)])


(rum/defc body-part-view < rum/reactive {:after-render (fn[state]
                                                         (notify-loaded)
                                                         state) }
  [columns rows-cursor selected-row-id-cursor]
  (println "body-part-view")
  (let [rows (rum/react rows-cursor)
        selected-row-id (rum/react selected-row-id-cursor)]
    [:div.table-body-div
     [:table {:cellSpacing 0}
      [:colgroup
       (map #(-> [:col {:style {:width (str (:width %) "%")}
                        :key (:index %)
                        }])
            columns)]
      [:tbody
       (map #(rum/with-key (row-view %
                                     columns
                                     (= selected-row-id (:id %)))
                           (:id %)) rows)]]]))


(rum/defc footer-part-view
  [columns]
  [:div.table-footer-div
   [:table {:cellSpacing 0}
    [:colgroup
     (map #(-> [:col {:style {:width (str (:width %) "%")}
                      :key (:index %)}])
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


(rum/defc header-part-view < rum/reactive
  "Заголовок таблицы с названиями столбцов,
   сортировкой и фильтрами"
  [columns col-filters-cursor sort-params-cursor]
  (let [col-filters (rum/react col-filters-cursor)
        sort-params (rum/react sort-params-cursor)]
    [:div.table-header-div
     [:table {:cellSpacing 0}
      [:colgroup
       (map #(-> [:col {:style {:width (str (:width %) "%")}
                        :key (:index %)}])
            columns)]
      [:thead
       ;; заголовок с сортировкой
       [:tr
        (map #(rum/with-key (header-th-view %
                                            (= % (:sort-column sort-params))
                                            (:sort-type sort-params)) (:index %))
             columns)]
       ;; фильтры
       [:tr
        (map #(rum/with-key
                (header-filter-th-view % (get col-filters (:index %)))
                (:index %))
             columns)]]]]))


(rum/defc mahtable-view < {:after-render (fn[state]
                                           (js/alignHeaderWidths)
                                           state)}
  [appstate]
  (let [columns (:columns @appstate)]
    [:div#table-div

    (header-part-view columns
                      (app-cursor :active-col-filters)
                      (app-cursor :sort-params))

    (body-part-view columns
                    (app-cursor :rows)
                    (app-cursor :selected-row-id))

    (footer-part-view columns)]))


(rum/defc rows-count-view < rum/reactive
  [rows-cursor]
  [:div "Всего строк: " (count (rum/react rows-cursor))])


(rum/defc loading-label-view < rum/reactive {:after-render (fn[state]
                                                         (println "dafaq did-update")
                                                         state) }
  [loading-cursor]
  (let [loading? (rum/react loading-cursor)]
    [:div {:style {:visibility (if loading? "visible" "hidden")}}
     [:div.ui.active.centered.inline.loader
      {:style {:margin-bottom "5px"}}]]))
;;     [:div
;;      {:class (if loading? "load-cl" "")}
;;      "zzz"]))


(rum/defc text-filter-popup-view
  []
  [:div.ui.basic.popup
   [:div.ui.list
    [:div.item
     [:div.ui.checkbox
      [:input {:type "checkbox" :name "some1"}]
      [:label {:for "some1"} "Масло 27"]]]
    [:div.item
     [:div.ui.checkbox
      [:input {:type "checkbox" :name "some2"}]
      [:label {:for "some2"} "Масло 28"]]]
    ]])

(defn init
  []
  (let [rand-rows (for [x (range 300)]
                    (random-row x))]
    (reset! init-rows rand-rows)
    (reset! (rum/cursor-in app-state [:rows]) @init-rows)
    (rum/mount (loading-label-view (app-cursor :loading?))
               (.getElementById js/document "loading-div"))
    (rum/mount (mahtable-view app-state)
               (.getElementById js/document "table-div"))
    (rum/mount (text-filter-popup-view)
               (.getElementById js/document "textfilterpopup-div"))
    (rum/mount (rows-count-view (app-cursor :rows))
               (.getElementById js/document "rows-count-div"))))


(init)















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

                      ;; показывать ли текстовый фильтр
                      :show-text-filter? false

                      ;; показываемые чекбоксы для текст.фильтра (должны быть в векторе!)
                      :current-filter-checkboxes nil
                      ;; пример мэпа для чекбокса:
                      ;; [{:index 1
                      ;;  :name ""
                      ;;  :checked? true/false}]
                      ;; кнопка "выбрать все"
                      :select-all-checkbox {:name "Выбрать все"
                                            :checked? true
                                            :indeterminate? false}
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


;;; -------------------------------------------
;;; Фильтрование
(defn filter-rows-numeric
  "Отфильтровать строки по числу"
  [rows filter-cond field-name value]
  (filter #((case filter-cond ; получаем оператор сравнения
              :gt >
              :lt <)
            (get % field-name) ; зн-ие поля
            value)
          rows))


(defn filter-rows-text
  "Отфильтровать строки по тексту"
  [rows field-name value]
  (filter #(re-find (re-pattern value) (get % field-name)) rows))


(defn get-filtered-rows
  "Получить отфильтрованные строки из общего списка строк и параметров"
  [init-rows filter-params]
  (if (u/nil-or-empty? filter-params)
    init-rows
    (reduce-kv (fn [rows flt-index flt-params]
                 (println "flt-params: " flt-params)
                 (-> rows
                     (#(if (:search-str flt-params)
                         (case (:filter-type flt-params)
                           :numeric (filter-rows-numeric %
                                                         (:filter-cond flt-params)
                                                         (:field-name flt-params)
                                                         (:search-str flt-params))
                           :text (filter-rows-text %
                                                   (:field-name flt-params)
                                                   (:search-str flt-params)))
                         %))))
               init-rows
               filter-params)))
;;; -------------------------------------------

(defn filter-rows!
  [filter-params]
  (println "filter-rows!")
  (println filter-params)
  (let [init-rows @init-rows
        filtered-rows (get-filtered-rows init-rows filter-params)]
    (when (= filtered-rows init-rows) (loading-off!))
    (reset! (rum/cursor-in app-state [:rows]) filtered-rows)
    ))



(def redraw-table-ch (a/chan))

(am/go (while true
         (let [redraw-type (a/<! redraw-table-ch)]
           (println ">>> redraw table")
           (case redraw-type
             :sort-redraw (sort-rows! @(app-cursor :sort-params))
             :filter-redraw (do
                              (filter-rows! @(app-cursor :active-col-filters))
                              (sort-rows! @(app-cursor :sort-params))))
           ;; если нет строк, таблица не будет перерисовываться
;;            (when (u/nil-or-empty? @(app-cursor :rows))
;;              (loading-off!))
           )))

(defn start-redrawing-table
  [redraw-type]
  (loading-on!)
  (deselect-row)
  (reset! (app-cursor :show-text-filter?) false)
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
             ;; если есть какое-то значение для числового поиска
;;              (when (or (some #(and (not (nil? (get-in % [1 :search-str])))
;;                                    (= :numeric (get-in % [1 :filter-type])))
;;                              new-s)
;;                        ;; если обнулили
;;                        (u/nil-or-empty? new-s))
               (start-redrawing-table :filter-redraw)
;;              )
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


(defn change-numeric-filter-search-str
  [column active-col-filter value]
  (println "change-NUM-filter-search-str " value)
  (swap! (rum/cursor-in app-state [:active-col-filters])
         #(assoc-in % [(:index column) :search-str] value))
;;   (println @(rum/cursor-in app-state [:active-col-filters]))
  )

(defn change-text-filter-search-str
  [column active-col-filter value]
  (println "change-TEXT-filter-search-str " value)
  (swap! (rum/cursor-in app-state [:active-col-filters])
         ;; если уже был фильтр на эту колонну
         #(if active-col-filter
            ;; изменяем одно поле
            (assoc-in % [(:index column) :search-str] value)
            ;; иначе создаем первоначальную инфу по фильтру
            (assoc % (:index column)
              (assoc (column->init-col-filter column)
                                     :search-str value))))
;;   (println @(rum/cursor-in app-state [:active-col-filters]))
  )


(defn current-filter-checkboxes-to-column
  "Получить список чекбоксов для фильтра для конкр.столбца"
  [column]
  (let [c-rows (get-filtered-rows @init-rows
                                  @(app-cursor :active-col-filters))]
    (into [] (map-indexed (fn[i r]
                            {:index i
                             :name (get r (:field-name column))
                             :checked? true})
                         c-rows))))


(defn show-filter-popup
  "Показать контекстное меню для добавления записи в категорию"
  [evt column]
  (do
    (println "show-filter-popup!")
    (let [top-offset (-> (.-target evt)
                         (js/$. )
                         (.offset)
                         (.-top)
                         (+ 20)
                         (str "px"))
          left-offset (-> (.-target evt)
                          (js/$. )
                          (.offset)
                          (.-left)
                          (+ 0)
                          (str "px"))]
      (.css (js/$. ".text-filter-window") #js {:top top-offset
                                               :left left-offset}))
    ;; обновляем список чекбоксов для этого столбца
    (reset! (app-cursor :current-filter-checkboxes) (current-filter-checkboxes-to-column column))
    ;; ставим признак, чтобы показывать попап
    (reset! (app-cursor :show-text-filter?) true)
    (println (app-cursor :show-text-filter?))))


(defn close-text-filter-popup
  "Закрыть контекстное меню"
  []
  (reset! (app-cursor :show-text-filter?) false))


(defn click-select-all-checkbox
  "Клик по 'выбрать все' чекбоксу"
  []
  (let [select-all-checkbox @(app-cursor :select-all-checkbox)
        checkboxes @(app-cursor :current-filter-checkboxes)
        toggle-all-to (fn [check?]
                        (do
                          (swap! (app-cursor :select-all-checkbox)
                                 #(-> %
                                      (assoc :checked? check?)
                                      (assoc :indeterminate? false)))
                          (swap! (app-cursor :current-filter-checkboxes)
                                 #(into [] (map (fn[chm](assoc chm :checked? check?)) %)))))]
    (toggle-all-to (not (:checked? select-all-checkbox)))))



(defn click-filter-checkbox
  "Клик по чекбоксу из списка"
  [checkbox]
  ;; находим в списке и меняем щелкнутый чекбокс
  (swap! (app-cursor :current-filter-checkboxes)
         #(assoc-in % [(:index checkbox) :checked?] (not (:checked? checkbox))))
  ;; после этого делаем остальные проверки
  (let [checkboxes @(app-cursor :current-filter-checkboxes)]
    ;; если все выделены
    (when-not (some #(not (:checked? %)) checkboxes)
      (swap! (app-cursor :select-all-checkbox) #(-> %
                                                    (assoc :checked? true)
                                                    (assoc :indeterminate? false))))
    ;; если все отключены
    (when-not (some #(:checked? %) checkboxes)
      (swap! (app-cursor :select-all-checkbox) #(-> %
                                                    (assoc :checked? false)
                                                    (assoc :indeterminate? false))))
    ;; если часть включены, а часть нет
    (when (and (some #(:checked? %) checkboxes)
               (some #(not (:checked? %)) checkboxes))
      (swap! (app-cursor :select-all-checkbox) #(assoc % :indeterminate? true)))
    ))



;; (defn show-filter-popup
;;   [evt]
;;   (println "show-filter-popup!"))
;;   (.popup (.popup (js/$. (.-target evt))
;;                   (js-obj "popup" (js/$. ".ui.popup")
;;                           "hoverable" true
;;                           "on" "manual"
;;                           "hideOnScroll" false
;;                           "exclusive" true
;;                           "position" "bottom left"))
;;           "show"))

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

;;       [:i.icons
;;        [:i.filter.icon]
;;        [:i.corner.pin.icon {:style {:font-size ".6em"}}]]
      [:i.filter.icon]

      )
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
            :on-change #(change-numeric-filter-search-str column
                                                          active-col-filter
                                                          (dom/target-value %))
            }]])

(rum/defc text-filter-view
  "Фильтр для текстовых значений"
  [column active-col-filter]
  [:div.ui.fluid.labeled.input
   [:div.ui.icon.label.filter-btn
    {:on-click #(show-filter-popup % column)}
    [:i.filter.icon]]
   [:input {:type "text"
            :class "filter-input"
            :value (u/value-or-empty-str (:search-str active-col-filter))
            :on-change #(change-text-filter-search-str column
                                                       active-col-filter
                                                       (dom/target-value %))
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
                                                         state)}
  [columns rows-cursor selected-row-id-cursor loading-cursor]
  (println "body-part-view")
  (let [rows (rum/react rows-cursor)
        loading? (rum/react loading-cursor)
        selected-row-id (rum/react selected-row-id-cursor)]
    [:div.table-body-div
     (if (not (u/nil-or-empty? rows))
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
                             (:id %)) rows)]]
       [:div.ui.center.aligned.basic.segment "Не найдено"])]))


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
                    (app-cursor :selected-row-id)
                    (app-cursor :loading?))

    (footer-part-view columns)]))


(rum/defc rows-count-view < rum/reactive
  [rows-cursor]
  [:div "Всего строк: " (count (rum/react rows-cursor))])


(rum/defc loading-label-view < rum/reactive {:after-render (fn[state]
                                                         (println "dafaq did-update")
                                                         state) }
  [loading-cursor]
  (println "loading-cursor: " loading-cursor)
  (let [loading? (rum/react loading-cursor)]
    [:div {:style {:visibility (if loading? "visible" "hidden")}}
     [:div.ui.active.centered.inline.loader
      {:style {:margin-bottom "5px"}}]]))
;;     [:div
;;      {:class (if loading? "load-cl" "")}
;;      "zzz"]))



(rum/defc filter-checkbox-view < {:did-update (fn[state]
                                                (.checkbox (.find (dom/rcomp->js$ (:rum/react-component state))
                                                                  ".ui.checkbox"))
                                                state)}
  "Вьюшка для чекбокса из списка"
  [checkbox]
  [:div.item
   [:div.ui.checkbox
    {:class (if (:checked? checkbox) "checked" "")
     :on-click #(click-filter-checkbox checkbox)}
    [:input {:type "checkbox" :name (:id checkbox) :checked (:checked? checkbox)}]
    [:label (:name checkbox)]]])



(rum/defc select-all-checkbox-view < {:did-update (fn[state]
                                                    (let [$elem (dom/rcomp->js$ (:rum/react-component state))
                                                          s-all-ch (first (:rum/args state))]
                                                      (.checkbox $elem)
                                                      (if (:indeterminate? s-all-ch)
                                                        (.checkbox $elem "set indeterminate")
                                                        (.checkbox $elem "set determinate"))
                                                      state))}
  "Вьюшка для 'выбрать все' чекбокса"
  [select-all-checkbox]
  [:div.ui.master.checkbox
   {:class (if (:checked? select-all-checkbox) "checked" "")
    :on-click click-select-all-checkbox}
   [:input {:type "checkbox" :checked (:checked? select-all-checkbox)}]
   [:label (:name select-all-checkbox)]])



(rum/defc text-filter-popup-view  < rum/reactive {:after-render (fn[state]
                                                                   (.resizable (js/$ ".text-filter-window")
                                                                               (js-obj "minHeight" 200
                                                                                       "minWidth" 200))
                                                                  state)}
  "Вьюшка для фильтра с галочками"
  [show-cursor checkboxes-cursor select-all-checkbox-cursor]
;;   (println "show-cursor: " show-cursor)
  (let [show-text-filter? (rum/react show-cursor)
        checkboxes (rum/react checkboxes-cursor)
        select-all-checkbox (rum/react select-all-checkbox-cursor)]
    [:div.text-filter-window
     {:style {:visibility (if show-text-filter? "visible" "hidden")}}
     ;; список с чекбоксами
     [:div.item-list
      [:div.ui.list
       [:div.item
        ;; "выбрать все" чекбокс
        (select-all-checkbox-view select-all-checkbox)]]
      [:div.ui.divider]
      ;; остальные варианты
      [:div.ui.list.actual-items
       (map #(rum/with-key
               (filter-checkbox-view %) (:index %))
            checkboxes)
       ]]
     ;; кнопки
     [:div.bottom-buttons
      [:div.ui.tiny.button "Ок"]
      [:div.ui.tiny.button {:on-click #(close-text-filter-popup)} "Отмена"]]
     ]))


(defn init
  []
  (let [rand-rows (for [x (range 300)]
                    (random-row x))]
    (reset! init-rows rand-rows)
    (reset! (rum/cursor-in app-state [:rows]) @init-rows)
    ;(reset! (app-cursor :current-filter-checkboxes) (current-filter-checkboxes-from-rows))
    (rum/mount (loading-label-view (app-cursor :loading?))
               (.getElementById js/document "loading-div"))
    (rum/mount (mahtable-view app-state)
               (.getElementById js/document "table-div"))
    (rum/mount (text-filter-popup-view (app-cursor :show-text-filter?)
                                       (app-cursor :current-filter-checkboxes)
                                       (app-cursor :select-all-checkbox))
               (.getElementById js/document "textfilterpopup-div"))
    (rum/mount (rows-count-view (app-cursor :rows))
               (.getElementById js/document "rows-count-div"))))


(init)















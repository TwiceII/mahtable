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
           :field-name (:field-name column) ; поиск по строке
           :filter-type :text
           :chosen-elements nil ; выбранные элементы (чекбоксы)
           :search-str nil}
    :numeric {:col-index (:index column)
              :field-name (:field-name column)
              :filter-type :numeric
              :search-str nil
              :filter-cond nil}))

;;; список рандомных брендов
(def brands-list ["масло" "корнишоны" "vitaland" "еще бренд" "magnum"])

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
               :field-type :numeric
               :field-format :percent
               :no-total? true}])

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

                      ;; чекбоксы для текст.фильтров столбцов
                      :filter-checkboxes nil

                      ;;; ----------------------
                      ;;; настройки для чекбокс-текстового фильтра

                      ;; показываемые чекбоксы (должны быть в векторе!)
                      :current-filter-checkboxes nil
                      ;; пример мэпа для чекбокса:
                      ;; [{:index 1
                      ;;  :name ""
                      ;;  :checked? true/false}]
                      ;; кнопка "выбрать все"
                      :select-all-checkbox {:name "Выбрать все"
                                            :checked? true
                                            :indeterminate? false}
                      ;; для какого столбца в данный момент выбираем
                      :current-checkbox-filter-column nil
                      ;;; ----------------------

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
  (->> rows
       (filter (fn [r] (re-find (re-pattern value) (get r field-name))))
;;        ;; если указаны элементы чекбокса
;;        (#(if (not (u/nil-or-empty? chosen-elems))
;;            (filter (fn [r] (some (fn[c](= c (get r field-name))) chosen-elems)) %)
;;            %))
       ))


(defn get-filtered-rows
  "Получить отфильтрованные строки из общего списка строк и параметров
  (БЕЗ учета чекбоксов в фильтрах)"
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


(defn get-chosen-from-filtered
  "Получить выбранные (чекнутые) строки из отфильтрованных
  (т.е. учитываем чекбоксы)"
  [filtered-rows filter-params]
  (if (u/nil-or-empty? filter-params)
    filtered-rows
    (reduce-kv (fn [rows flt-index flt-param]
                 (->> rows
                      ;; если указаны элементы чекбокса
                      (#(if (not (u/nil-or-empty? (:chosen-elements flt-param)))
                          (filter (fn [r] (some (fn[c](= c (get r (:field-name flt-param))))
                                                (:chosen-elements flt-param))) %)
                          %))))
               filtered-rows
               filter-params)))

;;; -------------------------------------------


(defn init-checkboxes-of-filter-param
  "Получить начальные чекбоксы после фильтрации"
  [rows column col-filter-param]
  (->> rows
       (map #(get % (:field-name column)))
       distinct ; убираем повт.зн-ия
       (map-indexed (fn [i r]
                      {:index i
                       :name r
                       ;; щелкнуто либо если список выбр.элементов пустой,
                       ;; либо если есть этот элемент в списке
                       :checked? (boolean (or (u/nil-or-empty? (:chosen-elements col-filter-param))
                                              (some (fn[c] (= r c))
                                                    (:chosen-elements col-filter-param))))})) ; получаем зн-ия
       (sort-by :name <) ; сортируем
       (into [])))


(defn reset-checkboxes-for-columns!
  "Обновить значения чекбоксов для фильтров столбцов"
  [rows filter-params]
  (reset! (app-cursor :filter-checkboxes)
          (reduce (fn [m col]
                    (assoc m (:index col) (init-checkboxes-of-filter-param
                                            rows
                                            col
                                            (get filter-params (:index col))
                                            )))
                  {}
                  (filter #(= (:field-type %) :text) @(app-cursor :columns)))))


(defn filter-rows!
  "Отфильтровать строки
  (и получить значения чекбоксов для текст.фильтров)"
  [filter-params]
  (let [init-rows @init-rows
        filtered-rows (get-filtered-rows init-rows filter-params)
        filtered-and-chosen-rows (get-chosen-from-filtered filtered-rows filter-params)]
    ;; получаем значения чекбоксов для фильтров столбцов
    (reset-checkboxes-for-columns! filtered-rows filter-params)
    ;; выключаем загрузку, если кол-во строк не изменилось
    (when (= filtered-and-chosen-rows init-rows) (loading-off!))
    ;; ставим новые данные
    (reset! (app-cursor :rows) filtered-and-chosen-rows)
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

(declare close-text-filter-popup)

(defn start-redrawing-table
  [redraw-type]
  (loading-on!)
  (deselect-row)
  (close-text-filter-popup)
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
               (start-redrawing-table :filter-redraw)
             ))


(defn random-row
  [row-number]
  {:id row-number
   :brand (u/random-from-coll brands-list)
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
  "Отключить фильтр для столбца (числового)"
  [column]
  (swap! (rum/cursor-in app-state [:active-col-filters])
         #(dissoc % (:index column))))


(defn change-numeric-filter-search-str
  [column active-col-filter value]
  (swap! (rum/cursor-in app-state [:active-col-filters])
         #(assoc-in % [(:index column) :search-str] value)))

(defn change-text-filter-search-str
  [column active-col-filter value]
  (swap! (rum/cursor-in app-state [:active-col-filters])
         ;; если уже был фильтр на эту колонну
         #(if active-col-filter
            ;; изменяем одно поле
            (assoc-in % [(:index column) :search-str] value)
            ;; иначе создаем первоначальную инфу по фильтру
            (assoc % (:index column)
              (assoc (column->init-col-filter column)
                                     :search-str value)))))


(defn examine-checkboxes-and-select-all
  "Проверка чекбокса 'Выбрать все' относительно остальных чекбоксов"
  [checkboxes]
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
    (swap! (app-cursor :select-all-checkbox) #(assoc % :indeterminate? true))))


;;; при изменении списка чекбоксов
(add-watch (app-cursor :current-filter-checkboxes)
           :checkbox-list-watcher
           (fn [k a old-s new-s]
             (examine-checkboxes-and-select-all new-s)))



(defn show-filter-popup
  "Показать контекстное меню для добавления записи в категорию"
  [evt column]
  (do
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
    ;; ставим текущий проверяемый столбец
    (reset! (app-cursor :current-checkbox-filter-column) column)
    ;; обновляем список чекбоксов для этого столбца
    (reset! (app-cursor :current-filter-checkboxes)
            (get @(app-cursor :filter-checkboxes) (:index column)))
    ;; ставим признак, чтобы показывать попап
    (reset! (app-cursor :show-text-filter?) true)
    (println (app-cursor :show-text-filter?))))


;; (defn assoc-in-or-init-then-assoc
;;   [m k v init-val]
;;   (if (contains? m k)
;;     (assoc-in m

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
  (let [checkboxes @(app-cursor :current-filter-checkboxes)]
    ;; находим в списке и меняем щелкнутый чекбокс
    (swap! (app-cursor :current-filter-checkboxes)
           (fn[at](assoc-in at [(u/index-of-pred #(= (:index %) (:index checkbox))
                                                 checkboxes) :checked?] (not (:checked? checkbox)))))))


(defn click-ok-checkbox-filters
  "При щелчке по 'Ок' в чекбоксовом фильтре"
  []
  (let [column @(app-cursor :current-checkbox-filter-column)
        all-current-checkboxes @(app-cursor :current-filter-checkboxes)
        chosen-checkboxes (->> all-current-checkboxes
                               (filter #(= (:checked? %) true))
                               ;; если выбраны все - значит не выбрано ничего, по сути
                               (#(if (= (count %) (count all-current-checkboxes))
                                   nil
                                   %)))]
    (swap! (app-cursor :active-col-filters)
           ;; если такой фильтр на текст уже есть
           #(if (contains? % (:index column))
              (assoc-in % [(:index column) :chosen-elements]
                        (map :name chosen-checkboxes))
              ;; иначе создаем первоначальную инфу по фильтру
              (assoc % (:index column)
                (assoc (column->init-col-filter column)
                  :chosen-elements (map :name chosen-checkboxes)))))))


;;; =======================================
;;; Views
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
   (:name column)])


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
    (if (not (u/nil-or-empty? (:chosen-elements active-col-filter)))
      [:i.check.square.icon]
      [:i.filter.icon])]
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
   (let [v (get row (:field-name column))]
     (case (:field-type column)
       :numeric (if (:field-format column)
                  (case (:field-format column)
                    :percent (str (u/money-str-with-zero v) "%"))
                  (u/money-str-with-zero v))
       :text v))])


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


(defn summ-by-field
  "Получить сумму из элементов по полю"
  [rows field-name]
  (reduce (fn [s r] (+ s (get r field-name)))
          0 rows))


(rum/defc footer-part-view < rum/reactive
  "Итого в футере таблицы"
  [columns rows-cursor]
  (let [rows (rum/react rows-cursor)]
    [:div.table-footer-div
     [:table {:cellSpacing 0}
      [:colgroup
       (map #(-> [:col {:style {:width (str (:width %) "%")}
                        :key (:index %)}])
            columns)]
      [:tbody
       [:tr
        (map #(->
                [:td {:style {:font-weight "bold"
                              :text-align (if (= (:field-type %) :numeric)
                                            "right" "left")}}
                 (if (and (= (:field-type %) :numeric)
                          (not (:no-total? %)))
                   (u/money-str-with-zero (summ-by-field rows (:field-name %)))
                   "")])
             columns)]]]]))

;;         [:td "Итого"]
;;         [:td]
;;         [:td 0]
;;         [:td 0]
;;         [:td]
;;         [:td]
;;         [:td]
;;         [:td]]]]])


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

    (footer-part-view columns
                      (app-cursor :rows))]))


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
     (if (not (u/nil-or-empty? checkboxes))
       [:div.show-checkboxes
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
         [:div.ui.tiny.button {:on-click #(click-ok-checkbox-filters)} "Ок"]
         [:div.ui.tiny.button {:on-click #(close-text-filter-popup)} "Отмена"]]]
       [:div.not-found
        [:div "Не найдено"]
        [:div.ui.tiny.button {:on-click #(close-text-filter-popup)} "Закрыть"]])
     ]))


(defn init
  []
  (let [rand-rows (for [x (range 300)]
                    (random-row x))]
    (reset! init-rows rand-rows)
    (reset! (rum/cursor-in app-state [:rows]) @init-rows)
    (reset-checkboxes-for-columns! @init-rows @(app-cursor :active-col-filters))

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















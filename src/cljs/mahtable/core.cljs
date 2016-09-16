(ns mahtable.core
  (:require [rum.core :as rum]))

(enable-console-print!)

(println "hey hey")

(-> "table"
    js/$
    (.floatThead #js {:scrollContainer #(-> (js/$ "#tablePlaceholder"))}))


;; (-> "#tablePlaceholder table"
;;     js/$
;;     (.fixedHeaderTable #js {:height "400" :themeClass "fancyDarkTable"}))

;$('#tablePlaceholder table').fixedHeaderTable({ height: '400', altClass: 'odd', themeClass: 'fancyDarkTable' });

(ns mahtable.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [mahtable.core-test]
   [mahtable.common-test]))

(enable-console-print!)

(doo-tests 'mahtable.core-test
           'mahtable.common-test)

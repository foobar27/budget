(ns budget.test-runner
  (:require
   [doo.runner :refer-macros [doo-tests]]
   [budget.core-test]))

(enable-console-print!)

(doo-tests 'budget.core-test)

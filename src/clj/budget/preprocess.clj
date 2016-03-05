(ns budget.preprocess
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [cheshire.core :as json]))

;; TODO remove
;; (distinct (map (comp :type parse-line) depenses))
(def parse-type
  {"Chapitre" :chapitre
   "Département" :departement
   "Section" :section
   "Article" :article})

(defn parse-value [s]
  (if (> (.length s) 0)
    (Long/parseLong (.replaceAll s " " ""))
    0))

(defn parse-line [[type _ _ id label v2014 v2015 v2016 v2017 v2018 v2019 mention]]
  {:type (parse-type type)
   :id (into [] (string/split id #"\."))
   :label label
   :v2016 (parse-value v2016)})

(defn load-csv-file [f]
  (with-open [f (io/reader (io/file (io/resource f)))]
    (into [] (map parse-line (drop 1 (csv/read-csv f))))))

(defn parse-lines [rows]
  {:name "Budget"
   :children (loop [rows rows
                    result {}]
               (if (seq rows)
                 (let [row (first rows)
                       key (into [] (interpose :children (:id row)))]
                   (recur (rest rows)
                          (-> result
                              (assoc-in (conj key :name) (:label row))
                              (assoc-in (conj key :size) (:v2016 row))))) ;; TODO all values
                 result))})

(defn walk-node [{:keys [children] :as node}]
  (if (seq children)
    (assoc node :children (into [] (map walk-node (vals children))))
    node))

(comment
  (require '[clojure.pprint :refer [pprint]])
  (def depenses (load-csv-file "budget2016-depenses.csv"))
  (def root (parse-lines depenses))
  (spit (io/file "/tmp/foo.json") (json/generate-string (walk-node root)))
  ;;(pprint (walk-node nodes))
  )
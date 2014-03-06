(ns abc.core
  (:require [abc.extractors.ruby :refer [parse]]))

(defn metric-result [r]
  (fn [metric]
    (let [metric-result (count (get-in r [:metrics metric] '()))]
      (* metric-result metric-result))))

(defn abc [r]
  (assoc r :score
    (int (Math/sqrt (apply +
                      (map (metric-result r)
                           [:branches :assignments :conditionals]))))))

(defn abcs [rs]
  (map abc rs))

(defn main [file]
  (println (abcs (parse (slurp file)))))

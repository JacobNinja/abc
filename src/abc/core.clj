(ns abc.core)

(defn metric-result [rs]
  (fn [metric]
    (let [metric-result (get-in rs [:metrics metric] 0)]
      (* metric-result metric-result))))

(defn abc [rs]
  (Math/sqrt (apply +
                    (map (metric-result rs)
                         [:branches :assignments :conditionals]))))

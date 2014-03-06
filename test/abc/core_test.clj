(ns abc.core-test
  (:require [clojure.test :refer :all]
            [abc.core :refer :all]))

(defn generate-data [n]
  (repeat n {:line 2 :range [3 5]}))

(deftest abc-test
  (testing "metric"
    (is (= (:score (abc {:method "foo" :metrics {:conditionals (generate-data 2)
                                          :branches (generate-data 5)
                                          :assignments (generate-data 10)}}))
           11))
    (is (= (map :score (abcs [{:method "foo"
                               :metrics {:conditionals (generate-data 2)
                                         :branches (generate-data 5)
                                         :assignments (generate-data 10)}}
                              {:method "bar"
                               :metrics {:conditionals (generate-data 2)
                                         :branches (generate-data 5)
                                         :assignments (generate-data 10)}}]))
           [11 11]))))

(ns abc.core-test
  (:require [clojure.test :refer :all]
            [abc.core :refer :all]))

(deftest abc-test
  (testing "metric"
    (is (= (abc {:method "foo" :metrics {:conditionals 2
                                          :branches 5
                                          :assignments 10}}))
           11)))

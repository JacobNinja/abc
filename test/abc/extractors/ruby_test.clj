(ns abc.extractors.ruby-test
  (:require [clojure.test :refer :all]
            [abc.extractors.ruby :refer :all]))

(def ruby
"def foo
  bar = 3
  wat if condition
  method_call()
end")

(deftest ruby-extractor-test
  (testing "assignment"
    (is (= [{:line 2 :range [10 17] :source "bar = 3"}]
           (:assignments (first (parse ruby))))))

  (testing "conditional"
    (is (= [{:line 3 :range [20 36] :source "wat if condition"}]
           (:conditionals (first (parse ruby))))))

  (testing "branching"
    (is (= [{:line 4 :range [39 52] :source "method_call()"}]
           (:branches (first (parse ruby)))))))

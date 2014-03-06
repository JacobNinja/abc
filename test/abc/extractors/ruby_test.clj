(ns abc.extractors.ruby-test
  (:require [clojure.test :refer :all]
            [abc.extractors.ruby :refer :all]))

(defn wrap-method [code]
  (str "def foo" \newline code \newline "end"))

(defn parse-wrapped [code]
  (parse (wrap-method code)))

(defn make-extract-source [type]
  (fn [code]
    (map :source (type (:metrics (first (parse-wrapped code)))))))

(defn make-check-source [type]
  (let [extract-source (make-extract-source type)]
    (fn [code]
      (is (= code (first (extract-source code)))))))

(def if-condition
"if something
   stuff
 else
   do_not
 end")

(def case-condition
"case something
  when true then false
  else nil
end")

(deftest ruby-extractor-test
  (testing "method"
    (is (= "foo"
           (:method (first (parse-wrapped ""))))))

  (testing "assignment"
    (let [check-source (make-check-source :assignments)]
      (check-source "bar = 3")
      (check-source "@bar = 3")
      (check-source "$bar = 3")
      (check-source "@@bar = 3")
      (check-source "self.bar = 3")
      (check-source "Foo.bar = 3")
      (check-source "foo += 3")
      (check-source "foo &= 3")
      (check-source "foo |= 3")
      (check-source "foo >>= 3")
      (check-source "foo %= 3")
      (check-source "foo ^= 3")
      (check-source "foo *= 3")
      ))

  (testing "conditional"
    (let [check-source (make-check-source :conditionals)]
      (check-source "wat if condition")
      (check-source if-condition)
      (check-source "wat unless condition")
      (check-source case-condition)
      ;(check-source "foo ? true : false") ; ? true : false
      (check-source "foo == 42")
      (check-source "foo != 42")
      (check-source "foo <= 42")
      (check-source "foo >= 42")
      (check-source "foo > 42")
      (check-source "foo < 42")
      (check-source "foo <=> 42")
      (check-source "foo =~ 42")
      (check-source "foo ==~ 42")
      ))

  (testing "branching"
    (let [check-source (make-check-source :branches)
          extract-source (make-extract-source :branches)]
      (check-source "method_call()")
      (check-source "foo.()")
      (check-source "Foo.new")
      (check-source "foo 1, 2, 3")
      ;(check-source "foo[bar]") ; foo[bar
      (is (= [] (extract-source "foo != bar")))
      )))

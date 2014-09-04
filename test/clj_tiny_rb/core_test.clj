(ns clj-tiny-rb.core-test
  (:require [clojure.test :refer :all]
            [clj-tiny-rb.core :refer :all]))

(deftest test-sort
  (testing "Basic sort for documentation"
    (is (= [1 2 3] (seq (tree 3 1 2)))))
  (testing "I can sort a range of shuffled numbers"
    (let [coll (shuffle (range 1000))]
      (is (= (sort coll) (seq (seq->tree coll)))))))
(ns mp-in-clojure.cats.rpn-calculator-test
  (:require [clojure.test :refer :all]
            [mp-in-clojure.cats.rpn-calculator :refer :all]
            [cats.monad.either :as either]
            [cats.monad.maybe :as maybe]))

(deftest solve-rpn-test
  (testing "without monads"
   (is (= (solve-rpn "1 2 * 4 +") 6.0))
   (is (= (solve-rpn "1 2 * 4 + 5 *") 30.0))
   (is (= (solve-rpn "1 2 * 4") 4.0))
   (is (thrown? NumberFormatException
                (solve-rpn "1 8 wharglbllargh")))))

(deftest solve-rpn'-test
  (testing "with maybe monad"
    (is (= (solve-rpn' folding-with-maybe "1 2 * 4 +")
           (maybe/just 6.0)))
    (is (= (solve-rpn' folding-with-maybe "1 2 * 4 + 5 *")
           (maybe/just 30.0)))
    (is (= (solve-rpn' folding-with-maybe "1 2 * 4")
           (maybe/nothing)))
    (is (= (solve-rpn' folding-with-maybe "1 8 wharglbllargh")
           (maybe/nothing))))
  (testing "with either monad"
    (is (= (solve-rpn' folding-with-either "1 2 * 4 +")
           (either/right 6.0)))
    (is (= (solve-rpn' folding-with-either "1 2 * 4 + 5 *")
           (either/right 30.0)))
    (is (= (solve-rpn' folding-with-either "1 2 * 4")
           (either/left)))
    (is (= (solve-rpn' folding-with-either "1 8 wharglbllargh")
           (either/left "invalid input")))))

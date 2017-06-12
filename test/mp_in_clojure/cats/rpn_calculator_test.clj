(ns mp-in-clojure.cats.rpn-calculator-test
  (:require [clojure.test :as t]
            [mp-in-clojure.cats.rpn-calculator :refer :all]
            [cats.monad.maybe :as maybe]))

(t/deftest solve-rpn-test
  (t/testing "without monads"
    (t/is (= (solve-rpn "1 2 * 4 +") 6.0))
    (t/is (= (solve-rpn "1 2 * 4 + 5 *") 30.0))
    (t/is (= (solve-rpn "1 2 * 4") 4.0))
    (t/is (thrown? NumberFormatException
                   (solve-rpn "1 8 wharglbllargh")))))

(t/deftest solve-rpn'-test
  (t/testing "with maybe monad"
    (t/is (= (solve-rpn'  "1 2 * 4 +") (maybe/just 6.0)))
    (t/is (= (solve-rpn' "1 2 * 4 + 5 *") (maybe/just 30.0)))
    (t/is (= (solve-rpn' "1 2 * 4") (maybe/nothing)))
    (t/is (= (solve-rpn' "1 8 wharglbllargh") (maybe/nothing)))))

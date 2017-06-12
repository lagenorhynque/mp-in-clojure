(ns mp-in-clojure.algo.dist-test
  (:require [clojure.test :as t]
            [mp-in-clojure.algo.dist :refer :all]
            [clojure.algo.monads :as m]))

;;; Monty Hall problem

(t/deftest monty-hall-problem
  (let [doors #{:a :b :c}
        dist1 (m/domonad dist-m
                [prize (uniform doors)
                 choice (uniform doors)]
                (if (= choice prize)
                  :win
                  :lose))
        dist2 (m/domonad dist-m
                [prize (uniform doors)
                 choice (uniform doors)
                 opened (uniform (disj doors prize choice))
                 choice' (uniform (disj doors opened choice))]
                (if (= choice' prize)
                  :win
                  :lose))]
    (t/is (= dist1 (dist :win 1/3, :lose 2/3)))
    (t/is (= dist2 (dist :win 2/3, :lose 1/3)))))

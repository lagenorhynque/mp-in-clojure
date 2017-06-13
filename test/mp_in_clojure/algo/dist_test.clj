(ns mp-in-clojure.algo.dist-test
  (:require [clojure.test :as t]
            [mp-in-clojure.algo.dist :refer :all]
            [clojure.algo.monads :as m]))

;;; throwing dice

(t/deftest throwing-dice
  (let [die (range 1 (inc 6))
        dist (prob->dist
              (m/domonad prob-m
                [d1 (uniform die)
                 d2 (uniform die)]
                (+ d1 d2)))]
    (t/is (= dist {2 1/36, 3 2/36, 4 3/36, 5 4/36, 6 5/36, 7 6/36,
                   8 5/36, 9 4/36, 10 3/36, 11 2/36, 12 1/36}))))

;;; Monty Hall problem

(t/deftest monty-hall-problem
  (let [doors #{:a :b :c}
        dist1 (prob->dist
               (m/domonad prob-m
                 [prize (uniform doors)
                  choice (uniform doors)]
                 (if (= choice prize)
                   :win
                   :lose)))
        dist2 (prob->dist
               (m/domonad prob-m
                 [prize (uniform doors)
                  choice (uniform doors)
                  opened (uniform (disj doors prize choice))
                  choice' (uniform (disj doors opened choice))]
                 (if (= choice' prize)
                   :win
                   :lose)))]
    (t/is (= dist1 {:win 1/3, :lose 2/3}))
    (t/is (= dist2 {:win 2/3, :lose 1/3}))))

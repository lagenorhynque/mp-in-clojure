(ns mp-in-clojure.algo.dist-test
  (:require [clojure.test :as t]
            [mp-in-clojure.algo.dist :refer :all]
            [clojure.algo.monads :as m]))

;;; Monty Hall problem

(t/deftest monty-hall-problem
  (let [doors #{:a :b :c}
        probs1 (dist->probs
                (m/domonad dist-m
                  [prize (uniform doors)
                   choice (uniform doors)]
                  (if (= choice prize)
                    :win
                    :lose)))
        probs2 (dist->probs
                (m/domonad dist-m
                  [prize (uniform doors)
                   choice (uniform doors)
                   opened (uniform (disj doors prize choice))
                   choice' (uniform (disj doors opened choice))]
                  (if (= choice' prize)
                    :win
                    :lose)))]
    (t/is (= probs1 {:win 1/3, :lose 2/3}))
    (t/is (= probs2 {:win 2/3, :lose 1/3}))))

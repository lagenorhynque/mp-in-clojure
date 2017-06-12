(ns mp-in-clojure.cats.dist-test
  (:require [clojure.test :as t]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [mp-in-clojure.cats.dist :refer :all]
            [cats.core :as m]
            [cats.labs.test :as lt]))

;;; Monty Hall problem

(t/deftest monty-hall-problem
  (let [doors #{:a :b :c}
        probs1 (dist->probs
                (m/mlet [prize (uniform doors)
                         choice (uniform doors)]
                  (m/return (if (= choice prize)
                              :win
                              :lose))))
        probs2 (dist->probs
                (m/mlet [prize (uniform doors)
                         choice (uniform doors)
                         opened (uniform (disj doors prize choice))
                         choice' (uniform (disj doors opened choice))]
                  (m/return (if (= choice' prize)
                              :win
                              :lose))))]
    (t/is (= probs1 {:win 1/3, :lose 2/3}))
    (t/is (= probs2 {:win 2/3, :lose 1/3}))))

;;; Monad laws

(defspec dist-first-monad-law 100
  (lt/first-monad-law
   {:ctx context
    :mf uniform
    :gen (gen/set gen/keyword {:min-elements 1})}))

(defspec dist-second-monad-law 100
  (lt/second-monad-law {:ctx context}))

(defspec dist-third-monad-law 100
  (lt/third-monad-law
   {:ctx context
    :f (comp m/return str)
    :g (comp m/return count)}))

(ns mp-in-clojure.cats.dist-test
  (:require [clojure.test :as t]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [mp-in-clojure.cats.dist :refer :all]
            [cats.core :as m]
            [cats.labs.test :as lt]))

;;; throwing dice

(t/deftest throwing-dice
  (let [die (range 1 (inc 6))
        probs (dist->probs
               (m/mlet [d1 (uniform die)
                        d2 (uniform die)]
                 (m/return (+ d1 d2))))]
    (t/is (= probs {2 1/36, 3 2/36, 4 3/36, 5 4/36, 6 5/36, 7 6/36,
                    8 5/36, 9 4/36, 10 3/36, 11 2/36, 12 1/36}))))

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

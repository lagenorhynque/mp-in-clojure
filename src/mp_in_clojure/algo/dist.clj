(ns mp-in-clojure.algo.dist
  (:require [clojure.algo.monads :as m]))

(defn dist [& kvs]
  (apply hash-map kvs))

(defn uniform [s]
  (let [n (count s)]
    (->> s
         (map (fn [x] [x (/ 1 n)]))
         (into {}))))

(m/defmonad dist-m
  [m-result (fn [v] {v 1})
   m-bind   (fn [mv f]
              (letfn [(add-prob [d [x p]]
                        (update d x (fnil #(+ % p) 0)))]
                (reduce add-prob
                        {}
                        (for [[x p] mv
                              [y q] (f x)]
                          [y (* p q)]))))])

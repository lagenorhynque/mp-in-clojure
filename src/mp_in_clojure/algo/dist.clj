(ns mp-in-clojure.algo.dist
  (:require [clojure.algo.monads :as m]))

(m/defmonad dist-m
  [m-result (fn [v] [[v 1]])
   m-bind   (fn [m f]
              (for [[x p] m
                    [y q] (f x)]
                [y (* p q)]))])

(defn uniform [s]
  (let [n (count s)]
    (->> s
         (map (fn [x] [x (/ 1 n)])))))

(defn dist->probs [dist]
  (letfn [(add-prob [d [x p]]
            (update d x (fnil #(+ % p) 0)))]
    (reduce add-prob {} dist)))

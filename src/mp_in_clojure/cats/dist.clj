(ns mp-in-clojure.cats.dist
  (:require [cats.core :as m]
            [cats.protocols :as p]
            [cats.util :as util]))

(declare context)

(deftype Dist [v]
  p/Contextual
  (-get-context [_] context)

  p/Extract
  (-extract [_] v)

  p/Printable
  (-repr [_]
    (str "#<Dist " (pr-str v) ">")))

(alter-meta! #'->Dist assoc :private true)

(util/make-printable Dist)

(defn uniform [s]
  (let [n (count s)]
    (->> s
         (map (fn [x] [x (/ 1 n)]))
         (into {}))))

(defn dist [s]
  (->Dist (uniform s)))

(defn dist? [v]
  (instance? Dist v))

(def context
  (reify
    p/Context

    p/Monad
    (-mreturn [_ v]
      (->Dist {v 1}))

    (-mbind [_ s f]
      (assert (dist? s)
              (str "Context mismatch: " (p/-repr s)
                   " is not allowed to use with dist context."))
      (letfn [(add-prob [d [x p]]
                (assoc d x (+ (get d x 0) p)))]
        (->Dist (reduce add-prob {}
                        (for [[x p] (p/-extract s)
                              [y q] (p/-extract (f x))]
                          [y (* q p)])))))

    p/Printable
    (-repr [_]
      "#<Dist>")))

(util/make-printable (type context))

;;; example: Monty Hall problem

(def doors #{:a :b :c})

(def dist1
  (m/mlet [prize (dist doors)
           choice (dist doors)]
    (m/return (if (= choice prize)
                :win
                :lose))))
;; => #<Dist {:win 1/3, :lose 2/3}>

(def dist2
  (m/mlet [prize (dist doors)
           choice (dist doors)
           opened (dist (disj doors prize choice))
           choice (dist (disj doors opened choice))]
    (m/return (if (= choice prize)
                :win
                :lose))))
;; => #<Dist {:lose 1/3, :win 2/3}>

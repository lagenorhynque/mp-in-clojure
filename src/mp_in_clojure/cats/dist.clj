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
    (str "#<Dist " (pr-str v) ">"))

  Object
  (equals [this obj]
    (= (.v this) (.v obj))))

(alter-meta! #'->Dist assoc :private true)

(util/make-printable Dist)

(defn dist [& kvs]
  (->Dist (apply hash-map kvs)))

(defn uniform* [s]
  (let [n (count s)]
    (->> s
         (map (fn [x] [x (/ 1 n)]))
         (into {}))))

(defn uniform [s]
  (->Dist (uniform* s)))

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
                          [y (* p q)])))))

    p/Printable
    (-repr [_]
      "#<Dist>")))

(util/make-printable (type context))

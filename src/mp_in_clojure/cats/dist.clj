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

(defn dist? [v]
  (instance? Dist v))

(def context
  (reify
    p/Context

    p/Monad
    (-mreturn [_ v]
      (->Dist [[v 1]]))

    (-mbind [_ m f]
      (assert (dist? m)
              (str "Context mismatch: " (p/-repr m)
                   " is not allowed to use with dist context."))
      (->Dist (for [[x p] (p/-extract m)
                    [y q] (p/-extract (f x))]
                [y (* p q)])))

    p/Printable
    (-repr [_]
      "#<Dist>")))

(util/make-printable (type context))

(defn uniform [s]
  (let [n (count s)]
    (->> s
         (map (fn [x] [x (/ 1 n)]))
         ->Dist)))

(defn dist->probs [dist]
  (letfn [(add-prob [d [x p]]
            (update d x (fnil #(+ % p) 0)))]
    (reduce add-prob {} (p/-extract dist))))

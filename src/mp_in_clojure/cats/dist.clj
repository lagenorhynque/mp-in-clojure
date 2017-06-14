(ns mp-in-clojure.cats.dist
  (:require [cats.core :as m]
            [cats.protocols :as p]
            [cats.util :as util]))

(declare context)

(deftype Prob [v]
  p/Contextual
  (-get-context [_] context)

  p/Extract
  (-extract [_] v)

  p/Printable
  (-repr [_]
    (str "#<Prob " (pr-str v) ">"))

  Object
  (equals [this obj]
    (= (.v this) (.v obj))))

(alter-meta! #'->Prob assoc :private true)

(util/make-printable Prob)

(defn prob? [v]
  (instance? Prob v))

(def context
  (reify
    p/Context

    p/Functor
    (-fmap [_ f fv]
      (->Prob (for [[x p] (p/-extract fv)]
                [(f x) p])))

    p/Applicative
    (-pure [_ v]
      (->Prob [[v 1]]))
    (-fapply [_ af av]
      (->Prob (for [[g p] (p/-extract af)
                    [x q] (p/-extract av)]
                [(g x) (* p q)])))

    p/Monad
    (-mreturn [m v]
      (p/-pure m v))

    (-mbind [_ mv f]
      (assert (prob? mv)
              (str "Context mismatch: " (p/-repr mv)
                   " is not allowed to use with prob context."))
      (->Prob (for [[x p] (p/-extract mv)
                    [y q] (p/-extract (f x))]
                [y (* p q)])))

    p/Printable
    (-repr [_]
      "#<Prob>")))

(util/make-printable (type context))

(defn uniform [s]
  (let [n (count s)]
    (->> s
         (map (fn [x] [x (/ 1 n)]))
         ->Prob)))

(defn prob->dist [prob]
  (letfn [(add-prob [d [x p]]
            (update d x (fnil #(+ % p) 0)))]
    (reduce add-prob {} (p/-extract prob))))

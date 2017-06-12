(ns mp-in-clojure.algo.rpn-calculator
  (:require [clojure.string :as str]
            [clojure.algo.monads :as m]))

;;; without monads

(defn- folding-function [[x y & ys :as xs] s]
  (cond
    (and x y (= s "*")) (conj ys (* y x))
    (and x y (= s "+")) (conj ys (+ y x))
    (and x y (= s "-")) (conj ys (- y x))
    :else (conj xs (Double/parseDouble s))))

(defn solve-rpn [s]
  (as-> s v
    (str/split v #"\s+")
    (reduce folding-function () v)
    (first v)))

;;; with maybe monad

(defn- read-maybe [s]
  (try
    (Double/parseDouble s)
    (catch NumberFormatException _
      nil)))

(defn- folding-function' [[x y & ys :as xs] s]
  (cond
    (and x y (= s "*")) (conj ys (* y x))
    (and x y (= s "+")) (conj ys (+ y x))
    (and x y (= s "-")) (conj ys (- y x))
    :else (m/with-monad m/maybe-m
            ((m/m-lift 1 #(conj xs %))
             (read-maybe s)))))

(defn solve-rpn' [s]
  (m/domonad m/maybe-m
    [result (reduce folding-function'
                    ()
                    (str/split s #"\s+"))
     :when (= (count result) 1)]
    (first result)))

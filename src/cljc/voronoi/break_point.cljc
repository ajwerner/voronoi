(ns voronoi.break-point
  (:require [voronoi.util :refer [Infinity -Infinity sqrt isNaN? sq close]]
            [voronoi.point :refer [->Point]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Break Points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defrecord BreakPoint [left right edge side begin])

(defn break-point-point [p sweep-y]
  (let [l (:left p)
        r (:right p)]
    (if (close (:y l) (:y r))
      ;; vertical line case
      (let [x (/ (+ (:x l) (:x r))
                 2)
            y (if (= (:y l) sweep-y)
                sweep-y
                (/ (+ (sq (- x (:x l)))
                      (sq (:y l))
                      (* -1 (sq sweep-y)))
                   (* 2 (- (:y l) sweep-y))))]
        (->Point x y))
      (let [px (:x l) ;; we could imagine using r rather than l
            py (:y l)
            m (:m (:edge p))
            b (:b (:edge p))
            d (* 2 (- py sweep-y))
            A 1
            B (- (* -2 px) (* d m))
            C (- (+ (sq px) (sq py))
                 (sq sweep-y)
                 (* d b))
            descrim (+ (sq B) (* -4 A C))
            x (if (<= descrim 0) ;; deal with near zero precision cases
                (/ (* -1 B) (* 2 A))
                (let [num (- (* -1 B) (sqrt descrim))]
                  (if (> (:y l) (:y r))
                    ;; if left, use more precise float logic by rationalizing
                    ;; the denominator
                    (/ (* 2 C) num)
                    (/ num (* 2 A)))))
            y (+ (* m x) b)]
        (->Point x y)))))

(defn new-break-point [left right edge side y]
  (let [p (->BreakPoint left right edge side 0)
        p (assoc p :begin (break-point-point p y))]
    p))

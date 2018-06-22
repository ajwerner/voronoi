(ns voronoi.break-point
  (:require [voronoi.util :refer [Infinity -Infinity sqrt isNaN? sq close]]
            [voronoi.point :refer [->Point]]
           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Break Points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#?(:clj (set! *warn-on-reflection* true))

(defprotocol IBreakPointPoint
  (point-x [this sweep-y])
  (point-y [this sweep-y x]))

(deftype ^:private BreakPointArgs
    [^double lx
     ^double ly
     ^double rx
     ^double ry
     ^double m
     ^double b
     ^boolean is-vert]
  IBreakPointPoint
  (point-x [this sweep-y]
    (if is-vert
      (/ (+ lx rx) 2)
      (let [px lx
            py ly
            oy ry
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
                  (if (> py oy)
                    ;; if left, use more precise float logic by rationalizing
                    ;; the denominator
                    (/ (* 2 C) num)
                    (/ num (* 2 A)))))]
        x)))
  (point-y [this sweep-y x]
    (if is-vert
      (if (= ly sweep-y)
        sweep-y
        (/ (+ (sq (- x lx))
              (sq ly)
              (* -1 (sq sweep-y)))
           (* 2 (- ly sweep-y))))
      (+ (* m x) b))))


(defn ^:private break-point-point-with-args
  "
  @param {IBreakPointPoint} args
  @param {number} y
  @return {voronoi.point.Point}
  "
  [^BreakPointArgs args ^double y]
  (let [x (point-x args y)
        y (point-y args y x)]
    (->Point x y)))

(defn break-point-point
  "@return {voronoi.point.Point}"
  [p ^double sweep-y]
  (break-point-point-with-args (:pp p) sweep-y))

(defn new-break-point [{lx :x ly :y :as left}
                       {rx :x ry :y :as right}
                       {m :m b :b :as edge}
                       side y]
  (let [a (BreakPointArgs. lx ly rx ry m b (close ly ry))]
    {:left left
     :right right
     :edge edge
     :side side
     :pp a
     :begin (break-point-point-with-args a y)}))

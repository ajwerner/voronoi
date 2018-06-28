(ns voronoi.edge
  (:require [voronoi.util :refer [Infinity -Infinity close]]
            [voronoi.point :refer [->Point map->Point
                                   y-ordered-epsilon-comparator
                                   x-ordered-comparator
                                   ccw midpoint]]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Edges
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrecord HalfEdge [site1 site2 m b is-vertical])

(defn new-half-edge [s1 s2]
  (let [is-vertical (close (:y s1) (:y s2))
        [m b] (if is-vertical
                [Infinity 0]
                (let [m (/ -1
                           (/ (- (:y s1) (:y s2))
                              (- (:x s1) (:x s2))))
                      mid (midpoint s1 s2)
                      b (- (:y mid)
                           (* m (:x mid)))]
                  [m b]))]
    (->HalfEdge s1 s2 m b is-vertical)))

(defn vertical-intersection [vert-edge non-vert-edge]
  (let [x (/ (+ (:x (:site1 vert-edge))
                (:x (:site2 vert-edge)))
             2)
        y (+ (* (:m non-vert-edge) x)
             (:b non-vert-edge))]
    (->Point x y)))

(defn intersect-edges [e1 e2]
  (let [v1 (:is-vertical e1)
        v2 (:is-vertical e2)]
    (cond
      (and v1 v2) nil
      v1 (vertical-intersection e1 e2)
      v2 (vertical-intersection e2 e1)
      (and (== (:m e1) (:m e2))
           (not (== (:b e1) (:b e2)))) nil
      :else (let [x (/ (- (:b e2) (:b e1))
                       (- (:m e1) (:m e2)))
                  y (+ (* (:m e1) x)
                       (:b e1))]
              (->Point x y)))))

(defn new-complete [{begin :begin
                     edge :edge
                     side :side
                     left :left
                     right :right
                     :as bp}
                    end]
  (println "2323" side bp end begin)
  (condp side =
    :left {:begin begin
           :end end
           :p0 end
           :p1 begin
           :left right
           :right left
           :edge edge}
    :right {:begin begin
            :end end
            :p0 begin
            :p1 end
            :left left
            :right right
            :edge edge}))

(ns voronoi.components.svg
  (:require [voronoi.util :refer [Infinity -Infinity isNaN? is-infinite?]]
            [voronoi.voronoi :as vor]
            [voronoi.arc :as arc]
            [voronoi.break-point :refer [break-point-point]]
            [voronoi.point :as point]
            [clojure.string :as string]
            [voronoi.points :as points]
            [reagent.core :as reagent]))

(defn parabola-point-y [foc dir x]
  (let [a (:x foc)
        b (:y foc)
        c dir
        sq (fn [x] (* x x))
        y (/ (+ (sq (- a x))
                (sq b)
                (* -1 (sq c)))
             (* 2 (- b c)))]
    y))


(defn draw-parabola [foc y xmin xmax]
  (let [y1 (parabola-point-y foc y xmin)
        cx (/ (+ xmin xmax) 2)
        denom (- (:y foc) y)
        cy (+ y1
              (* (- (/ xmin denom)
                    (/ (:x foc) denom))
                 (/ (- xmax xmin) 2)))
        y2 (parabola-point-y foc y xmax)]
    (if (not-any? #(or (is-infinite? %) (isNaN? %)) [y1 cx cy y2])
      [:path {:d (str
                  "M " xmin " " y1
                  " Q " cx " " cy " " xmax " " y2)
              :fill-opacity "0"
              :stroke "black"
              :stroke-width 1}])))


(defn draw-parabolas [arcs y xmin xmax]
  (if (> (count arcs) 0)
    (let [parabolas (for [[arc _] arcs]
                      (let [arc-l (arc/left-bound arc y)
                            arc-r (arc/right-bound arc y)
                            xmin (min (max (:x arc-l) xmin) xmax)
                            xmax (max (min (:x arc-r) xmax) xmin)
                            foc (:point arc)]
                        (if-not (= xmin xmax)
                          (draw-parabola (:point arc) y xmin xmax))))]
      (into [:g] (remove nil? parabolas)))))

(defn draw-points [points-cursor]
  (fn []
    [:g
     (doall
      (map-indexed
       (fn [i {x :x y :y :as p}]
         ^{:key i} [:circle {:cx x :cy y :r 1 :stroke-width .1 :stroke "black"}])
       @points-cursor))]))


(defn line [x1 y1 x2 y2 attrs]
  [:line (into attrs {:x1 x1 :y1 y1 :x2 x2 :y2 y2 :stroke-width .3})])

(defn draw-sweep-line [y xmin xmax]
  [:g (line xmin y xmax y {:stroke "black"
                           :stroke-width .1})])

(defn get-edges [edges]
  ;;(println edges)
  @edges)

(defn get-edge [edges idx]
  (-> @(reagent/track get-edges edges) (nth idx)))

(defn get-edges-count [edges]
  (-> @(reagent/track get-edges edges) count))

;; (defn completed-comp [completeds i]
;;   (let [ts (reagent/track get-completed completeds i)]
;;     (let [{{bx :x by :y} :begin
;;            {ex :x ey :y} :end
;;            :as completed} @t
;;           ok (not-any? infy? [bx by ex ey])
;;           ]
;;       (if ok
;;         [line bx by ex ey {:stroke "blue"}]))))

(defn draw-complete-half-edges [edges-cursor]
  (let []
    (fn []

      (let [edges @edges-cursor
            c [:g
               (doall
                (map-indexed
                 (fn [i c]
                   (let [{{bx :x by :y} :begin
                          {ex :x ey :y} :end} c
                         ok (not-any? is-infinite? [bx by ex ey])]
                     (if ok
                       ^{:key i} [line bx by ex ey {:stroke "blue"}]
                       (do
                         (let [edge (:edge c)]
                           (if (:is-vertical edge)
                             ^{:key i} [line bx by bx (- by 10000) {:stroke "green"}]
                             (let [x (if (= Infinity ex)
                                       (+ bx 1000)
                                       (- bx 1000))
                                   y (+ (* (:m edge) x) (:b edge))]
                               ^{:key i} [line bx by x y {:stroke "blue"}])))))))
                 edges))]]
        c))))

(defn draw-break [bp y]
  (let [{bx :x by :y} (:begin bp)
        {px :x py :y} (break-point-point bp y)]
    ^{:key bp} (line  bx by px py {:stroke (if (= :left (:side bp))
                                             "red"
                                             "cyan")})))



(defn draw-breaks [breaks y]
  [:g
   (map-indexed
    (fn [i bp]
      ^{:key i} [draw-break bp y i])
    breaks)])

(defn draw-sweep-state [voronoi xmin xmax]
  (fn []
    (let [{y :scan
            arcs :arcs
           :as v} @voronoi
          breaks (vor/get-breaks v)]
      [:g
       [draw-sweep-line y xmin xmax]
       [draw-parabolas arcs y xmin xmax]
       [draw-breaks breaks y]])))

(defn draw-polygons [vor-cursor]
  (fn []
    (if-let [p (vor/polygons @vor-cursor)]
      [:g.finished
       (map-indexed
        (fn [i points]
          (let [points-str
                (string/join " " (map #(str (:x %) "," (:y %)) points))
                {fx :x fy :y} (first points)
                ]
            ^{:key i} [:polygon {:points points-str}]

            ))
        (remove #(some (fn [{x :x}] (is-infinite? x)) %) p))])))

(defn draw-extent [extent-cursor]
  (fn []

    (if-let [extent @extent-cursor]
      (let [[xmin xmax ymin ymax] extent]
        [:g
         ^{:key 0} (line xmin ymin xmax ymin {:stroke "black"})
         ^{:key 1} (line xmin ymin xmin ymax {:stroke "black"})
         ^{:key 2} (line xmin ymax xmax ymax {:stroke "black"})
         ^{:key 2} (line xmax ymin xmax ymax {:stroke "black"})]))))

(defn interactive-svg [voronoi scroll]
  (let [scroll-cursor (swap! scroll #(if % % {:x -80
                                              :y -200
                                                   :x-width 1200
                                                   :y-width 1800
                                              :prev nil}))
        handle-tm (fn [{:keys [prev x y x-width y-width]
                        :as scroll} ev]
                    (let [t (aget (.-touches ev) 0)
                          scroll (assoc scroll :prev t)
                          shift (.-shiftKey ev)]
                      (if prev
                        (let [xdelta (* (- (.-screenX t)
                                           (.-screenX prev))
                                        (/ x-width 1200))
                              ydelta (* (- (.-screenY t)
                                               (.-screenY prev))
                                        (/ y-width 1800))]
                          (if shift
                            (let [size nil]
                              (-> scroll
                                  (update :x-width #(max (- % xdelta) 0))
                                  (update :y-width #(max (- % ydelta) 0))
                                  ))
                            (-> scroll
                                (update :x - (/ xdelta 1))
                                (update :y - (/ ydelta 1)))))
                        scroll)))
        tm (fn [ev]

             (swap! scroll handle-tm ev))
        clear (fn [ev]

                (swap! scroll #(assoc % :prev nil)))
        points-cursor (reagent/cursor voronoi [:points])
        edges-cursor (reagent/cursor voronoi [:edges])
        extent (reagent/cursor voronoi [:extent])
        ]
    (fn []
      (let [{:keys [x y x-width y-width]} @scroll
            view-box (string/join " " [x y x-width y-width])]
        [:div {:on-touch-move tm
               :on-touch-end clear
               :on-touch-cancel clear}
         [:svg {:viewBox view-box
                :preserveAspectRatio "xMidYMid meet"}

          [draw-points points-cursor]
          [draw-complete-half-edges edges-cursor]
          [draw-sweep-state voronoi (- x x-width) (+ x x-width x-width)]]]))))

(defn voronoi-svg
  "draws an svg
  expects a ratom for a voronoi diagram"
  [voronoi]
  (let [points-cursor (reagent/cursor voronoi [:points])
        edges-cursor (reagent/cursor voronoi [:edges])
        extent (reagent/cursor voronoi [:extent])
        [xmin xmax ymin ymax] (point/widen-by-percent @extent 10)]
    [:div
     [:svg {
            :view-box (string/join " " [xmin ymin (- xmax xmin) (- ymax ymin)])
            :preserveAspectRatio "xMaxYMax meet"}
      [draw-extent extent]
      [draw-polygons voronoi]
      [draw-points points-cursor]
      [draw-complete-half-edges edges-cursor]
      ;;[draw-sweep-state voronoi -1000 1000]
      ]]))

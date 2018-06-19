(ns voronoi.components.svg
  (:require [voronoi.util :refer [Infinity -Infinity isNaN?]]
            [voronoi.voronoi :as vor]
            [voronoi.arc :as arc]
            [voronoi.break-point :refer [break-point-point]]
            [clojure.string :as string]
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

(defn infy? [n]
  (or (== n Infinity)
      (== n -Infinity)))

(defn draw-parabola [foc y xmin xmax]
  (let [y1 (parabola-point-y foc y xmin)
        cx (/ (+ xmin xmax) 2)
        denom (- (:y foc) y)
        cy (+ y1
              (* (- (/ xmin denom)
                    (/ (:x foc) denom))
                 (/ (- xmax xmin) 2)))
        y2 (parabola-point-y foc y xmax)]
    (if (not-any? #(or (infy? %) (isNaN? %)) [y1 cx cy y2])
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
         ^{:key i} [:circle {:cx x :cy y :r .4 :stroke-width .1 :stroke "black"}])
       @points-cursor))]))


(defn line [x1 y1 x2 y2 attrs]
  [:line (into attrs {:x1 x1 :y1 y1 :x2 x2 :y2 y2 :stroke-width .8})])

(defn draw-sweep-line [y xmin xmax]
  [:g (line xmin y xmax y {:stroke "black"
                           :stroke-width .1})])

(defn get-completeds [completeds]
  ;;(println completeds)
  @completeds)

(defn get-completed [completeds idx]
  (-> @(reagent/track get-completeds completeds) (nth idx)))

(defn get-completed-count [completeds]
  (-> @(reagent/track get-completeds completeds) count))

(defn completed-comp [completeds i]
  (let [t (reagent/track get-completed completeds i)]
    (fn []
      (let [{{bx :x by :y} :begin
             {ex :x ey :y} :end
             :as completed} @t
            ok (not-any? infy? [bx by ex ey])]
        (if ok
          [line bx by ex ey {:stroke "blue"}])))))

(defn draw-completeds [completeds]
  (let [c @(reagent/track get-completed-count completeds)]
    [:g (for [i (range c)]
          ^{:key i} [completed-comp completeds i])]))

(defn draw-break [bp y]
  (let [{bx :x by :y} (:begin bp)
        {px :x py :y} (break-point-point bp y)]
    ^{:key bp} (line  bx by px py {:stroke "red"})))

(defn draw-breaks [breaks y]
  [:g
   (map-indexed
    (fn [i bp] ^{:key i} [draw-break bp y i])
    breaks)])

(defn draw-sweep-state [voronoi xmin xmax]
  (fn []
    (let [{y :scan
           arcs :arcs
           breaks :breaks} @voronoi]
      [:g
       [draw-sweep-line y xmin xmax]
       [draw-parabolas arcs y xmin xmax]
       [draw-breaks breaks y]])))

(defn voronoi-svg
  "draws an svg
  expects a ratom for a voronoi diagram"
  [voronoi scroll]
  (let [points-cursor (reagent/cursor voronoi [:points])
        completed-cursor (reagent/cursor voronoi [:completed])
        scroll-cursor (swap! scroll #(if % % {:x -80
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
                          (println (/ x-width 1200))

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
        ]
    (fn []
      (let [{:keys [x y x-width y-width]} @scroll
            view-box (string/join " " [x y x-width y-width])]
        [:div {:on-touch-move tm
               :on-touch-end clear
               :on-touch-cancel clear}
         [:svg {:viewBox view-box
                :preserveAspectRatio "xMaxYMax slice"}

          [draw-points points-cursor]
          [draw-completeds completed-cursor]
          [draw-sweep-state voronoi (- x x-width) (+ x x-width x-width)]]]))))

(ns voronoi.components.svg
  (:require [voronoi.util :refer [Infinity -Infinity isNaN?]]
            [voronoi.voronoi :as vor]
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
              :stroke "black"}])))


(defn draw-parabolas [arcs y xmin xmax]
  (if (> (count arcs) 0)
    (let [parabolas (for [arc (keys arcs)]
                      (let [arcL (vor/arc-left-point arc y)
                            arcR (vor/arc-right-point arc y)
                            xmin (min (max (:x arcL) xmin) xmax)
                            xmax (max (min (:x arcR) xmax) xmin)
                            foc (:point arc)]
                        (if-not (= xmin xmax)
                          (draw-parabola (:point arc) y xmin xmax))))]
      (into [:g] (remove nil? parabolas)))))

(defn draw-points [points-cursor]
  (fn []
    [:g
     (doall
      (for [{x :x y :y :as p} @points-cursor]
             ^{:key p} [:circle {:cx x :cy y :r 1 :stroke "black"}]))]))


(defn line [x1 y1 x2 y2 attrs]
  [:line (into attrs {:x1 x1 :y1 y1 :x2 x2 :y2 y2})])

(defn draw-sweep-line [y xmin xmax]
  [:g (line xmin y xmax y {:stroke "black"})])

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
        {px :x py :y} (vor/break-point-point bp y)]
    ^{:key bp} (line  bx by px py {:stroke "red"})))

(defn draw-breaks [breaks y]
  [:g (for [bp breaks] ^{:key bp} [draw-break bp y])])

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
  [voronoi viewbox-]
  (let [points-cursor (reagent/cursor voronoi [:points])
        completed-cursor (reagent/cursor voronoi [:completed])
        scroll (reagent/atom
                {:x -80
                 :y -200
                 :x-width 1200
                 :y-width 1800
                 :prev nil})
        handle-tm (fn [{:keys [prev x y]
                        :as scroll} ev]
                    (let [t (aget (.-touches ev) 0)
                          scroll (assoc scroll :prev t)
                          shift (.-shiftKey ev)
                          ]

                      (if prev
                        (let [xdelta (- (.-screenX t)
                                        (.-screenX prev))
                              ydelta (- (.-screenY t)
                                        (.-screenY prev))]
                          (if shift
                            (let [size nil]
                              (-> scroll
                                  (update :x-width #(max 0 (- % xdelta)))
                                  ;; (update :y-width - ydelta)
                                  ))
                            (-> scroll
                                (update :x - xdelta)
                                (update :y - ydelta))))
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
                :preserveAspectRatio "xMaxYMax slice"
                }
          [draw-points points-cursor]
          [draw-completeds completed-cursor]
          [draw-sweep-state voronoi (- x x-width) (+ x x-width x-width)]]]))))

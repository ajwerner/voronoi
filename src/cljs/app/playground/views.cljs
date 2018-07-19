(ns app.playground.views
  (:require [app.svg :as svg]
            [re-frame.core :as rf]
            [voronoi.util :as u]
            [voronoi.core :as vor]
            [voronoi.arc :as arc]
            [app.playground.subs]
            [app.playground.events]
            [voronoi.break-point :as bp]
            [re-com.core :as rc]))


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
    (if (not-any? #(or (u/is-infinite? %) (u/isNaN? %)) [y1 cx cy y2])
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

(defn draw-points-im [points]
  [:g
   (doall
     (map-indexed
       (fn [i {x :x y :y :as p}]
         ^{:key i} [:circle {:cx x :cy y :r 1 :stroke-width .1 :stroke "black" :fill "black"}])
       points))])

(defn draw-points [points-cursor]
  (fn []
    (draw-points-im @points-cursor)))

(defn line [x1 y1 x2 y2 attrs]
  [:line (into attrs {:x1 x1 :y1 y1 :x2 x2 :y2 y2 :stroke-width .3})])

(defn draw-sweep-line [y xmin xmax]
  [:g (line xmin y xmax y {:stroke "black"
                           :stroke-width .1})])


(defn draw-break [bp y]
  (let [{bx :x by :y} (:begin bp)
        {px :x py :y} (bp/break-point-point bp y)]
    ^{:key bp} (line  bx by px py {:stroke (if (= :left (:side bp))
                                             "red"
                                             "cyan")})))

(defn draw-breaks [breaks y]
  [:g
   (map-indexed
     (fn [i bp]
       ^{:key i} [draw-break bp y i])
     breaks)])


(defn draw-sweep-state [id]
  (fn []
    (let [v @(rf/subscribe id)
          {y    :scan
           arcs :arcs} v
          breaks (vor/get-breaks v)
          [xmin xmax] @(rf/subscribe [:playground/x-span])]
      [:g
       [draw-sweep-line y xmin xmax]
       [draw-parabolas arcs y xmin xmax]
       [draw-breaks breaks y]])))

(defn view-box-key [[id]]
  (keyword (namespace id) "view-box"))

(defn interactive-svg [id]
  (fn []
    (let []
      [rc/box
       :size "0 1 auto"
       :child
       [:svg {:viewBox             @(rf/subscribe [(view-box-key id) id])
              :preserveAspectRatio "xMidYMid meet"
              :width "100%"}
        [svg/voronoi-points id]
        [svg/voronoi-completed id]
        [draw-sweep-state id]]])))

(defn reset-button [id]
  (fn []
    [rc/box
     :size "0 1 auto"
     :child
     [rc/md-icon-button
      :md-icon-name "zmdi-replay"
      :on-click #(rf/dispatch [:reset-builder id])
      :style {:width "100%"}]]))

(defn play-pause-button [id]
  (fn []
    [rc/box
     :size "0 1 auto"
     :child (if @(rf/subscribe [:builder/running? id])
              [rc/md-icon-button
               :md-icon-name "zmdi-pause"
               :on-click #(rf/dispatch [:stop-builder id])
               :style {:width "100%"}]
              [rc/md-icon-button
               :md-icon-name "zmdi-play"
               :on-click #(rf/dispatch [:play-builder id])
               :style {:width "100%"}])]))

(defn control-bar [id]
  (fn []
    [rc/h-box
     :size "1 0 auto"
     :justify :start
     :children
     [[play-pause-button id]
      [reset-button id]
      [rc/gap :size "1"]
      [rc/box
       :child (str (.toFixed (:scan @(rf/subscribe id)) 2))]]]))

(defn new-app-thing [id]
  (fn []
    (when @(rf/subscribe [:playground/initialized?])
      [rc/v-box
       :size "auto"
       :width "100%"
       :height "100%"
       :max-width "550px"
       :margin "auto"
       :justify :start
       :children
       [[interactive-svg id]
        [control-bar id]]])))
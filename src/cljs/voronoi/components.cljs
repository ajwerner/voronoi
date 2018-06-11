(ns voronoi.components
  (:require [reagent.core :as reagent :refer [atom]]
            [voronoi.voronoi :as vor]
            [voronoi.util :refer [Infinity -Infinity isNaN?]]
            [voronoi.points :as points]
            [clojure.string :as string]
            [voronoi.control :refer [toggle-pause-scan!
                                     do-step!
                                     add-points!
                                     do-set-to!
                                     reset-state!
                                     do-set-by!]]))

(defn get-form-num [element-id]
  (let [el (.getElementById js/document element-id)]
    (try
      (let [num (js/Number el.value)]
        num)
      (catch :default e
        js/Number.NaN))))

(defn form-key-press [action]
  (fn [ev]
    (if (= ev.which 13)
      (do
        (.preventDefault ev)
        (action)))))

(defn on-submit-action[ev]
  (.preventDefault ev)
  false)

(def default-form-keypress
  (form-key-press (fn [])))


(defn arc-table [arcs y]
  (fn []
    [:div
     [:table
      [:thead
       [:tr
        [:th "Left"]
        [:th "Right"]
        [:th "Length"]
        [:th "Site"]
        [:th "BreakLeft"]
        [:th "BreakRight"]]]
      [:tbody
       (let [arcs @arcs
             y @y]
         (for [arc (keys arcs)]
           (let [[l r] (vor/arc-points arc y)
                 len (vor/distance l r)]
             ^{:key arc}
             [:tr
              [:td (str l) ]
              [:td (str r) ]
              [:td (str len)]
              [:td (str (:point arc))]
              [:td (str "(" (:left (:left arc)) ", " (:right (:left arc)) ")") ]
              [:td (str "(" (:left (:right arc)) ", " (:right (:right arc)) ")") ]])))
       ]]]))



(defn current-pos-display [scan]
  [:div [:h3 (.toFixed scan 2)] [:br]])

(defn button [text action]
  [:button {:href "#" :onClick action} text])


(defn make-text-input [id label action]
  [:div
   [:label {:for id} label]
   [:input {:type "text" :id id :onKeyPress action}]
   [:br]])

(defn text-input [id text action]
  [:div
   [:input {:type "text"
            :id id
            :onKeyPress (form-key-press action)}]
   [:button {:href "#" :onClick action} text]])

(defn control-panel [paused scan control-funcs]
  (fn []
    [:div {:id "control"}
     [current-pos-display @scan]
     [button (if @paused "play" "pause") (:toggle-pause-scan control-funcs)]
     [button "next" (:step control-funcs)]
     [button "clear" (:clear control-funcs)]
     [:br]
     [text-input "y-val" "set" (:set-to control-funcs)]
     [:br]
     [text-input "by-val" "step" (:set-by control-funcs)]
     [:br]]))


(defn do-ok [getter pred action]
  #(let [v (getter)] (if (pred v) (action v))))

(defn get-form-numbers [ids]
  (let [ids (for [id ids]
              (let [n (get-form-num id)]
                {(keyword id) (get-form-num id)}))
        ]
    (apply merge ids)))

(defn control-add-point [control-funcs]
  (let [point-prop-keys ["point-x" "point-y"]
        point-prop-ids (map keyword point-prop-keys)
        get-point #(get-form-numbers point-prop-keys)
        point-vals (fn [p] (map #(get p %) point-prop-ids))
        point-point (fn [{x :point-x y :point-y}] {:x x :y y})
        point-ok? #(not-any? isNaN? (point-vals %))
        do-reset-point #((:reset-state control-funcs) [(point-point %)])
        reset-point (do-ok get-point point-ok? do-reset-point)
        do-add-point #((:add-points control-funcs) [(point-point %)])
        add-point (do-ok get-point point-ok? do-add-point)]
    [:div
     [:form {:id "point" :onSubmit on-submit-action}
      [:div {:id "point-fields"}
       (make-text-input "point-x" "X" default-form-keypress)
       (make-text-input "point-y" "Y" default-form-keypress)
       [:button {:href "#" :onClick reset-point} "reset"]
       [:button {:href "#" :onClick add-point} "add"]]]]))


(defn control-add-circle [control-funcs]
  (let [circle-prop-keys ["circle-n" "circle-rad" "circle-x" "circle-y"]
        circle-prop-ids (map keyword circle-prop-keys)
        get-circle #(get-form-numbers circle-prop-keys)
        circle-vals (fn [c] (map #(get c %) circle-prop-ids))
        circle-ok? #(not-any? isNaN? (circle-vals %))
        circle-points #(apply points/circle-points (circle-vals %))
        do-reset-circle #((:reset-state control-funcs) (circle-points %))
        reset-circle (do-ok get-circle circle-ok? do-reset-circle)
        do-add-circle #((:add-points control-funcs) (circle-points %))
        add-circle (do-ok get-circle circle-ok? do-add-circle)]
    [:div
     [:form {:id "circle" :onSubmit on-submit-action}
      [:div {:id "circle-fields"}
       (make-text-input "circle-rad" "Radius" default-form-keypress)
       (make-text-input "circle-n" "N" default-form-keypress)
       (make-text-input "circle-x" "X" default-form-keypress)
       (make-text-input "circle-y" "Y" default-form-keypress)
       [:button {:href "#" :onClick reset-circle} "reset"]
       [:button {:href "#" :onClick add-circle} "add"]]
      ]]))


(defn control-add-random [control-funcs]
  (let [get-random-n #(get-form-num "random-n")
        is-number? #(not (isNaN? %))
        reset-random (do-ok get-random-n is-number?
                             #((:reset-state control-funcs) (points/random-points %)))
        add-random (do-ok get-random-n is-number?
                          #((:add-points control-funcs) (points/random-points %)))]
    [:div
     [:form {:id "random" :onSubmit on-submit-action}
      (make-text-input "random-n" "N" default-form-keypress)
      [:button {:href "#" :onClick reset-random} "reset"]
      [:button {:href "#" :onClick add-random} "add"]]]))


(defn control-add-panel [control-funcs]
  (let [add-mode (atom :random)
        valid-add-values #{"random" "circle" "point"}
        set-add-mode (fn [ev]
                       (if (valid-add-values ev.target.value)
                         (reset! add-mode (keyword ev.target.value))))]
    (fn []
      [:div [:select {:id "add-type"
                      :onChange set-add-mode}
             [:option {:value "random"} "Random"]
             [:option {:value "circle"} "Circle"]
             [:option {:value "point"} "Point"]]
       (case @add-mode
         :random (control-add-random control-funcs)
         :circle (control-add-circle control-funcs)
         :point (control-add-point control-funcs))])))

(defn control-funcs [data]
  {:toggle-pause-scan #(toggle-pause-scan! data)
   :reset-state #(reset-state! data %)
   :clear #(reset-state! data [])
   :step #(do-step! data)
   :set-to  #(let [to (get-form-num "y-val")]
               (if-not (isNaN? to)
                 (do-set-to! data to)))
   :set-by #(let [by (get-form-num "by-val")]
              (if-not (isNaN? by)
                (do-set-by! data by)))
   :add-points #(add-points! data %)})

(defn table-controler [show-table-cursor arcs-cursor scan-cursor]
  (let [toggle-show-table #(swap! show-table-cursor not)]
    (fn []
      [:div
       [:form {:onSubmit on-submit-action}
        "show table" [:input
                      (let [attrs {:type "checkbox"
                                   :onChange toggle-show-table
                                   :id "showTableToggle"}]
                        (if @show-table-cursor (assoc attrs :checked "true") attrs))]
        ]
       (if @show-table-cursor
         [arc-table arcs-cursor scan-cursor])])))

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
     (for [{x :x y :y :as p} @points-cursor]
       ^{:key p} [:circle {:cx x :cy y :r 1 :stroke "black"}])]))


(defn line [x1 y1 x2 y2 attrs]
  [:line (into attrs {:x1 x1 :y1 y1 :x2 x2 :y2 y2})])

(defn draw-sweep-line [y xmin xmax]
  [:g (line xmin y xmax y {:stroke "black"})])

(defn draw-edges [edges]
  (fn []
    [:g
     (for [{{bx :x by :y} :begin
            {ex :x ey :y} :end
            :as edge} @edges
           :when (not-any? infy? [bx by ex ey])]
       ^{:key edge}
       [line bx by ex ey {:stroke "blue"}])]))

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

(defn draw-it [voronoi]
  (let [points-cursor (reagent/cursor voronoi [:points])
        completed-cursor (reagent/cursor voronoi [:completed])]
    (fn []
      (let [
            [xmin xmax] [-50 1000]
            [ymin ymax] [-50 800]
            xwidth (- xmax xmin)
            ywidth (- ymax ymin)
            view-box (string/join " " [xmin ymin xwidth ywidth])]
        [:svg {:viewBox view-box
               :preserveAspectRatio "xMidYMid meet"}
         [draw-points points-cursor]
         [draw-edges completed-cursor]
         [draw-sweep-state voronoi (- xmin xwidth) (+ xmax xwidth)]]))))


(defn app-thing [data]
  (let [control (control-funcs data)
        vor (reagent/cursor data [:voronoi])
        paused (reagent/cursor data [:scan :paused])
        scan (reagent/cursor data [:voronoi :scan])
        show-table (reagent/cursor data [:display-state :show-table])
        arcs (reagent/cursor data [:voronoi :arcs])]
    (fn []
      [:section.voronoi-widget
       [:div.graphics
        [draw-it vor]]
       [:div.control-panel
        [control-panel paused scan control]
        [:div.add-bar [:h4 "Add Points"]
         [control-add-panel control]]]
       [table-controler show-table arcs scan]])))

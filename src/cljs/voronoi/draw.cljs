(ns voronoi.draw
  (:require [voronoi.voronoi :as vor]
            [quil.core :as q :include-macros true]
            [quil.middleware]
            [voronoi.util :refer [Infinity -Infinity]]))

(defn draw-points [points]
  (q/stroke-weight 2)
  (q/stroke 250 0 0)
  (q/color 255)
  (doseq [p points :let [{:keys [x y]} p]]
    (q/point x y)))

(def canvas-size [700 700])

(defn parabola-point [foc dir x]
  (let [
        a (:x foc)
        b (:y foc)
        c dir
        sq (fn [x] (* x x))
        y (/ (+ (sq (- a x))
                (sq b)
                (* -1 (sq c)))
             (* 2 (- b c)))
        ]
    {:x x :y y}))

(defn draw-parabola [foc y xmin xmax]
  (let [partitioned (partition 2 1 (concat (range xmin xmax 5) [xmax]))
        memoized-parabola-point (memoize (fn [x] (parabola-point foc y x)))
        ]
    (doseq [[begin end] partitioned]
      (let [delta (- end begin)
            xvs [(- begin delta) begin end (+ end delta)]
            vals (flatten (map (fn [x] [x (:y (memoized-parabola-point x))]) xvs))]
        (if (and (> (nth vals 4) 0) (> (nth vals 6) 0))
          (apply q/curve vals))))))

(defn line-style []
  (q/stroke-weight 1)
  (q/stroke 10)
  (q/fill 0 250 20))

(defn draw-sweep [y]
  (line-style)
  (q/line 0 y (nth canvas-size 0) y))

(defn clamp [extent p]
  (let [[{min-x :x min-y :y} {max-x :x max-y :y}] extent
        x (:x p)
        x (cond
            (< x min-x) min-x
            (> x max-x) max-x
            :else x)
        y (:y p)
        y (cond
            (< y min-y) min-y
            (> y max-y) max-y
            :else y)
        ]
    {:x x :y y}))

(def default-extent
  [{:x 0 :y 0}
   {:x 1000 :y 1000}])

(def big-val 10000)

(defn clamp-infinity [v ]
  (cond
    (= Infinity v) big-val
    (= -Infinity v) 0
    :else v))

(defn clamp-point [p]
  (update
   (update p :x clamp-infinity)
   :y clamp-infinity))

(defn draw-completed [completed]
  (line-style)
  (doseq [{begin :begin end :end} completed]
    (let [{x1 :x y1 :y} (clamp-point begin)
          {x2 :x y2 :y} (clamp-point end)]
      (q/line x1 y1 x2 y2))))

(defn draw-events [events y]
  (q/text-font (q/create-font "DejaVu Sans" 10 true))
  (if-not (empty? events)
    (if-let [rem (take 10 (subseq events >= {:x 0 :y y}))]
      (if-not (empty? rem)
        (doseq [[i p] (map-indexed vector rem)
                :let [s (str (select-keys p [:x :y :type :arc]))
                      s (if (= (:type p) :circle)
                          s ;;(str s " " (vor/circle-event-cos-vert p))
                          s)]]
          (q/text s 2 (* 11 (+ 1 i))))))))

(defn draw-scan [state]
  (let [{
         points :points
         {active :active} :scan
         {
          sweep-y :scan
          events :events
          arcs :arcs
          breaks :breaks
          completed :completed} :voronoi} state
        y sweep-y]
    (draw-sweep y)
    (draw-completed completed)
    ;; (draw-events events y)
    (doseq [arc (keys arcs)]
      (let [arcL (vor/arc-left-point arc y)
            arcR (vor/arc-right-point arc y)
            minX (:x arcL)
            maxX (:x arcR)
            minX (max minX 0)
            maxX (min maxX (nth canvas-size 0))
            foc (:point arc)
            d  (/ (- y (:y foc)) 2)
            d (if (> d 255) 255 (int d))
            mp (vor/midpoint arcL arcR)
            map (parabola-point foc y (:x mp))]
        (q/stroke (- 180 (vor/sq (+ d 2))) (+ 23 (* 3 d)) (+ (* d 4) 38))
        (draw-parabola (:point arc) y minX maxX)))
    (q/fill 250 0 0)
    (if-let [ev (first (filter #(= (:type %) :circle) events))]
      (let [rad (* 2 (- (:y ev) (:y (:vert ev))))]
        (q/no-fill)
        (q/point (:x (:vert ev)) (:y (:vert ev)))
        (q/ellipse (:x (:vert ev)) (:y (:vert ev)) rad rad)))
    (doseq [bp breaks]
      (let [p (vor/break-point-point bp y)
            [px py] [(:x p) (:y p)]
            begin (:begin bp)
            get (fn [point coord]
                  (cond
                    (= (coord point) Infinity) (nth canvas-size 1)
                    (= (coord point) -Infinity) 0
                    :else (coord point)))
            [bx by] [(get begin :x) (get begin :y)]
            edge (:edge bp)
            ]
        (q/stroke-weight 1)
        (q/stroke 0 0 250)
        (q/line bx by px py)
        (q/stroke-weight 4)
        (q/stroke 0 50 250)
        (q/point px py)
        ))
    state))

(defn draw [state]
  (q/background 250)
  (let [scan (:scan state)]
    (if (and (:active scan) (some? (:voronoi state)))
      (draw-scan state)
      state))
   (draw-points (:points state))
  )

(defn draw-f [state]
    (draw @state))

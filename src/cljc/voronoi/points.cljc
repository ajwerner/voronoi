(ns voronoi.points
  (:require [voronoi.util :refer [PI cos sin random]]))

(def canvas-size [400 400])

(defn point-list-to-point [[x y]] {:x x :y y})

(defn points-lists-to-points [points-lists]
  (into [] (map point-list-to-point points-lists)))

(defn grid [n [x1 y1] xgap ygap ]
  (points-lists-to-points
   (for [x (map #(+ x1 (* xgap %)) (range n))
         y (map #(+ y1 (* ygap %)) (range n))]
     [x y])))

(defn canvas-grid [n]
  (let [[maxx maxy] canvas-size
        xgap (/ maxx n)
        ygap (/ maxy n)
        points-lists (grid n [(/ xgap 2) (/ ygap 2)] xgap ygap)]
    (points-lists-to-points points-lists)))

(defn extent-grid [[xmin xmax ymin ymax] n]
  (let [xwidth (- xmax xmin)
        ywidth (- ymax ymin)
        xgap (/ xwidth n)
        ygap (/ ywidth n)
        points-lists (grid n [(/ xgap 2) (/ ygap 2)] xgap ygap)]
    (points-lists-to-points points-lists)))

(defn circle-points [n rad cx cy]
  (let [step-size (/ (* 2 PI) n)
        points (for [r (map #(* step-size %) (range n))]
                 [(+ cx (* rad (cos r)))
                  (+ cy (* rad (sin r)))])]
    (points-lists-to-points points)))


(defn random-points [n]
  (let [[maxx maxy] canvas-size
        points-lists (for [_ (range n)]
                       [(* (random) maxx)
                        (* (random) maxy)])]
    (points-lists-to-points points-lists)))

(defn zoom-by [points factor]
  (map #(update (update % :x * factor) :y * factor) points))

(defn translate-by [points [x y]]
  (map #(update (update % :x + x) :y + y) points))

(defn filter-by [points & preds]
  (filter #(apply every? (-> % preds)) points))

(def bad-grid-circle
  (concat (grid 5 [240 140] 40 40)
          (grid 5 [220 120] 50 50)
          (grid 5 [230 130] 45 45)
          (circle-points  50  30 220 220)
          (circle-points  121  75 220 220)))

(defn x< [val] (fn [[x _]] (< x val)))
(defn x> [val] (fn [[x _]] (> x val)))
(defn y< [val] (fn [[_ y]] (< y val)))
(defn y> [val] (fn [[_ y]] (> y val)))

(def bad-part
  (filter-by
   bad-grid-circle
   (x> 140)
   (x< 220.1)
   (y< 148)))

(def bad-part2
  (filter (fn [{x :x y :y}]
            (and (> x 213)
                 (< x 227.1)
                 (> y 200)
                 (< y 255)))
          bad-grid-circle))

(def circles1
  (concat
   (circle-points 40 30 330 330)
   (circle-points 40 188 187 330)
   (circle-points 40 30 175 110)
   (circle-points 79 30 310 75)
   (circle-points 50 75 189 221)
   (circle-points 30 10 400 400)))

(def circles2
  (concat
   (circle-points 40 30 250 250)
   (circle-points 40 188 250 250)
   (circle-points 40 54 250 250)
   (circle-points 100 110 250 250)
   (circle-points 79 67 250 250)
   (circle-points 50 75 250 250)
   (grid 40 [50 50]  10 10 )
   (circle-points 30 10 400 400)))


(def zoom-bad-parts (zoom-by bad-part 2.5))

(def zoom-bad-part2 (translate-by (zoom-by bad-part2 1.4) [0 -100]))

(def bad-grids (concat
                (grid 10 [100 100] 30 30)
                (grid 10 [115 115] 30 30)))

(def bad-grids2 (concat
                (grid 10 [100 100] 30 30)
                (grid 10 [85 115] 30 30)))

(def some-cool-stuff
  (disj
   (set
    (concat
     (circle-points 200 200 550 300)
     (circle-points 280 250 200 300)
     (circle-points 50 50 200 300)
     (circle-points 100 300 725 300)
     (circle-points 200 100 300 400)
     (circle-points 200 200  300 550)
     (circle-points 400 100  300 550)))
   {:x 200 :y 550}))

(def bad-shit
  (points-lists-to-points
   [[250 100]
    ;;[249.99999999999997 140]
    [250 140]
    [150 140.21]
    [350 140.21]
    [250 400]]))

(def bad-circles
  (filter
   (fn [{x :x y :y}]
     (and (< x 260)
          (> x 240)
          (< y 160)
          (> y 122)))
   circles2))

(def bad-c2
  (circle-points 1000 150 300 300))

(ns app.svg
  (:require [re-frame.core :as rf]
            [clojure.string :as string]
            [voronoi.core :as vor]
            [voronoi.point :as point]
            [voronoi.util :refer [Infinity -Infinity isNaN? is-infinite?]]))

(defn line [x1 y1 x2 y2 attrs]
  [:line (into attrs {:x1 x1 :y1 y1 :x2 x2 :y2 y2 :stroke-width .3})])


(defn draw-extent [extent]
  (if (some? extent)
    (let [[xmin xmax ymin ymax] extent]
      [:g
       ^{:key 0} (line xmin ymin xmax ymin {:stroke "black"})
       ^{:key 1} (line xmin ymin xmin ymax {:stroke "black"})
       ^{:key 2} (line xmin ymax xmax ymax {:stroke "black"})
       ^{:key 3} (line xmax ymin xmax ymax {:stroke "black"})])))

(rf/reg-sub
  ::voronoi-points
  (fn [[_ q] _]
    (rf/subscribe q))
  (fn [vor _]
    (->> vor
         (:points)
         (map-indexed
           (fn [i {x :x y :y :as p}]
             ^{:key i}
             [:circle
              {:cx x :cy y :r 1 :stroke-width .1 :stroke "black" :fill "black"}]))
         (into [:g]))))

(rf/reg-sub
  ::voronoi-completed
  (fn [[_ q] _] (rf/subscribe q))
  (fn [vor _]
    (->> vor
         (:edges)
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
                         ^{:key i} [line bx by x y {:stroke "blue"}]))))))))
         (into [:g]))))


(defn polygon-points [points]
  (string/join " " (map #(str (:x %) "," (:y %)) points)))

(rf/reg-fx
  :register-polygon-handler
  (fn [{:keys [id handler]}]
    (rf/dispatch [:register-polygon-handler id handler])))

(rf/reg-event-db
  :register-polygon-handler
  (fn [db [_ id handler]]
    (update db :polygon-handlers
            (fn [handlers]
              (assoc (if (nil? handlers ) {} handlers) id handler)))))

(rf/reg-sub
  :polygon-handler
  (fn [db [_ q]]
    (if-let [h (get-in db [:polygon-handlers q])]
      h)))

(rf/reg-sub
  ::voronoi-polygons
  (fn [[_ q] _]
    [(rf/subscribe q)
     (rf/subscribe [:polygon-handler q])])
  (fn [[vor h] [_ q]]
    (some->> vor
             (vor/polygons)
             (remove #(some (fn [{x :x}] (is-infinite? x)) (:cell %)))
             (map-indexed
               (fn [i {points :cell site :site}]
                 ^{:key i} [:polygon {:points        (polygon-points points)
                                      :on-mouse-over #(if h (rf/dispatch [h site]))
                                      :on-click      (fn [] nil)}]))
             (into [:g.finished]))))

(defn voronoi-polygons [q]
  (fn []
    @(rf/subscribe [::voronoi-polygons q])))

(defn voronoi-points [q]
  (fn []
    @(rf/subscribe [::voronoi-points q])))

(defn voronoi-completed [q]
  (fn []
    @(rf/subscribe [::voronoi-completed q])))


(defn voronoi-group-im
  [q]
  [:g
   [voronoi-polygons q]
   [voronoi-points q]])

(defn voronoi-group
  [q]
  [:g
   [voronoi-points q]
   [voronoi-completed q]
   [voronoi-polygons q]])

(defn view-box-extent [extent widen]
  (let [[xmin xmax ymin ymax] (point/widen-by-percent extent widen)]
    (string/join " " [xmin ymin (- xmax xmin) (- ymax ymin)])))

(defn voronoi-svg
  "draws an svg
  expects a ratom for a app diagram"
  [q]
  (fn []
    (let [voronoi @(rf/subscribe q)
          extent (:extent voronoi)
          ]
      [:svg {:view-box  (view-box-extent extent 10)
             :preserveAspectRatio "xMidYMid meet"
             :style               {:max-height "95vh"
                                   :width      "100%"
                                   }}
       [draw-extent extent]
       [voronoi-group q]])))

(ns app.playground.events
  (:require [re-frame.core :as rf]
            [voronoi.core :as vor]
            [voronoi.points :as points]))

(defonce initial-points points/some-cool-stuff)

(rf/reg-event-db
  :playground/initialize
  (fn [db _]
    (-> db
        (assoc :playground/builder (vor/new-voronoi-builder initial-points))
        (assoc :blog/builder (vor/new-voronoi-builder (points/random-points 8)))
        (assoc :playground/scroll {:x -80
                                   :y -200
                                   :x-width 1200
                                   :y-width 1800
                                   :prev nil}))))

(rf/reg-event-db
  :playground/touch-cancel
  (fn [db _]
    (update-in db [:playground/scroll :prev] #(do nil))))

(rf/reg-cofx
  :now
  (fn [coeffects _]
    (assoc coeffects :now (.now js/Date))))


(rf/reg-event-db
  :playground/touch-move
  (fn [db [_ ev asdf c]]
    (.log js/console ev (.-touches ev) asdf c)
    (let [{:keys [prev x y x-width y-width] :as scroll} (:playground/scroll db)
          t (aget (.-touches ev) 0)
          scroll (assoc scroll :prev t)
          shift (.-shiftKey ev)
          scroll (if prev
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
                   scroll)]
      (assoc db :scroll scroll))))
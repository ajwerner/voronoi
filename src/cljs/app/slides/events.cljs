(ns app.slides.events
  (:require [re-frame.core :as rf]
            [voronoi.core :as vor]
            [voronoi.points :as points]
            [app.slides.db :as db]
            [app.playground.events]))

(rf/reg-event-db
  ::initialize
  (fn [db _]
    (assoc db :slides/builder
           (vor/new-voronoi-builder (points/random-points 100)))))

(rf/reg-event-db
  :stop-builder
  (fn [db [_ id]]
    (assoc-in db [:play-builder id] nil)))

(rf/reg-event-fx
  :play-builder
  [(rf/inject-cofx :now)]
  (fn [{db :db now :now} [_ id & {:keys [rate interval]
                                  :or {rate 50
                                       interval 16}}]]
    (if-let [s (get-in db [:play-builder id])]
      {:db db}
      {:db (db/start-builder db id rate interval now)
       :run-timer {:id    id
                   :start now
                   :wait  0
                   :rate  rate
                   :cur   (get-in db id)}})))

(rf/reg-event-db
  :reset-builder
  (fn [db [_ id]]
    (db/reset-builder db id)))

(rf/reg-event-fx
 :update-builder
 [(rf/inject-cofx :now)]
  (fn [{db :db now :now} [_ id & {:keys [start took vor]}]]
    (let [db (db/update-builder db id start vor now)
          {:keys [last rate interval]} (db/play-state db id)]
      (cond-> {:db db}
        (= last now)
        (assoc :run-timer
               {:id    id
                :rate rate
                :start now
                :wait  (max 0 (- interval took))
                :cur   vor})))))

(rf/reg-event-db
 :builder/next-event
 (fn [db [_ id]]
   (db/builder-next db id)))

(rf/reg-event-db
 :builder/prev-event
 (fn [db [_ id]]
   (db/builder-prev db id)))


(defn now []
  (.now js/Date))

(rf/reg-fx
  :run-timer
  (fn [{:keys [id start wait cur rate] :as args} _]
    (-> (fn []
          (let [t (now)
                delta-t (- t start)
                delta (* (/ rate 1000.0) delta-t)
                s (vor/scan-to cur (+ (:scan cur) delta))
                took (- (now) t)]
            (rf/dispatch [:update-builder id :took took :vor s :start start])))
        (js/setTimeout wait))))

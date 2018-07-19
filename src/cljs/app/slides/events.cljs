(ns app.slides.events
  (:require [re-frame.core :as rf]
            [voronoi.core :as vor]
            [voronoi.points :as points]
            [app.playground.events]))

(rf/reg-event-db
  ::initialize
  (fn [db _]
    (assoc db :slides/builder
              (vor/new-voronoi-builder (points/random-points 45)))))

(rf/reg-event-db
  :stop-builder
  (fn [db [_ id]]
    (assoc-in db [:play-builder id] nil)))

(rf/reg-event-fx
  :play-builder
  [(rf/inject-cofx :now)]
  (fn [{db :db now :now} [_ id & {:keys [rate interval]
                                  :or {rate 30
                                       interval 16}}]]
    (if-let [s (get-in db [:play-builder id])]
      {:db db}
      {:db        (update db :play-builder
                          (fn [bp]
                            (let [v {:rate rate
                                     :interval interval
                                     :last now}]
                              (if (some? bp)
                                (assoc bp id v)
                                {id v}))))
       :run-timer {:id    id
                   :start now
                   :wait  0
                   :rate  rate
                   :cur   (get-in db id)}})))

(rf/reg-event-db
  :reset-builder
  (fn [db [_ id]]
    (assoc-in
      (-> db (update-in id vor/reset-to 0))
      [:play-builder id]
      nil)))

(rf/reg-event-fx
  :update-builder
  [(rf/inject-cofx :now)]
  (fn [{db :db now :now} [_ id & {:keys [start took vor]}]]
    (let [{:keys [last interval rate] :as s} (get-in db [:play-builder id])]
      (if (or (nil? s)
              (not= last start))
        {:db db}
        (-> {:db (assoc-in db id vor)}
            (assoc-in [:db :play-builder id :last] now)
            (assoc :run-timer {:id    id
                               :rate rate
                               :start now
                               :wait  (max 0 (- interval took))
                               :cur   vor}))))))

(defn now []
  (.now js/Date))

(rf/reg-fx
  :run-timer
  (fn [{:keys [id start wait cur rate] :as args} _]
    ;; (println "running it " wait start rate)
    (-> (fn []
          (let [t (now)
                delta-t (- t start)
                delta (* (/ rate 1000.0) delta-t)
                s (vor/scan-to cur (+ (:scan cur) delta))
                took (- (now) t)]
            (rf/dispatch [:update-builder id :took took :vor s :start start])))
        (js/setTimeout wait))))


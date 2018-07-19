(ns app.slides.db
  (:require [voronoi.core :as vor]))

(defn get-prevs [db id]
  (get-in db [:builder/prev id]))

(defn clear-prevs [db id]
  (assoc-in db [:builder/prev id] nil))

(defn add-prev [db id prev]
  (update-in db [:builder/prev id]
             #(if (some? %)
                (conj % prev)
                (list prev))))

(defn get-prev [db id]
  (first (get-prevs db id)))

(defn pop-prev [db id]
  (update-in db [:builder/prev id]
             #(if (some? %) (rest %) %)))

(defn reset-builder [db id]
  (-> db
      (update-in id vor/reset-to 0)
      (clear-prevs id)
      (assoc-in [:play-builder id] nil)))

(defn play-state [db id]
  (get-in db [:play-builder id]))

(defn run-was-last? [db id now]
  (let [{:keys [last]} (play-state db id)]
    (= last now)))

(defn update-builder [db id start vor now]
  (let [{:keys [last] :as s} (play-state db id)]
    (if (or (nil? s)
            (not= last start))
      db
      (-> db
          (assoc-in [:play-builder id :last] now)
          (assoc-in id vor)))))

(defn start-builder [db id rate interval now]
  (-> db
      (update :play-builder
       (fn [bp]
         (let [v {:rate rate
                  :interval interval
                  :last now}]
           (if (some? bp)
             (assoc bp id v)
             {id v}))))
      (clear-prevs id)))

(defn builder-next [db id]
  (let [prev (get-in db id)]
    (if (some? (:events prev))
      (-> db
          (update-in id vor/handle-event)
          (add-prev id prev))
      db)))

(defn builder-prev [db id]
  (if-some [p (get-prev db id)]
    (-> db
        (assoc-in id p)
        (pop-prev id))
    db))

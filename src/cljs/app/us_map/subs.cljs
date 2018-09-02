(ns app.us-map.subs
  (:require [re-frame.core :as rf]
            [voronoi.core :as vor]
            [app.us-map.events]
            [cljsjs.d3]
            [app.svg :as svg]))

(def ak-hi-names
  #{"Alaska" "Hawaii"})


(rf/reg-sub
  :us-map/map-data-ready?
  (fn [db _]
    (and (some? (:us-map/city-data db))
         (some? (:us-map/map-data db)))))

(rf/reg-sub
  :us-map/map-data
  (fn [db _]
    (:us-map/map-data db)))

(rf/reg-sub
  :us-map/city-data
  (fn [db _]
    (:us-map/city-data db)))

(rf/reg-sub
  :us-map/top-n
  (fn [db _]
    (if-let [n (:us-map/top-n db)] n 20)))

(rf/reg-sub
  :us-map/year
  (fn [db _]
    (if-let [year (:us-map/year db)] year :2010)))

(rf/reg-sub
  :us-map/point-data-year
  :<- [:us-map/year]
  :<- [:us-map/city-data]
  (fn [[year city-data] _]
    (if city-data
      (->> city-data
           (remove #(let [y (year %)]
                      (or (nil? y)
                          (== 0 y))))
           (sort-by year >)))))

(rf/reg-sub
  :us-map/point-data
  :<- [:us-map/point-data-year]
  :<- [:us-map/top-n]
  (fn [[point-data n] _]
    (take n point-data)))

(rf/reg-sub
  :us-map/real-n
  :<- [:us-map/point-data]
  (fn [point-data _]
    (count point-data)))

(rf/reg-sub
  :us-map/map-vor
  :<- [:us-map/point-data]
  (fn [point-data _]
    (if point-data
      (-> point-data
          (vor/new-voronoi-builder :extent [0 1000 0 1000])
          (vor/finish-builder)))))

(def albers-usa (js/d3.geoAlbersUsa))

(rf/reg-sub
  :us-map/view-box
  (fn [_ [_ id]]
    (rf/subscribe [id]))
  (fn [vor _]
    (svg/view-box-extent (:extent vor) 10)))

(rf/reg-sub
  :us-map/us-path
  :<- [:us-map/map-data]
  (fn [map-data _]
    (if map-data
      (let [path (js/d3.geoPath albers-usa)]
        (->> (:features map-data)
             (remove #(ak-hi-names (:NAME (:properties %))))
             (map (fn [f]
                    ^{:key (:NAME (:properties f))}
                    [:path {:d (path (clj->js f))}])))))))

(rf/reg-sub
  :us-map/cur-city
  (fn [db _]
    (:us-map/cur-city db)))

(rf/reg-sub
  :us-map/cur-city-info
  :<- [:us-map/cur-city]
  :<- [:us-map/year]
  (fn [[city year] _]
    (if (and city year)
      (str (:CityST city) " " (year city)))))

(rf/reg-sub
  :us-map/max-n
  :<- [:us-map/point-data-year]
  (fn [point-data _]
    (min 200 (count point-data))))

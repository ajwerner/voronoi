(ns app.us-map.events
  (:require [re-frame.core :as rf]
            [day8.re-frame.http-fx]
            [goog.labs.format.csv :as csv]
            [ajax.core :as ajax]
            [cljsjs.d3]))

(rf/reg-event-fx
  ::request-failed
  (fn [co x]
    (println x)
    co))
(rf/reg-event-fx
  ::initialize
  (fn [{db :db} _]
    {:db db
     ::initialize {}}))

(rf/reg-fx
  ::initialize
  (fn [_ _]
    (rf/dispatch [:us-map/fetch-city-data])
    (rf/dispatch [:us-map/fetch-map-data])
    (rf/dispatch [:register-polygon-handler [:us-map/map-vor] :us-map/polygon-over])))


;; ----------------------------------------------------------------
;; City data parsing
;;

(def ak-hi-abrevs
  #{"AK" "HI"})

(defn parse-csv-data [raw-data]
  (let [data (csv/parse raw-data)
        keys (->> (first data)
                  (map keyword)
                  repeat)]
    (map zipmap keys (rest data))))

(def year-re #"[0-9]{4}")
(defn ^:private cast-year-map-ints
  [city-map]
  (loop [ks (keys city-map) m city-map]
    (if-some [k (first ks)]
      (if (re-matches year-re (name k))
        (recur (rest ks) (update m k int))
        (recur (rest ks) m))
      m)))

(def albers-usa (js/d3.geoAlbersUsa))

(defn add-projected-coordinates [{lat :LAT_BING lon :LON_BING :as data}]
  (let [[x y] (albers-usa (clj->js [lon lat]))]
    (assoc data :x x :y y)))

(rf/reg-event-db
  ::process-city-data
  (fn [db [_ response]]
    (let [parsed (->> (parse-csv-data response)
                      (remove #(ak-hi-abrevs (:ST %)))
                      (map cast-year-map-ints)
                      (map add-projected-coordinates))]
      (assoc db :us-map/city-data parsed))))

(rf/reg-event-fx
  :us-map/fetch-city-data
  (fn [{db :db} _]
    {:db         db
     :http-xhrio {:method          :get
                  :uri             "assets/1790-2010_MASTER.csv.txt"
                  :response-format (ajax/text-response-format)
                  :on-success      [::process-city-data]
                  :on-failure      [::request-failed]}}))

;; ----------------------------------------------------------------
;; Map data
;;

(rf/reg-event-db
  ::process-map-data
  (fn [db [_ response]]
    (assoc db :us-map/map-data response)))


(rf/reg-event-fx
  :us-map/fetch-map-data
  (fn [{db :db} _]
    {:http-xhrio {:method          :get
                  :uri             "assets/gz_2010_us_040_00_500k.json"
                  :on-success      [::process-map-data]
                  :on-failure      [::request-failed]
                  :response-format (ajax/json-response-format {:keywords? true})}
     :db         db}))

;; ----------------------------------------------------------------
;; Inputs
;;

(rf/reg-event-db
  :us-map/polygon-over
  (fn [db [_ site]]
    (assoc db :us-map/cur-city site)))

(rf/reg-event-fx
  :us-map/set-top-nx
  (fn [{db :db} [_ n]]
    (let [db (assoc db :us-map/top-n n)
          year (:us-map/year db)
          vor (get-in db [:us-map/vor year n])]
      (cond-> {:db db}))))

(rf/reg-event-db
  :us-map/set-top-n
  (fn [db [_ n]]
    (assoc db :us-map/top-n n)))

(rf/reg-event-db
  :us-map/set-year
  (fn [db [_ year]]
    (assoc db :us-map/year year)))

(rf/reg-event-db
  :us-map/polygon-over
  (fn [db [_ site]]
    (assoc db :us-map/cur-city site)))

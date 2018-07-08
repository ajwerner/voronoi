(ns voronoi.components.us-map
  (:require [voronoi.voronoi :as vor]
            [goog.labs.format.csv :as csv]
            [voronoi.components.arc-table :refer [arc-table-and-toggle]]
            [voronoi.components.svg :refer [voronoi-svg interactive-svg voronoi-group-im]]
            [voronoi.components.events-panel :refer [events-panel]]
            [voronoi.components.control-panel :refer [control-panel]]
            [cljs.core.async :refer [<!]]
            [re-com.core :as rc]
            [re-frame.core :as rf]
            [day8.re-frame.http-fx]
            [ajax.core :as ajax]
            [cljsjs.d3]))


(rf/reg-event-fx
  ::request-failed
  (fn [co x]
    (println x)
    co))

(rf/reg-event-fx
  ::get-map-data
  (fn [{db :db} _]
    {:http-xhrio {:method          :get
                  :uri             "assets/gz_2010_us_040_00_500k.json"
                  :on-success      [::process-map-data]
                  :on-failure      [::request-failed]
                  :response-format (ajax/json-response-format {:keywords? true})}
     :db         db}))

(rf/reg-event-db
  ::process-map-data
  (fn [db [_ response]]
    (assoc db ::map-data response)))

(rf/reg-event-fx
  ::get-city-data
  (fn [{db :db} _]
    {:db         db
     :http-xhrio {:method          :get
                  :uri             "assets/1790-2010_MASTER.csv.txt"
                  :response-format (ajax/text-response-format)
                  :on-success      [::process-city-data]
                  :on-failure      [::request-failed]}}))

(def ak-hi-names
  #{"Alaska" "Hawaii"})

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
      (assoc db ::city-data parsed))))

(rf/reg-event-db
  ::set-top-n
  (fn [db [_ n]]
    (assoc db ::top-n n)))

(rf/reg-event-db
  ::set-year
  (fn [db [_ year]]
    (assoc db ::year year)))

(rf/reg-event-db
  :polygon-over
  (fn [db [_ site]]
    (assoc db ::cur-city site)))

(rf/reg-sub
  ::map-data-ready?
  (fn [db _]
    (and (some? (::city-data db))
         (some? (::map-data db)))))

(rf/reg-sub
  ::map-data
  (fn [db _]
    (::map-data db)))

(rf/reg-sub
  ::city-data
  (fn [db _]
    (::city-data db)))

(rf/reg-sub
  ::top-n
  (fn [db _]
    (if-let [n (::top-n db)] n 20)))

(rf/reg-sub
  ::year
  (fn [db _]
    (if-let [year (::year db)] year :2010)))

(rf/reg-sub
  ::point-data-year
  :<- [::year]
  :<- [::city-data]
  (fn [[year city-data] _]
    (if city-data
      (->> city-data
           (remove #(nil? (year %)))
           (sort-by year >)))))

(rf/reg-sub
  ::point-data
  :<- [::point-data-year]
  :<- [::top-n]
  (fn [[point-data n] _]
    (take n point-data)))

(rf/reg-sub
  ::map-vor
  :<- [::point-data]
  (fn [point-data _]
    (if point-data
      (-> point-data
          (vor/new-voronoi-builder :extent [0 1000 0 1000])
          (vor/finish-builder)))))

(rf/reg-sub
  ::us-path
  :<- [::map-data]
  (fn [map-data _]
    (if map-data
      (let [path (js/d3.geoPath albers-usa)]
        (->> (:features map-data)
             (remove #(ak-hi-names (:NAME (:properties %))))
             (map (fn [f]
                    ^{:key (:NAME (:properties f))}
                    [:path {:d (path (clj->js f))}])))))))

(rf/reg-sub
  ::cur-city
  (fn [db _]
    (::cur-city db)))

(rf/reg-sub
  ::cur-city-info
  :<- [::cur-city]
  :<- [::year]
  (fn [[city year] _]
    (if (and city year)
      (str (:CityST city) " " (year city)))))


(defn map-svg []
  (fn []
    (let [us @(rf/subscribe [::us-path])]
      [rc/box
       :child
       [:svg {:style {:width        "960px"
                      :height       "500px"
                      :stroke       "#aaa"
                      :stroke-width 0.5
                      :fill         "none"}}
        [:defs
         [:clipPath {:id "ko"} us]]
        [:g {:id "usa"} us]
        [:g {:clip-path "url(#ko)"}
         [voronoi-group-im (rf/subscribe [::map-vor])]]]])))

(defn map-title [n y]
  [rc/box
   :align :center
   :child
   [rc/title
    :level :level1
    :label (str "Top " n " cities by population in the continental US in " y)]])

(defn year-slider [y]
  [rc/h-box
   :align :start
   :children
   [[rc/box :width "50px" :child [rc/title :level :level3 :label "Year"]]
    [rc/box :width "50px" :child (str y)]
    [rc/box
     :child
     [rc/slider :width "700px" :model y :min 1790 :max 2010 :step 10
      :on-change #(rf/dispatch [::set-year (keyword (str %))])]]]])

(defn top-n-slider [n]
  [rc/h-box
   :align :start
   :children
   [[rc/box :size "50px" :child [rc/title :level :level3 :label "Top"]]
    [rc/box :size "50px" :child (str n)]
    [rc/box :size "1"
     :child [rc/slider :width "700px" :model n :min 3 :max 100
             :on-change #(rf/dispatch [::set-top-n %])]]]])

(defn cur-city-info []
  (fn []
    (let [cur-city @(rf/subscribe [::cur-city-info])]
      [rc/box :child
       (if cur-city
         [:p cur-city]
         [:p {:style {:font-style "italic"}}
          "Hover over a city"])])))

(defn map-sliders [n y]
  [rc/v-box
   :align :start
   :children
   [[year-slider y]
    [top-n-slider n]]])

(defn map-comp []
  (fn []
    (let [y (int (name @(rf/subscribe [::year])))
          n @(rf/subscribe [::top-n])]
      [rc/v-box
       :align :center
       :children
       [[map-title n y]
        [map-svg]
        [cur-city-info]
        [map-sliders n y]]])))

(defn map-thing []
  (fn []
    (if @(rf/subscribe [::map-data-ready?])
      [map-comp]
      [rc/throbber :size :large])))

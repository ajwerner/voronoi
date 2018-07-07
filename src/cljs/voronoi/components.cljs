(ns voronoi.components
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [voronoi.voronoi :as vor]
            [goog.labs.format.csv :as csv]
            [voronoi.points :as points]
            [voronoi.components.arc-table :refer [arc-table-and-toggle]]
            [voronoi.components.svg :refer [voronoi-svg interactive-svg voronoi-group]]
            [voronoi.components.events-panel :refer [events-panel]]
            [voronoi.components.control-panel :refer [control-panel]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [cljsjs.topojson :as topojson]
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
  :get-map-data
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
  :get-city-data
  (fn [coeffects _]
    (assoc coeffects
      :http-xhrio {:method          :get
                   :uri             "assets/1790-2010_MASTER.csv.txt"
                   :response-format (ajax/text-response-format)
                   :on-success      [::process-city-data]
                   :on-failure      [::request-failed]})))

(def year-re #"[0-9]{4}")
(defn ^:private cast-year-map-ints
  [city-map]
  (loop [ks (keys city-map) m city-map]
    (let [k (first ks)]
      (if (nil? k)
        m
        (if (re-matches year-re (name k))
          (recur (rest ks) (update m k int))
          (recur (rest ks) m))))))

(rf/reg-event-db
  ::process-city-data
  (fn [db [_ response]]
    (let [data (csv/parse response)
          as-maps (map zipmap
                       (->> (first data)
                            (map keyword)
                            repeat)
                       (rest data))
          city-data (map cast-year-map-ints as-maps)]
      (assoc db ::city-data city-data))))

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

(enable-console-print!)

(def albers-usa (js/d3.geoAlbersUsa))

(def ak-hi-names
  #{"Alaska" "Hawaii"})

(def ak-hi-abrevs
  #{"AK" "HI"})

(defn map-svg [us vor]
  [:svg {:style {:width        "960px"
                 :height       "500px"
                 :stroke       "#aaa"
                 :stroke-width 0.5
                 :fill         "none"}}
   [:defs
    [:clipPath {:id "ko"} us]]
   [:g {:clip-path "url(#ko)"}
    [voronoi-group vor]]
   [:g {:id "usa"}
    us]])

(defn map-comp [m point-data]
  (let [path (js/d3.geoPath albers-usa)
        us (->> (:features m)
                (remove #(ak-hi-names (:NAME (:properties %))))
                (map (fn [f]
                       ^{:key (:NAME (:properties f))}
                       [:path {:d (path (clj->js f))}])))
        make-vor (fn [n k]
                   (let [p (->> point-data
                                (remove #(ak-hi-abrevs (:ST %)))
                                (remove #(nil? (k %)))
                                (sort-by k >)
                                (take n)
                                (map (fn [{lat :LAT_BING
                                           lon :LON_BING :as data}]
                                       (let [[x y] (albers-usa (clj->js [lon lat]))]
                                         {:x x :y y :p data}))))]
                     (-> p
                         (vor/new-voronoi :extent [0 1000 0 1000])
                         (vor/finish))))
        set-nk (fn [v n k]
                 (-> v
                     (assoc :n n)
                     (assoc :k k)
                     (assoc :vor (make-vor n k))))
        set-n (fn [{k :k :as v} n]
                (set-nk v n k))
        set-k (fn [{n :n :as v} k]
                (set-nk v n k))
        state (atom (set-nk {} 20 :2010))
        n-c (reagent/cursor state [:n])
        k-c (reagent/cursor state [:k])
        vor-c (reagent/cursor state [:vor])
        update-n (fn [n] (swap! state set-n n))
        update-k (fn [kn]
                   (let [k (keyword (str kn))]
                     (swap! state set-k k)))]
    (fn []
      (let [k (int (name @k-c))
            n @n-c]
        [rc/v-box
         :children
         [[rc/box
           :align :center
           :child [rc/title  :label (str "Top " n " Cities in the Continental US in " k)]]
          [rc/box
           :child [map-svg us vor-c]]
          [rc/h-box
           :align :start

           :children
           [[rc/box :width "50px" :child [rc/title :level :level3 :label "Year"]]
            [rc/box :width "50px" :child (str k)]
            [rc/box
             :child
             [rc/slider :width "700px" :model k :min 1790 :max 2010 :step 10 :on-change update-k]]
            ]]
          [rc/h-box
           :justify :center
           :children
           [[rc/box :size "50px" :child [rc/title :level :level3 :label "Top"]]
            [rc/box :size "50px" :child (str n)]
            [rc/box :size "1"
             :child [rc/slider
                     :width "700px"
                     :model n
                     :min 3
                     :max 100
                     :on-change update-n]]]]]]))))

(defn map-thing []
  (fn []
    (let [m @(rf/subscribe [::map-data])
          d @(rf/subscribe [::city-data])
          r @(rf/subscribe [::map-data-ready?])]
      [rc/h-box
       :justify :center
       :children
       [[rc/box
         :child
         (if (and m d)
           [:div
            [map-comp m d]
            [:a {:href "#/misc"} "More Examples ->"]]
           [:div
            [:a {:href "#/misc"} "More Examples ->"]])]]])))

(defonce initial-points points/some-cool-stuff)

(defn new-app-thing [db id]
  (swap! db #(if % % {:voronoi (vor/new-voronoi initial-points)
                      :id      id
                      :scroll  nil}))
  (let [vor (reagent/cursor db [:voronoi])
        scroll (reagent/cursor db [:scroll])]
    (fn []
      [:section.voronoi-widget {:id id}
       [:div.graphics
        {:style {:position "fixed"
                 :overflow "hidden"}}
        [interactive-svg vor scroll]]
       [control-panel db]
       [arc-table-and-toggle vor]
       ;; [events-panel vor]
       ])))

(defn animation-playground [db]
  (let []
    (fn []
      [:div [:h2 "Voronoi diagrams"]
       [:div [:p "Just a pretty picture for now"]]
       [new-app-thing db "animation-playground"]
       [:div [:a {:href "#/intro"} "<- back"]]])))


(def misc-state
  [{:id     :crazy
    :points points/some-cool-stuff}
   {:id     :random
    :points (points/random-points 2000)}
   {:id     :circle
    :points (points/circle-points 71 200 100 100)}
   {:id     :circle2
    :points (points/circle-points 53 200 100 100)}
   {:id     :only-2
    :points [{:x 100 :y 100} {:x 200 :y 110}]
    :extent [70 220 90 120]}
   {:id     :only-2-1
    :points [{:x 100 :y 100} {:x 200 :y 100}]
    :extent [70 220 90 120]}
   {:id     :only-2-2
    :points [{:x 200 :y 200} {:x 200 :y 100}]
    :extent [70 220 90 300]}
   {:id     :only-2-3
    :points [{:x 200 :y 201} {:x 200 :y 100}]
    :extent [70 220 90 300]}
   {:id     :clip-bottom
    :points [{:x 210 :y 300} {:x 300 :y 100} {:x 49 :y 21}]}
   {:id     :clip-right
    :points [{:x 200 :y 180} {:x 300 :y 100} {:x 49 :y 21}]}
   {:id     :clip-top
    :points [{:x 200 :y 180} {:x 300 :y 100} {:x 251 :y 25}]}
   {:id     :clip-left
    :points [{:x 10 :y 180} {:x 300 :y 100} {:x 251 :y 40}]}
   {:id     :clip-left-straight
    :points [{:x 50 :y 25} {:x 50 :y 50} {:x 100 :y 75} {:x 100 :y 25}]}
   {:id     :clip-up-straight
    :points [{:x 50 :y 25} {:x 50 :y 75} {:x 100 :y 75} {:x 100 :y 25}]}
   {:id     :clip-up-right
    :points [{:x 50 :y 25} {:x 79 :y 100} {:x 100 :y 25}]}
   {:id     :clip-up-left
    :points [{:x 50 :y 25} {:x 73 :y 100} {:x 100 :y 25}]}
   {:id     :clip-up-middle
    :points [{:x 50 :y 25} {:x 75 :y 100} {:x 100 :y 25}]}
   {:id     :grid
    :points (points/grid 20 [0 0] 20 20)}])

(defn misc [db]
  (swap! db #(if %
               %
               (into {} (map (fn [s]
                               {(:id s)
                                (vor/finish (vor/new-voronoi (:points s) :extent (:extent s)))})
                             misc-state))))
  (let []
    (fn []
      [:div
       (into
         [:div.misc]
         (for [k (keys @db)]
           [:div {:id k} ^{:key k} [voronoi-svg (reagent/cursor db [k])]]))
       [:div.links
        [:a {:href "#/map"} "<- Map"]
        [:br]
        [:a {:href "#/intro"} "Tell me more ->"]]])))

(defn bulleted-list [& li-text-items]
  [:ul (map #(into ^{:key %} [:li] %) li-text-items)])

(defn intro []
  [:div [:h2 "Voronoi Diagrams"]
   [:div
    [:div
     [:h3 "What is this?"]
     [:p "This post is primarily about Voronoi diagram but along the way it's also about:"]
     [bulleted-list
      "Clojure/Clojurescript"
      "React/Reagent and Single Page applications"
      "Drawing in the browser (Processing/Quil and SVGs)"
      "Robust geometric predicates with floating point"]
     [:h3 "What is this not?"]
     [:p "Novel, this project has no novel contributions to offer to the world."
      " Any seemingly deep insight was much more deeply pursued by somebody else."
      " I'll try to point references to some things which I glanced at but often gave up on understanding completely for the moment."]]]
   [:div
    [:a {:href "#/misc"} "<- Examples"]
    [:br]
    [:a {:href "#/animation-playground"} "Interactive playground ->"]
    [:br]
    [:a {:href "#/voronoi-diagrams"} "About Diagrams ->"]]])

(defn voronoi-diagrams []
  [:div
   [:h2 "What's a Voronoi Diagram"]
   [:div
    [:p
     "Say you have a bunch of points in plane. "
     "Maybe these bare cities on a map*"
     "And you want to know the area closest to each city based on distance."
     "Voronoi diagrams are your tool"]
    [:p
     "A common use case for these diagrams are to create tooltips for graphs"
     "Efficient algorithms exist to find which polygon in a set contain some point"
     "(See point containment search, these are actually easy to make much faster when you can ensure that the polygons do not overlap)"
     "See BSP trees"]
    [:div
     [:a {:href "#/intro"} "<- Intro"]]]])

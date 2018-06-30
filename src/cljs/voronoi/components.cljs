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
            [re-com.core :as r]
            [cljsjs.d3]))

(defonce initial-points points/some-cool-stuff)

(defn get-map [db]
  (go
    (let [response (<! (http/get "assets/gz_2010_us_040_00_500k.json"
                                 {:with-credentials? false}))
          u  (:body response)]
      (reset! db u))))

(def year-re #"[0-9]{4}")

(defn get-data [db]
  (go (let [response (<! (http/get "assets/1790-2010_MASTER.csv.txt"
                                   {:with-credentials? false}))
            data (csv/parse (:body response))
            m (map zipmap
                   (->> (first data)
                        (map keyword)
                        repeat)
                   (rest data))
            f (fn [m]
                (loop [ks (keys m) m m]
                  (let [k (first ks)]
                    (if (nil? k)
                      m
                      (if (re-matches year-re (name k))
                        (recur (rest ks) (update m k int))
                        (recur (rest ks) m))))))
            mm (map f m)]
        (reset! db mm))))

(enable-console-print!)

(def albers-usa (js/d3.geoAlbersUsa))

(def ak-hi-names
  #{"Alaska" "Hawaii"})

(def ak-hi-abrevs
  #{"AK" "HI"})

(defn map-svg [us vor]
  [:svg {:style {:width "960px"
                 :height "500px"
                 :stroke "#aaa"
                 :stroke-width 0.5
                 :fill "none"}}
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
        [:div
         [map-svg us vor-c]
         [r/v-box
          :children
          [[r/h-box
            :align :start
            :children
            [[r/box :width "50px" :child [r/title :level :level3 :label "Year"]]
             [r/box :width "50px" :child (str k)]
             [r/box
              :child [r/slider
                      :model k
                      :min 1790
                      :max 2010
                      :step 10
                      :on-change update-k]]
             ]]
           [r/h-box
            :children
            [[r/box :width "50px" :child [r/title :level :level3 :label "Top"]]
             [r/box :width "50px" :child (str n)]
             [r/box :size "auto"
              :child [r/slider
                      :model n
                      :min 3
                      :max 100
                      :on-change update-n]]]]]]] ))))

(defn map-thing [map-c data-c]
  (fn []
    (let [m @map-c
          d @data-c]
      (if (and m d)
        [:div
         [map-comp m d]
         [:a {:href "#/misc"} "More Examples ->"]]
        [:div
         [:a {:href "#/misc"} "More Examples ->"]]))))

(defn new-app-thing [db id]
  (swap! db #(if % % {:voronoi (vor/new-voronoi initial-points)
                      :id id
                      :scroll nil}))
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

(defonce new-misc-state
  {:crazy (vor/finish (vor/new-voronoi initial-points))
   :random (vor/finish (vor/new-voronoi (points/random-points 2000)))
   :circle (vor/finish (vor/new-voronoi (points/circle-points 71 200 100 100)))
   :circle2 (vor/finish (vor/new-voronoi (points/circle-points 53 200 100 100)))
   :only-2 (vor/finish (vor/new-voronoi [{:x 100 :y 100}
                                         {:x 200 :y 110}]
                                        :extent [70 220 90 120]))
   :only-2-1 (vor/finish (vor/new-voronoi [{:x 100 :y 100}
                                           {:x 200 :y 100}]
                                          :extent [70 220 90 120]))
   :only-2-2 (vor/finish (vor/new-voronoi [{:x 200 :y 200}
                                           {:x 200 :y 100}]
                                          :extent [70 220 90 300]))
   :only-2-3 (vor/finish (vor/new-voronoi [{:x 200 :y 201}
                                           {:x 200 :y 100}]
                                          :extent [70 220 90 300]))
   :clip-bottom (vor/finish (vor/new-voronoi [{:x 210 :y 300}
                                              {:x 300 :y 100}
                                              {:x 49 :y 21}]))
   :clip-right (vor/finish (vor/new-voronoi [{:x 200 :y 180}
                                             {:x 300 :y 100}
                                             {:x 49 :y 21}]))
   :clip-top (vor/finish (vor/new-voronoi [{:x 200 :y 180}
                                           {:x 300 :y 100}
                                           {:x 251 :y 25}]))
   :clip-left (vor/finish (vor/new-voronoi [{:x 10 :y 180}
                                            {:x 300 :y 100}
                                            {:x 251 :y 40}]))
   :clip-left-straight (vor/finish (vor/new-voronoi [{:x 50 :y 25}
                                                     {:x 50 :y 50}
                                                     {:x 100 :y 75}
                                                     {:x 100 :y 25}]))
   :clip-up-straight (vor/finish (vor/new-voronoi [{:x 50 :y 25}
                                                   {:x 50 :y 75}
                                                   {:x 100 :y 75}
                                                   {:x 100 :y 25}]))
   :clip-up-right (vor/finish (vor/new-voronoi [{:x 50 :y 25}
                                                {:x 79 :y 100}
                                                {:x 100 :y 25}]))
   :clip-up-left (vor/finish (vor/new-voronoi [{:x 50 :y 25}
                                               {:x 73 :y 100}
                                               {:x 100 :y 25}]))
   :clip-up-middle (vor/finish (vor/new-voronoi [{:x 50 :y 25}
                                                 {:x 75 :y 100}
                                                 {:x 100 :y 25}]))
   :grid (vor/finish (vor/new-voronoi (points/grid 20 [0 0] 20 20)))})

(defn misc [db]
  (swap! db #(if % % new-misc-state))
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

(defn intro []
  [:div [:h2 "Voronoi Diagrams"]
   [:div
    [:div
     [:h3 "What is this?"]
     [:p
      "This post is primarily about Voronoi diagram"
      " but along the way it's also about:"]
     [:ul (map #(into ^{:key %} [:li] %)
               ["Clojure/Clojurescript"
                "React/Reagent and Single Page applications"
                "Drawing in the browser (Processing/Quil and SVGs)"
                "Robust geometric predicates with floating point"])]
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

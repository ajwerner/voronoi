(ns voronoi.components
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [voronoi.voronoi :as vor]
            [voronoi.points :as points]
            [voronoi.components.arc-table :refer [arc-table-and-toggle]]
            [voronoi.components.svg :refer [voronoi-svg interactive-svg voronoi-group]]
            [voronoi.components.events-panel :refer [events-panel]]
            [voronoi.components.control-panel :refer [control-panel]]
            [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]
            [cljsjs.topojson :as topojson]
            [cljsjs.d3] ))

(defonce initial-points points/some-cool-stuff)

(defn get-map [db]
  (go (let [response (<! (http/get "assets/gz_2010_us_040_00_500k.json"
                                   {:with-credentials? false}))]
        (let [u (clj->js (:body response))]
          (reset! db u)))))

(enable-console-print!)

(def albers-usa (js/d3.geoAlbersUsa))

(defn place [city state lat-lon pop]
  {:city city
   :state state
   :loc lat-lon
   :pop pop})

(def places
  [(place "Norfolk" "VA" [-76.2859, 36.8508] 245115)
   (place "New York" "NY" [-74.0060 40.7128] 8538000)
   (place "Houston" "TX"   [-95.3698 29.7604] 2303000)
   (place "San Francisco" "CA" [-122.4194 37.7759] 870887)
   (place "Chicago" "IL" [-87.6298 41.9791] 2705000)
   (place "Miami" "FL" [-80.1918 25.7617] 453579)]
  )

(def place-points
  (map #(albers-usa (clj->js (:loc %))) places))

(defn map-thing [map-c]
  (let [p (js/d3.geoPath albers-usa)]
    (fn []
      (if-let [m @map-c]
        (let [us (->> (.-features m)
                      (remove #(or (=  "Alaska" (.-NAME (.-properties %)))
                                   (=  "Hawaii" (.-NAME (.-properties %)))))
                      (map (fn [f]
                             ^{:key (.-NAME (.-properties f))}
                             [:path {:d (p f)}])))
              p (map (fn [[x y]] {:x x :y y}) place-points)
              pp (into [] p)
              v (vor/new-voronoi pp :extent [0 1000 0 1000])
              vv (vor/finish v)]
          [:svg {:style {:width "960px"
                         :height "900px"
                         :stroke "#aaa"
                         :stroke-width 0.5
                         :fill "none"}}
           [:defs
            [:clipPath {:id "ko"} us]]
           [:g {:clip-path "url(#ko)"}
            [voronoi-group (atom vv)]]
           [:g {:id "usa"}
            us]
           [:g {:id "points"}
            (for [[x y :as p] place-points]
              ^{:key p} [:circle {:cx x :cy y :r 1 :fill "black"}])]])))))

(defn new-app-thing [db id]
  (swap! db #(if % % {:voronoi (vor/new-voronoi [{:x 50 :y 25}
                                                 {:x 100 :y 25}]
                                                :extent [0 150 0 50])
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
    [:a {:href "#/animation-playground"} "voronoi diagrams ->"]
    [:br]
    [:a {:href "#/misc"} "misc <-"]
    [:br]
    [:a {:href "#/voronoi-diagrams"} "about diagrams"]]])

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
     "See BSP trees"]]])

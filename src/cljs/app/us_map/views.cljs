(ns app.us-map.views
  (:require [re-com.core :as rc]
            [app.svg :as svg]
            [app.us-map.subs]
            [re-frame.core :as rf]))

(defn map-svg []
  (fn []
    (let [us @(rf/subscribe [:us-map/us-path])]
      [rc/box
       :width "100%"
       :min-width "400px"
       :min-height "200px"
       :size "initial"
       :child
       [:svg {:style {:height "100%"
                      :stroke       "#aaa"
                      :stroke-width 0.5
                      :fill         "none"}
              :view-box "50 0 875 500"
              :preserve-aspect-ratio "xMidYMax meet"}
        [:defs
         [:clipPath {:id "ko"} us]]
        ;; [:g {:id "usa"} us] ;; adds US states
        [:g
         [svg/voronoi-points [:us-map/map-vor]]]
        [:g {:clip-path "url(#ko)"}
         [svg/voronoi-group-im [:us-map/map-vor]]]]])))


(defn map-title []
  (fn []
    (let [n @(rf/subscribe [:us-map/real-n])
          y @(rf/subscribe [:us-map/year])]
      [rc/box
       :align :center
       :child
       [rc/title
        :style {:text-align :center}
        :level :level3
        :label (str "Top " n " cities by population in the continental US in "
                    (name y))]])))

(defn slider [model min max step on-change]
  [rc/h-box
   :align :start
   :children
   [[rc/box :size "38px" :child (str model)]
    [rc/box :size "1"
     :child [rc/slider :width "350px" :model model :min min :max max :step step
             :on-change on-change
             :style {:height "30px"} :class "slides"]]]])

(defn year-slider []
  (fn []
    (let [y @(rf/subscribe [:us-map/year])]
      [slider (int (name y)) 1790 2010 10
       #(rf/dispatch [:us-map/set-year (keyword (str %))])])))

(defn top-n-slider []
  (fn []
    (let [n @(rf/subscribe [:us-map/real-n])
          max-n @(rf/subscribe [:us-map/max-n])]
      [slider n 3 max-n 1
       #(rf/dispatch [:us-map/set-top-n %])])))

(defn cur-city-info []
  (fn []
    (let [cur-city @(rf/subscribe [:us-map/cur-city-info])]
      [rc/box :child
       (if cur-city
         [:p cur-city]
         [:p {:style {:font-style "italic"}}
          "Hover over a city"])])))

(defn map-sliders []
  [rc/v-box
   :align :start
   :children
   [[year-slider]
    [top-n-slider]]])

(defn map-comp []
  [rc/v-box
   :align :center
   :min-width "375px"
   :max-height "94vh"
   :size "auto"
   :children
   [[map-title]
    [map-svg]
    [cur-city-info]
    [map-sliders]]])

(defn loading []
  [rc/box
   :align :center
   :justify :center
   :width "100%"
   :height "100%"
   :size "1 0 auto"
   :margin "auto"
   :child [rc/throbber :size :large]])

(defn map-thing []
  (fn []
    (if @(rf/subscribe [:us-map/map-data-ready?])
      [map-comp]
      [loading])))

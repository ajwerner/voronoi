(ns voronoi.core
  (:require [clojure.string :as str]
            [reagent.core :as reagent :refer [atom]]
            [re-frame.core :as rf]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [voronoi.voronoi :as vor]
            [voronoi.control :refer [reset-state!]]
            [voronoi.points :as p]
            [voronoi.components :as components]
            [voronoi.routes :as routes]))

;; -------------------------
;; Routing

(secretary/defroute map-p #"/map" []
  (rf/dispatch [:page :map]))

(secretary/defroute intro-p #"/(intro)?" []
  (rf/dispatch [:page :intro]))

(secretary/defroute misc-p "/misc" []
  (rf/dispatch [:page :tests]))

(secretary/defroute animation-page "/animation-playground" []
  (rf/dispatch [:page :playground]))

(secretary/defroute voronoi-description "/voronoi-diagrams" []
  (rf/dispatch [:page :voronoi-diagrams-intro]))


;; -------------------------
;; State

(defonce app-state (atom {:animation-page nil
                          :misc nil
                          :map-page {:outline nil
                                     :data nil}}))

;; -------------------------
;; Event handlers

(rf/reg-event-fx
 :initialize
 (fn [{}  _]
   {:db {:page :intro
         :animation-page nil
         :misc nil
         :map-page {:outline nil
                    :data nil}}
    :load-all-test-data nil}))

(rf/reg-event-fx
  :ev-load-test-data
  (fn [coeffects [_ test]]
    (assoc coeffects :load-test-data test)))

(rf/reg-fx
 :load-all-test-data
 (fn [fx _]
   (for [test components/misc-state]
     (rf/dispatch [:ev-load-test-data test]))
   fx))

(rf/reg-event-db
  :loaded-test-data
  (fn [db [_ id vor]]
    (update db :test-data
            #(if % (assoc % id vor) {id vor}))))

(rf/reg-fx
  :load-test-data
  (fn [_ [_ {id :id points :points extent :extent}]]
    (rf/dispatch [:loaded-test-data id (vor/finish (vor/new-voronoi points :extent extent))])))

(rf/reg-event-db
 :page
 (fn [db [_ new-page]]
   (assoc db :page new-page)))

(rf/reg-sub
 :current-page
 (fn [db _] (:page db)))

(secretary/set-config! :prefix "#")

;; -------------------------
;; Views
;; -------------------------

(reagent/cursor app-state [:animation-page])

(defonce misc-page
  #(components/misc
    (reagent/cursor app-state [:misc])))

(defonce map-page
  #(components/map-thing))

(defonce animation-playground-page
  #(components/animation-playground
    (reagent/cursor app-state [:animation-page])))

(def routes
  {:intro #'components/intro
   :voronoi-diagrams-intro #'components/voronoi-diagrams
   :tests #'misc-page
   :playground #'animation-playground-page
   :map #'map-page})

(defn ui []
  [:div.container
   [(@(rf/subscribe [:current-page]) routes)]])

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [ui] (.getElementById js/document "app")))

(defn init! []
  (accountant/configure-navigation!
    {:nav-handler
     (fn [path]
       (let [hash-index (str/index-of path \#)
             path (subs path (+ hash-index 1))]
         (secretary/dispatch! path)))
     :path-exists?
     (fn [path]
       (secretary/locate-route path))})
  (accountant/dispatch-current!)
  (rf/dispatch-sync [:initialize])
  (rf/dispatch [:get-city-data])
  (rf/dispatch [:get-map-data])
  (mount-root))

(defn ^:export main [] (init!))

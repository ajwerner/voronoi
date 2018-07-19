(ns app.core
  (:require [clojure.string :as str]
            [reagent.core :as reagent :refer [atom]]
            [re-frame.core :as rf]
            [secretary.core :as secretary :include-macros true]
            [accountant.core :as accountant]
            [app.components :as components]
            [app.us-map.events :as us-map]
            [app.playground.events]
            [app.slides.events :as slides]
            [app.examples.examples :as examples]))

;; -------------------------
;; Routing

(secretary/defroute map-p #"/(map)?" []
                    (rf/dispatch [:page [:map]]))

(secretary/defroute intro-p #"/intro" []
                    (rf/dispatch [:page [:intro]]))

(secretary/defroute examples-p "/examples" []
                    (rf/dispatch [:page [:tests]]))

(secretary/defroute references "/references" []
                    (rf/dispatch [:page [:references]]))

(secretary/defroute animation-page "/animation-playground" []
                    (rf/dispatch [:page [:playground]]))

(secretary/defroute voronoi-description "/app-diagrams" []
                    (rf/dispatch [:page [:voronoi-diagrams-intro]]))

(secretary/defroute slides "/slides/:i" [i]
                    (rf/dispatch [:page [:slides i]]))

(secretary/set-config! :prefix "#")

;; -------------------------
;; Views
;; -------------------------

(defonce examples-page #(components/examples-page))

(defonce map-page #(components/map-page))

(defonce animation-playground-page #(components/animation-playground))

;; -------------------------
;; Event handlers

(rf/reg-event-db
  :initialize
  (fn [db  _]
    (assoc db :routes {:intro  #'components/intro-page
                       :voronoi-diagrams-intro #'components/voronoi-diagrams
                       :tests #'examples-page
                       :playground #'animation-playground-page
                       :references #'components/references
                       :map #'map-page})))

(rf/reg-event-db
  :page
  (fn [db [_ new-page]]
    (assoc db :page new-page)))

(rf/reg-sub
  :page
  (fn [db _]
    (if-let [page-vec (:page db)]
      (into [((first page-vec) (:routes db))]
              (rest page-vec))
      [app.us-map.views/loading])
    ))

(defn ui []
  (fn []
    (if-let [p @(rf/subscribe [:page])] [:div p] [:div])))

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
  (rf/dispatch [:initialize])
  (rf/dispatch [::us-map/initialize])
  (rf/dispatch [::examples/initialize])
  (rf/dispatch [::slides/initialize])
  (rf/dispatch [:playground/initialize])
  (mount-root))

(defn ^:export main [] (init!))

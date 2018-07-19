(ns app.slides.subs
  (:require [re-frame.core :as rf]
            [app.svg :as svg]))

(rf/reg-sub
  :slides/builder
  (fn [db _]
    (:slides/builder db)))

(rf/reg-sub
  :builder/running?
  (fn [db [_ id]]
    (some? (get-in db [:play-builder id]))))

(rf/reg-sub
  :slides/view-box
  (fn [[_ id]]
    (rf/subscribe id))
  (fn [vor _]
    (svg/view-box-extent (:extent vor) 10)))
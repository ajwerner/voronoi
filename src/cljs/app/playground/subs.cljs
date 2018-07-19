(ns app.playground.subs
  (:require [re-frame.core :as rf]
            [clojure.string :as string]))

(rf/reg-sub
  :playground/initialized?
  (fn [db _]
    (some? (:playground/builder db))))

(rf/reg-sub
  :playground/scroll
  (fn [db _]
    (:playground/scroll db)))

(rf/reg-sub
  :playground/view-box
  :<- [:playground/scroll]
  (fn [{:keys [x y x-width y-width]} _]
    (string/join " " [x y x-width y-width])))

(rf/reg-sub
  :playground/builder
  (fn [db _]
    (:playground/builder db)))

(rf/reg-sub
  :playground/x-span
  :<- [:playground/scroll]
  (fn [{:keys [x x-width]} _]
    [(- x x-width) (+ x x-width x-width)]))
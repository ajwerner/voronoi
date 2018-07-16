(ns voronoi.voronoi
  (:require [voronoi.builder :as builder]
            [voronoi.diagram :as diagram]))

(defn new-voronoi [input & args]
  (diagram/finish (apply builder/new-builder input args)))

(def new-voronoi-builder builder/new-builder)
(def handle-event builder/handle-event)
(def scan-to builder/scan-to)
(def reset-to builder/reset-to)
(def get-breaks diagram/get-breaks)
(def polygons diagram/polygons)
(def finish-builder diagram/finish)

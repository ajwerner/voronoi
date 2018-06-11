(ns ^:figwheel-no-load voronoi.dev
  (:require
    [voronoi.core :as core]
    [devtools.core :as devtools]))

(devtools/install!)

(enable-console-print!)

(core/init!)

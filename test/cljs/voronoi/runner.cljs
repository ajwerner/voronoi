(ns voronoi.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [voronoi.basic]))

(doo-tests 'voronoi.basic)

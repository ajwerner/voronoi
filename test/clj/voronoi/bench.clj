(ns voronoi.bench
  (:require [clojure.test :refer [deftest run-tests run-all-tests is]]
            [voronoi.voronoi :as vor]
            [voronoi.points :as points]
            [criterium.core :as criterium]))


(defn test-random [size]
  (println size)
  (time
   (criterium/with-progress-reporting
     (criterium/bench
      (vor/finish-builder (vor/new-voronoi-builder (points/random-points size)))
      :verbose)))
  nil)

(deftest test-random-small
  (test-random 100)
  (test-random 200)
  (test-random 400)
  (test-random 800)
  (test-random 1600)
  (test-random 3200)
  (test-random 6400)
  (test-random 12800))

(run-all-tests)

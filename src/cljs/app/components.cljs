(ns app.components
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [reagent.core :as reagent :refer [atom]]
            [voronoi.core :as vor]
            [voronoi.points :as points]
            [app.components.arc-table :refer [arc-table-and-toggle]]
            [app.components.events-panel :refer [events-panel]]
            [app.components.control-panel :refer [control-panel]]
            [app.examples.examples :as examples]
            [app.slides.events :as slides]
            [app.playground.events]
            [app.playground.subs]
            [app.slides.events]
            [app.slides.subs]
            [app.us-map.views :as us-map]
            [app.playground.views :as playground]
            [re-com.core :as rc]
            [re-com.util :as rc-util]))

(defn bulleted-list [& li-text-items]
  [:ul (map #(into ^{:key %} [:li] %) li-text-items)])

(defn link [link-info text-fn]
  (if link-info
    [rc/box
     :child [:a {:href (:href link-info)} (text-fn link-info)]]))

(defn links [prev next]
  (let [prev-text (fn [{:keys [text]}]
                    (str "<- " text))
        next-text (fn [{:keys [text]}]
                    (str text " ->"))
        children (->> [[rc/gap :size "10px"]
                       (link prev prev-text)
                       [rc/gap :size "1"]
                       (link next next-text)
                       [rc/gap :size "10px"]]
                      (remove nil?)
                      (into []))]
    [rc/h-box
     :justify :center
     :width "100%"
     :height "100%"
     :children children]))

(defn page [content & {:keys [prev next]}]
  [rc/v-box
   :align :center
   :justify :between
   :children
   [[rc/box
     :min-height "94vh"
     :size "auto"
     :width "100%"
     :child [content]]
    [links prev next]]])

(defn p [& children]
  (let [c1 (first children)
        [m children] (if (map? c1)
                       [c1 (rest children)]
                       [{} children])
        m (rc-util/deep-merge
           {:style {:max-width "98vw"
                    :min-width "300px"
                    :margin "auto"}}
           m)]
    (into [rc/p m] children)))

(defn intro []
  [rc/box
   :align :center
   :child
   [rc/v-box
    :max-width "450px"
    :style {:padding-left  "5px"
            :padding-right "5px"}
    :children
    [[rc/title :level :level1 :label "What is this?"]
     [p
      "This post is primarily about Voronoi diagram but along the way it's also about:"]
     [bulleted-list
      "Clojure/Clojurescript"
      "React/Reagent and Single Page applications"
      "Drawing in the browser (Processing/Quil and SVGs)"
      "Robust geometric predicates with floating point"]
     [rc/title :level :level3 :label "Why did I do it?"]
     [p
      "Around winter I was expressing to my friend "
      [:a {:href "https://twitter.com/snikhilesh"} "Nik"]
      " that, despite all the time
      I've spent writing code as a programmer, I felt woefully unable to express
      ideas visually with code. When it comes to sharing interactive media, today's
      lingua franca is the web.
      Nik raved about the value of tight feedback loop when programming, especially visual programming.
      His suggestion was that I look into Clojure and ClojureScript as a productive and clean approach to UI development."]
     [p
      "That explains how I started messing around in Clojure, but as for why the Voronoi Diagrams, that has to do with a project I did in college. Around Christmas I worked my way through the Advent of Code in Clojure as a way of introducing myself to the langauge. Around the same time, I got an email about a Github star and then an issue on "
      [:a {:href "https://github.com/ajwerner/fortune"} "a Java implementation of Fortune's algoithm"]
      " that I had largely forgotten about (maybe for good reason)."]

     [rc/title :level :level3 :label "What is this not?"]
     [p
      "Novel, this project has no particularly novel contributions to offer to the world.
       Any seemingly deep insight was much more deeply pursued by somebody else.
       I'll try to point references to some things which I glanced at but often
       gave up on understanding completely for the moment."]
     [p
      " I  particularly want to highlight the work in"
      [:a {:href "https://github.com/d3/d3-app"}
       " D3 Voronoi "]
      "and the"
      [:a {:href "https://visionscarto.net/the-state-of-d3-app"}
       " wonderful writeup by Philippe Rivi\u00e8re"]
      ". Probably for all practical use cases, D3 will be a better choice for a variety of reasons.
      One could argue that much of the code in this project is just an ad-hoc re-implementation of
      functionality found in D3. This project was an outlet for me to improve my ability to express
      myself visually with code and to grapple with the challenges of building UI. I hope you enjoy it
      and find it to be valuable educational material."]
     [rc/title :level :level3 :label "Maybe interesting things"]
     [p
      "Despite not being ground-breaking, there are some things that this implementations sets out to do that hopefully
      make it a little interesting. We'll explore those a little bit on later pages."]
     [bulleted-list
      "Careful handling of co-circular and co-linear points"
      "Exposed algorithm state for pedagogical and artistic properties."
      ]
     [rc/title :level :level3 :label "What else?"]
     [p
      "There's a bunch of stuff still to do"]
     [bulleted-list
      "Better handling of very near points"
      "Some clipping things for edge cases where the top and bottom
      edges or right and stuff"
      ]]]])

(defn about-diagrams []
  [rc/v-box
   :align :center
   :children
   [[rc/title :level :level1 :label "What's a Voronoi Diagram?"]
    [p
     "Say you have a bunch of points in plane which we'll call sites.
      Maybe those sites correspond to cities projected onto a map.
      Then say you have some other point and you'd like to know the site to
      which it is the closest.
      The Voronoi diagram might be able to help you.
      A Voronoi diagram is a space partitioning algorithm which breaks a space into
      non-overlapping convex polygons where each corresponds to a site such that
      all points inside the polygon are closest to that site.
      For the rest of the discussion, we'll assume we're only dealing in 2D
      with Euclidean distance. The algorithm this project explores is attributed to "
     [:a {:href "http://portal.acm.org/citation.cfm?id=10549"} "Steve Fortune"]
     " and thus is called "
     [:a {:href "https://en.wikipedia.org/wiki/Fortune%27s_algorithm"} "Fortune's algorithm"]
     ". Let's look at the algorithm running and then talk about what's going on. "]
    [rc/box
     :max-height "500px"
     :height "100%"
     :width "100%"
     :max-width "800px"
     :size "auto"
     :child
     [playground/new-app-thing [:slides/builder]]]
    [p
     "If you remember back to high school math, a parabola is defined as the set of points equidistant from a point (focus) and a line (directrix). In this example, the sites are the foci and the sweep-line is the directrix. This ends up having nice properties in that the points on the Voronoi edges are traced out by the evolution of the parabolas as the sweep-line progresses across the plane."]
    ]])

(defn references-body []
  [rc/v-box
   :align :center
   :children
   [[rc/title :level :level2 :label "References"]
    [bulleted-list
     ["The implementation of Voronoi diagrams you should likely use on web pages. "
      [:a {:href "https://github.com/d3/d3-voronoi"} "d3 Voronoi"]]
     ["A wonderful writeup by Philippe Rivi\u00e8re. "
      [:a {:href "https://visionscarto.net/the-state-of-d3-voronoi"}
       "The state of d3 Voronoi"]]
     ["A great library for dealing with GeoJSON. "
      [:a {:href "https://github.com/d3/d3-geo"} "d3 Geo"]]
     ["GeoJSON for the US "[:a {:href "http://eric.clst.org/tech/usgeojson/"}
       "GeoJSON and KML Data For The United States"]]
     ["Historical census data driving the interactive map. "
      [:a {:href "https://github.com/cestastanford/historical-us-city-populations"}
       "United States Historical City Populations, 1790-2010"]]
     ["The book from which my original Java implementation was derived. "
      [:a {:href "http://www.amazon.com/Computational-Geometry-Applications-Mark-Berg/dp/3642096816"}
       "Computational Geometry: Algorithms & Applications"]]
     ["My Java implementation from college which is faster but more broken. "
      [:a {:href "https://github.com/ajwerner/fortune"}  "https://github.com/ajwerner/fortune"]]]]])

(defn references []
  [page references-body
   :prev {:text "Examples" :href "#/examples"}])

(defn intro-page []
  [page intro
   :prev {:text "Contrived example" :href "#/map"}
   :next {:text "About Voronoi Diagrams" :href "#/app-diagrams"}])

(defn map-page []
  [page us-map/map-thing
   :next {:text "Intro" :href "#/intro"}])

(defn voronoi-diagrams []
  [page  about-diagrams
   :prev {:text "Intro" :href "#/intro"}
   :next {:text "Examples" :href "#/examples"}])

(defn examples-page []
  [page examples/examples-page
   :prev {:text "About Voronoi Diagrams" :href "#/app-diagrams"}
   :next {:text "Animations" :href "#/animation-playground"}])

(defn animation-playground []
  [page (playground/new-app-thing [:playground/builder])
   :prev {:text "Examples" :href "#/examples"}
   :next {:text "References" :href "#/references"}])

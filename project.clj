(defproject voronoi "0.1.0"
  :description "A Voronoi Diagram Reagent Project"
  :url "http://ajwerner.github.io/app"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.312"
                  :scope "provided"]
                 [re-com "2.1.0"]
                 [org.clojure/data.avl "0.0.17"]
                 [testdouble/clojurescript.csv "0.3.0"]
                 [ring-server "0.5.0"]
                 [reagent "0.8.1"]
                 [reagent-utils "0.3.1"]
                 [ring "1.6.3"]
                 [secretary "1.2.3"]
                 [criterium "0.4.4"]
                 [venantius/accountant "0.2.4"
                  :exclusions [org.clojure/tools.reader]]
                 [compojure "1.6.1"]
                 [hiccup "1.0.5"]
                 [yogthos/config "1.1.1"]
                 [cljsjs/topojson "1.6.18-0"]
                 [cljsjs/d3geo "0.2.15-2"]
                 [cljsjs/d3 "4.12.0-0"]
                 [cljsjs/csv "1.1.1-0"]
                 [day8.re-frame/http-fx "0.1.6"]
                 [cljs-ajax "0.7.3"]
                 [ring/ring-defaults "0.3.1"]
                 [cljs-http "0.1.45"]
                 [re-frame "0.10.5"]]

  :plugins [[lein-environ "1.1.0"]
            [lein-cljsbuild "1.1.7"]
            [lein-asset-minifier "0.2.7"
             :exclusions [org.clojure/clojure]]]

  :ring {:handler voronoi.handler/app
         :uberwar-name "app.war"}

  :min-lein-version "2.5.0"
  :uberjar-name "app.jar"
  :main voronoi.server
  :clean-targets ^{:protect false}
  [:target-path
   [:cljsbuild :builds :app :compiler :output-dir]
   [:cljsbuild :builds :app :compiler :output-to]]

  :source-paths ["src/clj" "src/cljc"]
  :test-paths ["test/clj" "test/cljc"]
  :resource-paths ["resources" "target/cljsbuild"]

  :minify-assets
  {:assets
   {"resources/public/css/site.min.css" "resources/public/css/site.css"}}

  :cljsbuild
  {:builds {:test
            {
             :source-paths ["src/cljs" "src/cljc" "test/cljc" "test/cljs"]
             :compiler {:output-to "resources/public/js/testable.js"
                        :main voronoi.runner
                        :optimizations :advanced
                        :npm-deps {:topojson-client "3.0."}
                        :install-deps true}}
            :min
            {:source-paths ["src/cljs" "src/cljc" "env/prod/cljs"]
             :compiler
             {:main             "app.core"
              :output-to        "target/cljsbuild/public/js/app.js"
              :output-dir       "target/cljsbuild/public/js"
              :source-map       "target/cljsbuild/public/js/app.js.map"
              :optimizations :advanced
              :pretty-print  false}}
            :gh-page
            {:source-paths ["src/cljs" "src/cljc" "env/prod/cljs"]

             :compiler
             {:main             "app.core"
              :output-to        "docs/js/app.js"
              :output-dir       "docs/js"
              :source-map       "docs/js/app.js.map"
              :optimizations :advanced
              :pretty-print true
              :pseudo-names true
              :verbose true
              :closure-warnings
              {:check-types :warning ;; << ADD THIS
               :check-variables :warning
               :undefined-names :off
               :externs-validation :off
               :missing-properties :off}

              :npm-deps {:topojson "3.0.2"}
              :install-deps true}}
            :app
            {:source-paths ["src/cljs" "src/cljc" "env/dev/cljs"]
             :figwheel {:on-jsload "app.core/mount-root"}
             :compiler
             {:main "voronoi.dev"
              :asset-path "/js/out"
              :output-to "target/cljsbuild/public/js/app.js"
              :output-dir "target/cljsbuild/public/js/out"
              :source-map true
              :optimizations :none
              :pretty-print  true
              :npm-deps {:topojson "3.0.2"}
              :install-deps true
              :preloads [day8.re-frame-10x.preload]
              :closure-defines {"re_frame.trace.trace_enabled_QMARK_" true}
              }}}
   }

  :figwheel
  {:http-server-root "public"
   :server-port 3449
   :nrepl-port 7002
   :nrepl-middleware ["cemerick.piggieback/wrap-cljs-repl"]
   :css-dirs ["resources/public/css"]
   :ring-handler app.handler/app
   :repl-eval-timeout 100000}
  :profiles {:dev {:repl-options {:init-ns voronoi.repl
                                  :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

                   :dependencies [[binaryage/devtools "0.9.10"]
                                  [ring/ring-mock "0.3.2"]
                                  [ring/ring-devel "1.6.3"]
                                  [prone "1.5.2"]
                                  [org.clojure/clojurescript "1.10.339"]
                                  [org.clojure/core.async "0.4.474"]
                                  [figwheel-sidecar "0.5.16"]
                                  [org.clojure/tools.nrepl "0.2.13"]
                                  [com.cemerick/piggieback "0.2.2"]
                                  [pjstadig/humane-test-output "0.8.3"]
                                  [day8.re-frame/re-frame-10x "0.3.3-react16"]]

                   :source-paths ["env/dev/clj"]
                   :plugins [[lein-figwheel "0.5.16"]
                             [lein-cljsbuild "1.1.7"]
                             [lein-doo "0.1.10"]]
                   :injections [(require 'pjstadig.humane-test-output)
                                (pjstadig.humane-test-output/activate!)]

                   :env {:dev true}}

             :uberjar {:hooks [minify-assets.plugin/hooks]
                       :source-paths ["env/prod/clj"]
                       :prep-tasks ["compile" ["cljsbuild" "once" "min"]]
                       :env {:production true}
                       :aot :all
                       :omit-source true}})

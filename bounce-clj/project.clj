(defproject bouncing-balls "0.1.0-SNAPSHOT"
  :main bouncing-balls.main
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :min-lein-version "2.0.0"
  :dependencies [[org.clojure/clojure "1.11.3"]
                 [org.clojure/tools.namespace "1.5.0"]]
  :plugins []
  :resource-paths ["src" "resources"]
  :profiles
  {:dev {:dependencies []}})

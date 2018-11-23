(defproject grafter/matcha.alpha "0.1.7-SNAPSHOT"
  :description "A SPARQL-like query engine and DSL for querying in
  memory RDF models."
  :url "https://github.com/Swirrl/matcha"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0-beta8"]
                 [org.clojure/core.logic "0.8.11"]]

  :profiles {:dev {:dependencies [[grafter "0.11.4"]]}}

  :deploy-repositories [["releases" :clojars]
                       ["snapshots" :clojars]]
  ;;:plugins [[lein-tools-deps "0.4.1"]]
  ;;:lein-tools-deps/config {:config-files [:install :user :project]}
  )

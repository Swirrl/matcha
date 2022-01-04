(ns build
  (:require [clojure.tools.build.api :as b]
            [org.corfield.build :as bb])
  (:refer-clojure :exclude [test]))

(def lib 'grafter/matcha.alpha)
(def version (format "0.2.%s" (b/git-count-revs nil)))
(def jar-file (format "target/%s-%s.jar" (name lib) version))

(defn test
  "Run the tests"
  [opts]
  (bb/run-tests opts))

(defn build
  "Run the CI pipeline of tests (and build the JAR)."
  [opts]
  (-> opts
      (assoc :lib lib
             :version version
             :src-pom "template/pom.xml")
      (bb/clean)
      (bb/jar)))

(defn install
  "Install the JAR locally."
  [opts]
  (-> opts
      (assoc :lib lib :version version)
      (bb/install)))

(defn tag [{:keys [version] :as opts}]
  (let [vtag (str "v" version)]
    (b/git-process {:git-args ["tag" vtag]})
    (b/git-process {:git-args ["push" "origin" vtag]}))
  opts)

(defn deploy
  "Deploy the JAR to Clojars."
  [opts]
  (-> opts
      (assoc :lib lib :version version)
      (tag)
      (bb/deploy)))

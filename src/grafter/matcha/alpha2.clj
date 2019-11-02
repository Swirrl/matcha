(ns grafter.matcha.alpha2
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :as l :refer [fresh run*]]
            [clojure.core.logic.protocols :as lp]
            [clojure.core.logic.pldb :as pldb]
            [clojure.spec.alpha :as s]
            [clojure.core.logic.unifier :as u]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clojure.set :as set]))


(pldb/db-rel triple ^:index subject ^:index predicate ^:index object)

(defn triple-vector->idx-triple
  "Assume triples are either 3/tuple vectors or can be destructured as
  such.  Grafter Quad objects can be destructured in this manner."
  [[s p o]]
  [triple s p o])


(defn- varify [query-data]
  (let [distinct-qvars (set (for [triple query-data
                                  term (filter symbol? triple)]
                              term))
        sym->lvar (->> distinct-qvars
                       (map (juxt identity (comp l/lvar str)))
                       (into {}))]

    {::bindings sym->lvar
     ::query (for [triple query-data]
              (mapv (fn [term]
                      (sym->lvar term term)) triple))}))

(defn- goalify [{:grafter.matcha.alpha2/keys [query bindings] :as qdata}]
  (let [goals (for [bgp query]
                (apply triple bgp))

        result-var (l/lvar "results")
        compiled-goals (l/and* (conj goals
                                     (l/== result-var bindings)))]

    (assoc qdata
           ::result-var result-var
           ::compiled-goals compiled-goals)))

(defn compile-query
  "Compile a query"
  [q]
  (goalify (varify q)))

(defn run-query* [matcha-db {:grafter.matcha.alpha2/keys [bindings compiled-goals result-var] :as compiled-query}]
  (l/solutions (l/tabled-s true {:db [matcha-db]
                                 :reify-vars false})
               result-var
               compiled-goals))

(defn run-query
  "Compile a query"
  [matcha-db query]
  (run-query* matcha-db (compile-query query)))

(comment

  (def matcha-db (apply pldb/db (map triple-vector->idx-triple [[:rick :foaf/friend :katie]
                                                                [:katie :foaf/friend :julie]
                                                                [:julie :foaf/name "Julie"]])))

  
  (def foaf-query '[[?s :foaf/friend ?o]
                    [?o :foaf/friend ?o2]
                    [?o2 :foaf/name ?name]])
  
  (varify foaf-query)
  (goalify (varify foaf-query))


  (compile-query foaf-query)

  (run-query matcha-db foaf-query)
  
  )






;; (def my-query (->> '[[?s :foaf/friend ?o]
;;                      [?o :foaf/friend ?o2]
;;                      [?o2 :foaf/name ?name]]
;;                    substitute-vars
;;                    (map (fn [[s p o]] [triple s p o]))))



;;(def all-lvars (distinct (mapcat #(filter l/lvar? %) my-query)))

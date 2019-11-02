(ns grafter.matcha.alpha2
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :as l :refer [fresh run*]]
            [clojure.core.logic.protocols :as lp]
            [clojure.core.logic.pldb :as pldb]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as g]
            [clojure.core.logic.unifier :as u]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clojure.set :as set]
            [grafter.matcha.alpha :as ma]))


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

(def select-query '(select [?name]
                           [[?s :foaf/friend ?o]
                            [?o :foaf/friend ?o2]
                            [?o2 :foaf/name ?name]]))

(s/def ::var (s/spec (s/and ma/query-var?
                            (fn [v] (> (count (str v)) 1)))
                     :gen (fn []
                            (g/fmap (fn [s]
                                      (symbol (str "?" s)))
                                    (g/string-ascii)))))

(s/def ::s (s/or :uri uri?
                 :keyword keyword?
                 :var ::var))

(s/def ::p (s/or :uri uri?
                 :keyword keyword?
                 :var ::var))

(s/def ::primitive any?) ;; might want to exclude collections

(s/def ::o (s/or :uri uri?
                 :keyword keyword?
                 :var ::var
                 :primitive ::primitive))

(s/def ::bgp (s/spec (s/cat :s ::s :p ::p :o ::o)))

(s/def ::select-type (s/spec (s/conformer (fn [v]
                                            (if (or (= 'select v) (= 'grafter.matcha.alpha2/select v))
                                              'grafter.matcha.alpha2/select
                                              :clojure.spec.alpha/invalid)))
                             :gen #(s/gen (s/spec #{'select}))))

(defn- var-set
  "Utility function to find confromed :vars"
  [bindings]
  (->> bindings
       (filter (comp #{:var} first))
       (map second)
       set))

(defn all-projected-vars-are-bound?
  "Given a conformed query checks whether all projected variables are
  bound in the where clause."
  [{:keys [where projection]}]
  (let [where-bindings (->> where
                             (mapcat vals)
                             var-set)
        proj-bindings (var-set projection)]
    (set/subset? proj-bindings where-bindings)))

;; NOTE: this exists for consistency so that conformed query variables
;; in a :projection have the same representation as in a bgp.
;;
;; It should also allow room to grow the definition of projected to
;; share more SPARQL features.
(s/def ::projected (s/or :var ::var))

(s/def ::select (s/and (s/cat :query-type ::select-type
                              :projection (s/spec (s/+ ::projected))
                              :where (s/spec (s/+ ::bgp)))
                       all-projected-vars-are-bound?))

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

  (run-query matcha-db foaf-query))








;; (def my-query (->> '[[?s :foaf/friend ?o]
;;                      [?o :foaf/friend ?o2]
;;                      [?o2 :foaf/name ?name]]
;;                    substitute-vars
;;                    (map (fn [[s p o]] [triple s p o]))))



;;(def all-lvars (distinct (mapcat #(filter l/lvar? %) my-query)))

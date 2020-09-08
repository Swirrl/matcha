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

(defn make-index
  ([triples] (make-index triples {}))
  ([triples opts]
   (apply pldb/db (map triple-vector->idx-triple triples))))


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

(defn- goalify [{query :lvar/query
                 bindings :lvar/bindings
                 :as qdata}]

  (let [goals (for [bgp query]
                (apply triple bgp))

        result-var (l/lvar "results")
        compiled-goals (l/and* (conj goals
                                     (l/== result-var bindings)))]

    (assoc qdata
           ::result-var result-var
           ::compiled-goals compiled-goals)))

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

(defn- canonicalise-query-type [qt]
  (condp = qt
      'select 'grafter.matcha.alpha2/select
      'grafter.matcha.alpha2/select 'grafter.matcha.alpha2/select

      'construct 'grafter.matcha.alpha2/construct
      'grafter.matcha.alpha2/construct 'grafter.matcha.alpha2/construct

      'ask 'grafter.matcha.alpha2/ask
      'grafter.matcha.alpha2/ask 'grafter.matcha.alpha2/ask
      ))

(s/def ::select-type (s/spec (s/conformer canonicalise-query-type
                                          canonicalise-query-type)
                             :gen #(s/gen (s/spec #{'select}))))

(defmulti projection-vars first)

(defmethod projection-vars :one [binding]
  (-> binding
       second
       second
       ))

(defmethod projection-vars :many [bindings]
  (->> bindings
       second
       (mapv second)))

(defn- triple-vars
  "Utility function to find confromed :vars"
  [bindings]
  (->> bindings
       (filter (comp #{:var} first))
       (mapv second)))

(defn distinct-bindings [{:keys [where projection]}]

  {:bindings/where (apply set/union
                          (map (fn [triple]
                                 (->> triple
                                      vals
                                      triple-vars
                                      set)) where))

   :bindings/projection (projection-vars projection)})

(defn all-projected-vars-are-bound?
  "Given a conformed query checks whether all projected variables are
  bound in the where clause."
  [query]
  (let [{:bindings/keys [where projection]} (distinct-bindings query)]
    (set/subset? projection where)))

;; NOTE: ::projected exists for consistency so that conformed query
;; variables in a :projection have the same representation as in a
;; bgp. i.e. [:var ?var] rather than just ?var.
;;
;; It should also allow room to grow the definition of projected to
;; share more SPARQL features.
(s/def ::projected (s/or :var ::var))

(s/def ::where (s/spec (s/+ ::bgp)))

(s/def ::select (s/and (s/cat :query-type ::select-type
                              :projection (s/spec (s/or :many (s/+ ::projected)
                                                        :one ::projected))
                              :where ::where)
                       all-projected-vars-are-bound?))

(defn- varify [{:keys [where]
                where-bindings :bindings/where
                proj-bindings :bindings/projection :as query-data}]
  (let [distinct-qvars (set/union where-bindings proj-bindings)
        sym->lvar (->> distinct-qvars
                       (map (juxt identity (comp l/lvar str)))
                       (into {}))]

    (sc.api/spy)
    {:lvar/bindings sym->lvar

     ;;; TODO TODO TODO FIX THIS BIT
     :lvar/projection (mapv (fn [term]
                              (sym->lvar term term)) proj-bindings)

     :lvar/query (for [triple (s/unform ::where where)]
                   (mapv (fn [term]
                           (sym->lvar term term)) triple))}))

(defn- denamespace
  "Walk query form an remove namespaces from symbols.  This allows users to use the "
  [q]
  (walk/prewalk
   (fn [v] (if (symbol? v) (symbol (name v)) v))
   q))

(defn conform-query [query]
  (let [conformed (s/conform ::select (denamespace query))]
    (when (= :clojure.spec.alpha/invalid conformed)
      (throw (ex-info "Invalid query" {:error ::invalid-query
                                       ::query query})))

    (let [distinct-vars (distinct-bindings conformed)
          annotated-query (merge distinct-vars conformed)
          lvard (varify annotated-query)
          lvard-annotated-query (merge annotated-query lvard)]

      (goalify lvard-annotated-query))))


(defn compile-query
  "Compile a query"
  [q]
  (goalify (varify q)))

(defn run-query* [matcha-db {bindings :lvar/bindings
                             projection :lvar/projection
                             :grafter.matcha.alpha2/keys [compiled-goals result-var] :as compiled-query}]

                                        ;(sc.api/spy)
  (l/solutions (l/tabled-s true {:db [matcha-db]
                                 :reify-vars false})
               projection
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


  (def select-query '(select [?name]
                           [[?s :foaf/friend ?o]
                            [?o :foaf/friend ?o2]
                            [?o2 :foaf/name ?name]]))


  (run-query matcha-db
             (conform-query select-query))

  (run-query matcha-db
             (conform-query select-query))



  (s/conform ::select '(select ?name
                               [[?s :foaf/friend ?o]
                                [?o :foaf/friend ?o2]
                                [?o2 :foaf/name ?name]]))

  )








;; (def my-query (->> '[[?s :foaf/friend ?o]
;;                      [?o :foaf/friend ?o2]
;;                      [?o2 :foaf/name ?name]]
;;                    substitute-vars
;;                    (map (fn [[s p o]] [triple s p o]))))



;;(def all-lvars (distinct (mapcat #(filter l/lvar? %) my-query)))

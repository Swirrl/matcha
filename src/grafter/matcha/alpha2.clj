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




(def test-rel
  (fn [& query]
      (println "q" query)
      (fn [& stuff]
        (println stuff))))

(def matcha-db (apply pldb/db (map triple-vector->idx-triple [[:rick :foaf/friend :katie]
                                                              [:katie :foaf/friend :julie]
                                                              [:julie :foaf/name "Julie"]])))

(pldb/with-db matcha-db
  (l/run 2 [?name]
    (fresh [?s ?o ?o2]
      (l/and*
       [(triple ?s :foaf/friend ?o)
        (triple ?o :foaf/friend ?o2)
        (triple ?o2 :foaf/name ?name)]))))


;; walking stuff
(let [v (l/lvar)]
  (l/walk* ((l/== v 1) l/empty-s) v))


;; use solutions as a runtime variant of -run
(let [v (l/lvar)

      g (l/or* [(l/== v 1)
                (l/== v 2)])]

  (l/solutions l/empty-s v g))


(defn build-query []
  (let [?s (l/lvar "s")
        ?o (l/lvar "o")
        ?o2 (l/lvar "o2")
        ?name (l/lvar "name")

        t1 (apply triple [?s :foaf/friend ?o])
        t2 (apply triple [?o :foaf/friend ?o2])
        t3 (apply triple [?o2 :foaf/name ?name])]

    {:goal [t1 t2 t3]
     :bindings [?s ?o ?o2 ?name]}))

;; NOTE this one works!!!
(let [the-db (apply pldb/db [[triple :rick :foaf/friend :katie]])
      {:keys [bindings goal]} (build-query)
      results (l/lvar "results")

      ultimate-g #_(l/and* (conj goal (l/== results bindings))) (apply triple [results :foaf/friend :katie])

      ]
  (l/solutions (l/tabled-s true {:db [the-db]
                                 :reify-vars true})
               results
               ultimate-g))


;; THIS IS IT!!!!! THE ONE THAT WORKS!!!!
(let [the-db matcha-db #_(apply pldb/db [[triple :rick :foaf/friend :katie]])
      {:keys [bindings goal]} (build-query)
      results (l/lvar "results")

      ultimate-g (l/and* (conj goal (l/== results bindings)))


      ]
  (l/solutions (l/tabled-s true {:db [the-db]
                                 :reify-vars true})
               results
               ultimate-g))






#_(let*
    []
    (push-thread-bindings
     (hash-map #'l/*logic-dbs* (conj l/*logic-dbs* matcha-db)))
    (try
      (let*
          [opts {:db l/*logic-dbs*, :n 2, :occurs-check true}
           xs (lp/take* (fn* []
                             ((fn* ([arg]
                                    (fn*
                                     -inc
                                     ([]
                                      (let* [?name (l/lvar '?name)]
                                        (lp/bind (lp/bind
                                                  arg
                                                  (fn*
                                                   ([arg]
                                                    (fn*
                                                     -inc
                                                     ([]
                                                      (let*
                                                          [ ;?s (l/lvar '?s)
                                                           ?o (l/lvar '?o)
                                                           ?o2 (l/lvar '?o2)]
                                                          (lp/bind arg
                                                                   (l/and*
                                                                    [(triple (l/lvar '?s) :foaf/friend ?o)
                                                                     (triple ?o :foaf/friend ?o2)
                                                                     (triple ?o2 :foaf/name ?name)]))))))))
                                                 (l/reifyg ?name)))))))
                              (l/tabled-s
                               (:occurs-check opts)
                               (merge {:reify-vars true} opts)))))]
          (let*
              [nsols (:n opts)]
              (if nsols
                (let* [n nsols]
                  (take n xs ))
                xs )))
      (finally (pop-thread-bindings))))

(defn substitute-vars [query-data]
  (let [distinct-qvars (set (for [triple query-data
                                  term (filter symbol? triple)]
                              term))
        sym->lvar (zipmap distinct-qvars (repeatedly l/lvar))]

    (for [triple query-data]
      (mapv (fn [term]
             (sym->lvar term term)) triple))))


(def my-query (->> '[[?s :foaf/friend ?o]
                     [?o :foaf/friend ?o2]
                     [?o2 :foaf/name ?name]]
                   substitute-vars
                   (map (fn [[s p o]] [triple s p o]))))



(def all-lvars (distinct (mapcat #(filter l/lvar? %) my-query)))

(ns sparqlo.core
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :as l
             :refer [run* run fresh == membero everyg conde matche succeed fail]]))

(l/defne sparqlo
  "Match a db of triples against a bgp"
  [db bgp]
  ([[f . _] [s p o]]
   (== f [s p o]))
  ([[_ . r] bgp]
   (sparqlo r bgp)))

(defn matchdb
  "Match a single pattern against a db"
  [db bgp]
  (matche [db]
          ([[triple . _]] (== triple bgp))
          ([[_ . ?restdb]] (sparqlo ?restdb bgp))))

(defmacro query
  "Build a database query out of SPARQL like bgp's."
  [db project-vars bgps]
  ;; This macro works by converting the SPARQL like BGPs into a
  ;; core.logic program and evalauting it.

  (let [pvar? (set project-vars)
        syms (vec (distinct (->> bgps
                                 (mapcat identity)
                                 (filter symbol?)
                                 (remove pvar?))))]
    `(l/run* ~project-vars
       (fresh ~syms
         ~@(for [bgp bgps]
             `(matchdb ~db ~bgp))))))

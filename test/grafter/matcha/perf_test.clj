(ns grafter.matcha.perf-test
  (:require [clojure.core.logic.pldb :as pldb]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.test :refer :all]
            [criterium.core :as b]
            [grafter.matcha.alpha :as m]
            [grafter.matcha.db :as mdb]
            [grafter.rdf :as rdf]
            [grafter.vocabularies.core :as vocab]
            [grafter.vocabularies.rdf :refer [rdf:a]]
            [grafter.vocabularies.skos
             :refer
             [skos:broader
              skos:Concept
              skos:inScheme
              skos:member
              skos:narrower
              skos:topConceptOf]])
  (:import java.net.URI))

(s/def ::form any?)

(s/def ::bench-expr (s/cat :form ::form))

(s/def ::suite (s/+ ::bench-expr))

(defmacro report-bench [& args]
  (let [parsed (s/conform ::suite args)]
    (cons 'do (mapcat (fn [{:keys [form]}]
                        `[(println "Benchmarking:" '~form)
                          (b/quick-bench ~form)
                          (println)])
                      parsed))))

(defmacro def-perf-suite [name & forms]
  `(deftest  ~(with-meta name (merge {:perf true}
                                     (meta name)))
     ~@forms
     (is true)))

(def fully-indexed [:s :p :o [:s :p] [:s :o] [:p :o]])

(def-perf-suite ^:indexer index-triples-test
  (let [quads (into #{} (rdf/statements (io/resource "zib/column-db/column-db-xl-1.nt")))]
    (report-bench
     (mdb/index-triples [:s] quads)
     (mdb/index-triples [:s :p] quads)
     (mdb/index-triples [:s :p :o] quads)
     (mdb/index-triples [:s :p :o [:s :p]] quads)
     (mdb/index-triples [:s :p :o [:s :p] [:s :o]] quads)
     (mdb/index-triples fully-indexed quads))))

(comment

  ;; requires a large heap size of 4gb to run (a bit big for poor old travis)
  ;; so only enable the xl tests by default.

  ;; XXL tests run with a db of 459327 triples.
  ;; XXL is the same as XL but with all the SKOS parent relationships loaded too,
  ;; from the HMRC overseas trade dataset: http://gss-data.org.uk/data/hmrc-overseas-trade-statistics

  (def-perf-suite ^:indexer index-triples-test
    (let [quads (into #{} (rdf/statements (io/resource "zib/column-db/column-db-xxl-1.nt")))]
      (report-bench
       (mdb/index-triples [:s] quads)                ;; 1.88 secs
       (mdb/index-triples [:s :p] quads)             ;; 3.09 secs
       (mdb/index-triples [:s :p :o] quads)          ;; 4.30 secs
       (mdb/index-triples [:s :o [:s :p]] quads)     ;; 5.0 secs  (composites are slightly more expensive than just :s :p :o)
       (mdb/index-triples [:s :p :o [:s :p]] quads)  ;; 6.50 secs
       (mdb/index-triples [:s :p :o [:s :p] [:s :o]] quads) ;; 8.34 secs
       (mdb/index-triples fully-indexed quads)       ;; 10.57 secs
       ))))




(defn construct-by-type
  "A common matcha query function from zib codebase."
  [matcha-db type-uri]
  (m/construct {:grafter.rdf/uri ?s
                ?p ?o}
               [[?s rdf:a type-uri]
                [?s ?p ?o]] matcha-db))

(def zib (vocab/prefixer "http://github.com/swirrl/zib#"))

(def zib:AttributeColumn (zib "AttributeColumn"))
(def zib:Column (zib "Column"))
(def zib:DimensionColumn (zib "DimensionColumn"))
(def zib:MeasureColumn (zib "MeasureColumn"))
(def zib:hasCodeList (zib "hasCodeList"))

(def-perf-suite ^:indexes zib-column-db-constructs
  (let [quads (into #{} (rdf/statements (io/resource "zib/column-db/column-db-xl-1.nt")))]
    (println "Running zib-column-db-constructs with different indexes on" (count quads) "triples")
    (let [matcha-spo (mdb/index-triples [:s :p :o] quads)
          matcha-so (mdb/index-triples [:s :o] quads)
          matcha-s-po (mdb/index-triples [:s [:p :o]] quads)

          matcha-o (mdb/index-triples [:o] quads)

          matcha-p (mdb/index-triples [:p] quads)
          matcha-po (mdb/index-triples [[:p :o]] quads)
          matcha-fully-indexed (mdb/index-triples fully-indexed quads)]
      (report-bench
       (construct-by-type matcha-spo zib:AttributeColumn)  ;; 237 µs
       (construct-by-type matcha-so zib:AttributeColumn)   ;; 240 µs
       (construct-by-type matcha-s-po zib:AttributeColumn) ;; 259 µs

       (construct-by-type matcha-fully-indexed zib:AttributeColumn) ;; 253 µs

       (construct-by-type matcha-o zib:AttributeColumn)  ;; 217 ms
       (construct-by-type matcha-p zib:AttributeColumn)  ;; 331 ms
       (construct-by-type matcha-po zib:AttributeColumn) ;; 222 ms

       ;; ask also about zib:DimensionColumn - difference is that in
       ;; this data there are 5 results as opposed to just one.  So
       ;; this gives an indication of the cost per resource of
       ;; grouping into a :grafter.rdf/uri subject map.

       (construct-by-type matcha-spo zib:DimensionColumn)  ;; 1.7 ms
       (construct-by-type matcha-so zib:DimensionColumn)   ;; 1.9 ms
       (construct-by-type matcha-s-po zib:DimensionColumn) ;; 1.6 ms

       (construct-by-type matcha-fully-indexed zib:DimensionColumn) ;; 1.7 ms

       (construct-by-type matcha-o zib:DimensionColumn) ;; 1.0 sec !!!
       (construct-by-type matcha-p zib:DimensionColumn) ;; 1.232 sec !!!
       (construct-by-type matcha-po zib:DimensionColumn) ;; 1.094 sec !!!


       ))))

(def comp-with-most-dimvals (URI. "http://gss-data.org.uk/data/gss_data/trade/hmrc_ots_cn8/component/combined_nomenclature"))

(defn find-top-concepts-values [column-db]
  (let [concept-schemes (distinct (m/construct ?concept_scheme
                                               [[comp-with-most-dimvals zib:hasCodeList ?codelist]
                                                [?codelist skos:member ?code]
                                                [?code skos:inScheme ?concept_scheme]] column-db))
        top-concepts (m/construct {:grafter.rdf/uri ?top
                                   ?p ?o}
                                  [(m/values ?concept-scheme concept-schemes)
                                   [?top skos:topConceptOf ?concept-scheme]
                                   [?top ?p ?o]] column-db)]

    (count top-concepts)))

(def-perf-suite skos-values
  (let [quads (into #{} (rdf/statements (io/resource "zib/column-db/column-db-xxl-1.nt")))]
    (println "Running zib-column-db-constructs with different indexes on" (count quads) "triples")
    (let [matcha-spo (mdb/index-triples [:s :p :o] quads)]

      (report-bench
       (find-top-concepts-values matcha-spo) ;; 3.88 secs (old) 1.9 secs (new)
       ))))

(def-perf-suite construct-vs-select
  (let [db-unindexed (into #{} (rdf/statements (io/resource "zib/column-db/column-db-xxl-1.nt")))
        db-spo (mdb/index-triples [:s :p :o] db-unindexed)]
    (report-bench
     ;; tests indexing s p o + query time
     (count (m/select
              [?narrower ?p ?o]
              [[?narrower rdf:a skos:Concept]
               [?narrower ?p ?o]] db-spo))

     ;; should only be marginally slower than the above select ...
     (count (m/construct [?narrower ?p ?o]
                         [[?narrower rdf:a skos:Concept]
                          [?narrower ?p ?o]] db-spo)))))

(def-perf-suite compound-indexes
  (let [db-unindexed (into #{} (rdf/statements (io/resource "zib/column-db/column-db-xxl-1.nt")))
        db-po (mdb/index-triples [#{:p :o}] db-unindexed)]
    (report-bench
     (count (m/select
              [?narrower ?p ?o]
              [[?narrower rdf:a skos:Concept]
               [?narrower ?p ?o]] db-po)))))

(def-perf-suite construct-resource-grouping
  (let [db-unindexed (into #{} (rdf/statements (io/resource "zib/column-db/column-db-xxl-1.nt")))
        db-spo (mdb/index-triples [:s :p :o] db-unindexed)]
    (report-bench
     (count (m/construct [?narrower ?p ?o]
                         [[?narrower rdf:a skos:Concept]
                          [?narrower ?p ?o]] db-spo)))))

(comment

  ;(def quads (into #{} (rdf/statements (io/resource "zib/column-db/column-db-xl-1.nt"))))
  (def quads (into #{} (rdf/statements (io/resource "zib/column-db/column-db-xxl-1.nt"))))

  (def db-spo (mdb/index-triples quads))



  )

(ns grafter.matcha.alpha-test
  (:require [clojure.test :refer :all]
            [grafter.matcha.alpha :refer :all]
            [grafter.vocabularies.core :refer [prefixer]]
            [grafter.vocabularies.foaf :refer [foaf:knows]]
            [grafter.vocabularies.rdf :refer [rdfs:label]]
            [grafter.rdf.protocols :refer [->Triple] :as gp])
  (:import [grafter.rdf.protocols LangString RDFLiteral]
           [java.net URI]))

(deftest quote-query-vars-test
  ;; used to help macro expansion
  (is (=
       '{:foo (quote ?foo)
         :bar (quote ?bar)}
       (quote-query-vars '[?foo ?bar] '{:foo ?foo :bar ?bar}))))


(def data (prefixer "http://data/example/"))

(def rick (data "rick"))
(def martin (data "martin"))
(def katie (data "katie"))
(def julie (data "julie"))

(def friends [(->Triple rick rdfs:label "Rick")
              (->Triple martin rdfs:label "Martin")
              (->Triple katie rdfs:label "Katie")
              (->Triple julie rdfs:label "Julie")

              (->Triple rick foaf:knows martin)
              (->Triple rick foaf:knows katie)
              (->Triple katie foaf:knows julie)])

;; a non grafter representation of the above database
(def friends-vectors [[:rick :rdfs/label "Rick"]
                      [:martin :rdfs/label "Martin"]
                      [:katie :rdfs/label "Katie"]
                      [:julie :rdfs/label "Julie"]

                      [:rick :foaf/knows :martin]
                      [:rick :foaf/knows :katie]
                      [:katie :foaf/knows :julie]])

(deftest query-clojure-data
  (testing "Querying 3/tuples of clojure values"
    (is (= ((select-1 [[:rick :rdfs/label ?name]]) friends-vectors)
           "Rick"))))

(deftest select-test
  (testing "Select queries arity 1"
    (let [ricks-name (select [[rick rdfs:label ?name]])]
      (is (= ["Rick"]
             (ricks-name friends)))))

  (testing "Select queries arity 2"
    (testing "Rick knows"
      (let [rick-knows (select [?name]
                               [[rick foaf:knows ?p2]
                                [?p2 rdfs:label ?name]])]

        (is (= ["Martin" "Katie"]
               (rick-knows friends)))))

    (testing "Katie knows"
      (let [katie-knows (select [?p2 ?name]
                                [[katie foaf:knows ?p2]
                                 [?p2 rdfs:label ?name]])]

        (is (= [[julie "Julie"]]
               (katie-knows friends))))))

  (testing "Friend of friend"
    (let [foaf (select [?p1 ?p2 ?p3]
                     [[?p1 foaf:knows ?p2]
                      [?p2 foaf:knows ?p3]])]

      (is (= [[rick katie julie]]
             (foaf friends))))))

(deftest select-1-test
  (testing "Select-1 queries arity 1"
    (testing "Select queries arity 1"
      (let [ricks-name (select-1 [[rick rdfs:label ?name]])]
        (is (= "Rick"
               (ricks-name friends))))))

  (testing "Select-1 queries arity 2"
    (testing "Rick knows"
      (let [rick-knows (select-1 [?name]
                                 [[rick foaf:knows ?p2]
                                  [?p2 rdfs:label ?name]])]
        (is (= "Martin"
               (rick-knows friends)))))

    (testing "Katie knows"
      (let [katie-knows (select-1 [?p2 ?name]
                                [[katie foaf:knows ?p2]
                                 [?p2 rdfs:label ?name]])]

        (is (= [julie "Julie"]
               (katie-knows friends))))))

  (testing "Friend of friend"
    (let [foaf (select-1 [?p1 ?p2 ?p3]
                     [[?p1 foaf:knows ?p2]
                      [?p2 foaf:knows ?p3]])]

      (is (= [rick katie julie]
             (foaf friends))))))

(deftest construct-test
  (testing "Construct queries"
    (testing "Construct Rick"
      (let [construct-rick (construct {:grafter.rdf/uri rick
                                       ?p ?o}
                                      [[rick ?p ?o]])]
        (is (= {:grafter.rdf/uri rick
                rdfs:label "Rick"
                foaf:knows #{martin katie}}
               (first (construct-rick friends))))))

    (testing "Construct people Rick knows"
      (let [construct-rick-nested (construct {:grafter.rdf/uri rick
                                              foaf:knows {:grafter.rdf/uri ?p
                                                          rdfs:label ?name}}
                                             [[rick foaf:knows ?p]
                                              [?p rdfs:label ?name]])]
        (is (= {:grafter.rdf/uri rick
                foaf:knows #{{:grafter.rdf/uri martin, rdfs:label "Martin"}
                              {:grafter.rdf/uri katie, rdfs:label "Katie"}}}
               (first (construct-rick-nested friends))))))

    (let [friends-db-as-triples (set (map (fn [[s p o]] [s p o])
                                       friends))]
      (testing "Construct vector solution"
        (let [construct-all (construct [?s ?p ?o]
                                       [[?s ?p ?o]])]
          (is (= friends-db-as-triples
                 (set (construct-all friends))))))

      (testing "Construct with a single projected value"
        (let [all-s (construct [?s]
                               [[?s ?p ?o]])]
          (is (= (set (map vector (map :s friends)))
                 (set (all-s friends))))))

      (testing "Construct set solution"
        (let [construct-all (construct #{?s ?p ?o}
                                       [[?s ?p ?o]])]
          (is (= (set (map set friends-db-as-triples))
                 (set (construct-all friends)))))))

    (testing "Construct arbitrary datastructure"
      (let [construct-all (construct [:foo #{[1 2 ?p ?o]}]
                                     [[rick ?p ?o]])]

        (is (= (hash-set [:foo #{[1 2 foaf:knows katie]}]
                         [:foo #{[1 2 foaf:knows martin]}]
                         [:foo #{[1 2 rdfs:label "Rick"]}])
               (set (construct-all friends))))))

    (testing "Constructing maps"
      ;; NOTE constructing data into a plain map emits one
      ;; data-structure per solution.
      (let [construct-all (construct {:not/grafter.rdf.subject ?s
                                      ?p ?o}
                                     [[?s ?p ?o]])]
        (is (= (hash-set
                {:not/grafter.rdf.subject rick, rdfs:label "Rick"}
                {:not/grafter.rdf.subject rick, foaf:knows katie}
                {:not/grafter.rdf.subject rick, foaf:knows martin}
                {:not/grafter.rdf.subject julie, rdfs:label "Julie"}
                {:not/grafter.rdf.subject martin, rdfs:label "Martin"}
                {:not/grafter.rdf.subject katie, rdfs:label "Katie"}
                {:not/grafter.rdf.subject katie, foaf:knows julie})
               (set (construct-all friends))))))

    (testing "Constructing :grafter.rdf/uri's"
      ;; Unless the map contains a :grafter.rdf/uri key in which
      ;; case it is grouped accordingly.
      (let [construct-all (construct {:grafter.rdf/uri ?s
                                      ?p ?o}
                                     [[?s ?p ?o]])]
        (is (= (hash-set
                {:grafter.rdf/uri martin, rdfs:label "Martin"}
                {:grafter.rdf/uri rick,
                 foaf:knows #{martin katie},
                 rdfs:label "Rick"}
                {:grafter.rdf/uri julie, rdfs:label "Julie"}
                {:grafter.rdf/uri katie, foaf:knows julie, rdfs:label "Katie"})
               (set (construct-all friends))))))))

(deftest merge-dbs-test
  (testing "merge-dbs is idempotent"
    (is (= (merge-dbs [[1 2 3]] [[1 2 3]])
           (index-triples [[1 2 3]]))))

  (testing "merge-dbs is idempotent if triples are already indexed or not"
    (is (= (merge-dbs [[1 2 3]]
                      (index-triples [[1 2 3]]))
           (index-triples [[1 2 3]]))))

  (testing "merge-dbs works with multiple triples"
    (is (= (merge-dbs [[1 2 3]] [[4 5 6]])
           (index-triples [[1 2 3] [4 5 6]]))))

  (testing "merge-dbs works with more complex indexes"
    (is (= (merge-dbs [[:s :p :o]] [[:s :p2 :o]] [[:s :p2 :o2]])
           (index-triples [[:s :p :o] [:s :p2 :o] [:s :p2 :o2]]))))

  (testing "merge-dbs idempotent with complex indexed/unindexed data"
    (is (= (merge-dbs friends-vectors (index-triples friends-vectors) friends-vectors)
           (index-triples friends-vectors)))))


(deftest grafter-record-interrop
  ;; Test we avoid issue: https://github.com/Swirrl/matcha/issues/5
  ;; where grafter types cause an exception because they don't
  ;; implement core.logic protocols.
  (let [ls (gp/->LangString "foo" :en)]
    (is (= ls  ((construct-1 ?o [[?s ?p ?o]]) [[:a :b ls]]))))

  (let [rs (gp/->RDFLiteral "foo" (URI. "http://some/datatype"))]
    (is (= rs  ((construct-1 ?o [[?s ?p ?o]]) [[:a :b rs]]))))

  (let [quads [(gp/->Quad :s :p :o :g) (gp/->Quad :s :p2 :o2 :g)]]
    (is #{:o :o2} ((construct ?o [[?s ?p ?o]]) quads))))



(def friends-big (memoize (fn []
                            (into friends
                                  (->> (range 100000)
                                       (map #(vector (data (dec %)) foaf:knows (data %))))))))

(deftest bigish-dataset-queries
  (let [lotsa-data (friends-big)
        ricks-friends (select [?name]
                              [[rick foaf:knows ?p2]
                               [?p2 rdfs:label ?name]])]
    (is (= ["Martin" "Katie"]
           (ricks-friends lotsa-data)))))

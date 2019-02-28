(ns grafter.matcha.alpha-test
  (:require [clojure.test :refer :all]
            [grafter.matcha.alpha :as m :refer :all]
            [grafter.vocabularies.core :refer [prefixer]]
            [grafter.vocabularies.foaf :refer [foaf:knows]]
            [grafter.vocabularies.rdf :refer [rdfs:label]]
            [grafter.rdf.protocols :refer [->Triple] :as gp]
            [clojure.spec.alpha :as s])
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

(defmacro throws? [ex-type & body]
  `(is (try
         ~@body
         false
         (catch clojure.lang.ExceptionInfo e#
           (= (-> e# ex-data :type) ~ex-type)))))

(deftest runtime-validation-test

  ;; select validation
  (letfn [(selectq-2 [uri]
            (select [?name]
                    [[uri foaf:knows ?p]
                     [?p rdfs:label ?name]]))
          (selectq-3 [uri db]
            (select [?name]
                    [[uri foaf:knows ?p]
                     [?p rdfs:label ?name]]
                    db))]

    ;; valid syntax doesn't throw
    (is (= ["Martin" "Katie"] ((selectq-2 rick) friends)))
    (is (= ["Martin" "Katie"] (selectq-3 rick friends)))

    ;; literal set throws
    (throws? ::m/invalid-bgp ((selectq-2 #{rick}) friends))
    (throws? ::m/invalid-bgp (selectq-3 #{rick} friends))

    ;; bound arg set throws
    (throws? ::m/invalid-bgp (let [arg #{rick}] ((selectq-2 arg) friends)))
    (throws? ::m/invalid-bgp (let [arg #{rick}] (selectq-3 arg friends))))

  ;; construct validation
  (letfn [(constructq-2 [uri]
            (construct
              {:foaf/knows ?name}
              [[uri foaf:knows ?p]
               [?p rdfs:label ?name]]))
          (constructq-3 [uri db]
            (construct
              {:foaf/knows ?name}
              [[uri foaf:knows ?p]
               [?p rdfs:label ?name]]
              db))]

    ;; valid syntax doesn't throw
    (is (= [#:foaf{:knows "Martin"} #:foaf{:knows "Katie"}] ((constructq-2 rick) friends)))
    (is (= [#:foaf{:knows "Martin"} #:foaf{:knows "Katie"}] (constructq-3 rick friends)))

    ;; s-expressions in bgps don't throw
    (is (= [#:foaf{:knows "Martin"} #:foaf{:knows "Katie"}]
           ((construct {:foaf/knows ?name}
              [[(identity (identity rick)) foaf:knows ?p]
               [?p rdfs:label ?name]])
            friends)))

    ;; more complex s-expressions in bgps don't throw
    (is (= [#:foaf{:knows "Martin"} #:foaf{:knows "Katie"}]
           ((construct {:foaf/knows ?name}
              [[(((fn [_] (fn [x] x)) 1) rick) foaf:knows ?p]
               [?p rdfs:label ?name]])
            friends)))

    ;; literal set throws
    (throws? ::m/invalid-bgp ((constructq-2 #{rick}) friends))
    (throws? ::m/invalid-bgp (constructq-3 #{rick} friends))

    ;; bound arg set throws
    (throws? ::m/invalid-bgp (let [arg #{rick}] ((constructq-2 arg) friends)))
    (throws? ::m/invalid-bgp (let [arg #{rick}] (constructq-3 arg friends))))

  ;; ask validation
  (letfn [(askq-1 [uri]
            (ask [[uri foaf:knows ?p]
                  [?p rdfs:label ?name]]))
          (askq-2 [uri db]
            (ask [[uri foaf:knows ?p]
                  [?p rdfs:label ?name]]
                 db))]

    ;; valid syntax doesn't throw
    (is ((askq-1 rick) friends))
    (is (askq-2 rick friends))

    ;; literal set throws
    (throws? ::m/invalid-bgp ((askq-1 #{rick}) friends))
    (throws? ::m/invalid-bgp (askq-2 #{rick} friends))

    ;; bound arg set throws
    (throws? ::m/invalid-bgp (let [arg #{rick}] ((askq-1 arg) friends)))
    (throws? ::m/invalid-bgp (let [arg #{rick}] (askq-2 arg friends)))

    ;; no ?qvars are OK
    (is (let [col-uri rick] (m/ask [[col-uri foaf:knows martin]] friends)))))

(deftest immediate-and-function-macro-arities-equiv
  (let [uri rick]
    (let [v1 ((m/select [?name]
                        [[uri foaf:knows ?p]
                         [?p rdfs:label ?name]]) friends)
          v2 (m/select [?name]
                       [[uri foaf:knows ?p]
                        [?p rdfs:label ?name]] friends)]
      (is (= v1 v2)))

    (let [v1 ((m/select-1 [?name]
                          [[uri foaf:knows ?p]
                           [?p rdfs:label ?name]]) friends)
          v2 (m/select-1 [?name]
                         [[uri foaf:knows ?p]
                          [?p rdfs:label ?name]] friends)]
      (is (= v1 v2)))

    (let [v1 ((m/construct {:foaf/knows ?name}
                [[uri foaf:knows ?p]
                 [?p rdfs:label ?name]]) friends)
          v2 (m/construct {:foaf/knows ?name}
               [[uri foaf:knows ?p]
                [?p rdfs:label ?name]] friends)]
      (is (= v1 v2)))

    (let [v1 ((m/construct-1 {:foaf/knows ?name}
                             [[uri foaf:knows ?p]
                              [?p rdfs:label ?name]]) friends)
          v2 (m/construct-1 {:foaf/knows ?name}
                            [[uri foaf:knows ?p]
                             [?p rdfs:label ?name]] friends)]
      (is (= v1 v2)))

    (let [v1 ((m/ask [[uri foaf:knows ?p] [?p rdfs:label ?name]]) friends)
          v2 (m/ask [[uri foaf:knows ?p] [?p rdfs:label ?name]] friends)]
      (is (= v1 v2)))))

(deftest values-syntax-test
  (is (= (let [people #{rick}]
           (set
            (select [?name]
              [[?person foaf:knows ?o]
               [?o rdfs:label ?name]
               (values ?person people)]
              friends)))
         #{"Martin" "Katie"}))
  (is (= (let [people #{rick katie}]
           (set
            (select [?name]
              [[?person foaf:knows ?o]
               [?o rdfs:label ?name]
               (values ?person people)]
              friends)))
         #{"Martin" "Katie" "Julie"}))

  (is (= (let [people #{rick katie}
               names #{"Julie"}]
           (set
            (select [?name]
              [[?person foaf:knows ?o]
               (values ?person people)
               [?o rdfs:label ?name]
               (values ?name names)]
              friends)))
         #{"Julie"}))

  (throws? ::m/invalid-values
           (let [people rick]
             (set
              (select [?name]
                [[?person foaf:knows ?o]
                 [?o rdfs:label ?name]
                 (values ?person people)]
                friends)))
           #{"Martin" "Katie" "Julie"})
  (throws? ::m/invalid-values
           (let [people [rick]]
             (set
              (select [?name]
                [[?person foaf:knows ?o]
                 [?o rdfs:label ?name]
                 (values ?person people)]
                friends)))
           #{"Martin" "Katie" "Julie"}))

(def other:label (data "other-label"))

(def optional-friends
  [(->Triple rick rdfs:label "Rick")
   (->Triple martin rdfs:label "Martin")
   (->Triple katie rdfs:label "Katie")

   (->Triple julie other:label "Not a robot")

   (->Triple rick foaf:knows martin)
   (->Triple rick foaf:knows katie)
   (->Triple katie foaf:knows julie)

   (->Triple "Martin" :name/backwards "Nitram")
   (->Triple "Katie" :name/backwards "Eitak")
   (->Triple "Rick" :name/backwards "Kcir")])

(deftest optional-syntax-test

  (testing "OPTIONAL behaviour"
    (is (= (set
            (let [person katie]
              (select [?o ?name]
                [[person foaf:knows ?o]
                 (optional [[?o rdfs:label ?name]])
                 (optional [[?o other:label ?name]])]
                optional-friends)))
           #{[julie "Not a robot"]}))
    (is (= (set
            (let [person rick]
              (select [?o ?name]
                [[person foaf:knows ?o]
                 (optional [[?o rdfs:label ?name]])
                 (optional [[?o other:label ?name]])]
                optional-friends)))
           #{[martin "Martin"] [katie "Katie"]})))

  (testing "OPTIONAL behaviour with VALUES"
    (is (= (set
            (let [people #{rick katie}]
              (select [?o ?name]
                [[?person foaf:knows ?o]
                 (optional [[?o rdfs:label ?name]])
                 (optional [[?o other:label ?name]])
                 (values ?person people)]
                optional-friends)))
           #{[martin "Martin"] [katie "Katie"] [julie "Not a robot"]})))

  (testing "Where optional thing is just not there"
    (is (= (set
            (let [people #{rick katie}]
              (select [?o ?name]
                [[?person foaf:knows ?o]
                 [?o rdfs:label ?name]
                 (optional [[?o :who/am-i? ?dunno]])
                 (values ?person people)]
                optional-friends)))
           #{[martin "Martin"] [katie "Katie"]})))

  (testing "How about some optionals in your optionals?"
    (is (= (set
            (let [people #{rick katie}
                  names  #{"Martin"}]
              (select [?o ?eman]
                [[?person foaf:knows ?o]
                 (optional [[?o rdfs:label ?name]
                            (optional [[?name :name/backwards ?eman]
                                       (values ?name names)])])
                 (values ?person people)]
                optional-friends)))
           #{[martin "Nitram"] [katie '_0] [julie '_0]}))))

(defmacro valid-syntax? [[op & args]]
  (let [argspec (:args (get (s/registry) (resolve-sym op)))]
    (s/valid? argspec args)))

(def valid-syntax-symbol :hi-there)
(def invalid-syntax-symbol [])
(deftest macro-syntax-validation-test
  (let [people #{rick katie}
        names  #{"Martin"}]
    (is (valid-syntax?
         (select [?o ?eman]
           [[?person foaf:knows valid-syntax-symbol] ;; => :hi-there
            (optional [[?o rdfs:label ?name]
                       (optional [[?name :name/backwards ?eman]
                                  (values ?name names)])])
            (values ?person people)]
           optional-friends)))
    (is (not (valid-syntax?
              (select [?o ?eman]
                [[?person foaf:knows invalid-syntax-symbol] ;; => []
                 (optional [[?o rdfs:label ?name]
                            (optional [[?name :name/backwards ?eman]
                                       (values ?name names)])])
                 (values ?person people)]
                optional-friends))))))

(ns grafter.matcha.alpha-test
  (:require [clojure.test :refer :all]
            [grafter.matcha.alpha :as m :refer :all]
            [grafter.vocabularies.core :refer [prefixer]]
            [grafter.vocabularies.foaf :refer [foaf:knows]]
            [grafter.vocabularies.rdf :refer [rdfs:label]]
            [grafter-2.rdf.protocols :refer [->Triple] :as gp]
            [clojure.spec.alpha :as s])
  (:import [grafter_2.rdf.protocols LangString RDFLiteral]
           [java.net URI]
           [java.time LocalDate ZoneOffset]))

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

    (testing "Construct returns nil when no matches"
      (is (nil? (construct [?p ?o] [[:not-a-matching-query ?p ?o]] [[:a :b :c] [:d :e :f]])))

      (is (nil? (construct {:grafter.rdf/uri ?s ?p ?o}
                           [[?s :not :matching] [?s ?p ?o]]

                           [[:a :b :c] [:d :e :f]]))))

    (testing "Matches nil"
      (is (= "nil label"
             (construct-1 ?label
                          [[nil :label ?label]]

                          [[nil :label "nil label"]]))))

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


(deftest grafter-record-interop
  ;; Test we avoid issue: https://github.com/Swirrl/matcha/issues/5
  ;; where grafter types cause an exception because they don't
  ;; implement core.logic protocols.
  (let [ls (gp/->LangString "foo" :en)]
    (is (= ls  ((construct-1 ?o [[?s ?p ?o]]) [[:a :b ls]]))))

  (let [rs (gp/->RDFLiteral "foo" (URI. "http://some/datatype"))]
    (is (= rs  ((construct-1 ?o [[?s ?p ?o]]) [[:a :b rs]]))))

  (let [quads [(gp/->Quad :s :p :o :g) (gp/->Quad :s :p2 :o2 :g)]]
    (is #{:o :o2} ((construct ?o [[?s ?p ?o]]) quads)))


  (let [offset-date (gp/map->OffsetDate {:date (LocalDate/now) :timezone (ZoneOffset/UTC)})]
    (m/construct ?o [[?s ?p ?o]] [[:s :p offset-date]])))



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
  (is (= #{"Martin" "Katie"}
         (let [people #{rick}]
           (set
            (select [?name]
              [[?person foaf:knows ?o]
               [?o rdfs:label ?name]
               (values ?person people)]
              friends)))))

  (is (= #{"Martin" "Katie" "Julie"}
         (let [people #{rick katie}]
           (set
            (select [?name]
              [[?person foaf:knows ?o]
               [?o rdfs:label ?name]
               (values ?person people)]
              friends)))))

  (is (= #{"Julie"}
         (let [people [rick katie]
               names #{"Julie"}]
           (set
            (select [?name]
              [[?person foaf:knows ?o]
               (values ?person people)
               [?o rdfs:label ?name]
               (values ?name names)]
              friends)))))

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
           (let [people 1]
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
    (let [tiny-db [[:a :triple :here]]]
      (is (= #{[:a :triple :here]}
             (set
              (select [?s ?p ?o]
                [(optional [[?s ?p ?o]])]
                tiny-db))))

      (is (nil?
           (select [?s ?p ?o]
             [(optional [[:do :not :match]])]
             tiny-db)))

      (is (= [[:a :triple :here]]
             (select [?s ?p ?o]
               [[?s ?p ?o]
                (optional [[:optional :doesnt :match]
                           [:but :required-pattern :does]])]
               tiny-db)))

      (is (= #{[:a :triple :here]}
             (set
              (select [?s ?p ?o]
                [[?s ?p ?o]
                 (optional [[?s ?p ?o]])]
                tiny-db))))

      (is (= #{[:a :triple :here]}
             (set (select [?s ?p ?o]
                    [(optional [[:optional :doesnt :match]
                                [:but :other-optional :does]])
                     (optional [[?s ?p ?o]])]
                    tiny-db)))))

    (is (= #{[julie "Not a robot"]}
           (set
            (let [person katie]
              (select [?o ?name]
                [[person foaf:knows ?o]
                 ;;(optional [[?o rdfs:label ?name]])
                 (optional [[?o other:label ?name]])]
                optional-friends)))))

    (is (= #{[martin "Martin"] [katie "Katie"]}
           (set
            (let [person rick]
              (select [?o ?name]
                [[person foaf:knows ?o]
                 (optional [[?o rdfs:label ?name]])
                 (optional [[?o other:label ?name]])]
                optional-friends))))))

  (testing "OPTIONAL and multiple solutions"
    (let [db [(->Triple :john :status :online)
              (->Triple :john :prop1 "A")
              (->Triple :john :prop1 "B")
              (->Triple :john :prop2 :x)
              (->Triple :john :prop2 :y)]]
      (is (= #{[:john "B" :y] [:john "B" :x] [:john "A" :y] [:john "A" :x]}
             (set (select [?o ?p ?x]
                    [[?o :status ?status]
                     (optional [[?o :prop1 ?p]])
                     (optional [[?o :prop2 ?x]])]
                    db)))))

    (let [db [(->Triple :john :status :online)
              (->Triple :john :prop2 :x)
              (->Triple :john :prop2 :y)]]
      (is (= #{[:john '_1 :y] [:john '_1 :x]}
             (set (select [?o ?p ?x]
                    [[?o :status ?status]
                     (optional [[?o :prop1 ?p]])
                     (optional [[?o :prop2 ?x]])]
                    db))))))

  (testing "OPTIONAL behaviour with VALUES"
    (is (=
         #{[martin "Martin"] [katie "Katie"] [julie "Not a robot"]}
         (set
            (let [people #{rick katie}]
              (select [?o ?name]
                [[?person foaf:knows ?o]
                 (optional [[?o rdfs:label ?name]])
                 (optional [[?o other:label ?name]])
                 (values ?person people)]
                optional-friends))))))

  (testing "Where optional thing is just not there"
    (is (= #{[martin "Martin"] [katie "Katie"]}
           (set
            (let [people #{rick katie}]
              (select [?o ?name]
                [[?person foaf:knows ?o]
                 [?o rdfs:label ?name]
                 (optional [[?o :who/am-i? ?dunno]])
                 (values ?person people)]
                optional-friends))))))

  (testing "How about some optionals in your optionals?"
    (is (= #{[martin "Nitram"] [katie '_1] [julie '_0]}
           (set
            (let [people #{rick katie}
                  names  #{"Martin"}]
              (select [?o ?eman]
                [[?person foaf:knows ?o]
                 (optional [[?o rdfs:label ?name]
                            (optional [[?name :name/backwards ?eman]
                                       (values ?name names)])])
                 (values ?person people)]
                optional-friends)))))))

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
                optional-friends))))

    ;; Yes this should really be valid...
    (is (valid-syntax?
         (construct ?s
                    [[nil nil nil]])))))

(deftest build-test
  (testing "build"
    (let [db [[:s :p :o]
              [:s :p2 :o2]

              [:s2 :p :o3]]]

      (testing "with unbound subject"
        (let [ret (build ?s
                         {?p ?o}
                         [[?s ?p ?o]]
                         db)]
          (is (= #{{:grafter.rdf/uri :s
                    :p :o
                    :p2 :o2}

                   {:grafter.rdf/uri :s2
                    :p :o3}}
                 (set ret)))))

      (testing "with bound subject"
        (let [subject :s
              ret (build subject
                         {?p ?o}
                         [[subject ?p ?o]]
                         db)]
          (is (= #{{:grafter.rdf/uri :s
                    :p :o
                    :p2 :o2}}
                 (set ret)))))

      (testing "with hardcoded subject value"
        (let [ret (build :s
                         {?p ?o}
                         [[:s ?p ?o]]
                         db)]
          (is (= #{{:grafter.rdf/uri :s
                    :p :o
                    :p2 :o2}}
                 (set ret))))))

    (testing "Optionals and predicate grouping"
      (let [db [[:s :label "s"]
                [:s :label "s another"]
                [:s :p2 :o2]
                [:s :optional "optional"]
                [:s2 :label "s2"]
                [:s2 :p2 :o2]]

            ret (build ?s
                       {:label ?label
                        :optional ?opt}

                       [[?s :label ?label]
                        (grafter.matcha.alpha/optional [[?s :optional ?opt]])]
                       db)]

        (is (= #{{:grafter.rdf/uri :s,
                  :label #{"s" "s another"},
                  :optional "optional"}
                 {:grafter.rdf/uri :s2, :label "s2"}}

               (set ret)))))))

(deftest build-1-test
  (let [db [[:s :p :o]
            [:s :p2 :o2]
            [:s :p2 :o3]
            [:s2 :p :o]
            [:s2 :p2 :o2]]
        ret (build-1 ?s
                     {?p ?o}
                     [(values ?s [:s])
                      [?s ?p ?o]]
                     db)]
    (is (= {:grafter.rdf/uri :s, :p2 #{:o3 :o2}, :p :o}
           ret))))

(deftest issue-21-test
  (testing "Order of `optional`s shouldn't matter."
    (let [data [[1 :p :a]
                [1 :p2 :X]
                [1 :p3 :Z]
                [3 :q :x]]
          ;; two 'equal' queries with different ordering of
          ;; `optional`s
          result-ab (build [:id ?id]
                           {:id ?id
                            :optional-a ?oa
                            :optional-b ?ob}
                           [[?id :p ?o]
                            (optional [[?id :p2 ?oa]])
                            (optional [[?id :p3 ?ob]])]
                           data)
          result-ba (build [:id ?id]
                           {:id ?id
                            :optional-a ?oa
                            :optional-b ?ob}
                           [[?id :p ?o]
                            (optional [[?id :p3 ?ob]])
                            (optional [[?id :p2 ?oa]])]
                           data)]
      (is (= (select-keys (first result-ab) [:optional-a :optional-b])
             {:optional-a :X :optional-b :Z}))
      (is (= result-ab result-ba))
      result-ab)))

(def catalog-data [[:crime :a :dcat/Dataset]
                   [:crime :dcterms/title "Crime"]

                   [:crime :dcterms/spatial :manchester]
                   [:crime :dcat/spatialResolutionInMeters 50]

                   [:crime :dcterms/description "Has all optional fields"]
                   [:crime :dcterms/publisher :ons]
                   [:crime :dcterms/creator :moj]

                   [:operations :a :dcat/Dataset]
                   [:operations :dcterms/title "Operational Procedures"]
                   [:operations :dcterms/description "Has one optional (creator)"]
                   [:operations :dcterms/creator :nhs]

                   [:deprivation :a :dcat/Dataset]
                   [:deprivation :dcterms/title "Covid"]
                   [:deprivation :dcterms/description "Has one optional (publisher)"]
                   [:deprivation :dcterms/publisher :dluhc]

                   [:not-in-results :a :Ontology]
                   [:not-in-results :dcterms/title "Should not be found"]])

(deftest catalog-example-with-optionals
  (testing "catalog example with multiple optionals"
    (testing "select"
      (is
       ;; NOTE select's return unbound variables not 'nil'
       (= #{'[:operations "Operational Procedures" _0 :nhs _1 _2]
            '[:deprivation "Covid" :dluhc _3 _4 _5]
            '[:crime "Crime" :ons :moj :manchester 50]}

          (set (select [?ds ?title ?pub ?creator ?area ?resolution]
                       [[?ds :a :dcat/Dataset]
                        [?ds :dcterms/title ?title]
                        (optional
                         [[?ds :dcterms/spatial ?area]
                          [?ds :dcat/spatialResolutionInMeters ?resolution]])
                        (optional
                         [[?ds :dcterms/publisher ?pub]])
                        (optional
                         [[?ds :dcterms/creator ?creator]])]

                       catalog-data)))))

    (testing "build"
      (is
       (= #{{:grafter.rdf/uri :operations
             :dcterms/creator :nhs}
            {:grafter.rdf/uri :crime
             :dcterms/spatial :manchester
             :dcat/spatialResolutionInMeters 50
             :dcterms/publisher :ons
             :dcterms/creator :moj}
            {:grafter.rdf/uri :deprivation
             :dcterms/publisher :dluhc}}

          (set (build ?ds {:dcterms/creator ?creator
                           :dcterms/publisher ?pub
                           :dcterms/spatial ?area
                           :dcat/spatialResolutionInMeters ?resolution}

                      [[?ds :a :dcat/Dataset]
                       [?ds :dcterms/title ?title]
                       (optional
                        [[?ds :dcterms/spatial ?area]
                         [?ds :dcat/spatialResolutionInMeters ?resolution]])
                       (optional
                        [[?ds :dcterms/publisher ?pub]])
                       (optional
                        [[?ds :dcterms/creator ?creator]])]

                      catalog-data)))))))

(deftest optionals-with-values
  (is (= '([:crime _0 :ons :moj :manchester 50]
           [:deprivation _0 :dluhc _0 _1 _2])
         (select [?ds ?title ?pub ?creator ?area ?resolution]
                 [(values ?ds
                          [:crime
                           :deprivation])
                  (optional
                   [[?ds :dcterms/spatial ?area]
                    [?ds :dcat/spatialResolutionInMeters ?resolution]])
                  (optional
                   [[?ds :dcterms/publisher ?pub]])
                  (optional
                   [[?ds :dcterms/creator ?creator]])]

                 catalog-data))))

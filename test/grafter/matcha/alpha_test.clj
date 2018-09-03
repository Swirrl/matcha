(ns grafter.matcha.alpha-test
  (:require [clojure.test :refer :all]
            [grafter.matcha.alpha :refer :all]))

(def friends [[:rick :rdfs/label "Rick"]
              [:martin :rdfs/label "Martin"]
              [:katie :rdfs/label "Katie"]
              [:julie :rdfs/label "Julie"]

              [:rick :foaf/knows :martin]
              [:rick :foaf/knows :katie]
              [:katie :foaf/knows :julie]])

(def friends-big (into friends
                       (->> (range 1000000)
                            (map #(vector (dec %) :foaf/knows %)))))

(deftest select-test
  (testing "Select queries arity 1"
    (let [ricks-name (select [[:rick :rdfs/label ?name]])]
      (is (= ["Rick"]
             (ricks-name friends)))))

  (testing "Select queries arity 2"
    (testing "Rick knows"
      (let [rick-knows (select [?name]
                               [[:rick :foaf/knows ?p2]
                                [?p2 :rdfs/label ?name]])]

        (is (= ["Martin" "Katie"]
               (rick-knows friends)))))

    (testing "Katie knows"
      (let [katie-knows (select [?p2 ?name]
                                [[:katie :foaf/knows ?p2]
                                 [?p2 :rdfs/label ?name]])]

        (is (= [[:julie "Julie"]]
               (katie-knows friends))))))

  (testing "Friend of friend"
    (let [foaf (select [?p1 ?p2 ?p3]
                     [[?p1 :foaf/knows ?p2]
                      [?p2 :foaf/knows ?p3]])]

      (is (= [[:rick :katie :julie]]
             (foaf friends))))))

(deftest select-1-test
  (testing "Select-1 queries arity 1"
    (testing "Select queries arity 1"
      (let [ricks-name (select-1 [[:rick :rdfs/label ?name]])]
        (is (= "Rick"
               (ricks-name friends))))))

  (testing "Select-1 queries arity 2"
    (testing "Rick knows"
      (let [rick-knows (select-1 [?name]
                                 [[:rick :foaf/knows ?p2]
                                  [?p2 :rdfs/label ?name]])]
        (is (= "Martin"
               (rick-knows friends)))))

    (testing "Katie knows"
      (let [katie-knows (select-1 [?p2 ?name]
                                [[:katie :foaf/knows ?p2]
                                 [?p2 :rdfs/label ?name]])]

        (is (= [:julie "Julie"]
               (katie-knows friends))))))

  (testing "Friend of friend"
    (let [foaf (select-1 [?p1 ?p2 ?p3]
                     [[?p1 :foaf/knows ?p2]
                      [?p2 :foaf/knows ?p3]])]

      (is (= [:rick :katie :julie]
             (foaf friends))))))

(deftest construct-test
  (testing "Construct queries"
    (testing "Construct Rick"
      (let [construct-rick (construct {:grafter.rdf/subject :rick
                                       ?p ?o}
                                      [[:rick ?p ?o]])]
        (is (= {:grafter.rdf/subject :rick
                :rdfs/label "Rick"
                :foaf/knows #{:martin :katie}}
               (first (construct-rick friends))))))

    (testing "Construct people Rick knows"
      (let [construct-rick-nested (construct {:grafter.rdf/subject :rick
                                              :foaf/knows {:grafter.rdf/subject ?p
                                                           :rdfs/label ?name}}
                                             [[:rick :foaf/knows ?p]
                                              [?p :rdfs/label ?name]])]
        (is (= {:grafter.rdf/subject :rick
                :foaf/knows #{{:grafter.rdf/subject :martin, :rdfs/label "Martin"}
                              {:grafter.rdf/subject :katie, :rdfs/label "Katie"}}}
               (first (construct-rick-nested friends))))))

    (testing "Construct vector solution"
      (let [construct-all (construct [?s ?p ?o]
                                     [[?s ?p ?o]])]
        (is (= (set friends2)
               (set (construct-all friends2))))))

    (testing "Construct set solution"
      (let [construct-all (construct #{?s ?p ?o}
                                     [[?s ?p ?o]])]
        (is (= (set (map set friends2))
               (set (construct-all friends2))))))

    (testing "Construct arbitrary datastructure"
      (let [construct-all (construct [:foo #{[1 2 ?p ?o]}]
                                     [[:rick ?p ?o]])]

        (is (= (hash-set [:foo #{[1 2 :foaf/knows :katie]}]
                         [:foo #{[1 2 :foaf/knows :martin]}]
                         [:foo #{[1 2 :rdfs/label "Rick"]}])
               (set (construct-all friends2))))))

    (testing "Constructing maps"
      ;; NOTE constructing data into a plain map emits one
      ;; data-structure per solution.
      (let [construct-all (construct {:not/grafter.rdf.subject ?s
                                      ?p ?o}
                                     [[?s ?p ?o]])]
        (is (= (hash-set
                {:not/grafter.rdf.subject :rick, :rdfs/label "Rick"}
                {:not/grafter.rdf.subject :rick, :foaf/knows :katie}
                {:not/grafter.rdf.subject :rick, :foaf/knows :martin}
                {:not/grafter.rdf.subject :julie, :rdfs/label "Julie"}
                {:not/grafter.rdf.subject :martin, :rdfs/label "Martin"}
                {:not/grafter.rdf.subject :katie, :rdfs/label "Katie"}
                {:not/grafter.rdf.subject :katie, :foaf/knows :julie})
               (set (construct-all friends2))))))

    (testing "Constructing :grafter.rdf/subject's"
      ;; Unless the map contains a :grafter.rdf/subject key in which
      ;; case it is grouped accordingly.
      (let [construct-all (construct {:grafter.rdf/subject ?s
                                      ?p ?o}
                                     [[?s ?p ?o]])]
        (is (= (hash-set
                {:grafter.rdf/subject :martin, :rdfs/label "Martin"}
                {:grafter.rdf/subject :rick,
                 :foaf/knows #{:martin :katie},
                 :rdfs/label "Rick"}
                {:grafter.rdf/subject :julie, :rdfs/label "Julie"}
                {:grafter.rdf/subject :katie, :foaf/knows :julie, :rdfs/label "Katie"})
               (set (construct-all friends2))))))))


;; TODO example of using a var binding

;; TODO add bindings/values equivalent

(ns sparqlo.core-test
  (:require [clojure.test :refer :all]
            [sparqlo.core :refer :all]))

(def friends [[:rick :rdfs/label "Rick"]
              [:martin :rdfs/label "Martin"]
              [:katie :rdfs/label "Katie"]
              [:julie :rdfs/label "Julie"]

              [:rick :foaf/knows :martin]
              [:rick :foaf/knows :katie]
              [:katie :foaf/knows :julie]])

(deftest simple-query
  (testing "Rick knows"
    (let [res (query friends
                     [?p2 ?name]
                     [[:rick :foaf/knows ?p2]
                      [?p2 :rdfs/label ?name]])]

      (is (= [[:martin "Martin"]
              [:katie "Katie"]]
             res))))

  (testing "Katie knows"
    (let [res (query friends
                     [?p2 ?name]
                     [[:katie :foaf/knows ?p2]
                      [?p2 :rdfs/label ?name]])]

      (is (= [[:julie "Julie"]]
             res))))

  (testing "Friend of friend"
    (let [res (query friends
                     [?p1 ?p2 ?p3]
                     [[?p1 :foaf/knows ?p2]
                      [?p2 :foaf/knows ?p3]
                      [?p3 :rdfs/label ?name]])]

      (is (= [[:rick :katie :julie]]
             res)))))

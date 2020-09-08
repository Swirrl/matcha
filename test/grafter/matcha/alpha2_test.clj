(ns grafter.matcha.alpha2-test
  (:require [grafter.matcha.alpha2 :as sut]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as g]
            [clojure.test :as t :refer [is deftest testing]]))

(def matcha-db (sut/make-index [[:rick :foaf/friend :katie]
                                [:katie :foaf/friend :julie]
                                [:julie :foaf/name "Julie"]]))

(deftest select-query-test



  (let [conf-q (sut/conform-query '(select [?name]
                                           [[?s :foaf/friend ?o]
                                            [?o :foaf/friend ?o2]
                                            [?o2 :foaf/name ?name]]))]

    (is (= ["Julie"] (sut/run-query matcha-db conf-q))))



  )

(ns grafter.matcha.exp
  (:require [clojure.core.logic :as l]
            [grafter.matcha.db :as pldb]))

(defn build-index [triples]
  (pldb/index-triples triples))

(defn make-person [person i]
  (keyword (str person i)))

(def the-db
  (build-index (mapcat (fn [i]
                         (let [rick (make-person "rick" i)
                               katie (make-person "katie" i)
                               gary (make-person "gary" i)
                               julie (make-person "julie" i)
                               farsanna (make-person "farsanna" i)
                               ray (make-person "ray" i)
                               jo (make-person "jo" i)
                               ian (make-person "ian" i)
                               michelle (make-person "michelle" i)
                               claire (make-person "claire" i)
                               cat (make-person "cat" i)
                               martin (make-person "martin" i)]
                           [[rick :a :Person]
                            [katie :a :Person]
                            [cat :a :Person]

                            [rick :knows katie]


                            [julie :a :Person]
                            [farsanna :a :Person]
                            [ray :a :Person]
                            [jo :a :Person]
                            [ian :a :Person]
                            [michelle :a :Person]

                            [rick :label "Rick"]
                            [katie :knows julie]
                            [katie :knows farsanna]
                            [katie :knows jo]
                            [rick :knows ray]

                            [rick :knows martin]
                            [katie :knows martin]
                            [ray :knows rick]
                            [ray :knows claire]
                            [ray :knows gary]

                            [ray :name "Ray"]
                            [claire :name "Claire"]

                            [cat :knows katie]
                            [cat :name "Cat"]]))
                       (range 1))))

(comment
  (time (vec (pldb/with-db the-db
               (l/run* [?person ?personb ?personbname]
                 (pldb/triple ?person :knows :katie100)
                 (pldb/triple ?personb :knows ?person)
                 (pldb/triple ?personb :name ?personbname)
                 (pldb/triple ?person :a :Person)))))

  )

(comment


  (def the-db
    (pldb/db-triple-2 #{:s :o}  (mapcat (fn [i]
                                            (let [rick (make-person "rick" i)
                                                  katie (make-person "katie" i)
                                                  gary (make-person "gary" i)
                                                  julie (make-person "julie" i)
                                                  farsanna (make-person "farsanna" i)
                                                  ray (make-person "ray" i)
                                                  jo (make-person "jo" i)
                                                  ian (make-person "ian" i)
                                                  michelle (make-person "michelle" i)
                                                  claire (make-person "claire" i)
                                                  cat (make-person "cat" i)
                                                  martin (make-person "martin" i)]
                                              [[rick :a :Person]
                                               [katie :a :Person]
                                               [cat :a :Person]

                                               [rick :rdfs/label "Rick"]
                                               [katie :rdfs/label "Katie"]
                                               [cat :rdfs/label "Cat"]
                                               [rick :knows katie]
                                               [cat :knows katie]]))
                                          (range 200000))))

  (time (vec (pldb/with-db the-db
               (l/run* [?person ?label ?p]
                 (pldb/triple ?person :knows :katie0)
                 (pldb/triple ?person :a :Person)
                 (pldb/triple ?person :rdfs/label ?label)
                 ;;(triple ?personb :knows ?person)
                 ;;(triple ?personb :name ?personbname)
                 ))))

  (pldb/index-triples [[:s :p :o] [:s :p :o2] [:s :p2 :o2]])

  (grafter.matcha.alpha/construct {:grafter.rdf/uri ?s ?p ?o} [[?s ?p ?o]] (pldb/index-triples [[:s :p :o] [:s :p :o2] [:s :p2 :o2]]))



  (time (vec (pldb/with-db the-db
               (l/run* [?person ?p]
                 (pldb/triple ?person ?p :Person)
                 (pldb/triple ?person :knows :katie0)
                 ;;(pldb/triple ?personb :knows ?person)
                 ;;(pldb/triple ?personb :name ?personbname)
                 ))))


  )

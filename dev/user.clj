(ns user
  (:require [clojure.spec.alpha :as s]
            [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb]))


(require '[grafter.matcha.alpha :as m])

(def friends
  [[:rick :rdfs/label "Rick"]
   [:martin :rdfs/label "Martin"]
   [:katie :rdfs/label "Katie"]
   [:julie :rdfs/label "Julie"]

   [:rick :foaf/knows :martin]
   [:rick :foaf/knows :katie]
   [:katie :foaf/knows :julie]])

(pldb/with-db
  friends
  (l/run* [?s ?p ?o]
    (l/fresh []
      (m/triple ?s ?p ?o)
      )
    )
  )

;; (defn constructq [uri]
;;   (m/construct
;;    {:grafter.rdf/uri uri
;;     :foaf/knows {:grafter.rdf/uri uri :rdfs/label ?name}}
;;    [[uri :foaf/knows ?p]
;;     [?p :rdfs/label ?name]]))

(defn selectq [uri]
  (m/select
   [[uri :foaf/knows ?p]
    [?p :rdfs/label ?name]]))

;; (defn askq [uri]
;;   (m/ask
;;    [[uri :foaf/knows ?p]
;;     [?p :rdfs/label ?name]]))

;; (construct {:a a :b b}
;;            [[a some:predicate ?val]
;;             [a b "some constraint"]])

;; ;;; Database argument
;; ;;; =================
;; ;;; It's traditional to pass db/config/dependencies as the first argument,
;; ;;; personally I like that tradition. How often do you find you close over the
;; ;;; db in a function? E.G.,
;; ;;;
;; ;;; `(fn [arg] ((construct {:arg arg} [[arg what:evs "thing]]) db))`

;; ;;; Option 1: Implicit Magic
;; ;;; ========================
;; ;;; Find all unbound symbols in expression and bind them in order of appearance
;; ;;; as function arguments

;; ;;; Option 2: Explicit Magic
;; ;;; ========================
;; ;;; Find all unbound symbols in expression and bind them via kwargs (or symargs)
;; ;;; or a binding map argument

;; ;;; construct : Monad db => [var] -> form -> bgps -> ([var] -> db result)
;; (defmacro construct [arglist form bgps])

;; ;;; run-db : Monad db => ([var] -> db result) -> DB -> result
;; (defn run-db [& f db])

;; ;;; with-db : Monad db => [var DB] -> db &body ->

;; (defmacro construct-fn {:style/indent :defn}
;;   ([arglist construct-form bgps]))

;; (s/fdef construct-fn
;;   :args (s/or :ary-3 (s/cat :arglist :clojure.core.specs.alpha/param-list
;;                             :construct-form any?
;;                             :bgps ::m/bgps)
;;               :ary-4 (s/cat :db any?
;;                             :arglist :clojure.core.specs.alpha/param-list
;;                             :construct-form any?
;;                             :bgps ::m/bgps)))

;; (let [db []
;;       query (fn-construct [arg pred]
;;               {:binding arg :some-var ?var}
;;               [[arg some:predicate ?val]
;;                [arg pred "some constraint"]])]
;;   (query db))

;; (comment

;;   (let [arg #{:rick}] ((constructq arg) friends))

;;   ((selectq #{:rick}) friends)

;;   ((askq #{:rick}) friends)

;;   (require '[grafter.rdf :as rdf])

;;   (def colls (rdf/statements "dev/collections.nt"))

;;   (map prn (take 10 ((m/select [[?s ?p ?o] [?o ?p' "Areas"]]) colls)))

;;   (((fn [x] (m/select [[?s ?p ?o] [?o ?p' "Areas"]])) colls))

;;   (map prn (take 10 colls))

;;   )

;;  ;; -*- eval: (define-clojure-indent (construct-fn :defn)) -*-

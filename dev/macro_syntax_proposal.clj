(ns macro-syntax-proposal
  (:require [clojure.spec.alpha :as s]
            [grafter.matcha.alpha :as m]))

(defmacro construct [& body])

(fn [arg1 arg2 db]
  ((construct form [[arg1 some:thing ?o]
                    [arg1 thing:else arg2]]) db))

(let [db ...]
  (let [f (fn [arg1 arg2]
            ((construct form [[arg1 some:thing ?o]
                              [arg1 thing:else arg2]]) db))]
    (f arg1 arg2)))

(defmacro construct-fn {:style/indent :defn} [arglist construct-form bgps]
  ;; ...
  )

(defmacro construct-fn {:style/indent :defn} [arglist opts? construct-form bgps]
  ;; ...
  )

(construct-fn [arg1 arg2]
  {:binding arg1 :some-var? ?var}
   [[arg1 some:thing ?o]
    [arg1 thing:else arg2]]) ; -> (fn [db arg1 arg2] ...)

(construct-fn [arg1 arg2]
  {::reason true}
  {:binding arg1 :some-var? ?var}
  [[arg1 some:thing ?o]
   [arg1 thing:else arg2]]) ;: -> (fn [{:keys [arg1 arg2]} db] ...)

(defmacro construct* [& body])

(let [db []
      arg1 :arg/arg1
      arg2 :arg/arg2
      some:thing :some/thing
      thing:else :thing/else]
  (m/construct
   {::reason true}
   {:binding arg1 :some-var? ?var}
   [[arg1 some:thing ?o]
    [arg1 thing:else arg2]]
   db
   )) ;: -> (fn [{:keys [arg1 arg2]} db] ...)

(let [db []
      arg1 :arg/arg1
      arg2 :arg/arg2
      some:thing :some/thing
      thing:else :thing/else]
  (m/select
   [?value]
   [[arg1 some:thing ?o]
    [arg1 thing:else arg2]]
   db))

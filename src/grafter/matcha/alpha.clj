(ns grafter.matcha.alpha
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :as l :refer [fresh run*]]
            [clojure.core.logic.pldb :as pldb]
            [clojure.core.logic.unifier :as u]
            [clojure.string :as string]
            [clojure.walk :as walk]))

(pldb/db-rel triple subject predicate object)

(defn triple-vector->idx-triple
  "Assume triples are either 3/tuple vectors or can be destructured as
  such.  Grafter Quad objects can be destructured in this manner."
  [[s p o]]
  [triple s p o])

(defn index-triples
  "Return an indexed database of triples. Indexing a database will
  result in better performance if you want to run multiple queries
  over the same database.

  All query functions should accept either a sequence of triples or an
  indexed database."
  [db]
  (with-meta (apply pldb/db (map triple-vector->idx-triple db))
    {::index true}))

(defn ^:no-doc index-if-necessary
  "Index db if it's not already index."
  [db]
  (if (::index (meta db))
    db
    (index-triples db)))

(defn query-var?
  "Test whether supplied sym is a query variable.  Query variables are
  symbols who's name begin with a ?."
  [sym]
  (and (symbol? sym)
       (string/starts-with? (str sym) "?")))

(defn- find-vars [bgps]
  (let [vars (->> bgps
                     (mapcat identity)
                     (filter query-var?)
                     distinct
                     vec)]
    (if (seq vars)
      vars
      '[q])))

#_(defmacro bgp [& query-patterns]
  (let [syms (vec (find-vars query-patterns))
        query-patterns (map (fn [[s p o]]
                              `(triple ~s ~p ~o)) query-patterns)]

    `(fresh ~syms
      ~@query-patterns)))

(defmacro select
  ([bgps]
   `(select ~(find-vars bgps) ~bgps))
  ([project-vars bgps]
   (let [pvar? (set project-vars)
         syms (vec (->> (find-vars bgps)
                        (remove pvar?)))
         query-patterns (map (fn [[s p o]]
                               `(triple ~s ~p ~o)) bgps)]

     `(fn [db-or-idx#]
        (let [idx# (index-if-necessary db-or-idx#)]
          (pldb/with-db idx#
            (l/run* ~project-vars
              (fresh ~syms
                ~@query-patterns))))))))

(defmacro select-1
  ([bgps]
   `(select-1 ~(find-vars bgps) ~bgps))
  ([project-vars bgps]
   `(fn [db#]
      (first ((select ~project-vars ~bgps) db#)))))

(defn find-vars-in-tree [tree]
  (filterv query-var? (tree-seq coll? seq tree)))


(defn unify-solutions [projected-vars solutions]
  (map (fn [s]
         (u/unifier (vector projected-vars s)))
       solutions))

(defn replace-vars-with-vals [construct-pattern binding-maps]
  (map (fn [binding-map]
         (walk/postwalk-replace binding-map construct-pattern))
       binding-maps))

(defn ^:no-doc quote-query-vars
  "Used to help macro expansion.  We need to quote only ?query-variables
  and leave other symbols unqouted so they pickup their values from
  the environment."
  [query-vars construct-pattern]
  (let [replacements (zipmap query-vars (map (fn [qv]
                                               `(quote ~qv)) query-vars))]
    (walk/postwalk-replace replacements construct-pattern)))

(defn group-subjects [solutions]
  (if-let [subj-maps (seq (filter :grafter.rdf/subject solutions))]
    (into []
          (comp
           (map (fn [v]
                  (apply merge-with
                         (fn [a b]
                           (cond
                             (set? a)
                             (conj a b)
                             :else
                             (set [a b])))
                         v)))
           (map (fn [m]
                  (let [vs (:grafter.rdf/subject m)
                        v (if (set? vs)
                            (first vs)
                            vs)]
                    (assoc m :grafter.rdf/subject v)))))
          (vals (group-by :grafter.rdf/subject subj-maps)))
    solutions))

(defmacro construct
  [construct-pattern bgps]
  (let [pvars (set (find-vars-in-tree construct-pattern))
        syms (vec (->> (find-vars bgps)
                       (remove (set pvars))))
        query-patterns (map (fn [[s p o]]
                              `(triple ~s ~p ~o)) bgps)
        pvarvec (vec pvars)]

    `(fn [db-or-idx#]
       (let [idx# (index-if-necessary db-or-idx#)
             solutions# (pldb/with-db idx#
                          (l/run* ~pvarvec
                            (fresh ~syms
                              ~@query-patterns)))
             ;; create a sequence of {?var :value} binding maps for
             ;; each solution.
             vars->vals# (unify-solutions (quote ~pvarvec) solutions#)

             subj-maps# (replace-vars-with-vals ~(quote-query-vars pvarvec construct-pattern)
                                                vars->vals#)

             grouped# (group-subjects subj-maps#)]
         grouped#))))

(defmacro construct-1 [construct-pattern bgps]
  `(fn [db#]
     (first ((construct ~construct-pattern ~bgps) db#))))

(defmacro ask [bgps]
  `(let [f# (select ~bgps)]
    (fn [db#]
      (if (seq (f# db#))
        true
        false))))

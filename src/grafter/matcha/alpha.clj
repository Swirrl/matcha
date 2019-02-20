(ns grafter.matcha.alpha
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :as l :refer [fresh run*]]
            [clojure.core.logic.protocols :as lp]
            [clojure.core.logic.pldb :as pldb]
            [clojure.spec.alpha :as s]
            [clojure.core.logic.unifier :as u]
            [clojure.string :as string]
            [clojure.walk :as walk]
            [clojure.set :as set]))

(defmacro ^:private when-available [syms & body]
  (when (every? some? (map resolve syms))
    `(do ~@body)))

(try
  ;; avoid issue: https://github.com/Swirrl/matcha/issues/5
  (require '[grafter.rdf.protocols :as gp])
  (import '[grafter.rdf.protocols RDFLiteral LangString Quad])

  (catch java.io.FileNotFoundException _))

(when-available #{LangString RDFLiteral}
  (extend-protocol lp/IUninitialized
    LangString
    (lp/-uninitialized [coll] coll)

    RDFLiteral
    (lp/-uninitialized [coll] coll)))

(pldb/db-rel triple ^:index subject ^:index predicate ^:index object)

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

(s/def ::atomic (s/and some? (comp not coll?)))

(s/def ::triple
  (s/tuple ::atomic ::atomic ::atomic))

(s/def ::bgp ::triple)

(s/def ::bgps (s/coll-of ::bgp :kind vector?))

(defn valid-bgps? [bgps]
  (letfn [(valid-atomic? [x] (and some? (not (coll? x))))
          (valid-bgp? [bgp]
            (and (= (count bgp) 3)
                 (every? valid-atomic? bgp)))]
    (and (coll? bgps)
         (every? valid-bgp? bgps))))

(defn validate-bgps [bgps error-message error-data]
  (let [quote-qvars (partial walk/postwalk #(if (query-var? %) `(quote ~%) %))]
    `(let [bgps# ~(quote-qvars bgps)]
       (when-not (valid-bgps? bgps#)
         (throw (ex-info (str ~error-message)
                         (merge {:bgps bgps#}
                                ~(quote-qvars error-data))))))))

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
        ~(validate-bgps bgps
                        "Invalid data syntax passed to `select` query at runtime"
                        {:type ::select-validation-error
                         :project-vars project-vars})
        (let [idx# (index-if-necessary db-or-idx#)]
          (pldb/with-db idx#
            (l/run* ~project-vars
              (fresh ~syms
                ~@query-patterns))))))))

(s/fdef select
  :args (s/or :ary-1 (s/cat :bgps ::bgps)
              :ary-2 (s/cat :project-vars (s/coll-of query-var?)
                            :bgps ::bgps)))

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
         (let [vars (if (= 1 (count projected-vars))
                      (first projected-vars)
                      projected-vars)]
           (u/unifier (vector vars s))))
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
  (if-let [subj-maps (seq (filter :grafter.rdf/uri solutions))]
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
                  (let [vs (:grafter.rdf/uri m)
                        v (if (set? vs)
                            (first vs)
                            vs)]
                    (assoc m :grafter.rdf/uri v)))))
          (vals (group-by :grafter.rdf/uri subj-maps)))
    solutions))

(defmacro construct
  [construct-pattern bgps]
  (let [pvars (find-vars-in-tree construct-pattern)
        syms (vec (->> (find-vars bgps)
                       (remove (set pvars))))
        query-patterns (map (fn [[s p o]]
                              `(triple ~s ~p ~o)) bgps)
        pvarvec (vec pvars)]

    `(fn [db-or-idx#]
       ~(validate-bgps bgps
                       "Invalid data syntax passed to `construct` query at runtime"
                       {:type ::construct-validation-error
                        :construct-pattern construct-pattern})
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

(s/fdef construct
  :args (s/cat :construct-pattern any? :bgps ::bgps))

(defmacro construct-1 [construct-pattern bgps]
  `(fn [db#]
     (first ((construct ~construct-pattern ~bgps) db#))))

(defmacro ask [bgps]
  `(let [f# (select ~bgps)]
    (fn [db#]
      ~(validate-bgps bgps
                      "Invalid data syntax passed to `ask` query at runtime"
                      {:type ::ask-validation-error})
      (if (seq (f# db#))
        true
        false))))

(s/fdef ask
  :args (s/cat :bgps ::bgps))

(defn merge-dbs
  "Merges all supplied Matcha databases together into one.  Any
  individual database form can either be indexed already with
  index-triples, or a sequence of triples, in which case the triples
  will be indexed before being combined with any other databases."
  [& dbs]
  (->> dbs
       (map index-if-necessary)
       (apply (partial merge-with
                       (fn [a b]
                         (let [unindexed {::pldb/unindexed (set/union (::pldb/unindexed a)
                                                                      (::pldb/unindexed b))}

                               rem-a (dissoc a ::pldb/unindexed)
                               rem-b (dissoc b ::pldb/unindexed)]

                           (merge unindexed
                                  (merge-with (partial merge-with set/union)
                                              rem-a rem-b))))))))

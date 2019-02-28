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
      '[?_])))

(defmacro values
  "Binds a ?qvar binding to elements of a set inside the query. MUST be used
  inside a (select|construct|ask|etc) query.

  SYNTAX: (values binding bound-value)
          binding: ?qvar
          bound-value: any?

  E.G.,
  (let [subjects #{:a :b :c}]
    (select [?s ?p ?o]
      [[?s ?p ?o]
       (values ?s subjects)]))"
  [binding bound-value]
  (assert nil "`values` used not in a query block"))

(defn collection? [x]
  (or (instance? java.util.Collection x)
      (map? x)))

(s/def ::sexp
  (s/and list? (s/cat :op (s/or :ifn? ifn? :sexp ::sexp) :* (s/* any?))))

(s/def ::atomic
  (s/and some? (s/or :sexp ::sexp :non-coll (comp not collection?))))

(s/def ::triple
  (s/tuple ::atomic ::atomic ::atomic))

(s/def ::bgp ::triple)

(defn valid-bgps? [bgps]
  (letfn [(valid-atomic? [x] (and some? (not (collection? x))))
          (valid-bgp? [bgp]
            (and (= (count bgp) 3)
                 (every? valid-atomic? bgp)))]
    (and (sequential? bgps)
         (every? valid-bgp? bgps))))

(defn resolve-sym [x]
  (let [v (resolve x)]
    (symbol (str (.name (.ns v))) (str (.sym v)))))

(defmacro clause [name & argspec]
  `(letfn [(clause?# [x#]
             (= '~(resolve-sym name) (resolve-sym x#)))]
     (s/and list? (s/cat :op clause?# ~@argspec))))

(s/def ::values
  (clause values :binding query-var? :bound (comp not query-var?)))

(s/def ::clause ::values)

(s/def ::pattern-row (s/or :bgp ::bgp :clause ::clause))

(s/def ::bgps (s/coll-of ::pattern-row :kind vector?))

(defn- parse-values [{:keys [binding bound]}]
  `(l/membero ~binding (vec ~bound)))

(declare parse-patterns)

(defn- parse-clause [[type row]]
  (case type
    :values (parse-values row)))

(defn- parse-pattern-row [[type row]]
  (case type
    :bgp `(triple ~@(s/unform ::bgp row))
    :clause (parse-clause row)))

(defn- parse-patterns [conformed]
  (mapv parse-pattern-row conformed))

(defn valid-values? [values-syms]
  (let [invalid (into {} (remove (comp set? second) values-syms))]
    (when (seq invalid)
      (throw
       (ex-info
        (str "Invalid Argument: `values` bound arguments must be sets\n"
             (format "%s were not" (pr-str invalid)))
        {:type ::invalid-values
         :args invalid})))))

(defn extract-validation [bound-vars conformed-bgps]
  (letfn [(pair-quote [x]
            {(list 'quote x) x})
          (extract-clause [[type row]]
            (case type
              :values {:values (pair-quote (:bound row))}))
          (extract-row [[type row]]
            (case type
              :clause (extract-clause row)))]
    (reduce (partial merge-with merge)
            (map extract-row conformed-bgps))))

(defn- solve* [qtype bound-vars pvars bgps db-or-idx]
  (let [conformed (s/conform ::bgps bgps)
        validation (extract-validation bound-vars conformed)]
    `(do
       ~@(some->> validation :values (list `valid-values?) list)
       (pldb/with-db (index-if-necessary ~db-or-idx)
         (l/run* ~(vec pvars)
           (fresh ~(->> bgps find-vars (remove (set pvars)) vec)
             ~@(parse-patterns conformed)))))))

(defmacro select
  "Query a `db-or-idx` with `bgps` patterns.

  If called with 1 argument, `select` finds all `?vars` in the `bgps` patterns
  and returns a function of 1 argument: the `db-or-idx`, which returns a
  sequence of results.

  If called with 2 arguments, returns a function of 1 argument: the `db-or-idx`,
  which returns a sequence of results with the `?vars` in `project-vars`
  projected.

  If called with 3 arguments, queries the `db-or-idx` directly, returning a
  sequence of results with the `?vars` in `project-vars` projected."
  ([bgps]
   `(select ~(find-vars bgps) ~bgps))
  ([project-vars bgps]
   `(fn [db-or-idx#]
      (select ~project-vars ~bgps db-or-idx#)))
  ([project-vars bgps db-or-idx]
   (solve* 'select {:project-vars project-vars} project-vars bgps db-or-idx)))

(s/fdef select
  :args (s/or
         :ary-1 (s/cat :bgps ::bgps)
         :ary-2 (s/cat :project-vars (s/coll-of query-var?) :bgps ::bgps)
         :ary-3 (s/cat :project-vars (s/coll-of query-var?) :bgps ::bgps :db any?))
  :ret (s/or
        :ary-1-and-2 (s/fspec
                      :args (s/cat :db-or-idx any?)
                      :ret (s/coll-of any?))
        :ary-3 (s/coll-of any?)))

(defmacro select-1
  "Query a `db-or-idx` with `bgps` patterns.

  If called with 1 argument, `select` finds all `?vars` in the `bgps` patterns
  and returns a function of 1 argument: the `db-or-idx`, which returns the first
  result.

  If called with 2 arguments, returns a function of 1 argument: the `db-or-idx`,
  which returns the first result with `?vars` in `project-vars`projected.

  If called with 3 arguments, queries the `db-or-idx` directly, returning the
  first result with `?vars` in `project-vars` projected."
  ([bgps]
   `(select-1 ~(find-vars bgps) ~bgps))
  ([project-vars bgps]
   `(comp first (select ~project-vars ~bgps)))
  ([project-vars bgps db]
   `(first (select ~project-vars ~bgps ~db))))

(s/fdef select-1
  :args (s/or
         :ary-1 (s/cat :bgps ::bgps)
         :ary-2 (s/cat :project-vars (s/coll-of query-var?) :bgps ::bgps)
         :ary-3 (s/cat :project-vars (s/coll-of query-var?) :bgps ::bgps :db any?))
  :ret (s/or
        :ary-1-and-2 (s/fspec
                      :args (s/cat :db-or-idx any?)
                      :ret any?)
        :ary-3 any?))

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
  "Query a `db-or-idx` with `bgps` patterns, and return data in the form of the
  `construct-pattern`.

  If called with 2 arguments, returns a function of 1 argument: the `db-or-idx`,
  which returns a sequence of results in the form of the `construct-pattern`.

  If called with 3 arguments, queries the `db-or-idx` directly, returning a
  sequence of results in the form of the `construct-pattern`."
  ([construct-pattern bgps]
   `(fn [db-or-idx#]
      (construct ~construct-pattern ~bgps db-or-idx#)))
  ([construct-pattern bgps db-or-idx]
   (let [pvars (find-vars-in-tree construct-pattern)
         pvarvec (vec pvars)
         err-data {:construct-pattern construct-pattern}]
     `(->> ~(solve* 'construct err-data pvars bgps db-or-idx)
           ;; create a sequence of {?var :value} binding maps for
           ;; each solution.
           (unify-solutions (quote ~pvarvec))
           (replace-vars-with-vals ~(quote-query-vars pvarvec construct-pattern))
           (group-subjects)))))

(s/def ::construct-pattern any?)

(s/fdef construct
  :args (s/or
         :ary-2 (s/cat
                 :construct-pattern any?
                 :bgps ::bgps)
         :ary-3 (s/cat
                 :construct-pattern any?
                 :bgps ::bgps
                 :db-or-idx any?))
  :ret (s/or
        :ary-2 (s/fspec
                :args (s/cat :db-or-idx any?)
                :ret (s/coll-of ::construct-pattern))
        :ary-3 (s/coll-of ::construct-pattern)))

(defmacro construct-1
  "Query a `db-or-idx` with `bgps` patterns, and return data in the form of the
  `construct-pattern`.

  If called with 2 arguments, returns a function of 1 argument: the `db-or-idx`,
  which returns the first result in the form of the `construct-pattern`.

  If called with 3 arguments, queries the `db-or-idx` directly, returning the
  first result in the form of the `construct-pattern`."
  ([construct-pattern bgps]
   `(comp first (construct ~construct-pattern ~bgps)))
  ([construct-pattern bgps db]
   `(first (construct ~construct-pattern ~bgps ~db))))

(s/fdef construct-1
  :args (s/or
         :ary-2 (s/cat
                 :construct-pattern any?
                 :bgps ::bgps)
         :ary-3 (s/cat
                 :construct-pattern any?
                 :bgps ::bgps
                 :db-or-idx any?))
  :ret (s/or
        :ary-2 (s/fspec
                :args (s/cat :db-or-idx any?)
                :ret ::construct-pattern)
        :ary-3 ::construct-pattern))

(defmacro ask
  "Predicate: are there results in a `db-or-idx` matching `bgps` patterns?

  If called with 1 argument, returns a function of 1 argument: the `db-or-idx`.

  If called with 2 arguments, queries the `db-or-idx` directly."
  ([bgps]
   `(fn [db#] (ask ~bgps db#)))
  ([bgps db]
   `(boolean (seq ~(solve* 'ask {} '[?_] bgps db)))))

(s/fdef ask
  :args (s/or :ary-1 (s/cat :bgps ::bgps)
              :ary-2 (s/cat :bgps ::bgps :db any?)))

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

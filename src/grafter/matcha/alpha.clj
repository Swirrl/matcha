(ns grafter.matcha.alpha
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :as l :refer [fresh]]
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

(defn require-grafter-protocols [require-form]
  (try
    (require require-form)
    :required
    (catch java.io.FileNotFoundException _
      :fne)
    (catch java.lang.ClassNotFoundException _
      :cnfe)
    (catch clojure.lang.Compiler$CompilerException _
      ;; this ones a little aggressive...
      :cce
      )))

(require-grafter-protocols '[grafter.rdf.protocols])
(require-grafter-protocols '[grafter-2.rdf.protocols])

(when-available #{grafter.rdf.protocols.LangString grafter.rdf.protocols.RDFLiteral}
  (extend-protocol lp/IUninitialized
    grafter.rdf.protocols.LangString
    (lp/-uninitialized [coll] coll)

    grafter.rdf.protocols.RDFLiteral
    (lp/-uninitialized [coll] coll)))

(when-available #{grafter_2.rdf.protocols.LangString grafter_2.rdf.protocols.RDFLiteral  grafter_2.rdf.protocols.OffsetDate}
  (extend-protocol lp/IUninitialized
    grafter_2.rdf.protocols.LangString
    (lp/-uninitialized [coll] coll)

    grafter_2.rdf.protocols.RDFLiteral
    (lp/-uninitialized [coll] coll)
    grafter_2.rdf.protocols.OffsetDate
    (lp/-uninitialized [coll] coll)))

(declare triple) ;; avoid clj-kondo linter warnings. The db-rel macro
                 ;; below really defs the triple var.
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
  (let [vars (->> bgps flatten (filter query-var?) distinct vec)]
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
  [_binding _bound-value]
  (assert nil "`values` used not in a query block"))

(defmacro optional
  "Makes a graph pattern optional. I.E., the pattern inside (optional [...]) is
  optional, patterns outside are required. Can be arbitrarily nested. MUST be
  used inside a (select|construct|ask|etc) query.

  SYNTAX: (optional bgps)
          bgps: ::bgps

  E.G.,
  (select [?o ?eman]
    [[?person foaf:knows somebody]
     (optional [[?o rdfs:label ?name]
                (optional [[?name :name/backwards ?eman]
                           (values ?name names)])])]
    optional-friends)"
  {:style/indent :defn ::clause true}
  [_bgps]
  (assert nil "`optional` used not in a query block"))

(defn collection? [x]
  (or (instance? java.util.Collection x)
      (map? x)))

(s/def ::sexp
  (s/and list? (s/cat :op (s/or :ifn? ifn? :sexp ::sexp) :* (s/* any?))))

(defn- valid-symbol-atomic? [x]
  (and (symbol? x)
       (if-let [v (resolve x)]
         (and (bound? v)
              (let [value (deref v)]
                (and (some? value)
                     (not (collection? value)))))
         (simple-symbol? x))))

(s/def ::atomic
  (s/nilable (s/or :symbol valid-symbol-atomic?
                   :literal (s/and (complement symbol?)
                                   (s/or :sexp ::sexp
                                         :non-coll (complement collection?))))))

(s/def ::triple
  (s/tuple ::atomic ::atomic ::atomic))

(s/def ::bgp ::triple)

(defn resolve-sym [x]
  (let [v (resolve x)]
    (symbol (str (.name (.ns v))) (str (.sym v)))))

(defmacro clause [name & argspec]
  `(letfn [(clause?# [x#]
             (= '~(resolve-sym name) (resolve-sym x#)))]
     (s/and list? (s/cat :op clause?# ~@argspec))))

(s/def ::values
  (clause values :binding query-var? :bound (comp not query-var?)))

(s/def ::optional
  (clause optional :bgps ::bgps))

(s/def ::clause
  (s/or :values ::values :optional ::optional))

(s/def ::pattern-row (s/or :bgp ::bgp :clause ::clause))

(s/def ::bgps (s/coll-of ::pattern-row :kind vector?))

(defn- parse-values [{:keys [binding bound]}]
  `(l/membero ~binding (vec ~bound)))

(declare parse-patterns)

(defn- parse-optional [{:keys [bgps]}]
  (parse-patterns bgps))

(defn- parse-clause [[type row]]
  (case type
    :values (parse-values row)
    :optional (parse-optional row)))

(defn- parse-pattern-row [[type row]]
  (case type
    :bgp `(triple ~@(s/unform ::bgp row))
    :clause (parse-clause row)))

(defn- parse-patterns [conformed]
  (let [optional? (fn [[k v]] (and (= k :clause) (= :optional (first v))))
        optionals (filter optional? conformed)
        requireds (remove optional? conformed)]
    (vec
     (concat
      (mapv parse-pattern-row requireds)
      (when (seq optionals)
        [`(l/conde
           ~@(map parse-pattern-row optionals)
           ~@(when (seq requireds) [[`l/succeed]]))])))))

(defn valid-bgps? [bgps-syms]
  (letfn [(valid-atomic? [[_ x]] (not (collection? x)))]
    (let [invalid (into {} (remove valid-atomic? bgps-syms))]
      (when (seq invalid)
        (throw
         (ex-info (str "Invalid Argument: `bgp` elements must be atomic values\n"
                       (format "%s were not" (pr-str invalid)))
                  {:type ::invalid-bgp
                   :args invalid}))))))

(defn- flat-coll? [c]
  (or (sequential? c)
      (set? c)
      (nil? c)))

(defn valid-values? [values-syms]
  (let [invalid (into {} (remove (comp flat-coll? second) values-syms))]
    (when (seq invalid)
      (throw
       (ex-info
        (str "Invalid Argument: `values` bound arguments must be sequential?, set? or nil?\n"
             (format "%s were not" (pr-str invalid)))
        {:type ::invalid-values
         :args invalid})))))

(defn extract-validation [bound-vars conformed-bgps]
  (letfn [(pair-quote [x]
            {(list 'quote x) x})
          (binding? [x]
            (and (simple-symbol? x)
                 (not (string/starts-with? (str x) "?"))
                 (get bound-vars x)))
          (extract-clause [[type row]]
            (case type
              :values {:values (pair-quote (:bound row))}
              :optional (extract-validation bound-vars (:bgps row))))
          (extract-row [[type row]]
            (case type
              :bgp {:bgp (->> row
                              (map second)
                              (filter binding?)
                              (map pair-quote)
                              (apply merge))}
              :clause (extract-clause row)))]
    (reduce (partial merge-with merge)
            (map extract-row conformed-bgps))))

(defn- solve* [_qtype bound-vars pvars bgps db-or-idx]
  (let [conformed (s/conform ::bgps bgps)
        validation (extract-validation bound-vars conformed)]
    `(do
       ~@(some->> validation :bgp (list `valid-bgps?) list)
       ~@(some->> validation :values (list `valid-values?) list)
       (seq (pldb/with-db (index-if-necessary ~db-or-idx)
              (l/run* ~(vec pvars)
                (fresh ~(->> bgps find-vars (remove (set pvars)) vec)
                  ~@(parse-patterns conformed))))))))

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

(defn split-optionals
  "Returns a map of {:bgps ... :optionals [...]}

  Where :bgps value is the input form with `(optional ...)` forms
  removed, and :optionals is s vector of optional forms."
  [bgps]
  (let [optionals (java.util.ArrayList.)
        optional? (fn [v]
                    (and (seq? v)
                         (symbol? (first v))
                         (= #'optional (resolve (first v)))))
        pruned (walk/postwalk
                (fn [form]
                  (if-not (vector? form)
                    form
                    (reduce (fn [result item]
                              (if (optional? item)
                                (do
                                  (.add optionals item)
                                  result)
                                (conj result item)))
                            []
                            form)))
                bgps)]
    {:bgps pruned
     :optionals (into [] optionals)}))

(defn- decompose-optionals
  "Returns a map of {:bgps ... :optionals ... :opt-vars ...}.

  The :opt-vars is a set of symbols that occur only in `pvars` and
  inside `optional` forms. See also: [[split-optionals]]"
  [bgps pvars]
  (let [{bgps' :bgps optional-forms :optionals
         :as m} (split-optionals bgps)
        all-opts (set (mapcat find-vars optional-forms))
        pvars (set pvars)
        mandatory (set/intersection (set (find-vars bgps')) pvars)
        opts (set/difference all-opts mandatory)]
    (assoc m :opt-vars (set/difference (set/intersection pvars opts) mandatory))))

(defn- unbound? [v] (and (symbol? v) (.startsWith ^String (name v) "_")))

(defn -make-select-solution
  "Creates a solution vector according where values in reqs and opts are
  in the right position (as specified in select's 'projected values').
  
  - reqs - vector of required values
  - opts - vector of optional values
  - required-indices - vector of ints
  - optional-indices - vector of ints"
  [reqs opts required-indices optional-indices]
  {:pre [(vector? reqs)
         (vector? opts)
         (vector? required-indices)
         (vector? optional-indices)
         (= (count reqs) (count required-indices))
         (= (count opts) (count optional-indices))]}
  (let [v (transient (vec (repeat (+ (count reqs) (count opts)) :?)))
        v (reduce (fn [v i] 
                    (assoc! v (nth required-indices i) (nth reqs i)))
                  v
                  (range (count reqs)))
        v (reduce (fn [v i] 
                    (assoc! v (nth optional-indices i) (nth opts i)))
                  v
                  (range (count opts)))]
    (persistent! v)))

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
  {:style/indent :defn}
  ([bgps]
   `(select ~(find-vars bgps) ~bgps))
  ([project-vars bgps]
   `(fn [db-or-idx#]
      (select ~project-vars ~bgps db-or-idx#)))
  ([project-vars bgps db-or-idx]
   (let [var->index (->> (map-indexed (fn [i v] [i v]) project-vars)
                         (reduce (fn [m [i v]] (assoc! m v i)) (transient {}))
                         (persistent!))
         {:keys [opt-vars]} (decompose-optionals bgps (set project-vars))
         [requireds optionals] [(filterv (complement opt-vars) project-vars)
                                (filterv opt-vars project-vars)]
         optional-indices (mapv var->index optionals)
         required-indices (mapv var->index requireds)
         solutions-sym (gensym "solutions_")
         optionals-syms (mapv #(gensym (str "optional_" % "_")) optionals)
         optionals-v-syms (mapv #(gensym (str "optional_v_" % "_")) optionals)]
     `(let [~solutions-sym ~(solve* 'select &env project-vars bgps db-or-idx)
            ~solutions-sym (if (= 1 ~(count project-vars))
                             (map (fn [s#] [s#]) ~solutions-sym)
                             ~solutions-sym)
            ;; unpack values from solutions if asked for only 1 value
            make-solution# (if (= 1 ~(count project-vars))
                             (fn [sol# & _args#] (first sol#))
                             -make-select-solution)
            unbounds# (atom 0)
            unbound!# (fn [] (let [v# @unbounds#]
                               (swap! unbounds# inc)
                               v#))
            optional-vals-fn# (fn [solutions# indices#]
                                ;; get all values for each ?var (think: column)
                                (for [index# ~optional-indices]
                                  (let [s# (->> (mapv #(get % index#) solutions#)
                                                (remove ~unbound?))]
                                    ;; we can't remove everything,
                                    ;; at least 1 unbound needs to be returned
                                    (if (seq s#)
                                      s#
                                      [(symbol (str "_" (unbound!#)))]))))
            group-fn# (fn [sol#]        ;returns vector or required values only
                        (mapv (fn [index#] (get sol# index#)) 
                              ~required-indices))
            grouped# (group-by group-fn# ~solutions-sym)
            ;; for each group, extract "columns" of (optional) values
            grouped-by-req#
            (reduce-kv (fn [m# k# v#]
                         (assoc m# k#
                                (optional-vals-fn# v# ~optional-indices)))
                       {}
                       grouped#)
            ;; grouped-by-req: {REQ1 ((v1 v2 v3) (u1 u2 ...)) REQ2 (...)}
            ~optionals-v-syms (optional-vals-fn# ~solutions-sym ~optional-indices)]
        (seq (for [[req-v# ~optionals-v-syms] grouped-by-req#
                   ~@(vec (interleave optionals-syms optionals-v-syms))]
               (make-solution# req-v# ~optionals-syms
                               ~required-indices ~optional-indices)))))))

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

(defn unify-solutions
  "Returns a seq of maps."
  [projected-vars solutions]
  ;; projected-vars :: [?id ?x ...]
  ;; solutions :: ([v1 v2 _0 ...])  
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

(defn handle-optionals
  "Takes a seq of maps (solutions) and removes top level keys
  mapped to an unbound value from each map."
  [construct-pattern optionals solutions]
  ;; This way, when merging solution by subject, we don't have to deal with
  ;; sets of values.
  (let [;; these are *potentially* optional keys
        ;; optionals can have unbound solutions,
        ;; while non-optionals will have actual values
        opt-keys (reduce-kv (fn [s k v]
                              (if (contains? optionals v)
                                (conj s k)
                                s))
                            #{}
                            construct-pattern)]
    (for [solution solutions]
      (reduce-kv (fn [m k v]
                   (if (and (contains? opt-keys k)
                            (unbound? v))
                     (dissoc m k)
                     m))
                 solution
                 solution))))

(defn ^:no-doc quote-query-vars
  "Used to help macro expansion.  We need to quote only ?query-variables
  and leave other symbols unqouted so they pickup their values from
  the environment."
  [query-vars construct-pattern]
  (let [replacements (zipmap query-vars (map (fn [qv]
                                               `(quote ~qv)) query-vars))]
    (walk/postwalk-replace replacements construct-pattern)))

(def ^:private group-predicates-xf
  "Transducer taking in a seq of seqs where the
  inner seqs are solutions for the same subject.

  E.g.:
  (({:id 1 ...} {:id 1 ...} ...) ({:id 2 ...} {:id 2 ..} ...) ...)"
  (map (fn [v]
         (apply merge-with
                (fn [a b]
                  (cond
                    (set? a)
                    (conj a b)
                    
                    :else
                    (set [a b])))
                v))))

(def ^:private make-group-predicates-xf
  "Returns a transducer taking in a seq of seqs where the inner seqs are
  solutions for the same subject.

  E.g.:
  (({:id 1 ...} {:id 1 ...} ...) ({:id 2 ...} {:id 2 ..} ...) ...)"
  (fn [subject-k]
    
    (let [merger-fn (fn [a b]
                      (cond
                        (set? a)  (conj a b)
                        :else (set [a b])))
          remove-subject-k (fn [sol] (dissoc sol subject-k))
          part-fn (fn part-fn [part]
                    (if-not (seq part)
                      part
                      (let [subj (-> part first (get subject-k))
                            merged (->> part
                                        (map remove-subject-k)
                                        (apply merge-with merger-fn))]
                        (cond-> merged
                          subj (assoc subject-k subj)))))]
      (map part-fn))))

(def ^:private unsetify-grafter-uri
  (map (fn [m]
         (let [vs (:grafter.rdf/uri m)
               v (if (set? vs)
                   (first vs)
                   vs)]
           (assoc m :grafter.rdf/uri v)))))

(defn group-subjects-for-construct [construct-pattern solutions]
  (if (and (map? construct-pattern) (:grafter.rdf/uri construct-pattern))
    (into []
          (comp
           group-predicates-xf
           unsetify-grafter-uri)
          (vals (group-by :grafter.rdf/uri solutions)))
    solutions))

(def ^:private clean-up-subject-map
  "Removes any keys with unbound vars as values and flattens any sets
  that have just one value into scalars."
  (map (fn cleanup [e]
         (reduce-kv (fn [m k v]
                      (cond
                        (symbol? v)
                        (dissoc m k)

                        (and (set? v) (= 1 (count v)))
                        (assoc m k (first v))

                        :else m))
                    e
                    e))))

(defn group-subjects-for-build
  "- `solutions` - a seq of maps"
  [subject-k solutions]
  (into []
        (comp
         (make-group-predicates-xf subject-k)
         clean-up-subject-map)
        (vals (group-by subject-k solutions))))

(defmacro build
  "Query a `db-or-idx` with `bgps` patterns, and return data grouped by
  subject and predicates into resource object maps.

  `subject` can be a concrete value, a value bound in lexical scope, a
  `?query-var` symbol used in the `bgps` or a 2-tuple key value pair
  of `[:keyword ?query-var]` or [:keyword :concrete-value]. If the
  vector form is used case `:keyword` will be the key used to identify
  the subject of the maps in the response. If only `?query-var` or a
  concrete value and no `:keyword` is specified then the default
  keyword of `:grafter.rdf/uri` is used.

  NOTE: unlike `construct`, `build` will eliminate any unbound
  variables from the response maps that may arrise from using an
  optional.

  If called with 3 arguments, returns a function of 1 argument: the `db-or-idx`,
  which returns a sequence of results in the form of the `construct-pattern`.

  If called with 4 arguments, queries the `db-or-idx` directly, returning a
  sequence of results in the form of the `construct-pattern`."
  ([subject construct-pattern bgps]
   `(fn [db-or-idx#]
      (build ~subject
             ~construct-pattern ~bgps db-or-idx#)))
  ([subject construct-pattern bgps db-or-idx]
   (let [[subject-k subject-var] (if (vector? subject)
                                   subject
                                   [:grafter.rdf/uri subject])
         pvars (distinct
                (if (query-var? subject-var)
                  (cons subject-var (find-vars-in-tree construct-pattern))
                  (find-vars-in-tree construct-pattern)))
         pvarvec (vec pvars)
         {bgps-without-optionals :bgps
          :keys [opt-vars optionals]} (decompose-optionals bgps pvars)
         bgps (into bgps-without-optionals optionals)
         quoted-pvars (quote-query-vars pvarvec (merge {subject-k subject-var}
                                                       construct-pattern))]
     ;; pvars :: (?id ?x ?y ) etc
     `(->> ~(solve* 'build &env pvars bgps db-or-idx)
           ;; create a sequence of {?var :value} binding maps for
           ;; each solution.
           (unify-solutions (quote ~pvarvec))
           (replace-vars-with-vals ~quoted-pvars)
           (handle-optionals ~quoted-pvars ~(quote-query-vars opt-vars opt-vars))
           (group-subjects-for-build ~subject-k)
           seq))))

(defmacro build-1
  "Like `build` but returns only the first resource object.

  NOTE: it is not lazy, so to make this efficient you should be
  selective in your `bgps`."
  ([subject-kv construct-pattern bgps]
   `(fn [db-or-idx#]
      (build ~subject-kv
             ~construct-pattern ~bgps db-or-idx#)))
  ([subject construct-pattern bgps db-or-idx]
   `(first (build ~subject ~construct-pattern ~bgps ~db-or-idx))))

(defmacro construct
  "NOTE: If you want to construct maps, you will likely be better
  using `build` instead.

  Query a `db-or-idx` with `bgps` patterns, and return data in the
  form of the `construct-pattern`.

  If called with 2 arguments, returns a function of 1 argument: the `db-or-idx`,
  which returns a sequence of results in the form of the `construct-pattern`.

  If called with 3 arguments, queries the `db-or-idx` directly, returning a
  sequence of results in the form of the `construct-pattern`."
  ([construct-pattern bgps]
   `(fn [db-or-idx#]
      (construct ~construct-pattern ~bgps db-or-idx#)))
  ([construct-pattern bgps db-or-idx]
   (let [pvars (find-vars-in-tree construct-pattern)
         pvarvec (vec pvars)]
     `(->> ~(solve* 'construct &env pvars bgps db-or-idx)
           ;; create a sequence of {?var :value} binding maps for
           ;; each solution.
           (unify-solutions (quote ~pvarvec))
           (replace-vars-with-vals ~(quote-query-vars pvarvec construct-pattern))
           (group-subjects-for-construct (quote ~construct-pattern))
           seq))))

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
   `(boolean (seq ~(solve* 'ask &env '[?_] bgps db)))))

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

(ns grafter.matcha.db
  (:require [clojure.core.logic :as l]
            [clojure.set :as set]
            [net.cgrand.xforms :as xf]))

;; ----------------------------------------

(def empty-db {})

(defmacro with-dbs [dbs & body]
  `(binding [l/*logic-dbs* (concat l/*logic-dbs* ~dbs)]
          ~@body))

(defmacro with-db [db & body]
  `(binding [l/*logic-dbs* (conj l/*logic-dbs* ~db)]
          ~@body))

;; ----------------------------------------

(defn contains-lvar? [x]
  (some l/lvar? (tree-seq coll? seq x)))

(defn ground? [resolved-term]
  (not (contains-lvar? resolved-term)))

(def positionalise-indexes {:s 0
                            :p 1
                            :o 2
                            [:p :s] #{0 1}
                            [:s :p] #{0 1} ; canonicalise seq forms
                            [:s :o] #{0 2}
                            [:o :s] #{0 2}
                            [:p :o] #{1 2}
                            [:o :p] #{1 2}
                            #{:s :p} #{0 1}
                            #{:s :o} #{0 2}
                            #{:p :o} #{1 2}})

(def keywordify-indexes (zipmap (vals positionalise-indexes)
                                (keys positionalise-indexes)))

;(def index-priorities (map positionalise-indexes [[:s :o] [:s :p] [:p :o] :s :o :p]))

(defn find-smallest-index [db paths]
  #_(let [chosen-path (first (sort-by (fn [path]
                                        (count (get-in db path))) paths))]
      (get-in db chosen-path))
  (apply set/intersection
         (map (fn [path]
                (get-in db path)) paths)))

(def call-count (atom 0))

(defn potential-indexes [db [s p o]]
  (let [available-idx? (::available-indexes db)
        gs (ground? s)
        gp (ground? p)
        go (ground? o)]

    (cond
      (and gs gp go) (::unindexed db)
      (and gs gp (available-idx? #{0 1}))
      (let [paths [[#{0 1} [s p]]
                   [0 s]
                   [1 p]]]
        (find-smallest-index db paths))

      (and gs go (available-idx? #{0 2}))
      (let [paths [[#{0 2} [s o]]
                   [0 s]
                   [1 s]]]
        (find-smallest-index db paths))

      (and gp go (available-idx? #{1 2}))
      (let [paths [[#{1 2} [p o]]
                   [1 p]
                   [2 o]]]
        (find-smallest-index db paths))

      (and gs gp
           (available-idx? 0)
           (available-idx? 1))
      (let [paths [[0 s] [1 p]]]
        (find-smallest-index db paths))

      (and gs go
           (available-idx? 0)
           (available-idx? 2))
      (let [paths [[0 s] [2 o]]]
        (find-smallest-index db paths))

      (and gp go
           (available-idx? 1)
           (available-idx? 2))
      (let [paths [[1 p] [2 o]]]
        (find-smallest-index db paths))

      (and gs (available-idx? 0)) (get-in db [0 s])
      (and gp (available-idx? 1)) (get-in db [1 p])
      (and go (available-idx? 2)) (get-in db [2 o])
      :else (::unindexed db))))

(def triple
  (fn [& query]
    (fn [subs]
      (let [dbs (-> subs clojure.core/meta :db)
            db (first dbs) ; assume there is only one db - does not
                           ; generalise to core.logic but is ok for
                           ; current matcha usage.

            resolved-term (l/walk* subs query)
            results (potential-indexes db resolved-term)]

        (l/to-stream
         (sequence
          (map (fn [potential]
                 ((l/== query potential) subs)))
          results))))))

(defn build-index [indexer facts]
  (apply merge-with set/union
         (->> facts
              (map indexer)
              )))

(defn add-to-set [s v]
  (if s
    (conj s v)
    #{v}))

(defn index-triples
  ([facts]
   (index-triples #{:s :p :o} facts))
  ([indexes facts]
   (let [indexes (map positionalise-indexes indexes)
         apply-indexes (xf/for [[s p o :as triple] %
                                i indexes]
                         (case i
                           0 [0 (nth triple 0) triple]
                           1 [1 (nth triple 1) triple]
                           2 [2 (nth triple 2) triple]
                           #{0 1} [#{0 1} [(nth triple 0) (nth triple 1)] triple]
                           #{0 2} [#{0 2} [(nth triple 0) (nth triple 2)] triple]
                           #{1 2} [#{1 2} [(nth triple 1) (nth triple 2)] triple]))

         db (transduce (comp (map (fn [[s p o]]
                                    [s p o]))
                             apply-indexes)
                       (fn
                         ([v] v)
                         ([acc idx-spec]
                          (let [path (subvec idx-spec 0 2)
                                triple (last idx-spec)]
                            (-> acc
                                (update-in path add-to-set triple)
                                (update ::unindexed add-to-set triple)))))
                       {}
                       facts)
         available-indexes (set (keys (dissoc db ::unindexed)))]

     (with-meta (assoc db ::available-indexes available-indexes)
       {:grafter.matcha.alpha/index true}))))

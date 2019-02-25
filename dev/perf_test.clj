(ns perf-test
  (:require [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as gen]
            [grafter.matcha.alpha :as m]))

(def atomic-gen
  (s/gen
   (s/or :keyword keyword?
         :symbol  symbol?
         :uri     uri?
         :boolean boolean?
         :number  number?
         :inst    inst?
         :string  string?)))

(s/def ::atomic (s/with-gen (s/and any? some? (comp not coll?))
                  (constantly atomic-gen)))
(s/def ::bgp (s/tuple ::atomic ::atomic ::atomic))
(s/def ::bgps (s/coll-of ::bgp :kind vector? :max-count 10))

(defmacro timer [expr]
  `(let [start# (. System (nanoTime))
         ret# ~expr
         ms# (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
     {:ret ret# :ms ms#}))

(defn runtime-validation-performance [n]
  (let [generated-bgps (mapv (fn [_] (gen/generate (s/gen ::bgps))) (range n))
        _ (printf "Generated %d bgpss\n" n)
        spec (timer (mapv (partial s/valid? ::m/bgps) generated-bgps))
        _ (printf "Spec validation of %d bgpss:   %s ms\n" n (:ms spec))
        manual (timer (mapv valid-bgps? generated-bgps))]
    (printf "Manual validation of %d bgpss: %s ms\n" n (:ms manual))))

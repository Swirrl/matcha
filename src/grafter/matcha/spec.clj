(ns grafter.matcha.spec
  (:require [grafter.rdf.protocols :as pr]
            [clojure.spec.alpha :as s]
            [clojure.spec.gen.alpha :as g]))

(s/def ::uri uri?)

(s/def ::bnode keyword?)

(s/def ::subject (s/or :uri ::uri
                       :bnode ::bnode))

(s/def ::predicate ::uri)

(defmulti object-type class)

(defmethod object-type Integer [_] integer?)

(defmethod object-type Number [_] number?)

;; xsd:string
(defmethod object-type String [_] string?)

(defmethod object-type java.net.URI [_] uri?)

(defmethod object-type clojure.lang.Keyword [_] keyword?)

(defmethod object-type java.lang.Boolean [_] keyword?)

(defmethod object-type java.util.Date [_] inst?)

(defmethod object-type java.sql.Time [_] inst?)

;; TODO fix lang string support

(s/def ::string string?)

;; https://tools.ietf.org/html/bcp47
(def gen-langs (fn [] (s/gen #{:en :jp :fr :en-US :en-Latn-GB-boont-r-extended-sequence-x-private :zh-Hant})))

(s/def ::lang (s/with-gen keyword? gen-langs))

(s/def ::lang-map (s/keys :req-un [::lang ::string]))

(s/def ::lang-string (s/with-gen (s/and ::lang-map
                                        #(instance? grafter.rdf.protocols.LangString %))
                       #(g/fmap
                         pr/map->LangString
                         (s/gen ::lang-map))))

(defmethod object-type grafter.rdf.protocols.LangString [_]
  ::lang-string)

(s/def ::object (s/multi-spec
                 object-type (fn [a b] a)))

(s/def ::triple (s/tuple ::subject ::predicate ::object))

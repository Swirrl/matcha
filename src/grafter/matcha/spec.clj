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

(defmethod object-type String [_] string?)

(defmethod object-type java.net.URI [_] uri?)

(defmethod object-type clojure.lang.Keyword [_] keyword?)

(defmethod object-type java.lang.Boolean [_] keyword?)

(defmethod object-type java.util.Date [_] inst?)

(defmethod object-type java.sql.Time [_] inst?)

(s/def ::lang-string (s/with-gen string?
                       #(g/fmap
                         (fn [[s l]]
                           (pr/->LangString s l)))
                       (g/tuple (g/string) (g/keyword))))

(defmethod object-type grafter.rdf.protocols.LangString [_]
  ::lang-string)

(s/def ::object (s/multi-spec
                 object-type (fn [a b] a)))

(s/def ::triple (s/tuple ::subject ::predicate ::object))

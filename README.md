# Matcha

A Clojure DSL to query in memory RDF models with a SPARQL like
language.

## Usage

First lets define an in memory database of triples.

Triples can be vectors of clojure values or
grafter.rdf.protocols.Statement records.

```clojure
(def friends [[:rick :rdfs/label "Rick"]
              [:martin :rdfs/label "Martin"]
              [:katie :rdfs/label "Katie"]
              [:julie :rdfs/label "Julie"]

              [:rick :foaf/knows :martin]
              [:rick :foaf/knows :katie]
              [:katie :foaf/knows :julie]])
```

Now we can build our query function:

```clojure
(def rick-knows (select [?name]
                  [[:rick :foaf/knows ?p2]
                   [?p2 :rdfs/label ?name]]))
```

And we can call it like so:

```clojure
(rick-knows friends) ;; ["Martin" "Katie"]
```

## License

Copyright © Swirrl IT Ltd 2018

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

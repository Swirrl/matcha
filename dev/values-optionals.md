

``` clojure
(select [?s ?p ?o ?olabel]
  (optional
     [[?s ?p ?o]]
     [[?o :label ?olabel]])
     [[?o :label ?olabel]])
     [[?o :label ?olabel]])
```

``` sparql
SELECT ?s ?p ?o ?olabel WHERE {
  { ?s ?p ?o }
  OPTIONAL
  { ?o rdfs:label ?olabel .
  }
}
```

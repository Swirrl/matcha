# Performance Tips

Matcha performance can be both quite good and terrible, depending on
how you write your queries and index your data.

## Clause Order

This is probably the biggest factor in writing performant queries.
Clause order currently matters a lot in Matcha.  There may be a 2, 3
(or more) orders of magnitude performance difference between the
following two queries; which will return identical results:


```clojure
;; Fast
(defn lookup-friends [person-uri db]
  (first (construct {:grafter.rdf/uri ?friend
                     ?p ?o} [[person-uri :foaf/friend ?friend]
                             [?friend ?p ?o]])) db)
```

This query is fast because queries are evaluated top down and the
clause `[person-uri :foaf/friend ?friend]` narrows the results
immediately to just those stemming from the single resource
`person-uri`.

Compared to this:

```clojure
;; Many many times slower...
(defn lookup-friends [person-uri db]
  (first (construct {:grafter.rdf/uri ?friend
                     ?p ?o} [[?friend ?p ?o]
                             [person-uri :foaf/friend ?friend]])) db)
```

Which is much slower, because the clause `[?friend ?p ?o]` is
evaluated first and it matches every triple in the database before
comparing them to `[person-uri :foaf/friend ?friend]` for elimination.

Typically, the fastest queries will bind `subject` or `object` to a
single value on their first line.

It's also worth bearing in mind that all clauses (not just the first)
should be ordered so as to try and eliminate potential solutions as
early as possible.

At some point in the future Matcha may analyse the query to formulate
a query plan to help people avoid this performance pitfall.

## Indexing
### Avoid reindexing

By default Matcha indexes `:s`, `:p` and `:o` with the `index-triples`
function.  If a query is given unindexed data (as a sequence of
triples) Matcha will first attempt to build the default indexes, and
then run the query.  However any subsequent queries given this same
data will then need to reindex the database, e.g. in this code:

```clojure
(let [triples [[:s :p 1] [:s :p 2] [:s :p 3] ,,, [:s :p 99999]]]
  (select ?something [,,,some-where-clause,,,] triples)
  ,,,
  (select ?otherthing [,,,some-where-clause,,,] triples)

  )
```

the data will be indexed twice.  Hence it is recommended that you
adopt the following idiom to avoid needless reindexing:

```clojure
(let [triples (index-triples [[:s :p 1] [:s :p 2] [:s :p 3] ,,, [:s :p 99999]])]
  (select ?something [,,,some-where-clause,,,] triples)
  ,,,
  (select ?otherthing [,,,some-where-clause,,,] triples)

  )
```

### Optimizing indexes

Matcha now builds all specified indexes in a single pass over the
incoming triples.  However each extra index adds overhead, both in
terms of CPU time and memory overhead.

So when querying larger datasets (100,000's of triples) in particular
you can save seconds on indexing time by only indexing indicies that
are actually used by your queries.

Matcha supports all 6 possibilities for indexes and their composites:

- `:s`
- `:p`
- `:o`
- `#{:s :p}`
- `#{:s :o}`
- `#{:p :s}`

The below example indexes a database on `:s`, `:p`, `:o` and the
composite `#{:p :o}` index:

```clojure
(def db (index-triples [:s :p :o #{:p :o}] triples))
```

### Composite indexes

You can sometimes improve query performance by adding composite
indicies to your index.  For example this query over 459,327 triples
takes 16.86 seconds to return 56,772 concept resource objects:

```clojure
(let [db-spo (index-triples [:s :p :o] triples)] ;; 5 seconds
  (construct {:grafter.rdf/uri ?s, ?p ?o} [[?narrower rdf:a skos:Concept] [?narrower ?p ?o]] db-spo)) ;; 16 seconds
```

Where as the same query with a single composite index on `#{:p :o}` is
10x quicker taking just 2.2 seconds to both build the composite index
and return all 56,772 matching concepts:

```clojure
(let [db-spo (index-triples [#{:p :o}] triples)]
  (construct {:grafter.rdf/uri ?s, ?p ?o} [[?narrower rdf:a skos:Concept] [?narrower ?p ?o]] db-spo))
```

This query is comparatively so efficient because `#{:p :o}` indexes
the `rd:a skos:Concept` pair directly, and index triples only builds
one index.

As always though for the best results you really need to measure the
performance for your specific case.  It may for example only be worth
building an index if you're returning lots results, or needing to
eliminate lots of intermediate results.

Also bear in mind that the above query and dataset is on the more
extreme side as there are both lots of results and a large database.

As a guide to getting good performance the most useful indexes are
typically `:s` and `:o` (depending on which direction you're more
commonly walking in).  `:p` is often useful but typically less so than
`:s` and `:o`.  The composite `#{:p :o}` is good when you know a
property object pair, such as `rdf:a skos:Concept`, or `?obs :dim
:dim-val`; but usually only makes a difference on larger
datasets/queries.

`:p` _may_ also be more useful if you have an `optional` in your
query, as often it's a rare property you're looking for; in which case
`:p` may help narrow the candidate solutions substantially.



### Values clauses are usually better at the top

As with other clauses values clauses are currently evaluated top down.
This means that if for example you have a set of areas that you want
to list as resource objects then you'll probably want to put that
clause at the top in order to narrow the solutions quickly:

```clojure
(let [areas #{,,,}]
  (construct {:grafter.rdf/uri ?s ?p ?o} [(values ?s areas)
                                          [?s rdf:a somekindof:Area]
                                          [?s ?p ?o]]))
```

You're also usually better from a performance perspective using a
`values` clause in the query than iterating over the items and running
the query from the outside.


## Select vs Construct

`construct` is the most general and useful Matcha query operator,
however it currently pays a performance price for its expressivity.  I
have opened a bug to address this, so the performance should
eventually be comparable to select, but currently if you just one
result we can see a 2 or more orders of magnitude performance
difference:

```clojure
(time (first (m/construct [?p] [[?s ?p ?o]] db-spo)))
  "Elapsed time: 9690.229029 msecs"
```

vs

```clojure
(time (first (m/select [?p] [[?s ?p ?o]] db-spo)))
"Elapsed time: 8.365042 msecs"
```

Or for all results we see select is twice as fast:

```clojure
(time (do (vec (m/construct [?p] [[?s ?p ?o]] db-spo)) nil))
"Elapsed time: 7934.345399 msecs"
```

```clojure
(time (do (vec (m/select [?p] [[?s ?p ?o]] db-spo)) nil))
"Elapsed time: 3165.456641 msecs"
```

It should be possible to make construct almost as fast as select, but
it'll never be faster than it.  In particular one should be aware that
the `:grafter.uri/subject` grouping behaviour requires `construct` to
first consume the whole sequence of solutions, so when this key is
used it can't be lazy.

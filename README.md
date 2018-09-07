# Matcha

[![Clojars Project](https://img.shields.io/clojars/v/grafter/matcha.alpha.svg)](https://clojars.org/grafter/matcha.alpha)

*WARNING: Alpha Software Subject to Change*

A Clojure DSL to query in memory triple models with a SPARQL like
language.  Matcha provides simple BGP (Basic Graph Pattern) style
queries on in memory graphs of linked data triples.

![Matcha](https://raw.githubusercontent.com/Swirrl/matcha/master/doc/matcha.jpg "Matcha")

Whilst Matcha is intended to query RDF models it can also be used to
query arbitrary clojure data, so long as it consists of Clojure values
stored in 3/tuple vectors, each entity of the triple is assumed to
follow Clojure value equality semantics.

The primary use cases for Matcha are to make handling graphs of RDF
data easy by querying data with SPARQL-like queries.  A typical
workflow is to `CONSTRUCT` data from a backend SPARQL query, and then
use Matcha to query this graph locally.

## Features

- SPARQL-like BGP queries across multiple triple patterns
- Ability to index your database, with `index-triples`.  In order to
  be queried Matcha needs to have indexed the data; if your data is
  unindexed it will index it before running the query, and then
  dispose of the index.  This can lead to poor performance when you
  want to query the same set of data multiple times.
- Construct graph query results directly into clojure datastructures.

## Limitations

The initial implementation is macro heavy.  This means use cases where
you want to dynamically create in memory queries may be more awkward.

Currently there is no support for the following SPARQL-like features:

1. OPTIONALs, though we can probably add these (planned)
2. Reasoning on in memory vocabularies with RDFS (maybe OWL)
3. Clojurescript support (planned)
4. Ability to bind a query variable to one or more triples, like
   SPARQL `BIND` or `VALUES` clauses.

## Usage

Matcha defines some primary query functions `select`, `select-1`,
`construct`, `construct-1` and `ask`.

First lets define an in memory database of triples, in reality this
could come from a SPARQL query `CONSTRUCT`, but here we'll just define
some RDF-like data inline.

Triples can be vectors of clojure values or any datastructure that
supports positional destructuring via `clojure.lang.Indexed`, this
allows Matcha to work `grafter.rdf.protocols.Statement` records.  Matcha works with
any clojure values in the triples, be they java URI's, or clojure
keywords.

```clojure
(def friends [[:rick :rdfs/label "Rick"]
              [:martin :rdfs/label "Martin"]
              [:katie :rdfs/label "Katie"]
              [:julie :rdfs/label "Julie"]

              [:rick :foaf/knows :martin]
              [:rick :foaf/knows :katie]
              [:katie :foaf/knows :julie]])
```

Now we can build our query functions:

### General Query Semantics

There are two main concepts to Matcha queries.  They typically define:

1. a projection, which states what variables to return to your Clojure
program, and the datastructure they should be returned in.
2. a Basic Graph Pattern (BGP), that defines the pattern of the graph
   traversal.

BGPs have some semantics you need to be aware of:

- Clojure symbols beginning with a `?` are treated specially as query
  variables.
- Other symbols are resolved to their values.

### `select`

`select` compiles a query function from your arguments, that returns
results as a sequence of tuples.  It is directly analagous to SPARQL's
`SELECT` query:

```clojure
(def rick-knows (select [?name]
                  [[:rick :foaf/knows ?p2]
                   [?p2 :rdfs/label ?name]]))
```

When called with two arguments `select` expects the first argument to
be a vector of variables to project into the solution sequence, the
second argument is analagous to a SPARQL `WHERE` clause and should be
a BGP.

We can then run the query like so:

```clojure
(rick-knows friends) ;; ["Martin" "Katie"]
```

There is also `select-1` which is just like `select` but returns just
the first solution.

### `construct`

`CONSTRUCT`s are the most powerful query type, as they allow you to
construct arbitrary clojure data structures directly from your query
results, and position the projected query variables where ever you
want within the structure.

```clojure
(def query (construct {:grafter.rdf/uri :rick
                                :foaf/knows {:grafter.rdf/uri ?p
                                :rdfs/label ?name}}
         [[:rick :foaf/knows ?p]
         [?p :rdfs/label ?name]]))
```

Produces:

```clojure
{:grafter.rdf/uri :rick
                :foaf/knows #{{:grafter.rdf/uri :martin, :rdfs/label "Martin"}
                              {:grafter.rdf/uri :katie, :rdfs/label "Katie"}}}
```

Maps in a projection that contain the special key of
`:grafter.rdf/uri` trigger extra behaviour, and cause the query
engine to group solutions by subject, and merge values into clojure
sets.  For example in the above query you'll notice that `foaf:knows`
groups its solutions.  If you don't want these maps to be grouped,
don't include the magic key `:grafter.rdf/uri` in the top level
projection.

There is also `construct-1` which is just like `construct` but returns
only the first solution.

See the [unit
tests](https://github.com/Swirrl/matcha/blob/ae2449483d5a7849ac60a3e5b6a29e459d74ad8e/test/grafter/matcha/alpha_test.clj#L113)
for more examples, including examples that use Matcha with Grafter
Statements and vocabularies.

### `ask`

`ask` is the only query that doesn't specify an explicit projection.
It accepts a BGP, like the other query types and returns a boolean
result if there were any matches found.

```clojure
(def any-triples? (ask [[?s ?p ?o]])

(any-triples? friends) ;; => true
```

## Performance

Matcha is intended to be used on modest sizes of data, typically
thousands of triples, and usually no more than a few hundred thousand
triples.  Proper benchmarking hasn't yet been done but finding all
solutions on a database of a million triples can be done on a laptop
in less than 10 seconds.  Query time scaling seems to be roughly
linear with the database size.


## License

Copyright © Swirrl IT Ltd 2018

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

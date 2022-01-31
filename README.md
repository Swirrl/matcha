# Matcha

[![Clojars Project](https://img.shields.io/clojars/v/grafter/matcha.alpha.svg)](https://clojars.org/grafter/matcha.alpha) [![cljdoc badge](https://cljdoc.org/badge/grafter/matcha.alpha)](https://cljdoc.org/d/grafter/matcha.alpha)

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

- SPARQL-like BGP queries across multiple triple patterns.
- Parameterised queries using just clojure `let`.
- Ability to index your database, with `index-triples`.  In order to
  be queried Matcha needs to have indexed the data; if your data is
  unindexed it will index it before running the query, and then
  dispose of the index.  This can lead to poor performance when you
  want to query the same set of data multiple times.
- Construct graph query results directly into clojure datastructures.
- Support for `VALUES` clauses (unlike in SPARQL we do not yet support
  binding arbitrary tuples/tables).  So we only support the
  `VALUES ?x { ... }` form.
- Support for `OPTIONAL`s with SPARQL-like semantics.

## Limitations

The initial implementation is macro heavy.  This means use cases where
you want to dynamically create in memory queries may be more awkward.

Currently there is no support for the following SPARQL-like features:

1. Reasoning on in memory vocabularies with RDFS (maybe OWL)
2. Clojurescript support (planned)

## Usage

Matcha defines some primary query functions `select`, `select-1`,
`build`, `build-1`, `construct`, `construct-1` and `ask`.

First lets define an in memory database of triples, in reality this
could come from a SPARQL query `CONSTRUCT`, but here we'll just define
some RDF-like data inline.

Triples can be vectors of clojure values or any datastructure that
supports positional destructuring via `clojure.lang.Indexed`, this
allows Matcha to work `grafter.rdf.protocols.Statement` records.
Matcha works with any clojure values in the triples, be they java
URI's, or clojure keywords.

```clojure
(def friends-db [[:rick :rdfs/label "Rick"]
                 [:martin :rdfs/label "Martin"]
                 [:katie :rdfs/label "Katie"]
                 [:julie :rdfs/label "Julie"]

                 [:rick :foaf/knows :martin]
                 [:rick :foaf/knows :katie]
                 [:katie :foaf/knows :julie]

                 [:rick :a :foaf/Person]
                 [:katie :a :foaf/Person]
                 [:martin :a :foaf/Person]])
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

### `build`

`build` always groups returned solutions into a sequence of clojure
maps, where the subjects are grouped into maps, and the maps are
grouped by their properties. If a property has multiple values they
will be rolled up into a set, otherwise they will be a scalar value.

Each map returned by `build` typically represents a resource in the
built graph, which is projected into a sequence of maps, with
potentially multi-valued keys.

It takes a binding for `?subject` of the map, a map form specifying
the projection of other property/value bindings a `bgp` and a
database.

``` clojure
(build ?person
       {:foaf/knows ?friends}
       [[?person :foaf/knows ?friends]]
       friends-db)

;; => ({:grafter.rdf/uri :rick, :foaf/knows #{:martin :katie}}
;;     {:grafter.rdf/uri :katie, :foaf/knows :julie}
```

NOTE: `:foaf/knows` is projected into a set of values for `:rick`, but
a single scalar value for `:katie`.

The `?subject` is by default associated with the key
`:grafter.rdf/uri`. If you wish to specify this key yourself you can
by providing a key/value pair as the subject: e.g. substituting
?person for `[:id ?person]` changes the return values like so:

``` clojure
(build [:id ?person]
       {:foaf/knows ?friends}
       [[?person :foaf/knows ?friends]]
         friends-db)
;; => ({:id :rick, :foaf/knows #{:martin :katie}}
;;     {:id :katie, :foaf/knows :julie}
```

Because `build` knows it is always returning a sequence of maps, it
will remove any keys corresponding to unbound variables introduced
through optionals.  This is unlike `construct`.

### `select`

`select` compiles a query from your arguments, that returns results as a
sequence of tuples. It is directly analagous to SPARQL's `SELECT` query.

The `bgp` argument is analagous to a SPARQL `WHERE` clause and should be
a BGP.

When called with one argument, `select` projects all `?qvar`s used in the
query.  This is analagous to `SELECT *` in SPARQL:

```clojure
(def rick-knows
  (select
    [[:rick :foaf/knows ?p2]
    [?p2 :rdfs/label ?name]]))

(rick-knows friends-db)
;; => ["Martin" "Katie"]
```

When called with two arguments `select` expects the first argument to be a
vector of variables to project into the solution sequence.

```clojure
(def rick-knows (select [?name]
                  [[:rick :foaf/knows ?p2]
                   [?p2 :rdfs/label ?name]]))

(rick-knows friends-db)
;; => ["Martin" "Katie"]
```

There is also `select-1` which is just like `select` but returns just
the first solution.

### `construct`

NOTE: if you're using you `construct` to return maps, you should first
consider using `build` which fixes some issues present in common
`construct` usage.

`CONSTRUCT`s allow you to construct arbitrary clojure data structures
directly from your query results, and position the projected query
variables where ever you want within the projected datastructure
template.

Args:
 * `construct-pattern`: an arbitrary clojure data structure. Results
   will be projected into the `?qvar` "holes".
 * `bgps`: this argument is analagous to a SPARQL `WHERE` clause and should be
   a BGPs.
 * `db-or-idx`: A matcha "database".

When called with two arguments `construct` returns a query function
that accepts a `db-or-idx` as its only argument. When called, the
function returns a sequence of matching tuples in the form of the
`construct-pattern`.

```clojure
(construct {:grafter.rdf/uri :rick
            :foaf/knows {:grafter.rdf/uri ?p
                         :rdfs/label ?name}}
  [[:rick :foaf/knows ?p]
   [?p :rdfs/label ?name]])

;; => (fn [db-or-idx] ...)
```

When called with 3 arguments, queries the `db-or-idx` directly, returning a
sequence of results in the form of the `construct-pattern`.

```clojure
(construct {:grafter.rdf/uri :rick
            :foaf/knows {:grafter.rdf/uri ?p
                         :rdfs/label ?name}}
  [[:rick :foaf/knows ?p]
   [?p :rdfs/label ?name]]
  friends-db)

;; => {:grafter.rdf/uri :rick
;;     :foaf/knows #{{:grafter.rdf/uri :martin, :rdfs/label "Martin"}
;;                   {:grafter.rdf/uri :katie, :rdfs/label "Katie"}}}
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

(any-triples? friends-db) ;; => true
```

### Parameterising queries

You can parameterise Matcha queries simply by adding a lexical binding or wrapping a function call over your Matcha query.  For example

```clojure
(defn lookup-friends [person-id database]
  (->> database
       (construct {:grafter.rdf/uri ?friend
                   :name ?name}
                   [[person-id :foaf/knows ?friend]
                    [?friend :rdfs/label ?name]]))

(lookup-friends :rick friends-db)

;; => [{:grafter.rdf/uri :martin, :name "Martin"}
;;     {:grafter.rdf/uri :katie, :name "Katie"}]
```

### OPTIONALs

We support SPARQL-like `OPTIONAL`s in all query types with the following syntax:

```clojure
(defn lookup-name [person-id database]
  (select [?name]
    [[person-id :a :foaf/Person]
     (optional [[person :rdfs/label ?name]])
     (optional [[person :foaf/name ?name]])]))
```

### VALUEs

We support dynamic VALUEs clauses in all query types like so:

```clojure
(defn lookup-names [person-ids database]
  (select [?name]
    [(values ?person-id person-ids)
     [?person-id :rdfs/label ?name]]))

(lookup-names [:rick :katie] friends-db) ;; => ["Rick", "Katie"]
```

You can also hardcode the values into the query:

```clojure
(defn lookup-names [person-ids database]
  (select [?name]
    [(values ?person-id [:rick :katie])
     [?person-id :rdfs/label ?name]]))
```

Any "flat collection" (i.e. a `sequential?` or a `set?`) is valid
on the right hand side of a `values` binding.

## Performance

Matcha is intended to be used on modest sizes of data, typically
thousands of triples, and usually no more than a few hundred thousand
triples.  Proper benchmarking hasn't yet been done but finding all
solutions on a database of a million triples can be done on a laptop
in less than 10 seconds.  Query time scaling seems to be roughly
linear with the database size.

## Avoiding clj-kondo lint errors with matcha macros

Matcha exports some clj-kondo configuration which prevents clj-kondo
warning about unbound variables when using the matcha query macros.

You can import these configs into your project with the following
command:

```
$ clj-kondo --copy-configs --dependencies --lint "$(clojure -Spath)"
Imported config to .clj-kondo/grafter/matcha.alpha. To activate, add "grafter/matcha.alpha" to :config-paths in .clj-kondo/config.edn.
```

Then simply add the following to `.clj-kondo/config.edn`:

```
{:config-paths ["grafter/matcha.alpha"]}
```

## Developing Matcha

Matcha uses [`tools.build`](https://clojure.org/guides/tools_build) and
[`tools.deps`](https://clojure.org/guides/deps_and_cli) for builds,
development and testing.

The command:

```
$ clojure -T:build test
```

Will run the tests, whilst

```
$ clojure -T:build build
$ clojure -T:build install
```

can be used to build and install a jar into your local mvn repository.

However for consuming local Matcha changes in local projects you are
usually better using `tools.deps` `:classpath-overrides`, or creating
a branch and consuming via a `:git/url`.

## Deploying to Clojars

For [deployments CircleCI is setup](https://github.com/Swirrl/matcha/blob/fafe7478ae605c4cb2a0253714c3bd286e1ca185/.circleci/config.yml#L46-L55)
to automatically deploy tags of the form `vX.Y.Z` where `X.Y.Z` are
`major.minor.patch` numbers.  If you have permissions (i.e. you are
a Swirrl developer) the recommended workflow is to create a new
release of the `main` branch in github with a tag that bumps the
version number appropriately.

_NOTE_: For this step to work you will need appropriate deployment
privileges on clojars.org.

## License

Copyright © Swirrl IT Ltd 2018

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

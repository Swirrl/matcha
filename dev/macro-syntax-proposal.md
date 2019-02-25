```clojure
(ns macro-syntax-proposal
  (:require [clojure.spec.alpha :as s]))

(defmacro construct [& body])
```

# What's the problem?

There's a bit of boilerplate where you want to pass in some argument to a
`construct` macro form, and passing the db argument to the resulting
function is a bit unwieldy. E.G.,

 ```clojure
(fn [arg1 arg2 db]
  ((construct form [[arg1 some:thing ?o]
                    [arg1 thing:else arg2]]) db))
 ```

# Database argument

``` clojure
(fn [db arg] ((construct {:arg arg} [[arg what:evs "thing"]]) db))`
```

``` clojure
(defmacro construct-fn
  {:style/indent :defn}
  [arglist construct-form bgps]
  ;; ...
  )
```

This is maybe the easy suggestion, collapse the pattern into a macro, then usage
becomes:

``` clojure
(def query
  (construct-fn [arg1 arg2]
    {:binding arg1 :some-var? ?var}
    [[arg1 some:thing ?o]
     [arg1 thing:else arg2]])) ; -> (fn [db arg1 arg2] ...)

(let [db ...]
  (query db arg1 arg2))
```

You're getting rid of the `(())` double paren call, but gaining a mysterious
first `db` argument. Without types/specs/docstrings this might be confusing.

You could force a placebo `db` binding, so at least the definition would mirror
the call, but as you wouldn't use the `db` binding it could also be confusing.

``` clojure
(construct-fn [db arg1 arg2] ...)
```

In Haskell you'd most likely stick the DB in a Monad and forget about it...

``` haskell
construct : arglist -> form -> bgps -> (arglist -> MonadDB result)

run : MonadDB result -> DB -> result
```

... but that doesn't really gain you anything in Clojure except an extra word in
your `(())`.

``` clojure
(run (...) db)
;; vs.
((...) db)
```



# Option 1: Implicit Magic - probably bad

Find all unbound symbols in expression and bind them in order of appearance
as function arguments

# Option 2: Explicit Magic

Find all unbound symbols in expression and bind them via kwargs (or symargs)
or a binding map argument

;;; construct : Monad db => [var] -> form -> bgps -> ([var] -> db result)
(defmacro construct [arglist form bgps])

;;; run-db : Monad db => ([var] -> db result) -> DB -> result
(defn run-db [& f db])

;;; with-db : Monad db => [var DB] -> db &body ->

(defmacro construct-fn {:style/indent :defn}
  ([arglist construct-form bgps]))

(s/fdef construct-fn
  :args (s/or :ary-3 (s/cat :arglist :clojure.core.specs.alpha/param-list
                            :construct-form any?
                            :bgps ::m/bgps)
              :ary-4 (s/cat :db any?
                            :arglist :clojure.core.specs.alpha/param-list
                            :construct-form any?
                            :bgps ::m/bgps)))

(let [db []
      query (fn-construct [arg pred]
              {:binding arg :some-var ?var}
              [[arg some:predicate ?val]
               [arg pred "some constraint"]])]
  (query db))

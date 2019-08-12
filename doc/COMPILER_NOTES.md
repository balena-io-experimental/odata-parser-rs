### Draft notes and ideas about implementing an efficient AST-\>SQL compiler.

When the server receives an OData query for processing there are a few
different components that this query is composed of. Here we try to make a
sketch of how these components translate to SQL.

An OData query can contain the following components (there are a few more but
they don't affect the SQL that much):

* `$resource` - This represents the main resource of the query. It can itself
  contain traversal of multiple objects
* `$select` - Narrows down the properties returned for each entity
* `$expand` - Requests inline data for related resources
* `$filter` - Narrows down entities returned
* `$search` - Search terms (will probably be left unimplemented for now)
* `$orderby` - Ordering requirements
* `$skip` - Skips N results
* `$top` - Limits result to N entities
* `$count` - Requests inline count of resources
* `$levels` - Traversal of self-referencial properties
* `$compute` - Requests a computed term

It's important to note that most of these properties can be nested within
`$expand` and `$select` options (e.g expand on application->devices but only
return the $top 5). Therefore, the query structure and compiler must be
recursive and have a well defined way of composing queries. This is needed for
both query generation but also for result parsing.

Roughly, query options will appear in the following places in a query:

```sql
SELECT
	$select, ARRAY($expand)
FROM
	($resource query)
	LATERAL (<permission query>) AS permission_path
	LATERAL (SELECT $compute) AS computed_prop_a
WHERE
	permission_path AND
	$filter
ORDER BY
	$order_by
LIMIT $top $skip
```

There are a few notable things about this structure. First, the liberal use of
LATERAL joins. Lateral joins allows us to compute particular properties about
the current entity once, and reuse them elsewhere in the query without
recomputing them. For example, we can have a LATERAL JOIN that computes a
$compute field which is then use in both the $filter section and the $select
section. If it were not for the LATERAL join the only option would be to inline
the computation, causing the database to compute it twice or more times.

Also, by computing the permission path in a LATERAL join we can know which
particular graph traversal allowed an entity to be returned. This is an
extremely useful result because it allows us to efficienly compute column
permissions, expand predicated on a particular permission path, etc. To
illustrace this, imagine that there were two permission traversals that allowed
someone to read an application object but only one of them allowed reading the
application name. Then the query would look like this:

```sql
SELECT
	CASE WHEN path_a THEN name ELSE NULL AS name,
FROM
	application,
	LATERAL (<permission query>) AS permission_path_a
	LATERAL (<permission query>) AS permission_path_b
WHERE
	permission_path_a OR permission_path_b
```

If the two permission queries were to be inlined in the WHERE clause we would
have no way of knowing which of the two evaluated to true in our SELECT
stataement. Of course, forcing the computation of all paths is slower in the
average case than using an OR since the latter will short-circuit the
computation. The engine should therefore automatically choose to combine
permission queries when the individual results don't matter.

### Parsing the database results

In order to efficiently gerenate and parse database results the system relies
on the ARRAY() construct and the RECORD() construct. These have very efficient
binary formats and do not need to be passed through a JSON
serialize/deserialize step. Also, since Rust allows very low level access to
the result we can easily parse the binary format. Unfortunately not all details
of this protocol are documented in a literate document, but we can easily read
the serialization routines from the postgres source code to understand how
arrays of tuples and other combinations are transferred over the wire. As an
example, this is the routine that sends a native ARRAY over the wire:

https://github.com/postgres/postgres/blob/REL_11_5/src/backend/utils/adt/arrayfuncs.c#L1541-L1644

Since the query we receive can be arbitrarily nested, the best strategy to
parse the result from the database would be to expoit the already nested
structure of the AST that produced the query to parse the result. You can
imagine each node parsing the bytes relevant to it and then passing the data on
to the next consumer. This is a very rough idea and there might be an AST ->
SQL consumer transformation needed.

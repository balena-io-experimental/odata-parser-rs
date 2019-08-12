When we talk about permissions we talk about then in the context of a subject.
A data system could have many different subjects and the subjects might be of
the same type either. Indeed, a multi-tenant system defines one subject per
user but there could also be automated clients that have their own permissions.

Permissions are defined by describing a particular traversal (path) of the
entities in the entity graph. There can be many paths associated with a
particular subjects and they might share nodes. Therefore, the collection of
all paths form a permission graph. Each node of a permission graph represents a
traversal of all incoming edges together with an optional filter. We represent
a node as `[->property($filter)]`.The first node of each path can be thought as
traversing a virtual ROOT object that is connected to all other entities.

Defining permissions in this manner allows a much more natural way to express
common patterns in web applications. For example, a graph like the following
expresses in a consise way that a user can access their user object, all the
applications that belong to them, all the devices that belong to that
application, and all the releases that belong to that application.

```
          [ROOT]
            |
            v
[->users(id = SESSION.USER)]
            |
            v
     [->applications]
            |
           / \
          /   \
         v     v
 [->devices]  [->releases]
```

This graph can be futher annotated to allow specific actions to be performed on
specific objects and also constraint the properties that are visible on each
node. For example we could easily hide the password field from the user object.

Having permissions defined in this manner allows a much more efficient query
generation when evaluating a query. To illustrate this point, consider a query
that requests the current user object but also expands on applications and
devices. Since the system has knowledge of the graph, it knows that if a query
can access the user object then the application device device access is implied
without futher checks. Therefore we can emit a very efficient `JOIN` operation
that is only predicated on the ids matching instead of re-evaluating
permissions.

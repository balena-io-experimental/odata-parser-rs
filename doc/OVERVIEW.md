# Overview

## Goals of this project

This project aims to be a fast and complete implementation of the OData v4.01
protocol, while offering advanced features like automatic schema migrations and
a powerful permission system.

## Non-goals

This project is not concerned with how data is ultimately stored on disk or
memory, nor with how to efficiently traverse these data structures to execute a
query. We leave all these details to be implemented by the underlying storage
engine. We choose to focus on postgres as the main supported data backend but
more could be integrated in the future.

## Core data structures

The core data structure is the data schema, or model. These terms are used
interchangeably. The schema defines the kind of entities that exist, their
properties, and the relationships between them. The terminology and structure
follows the schema definition structure of [OData v4.01](https://docs.oasis-open.org/odata/odata-csdl-json/v4.01/csprd04/odata-csdl-json-v4.01-csprd04.html).
This section serves as a summary of the core ideas, but the spec should be
treated as the authoritative document.

### Entities

The main object of the universe is an Entity. Every entity that exists in the
universe must be uniquely identified by the combination of its entity type and
its identifying properties. The simplest manifestation of this is an Entity
that has an `id` property that contains a unique value for each instance of the
entity type.

The shape of an entity is determined by the definition of its corresponding
entity type. By shape we mean the list of properties and their types, whether
or not they are nullable, the connections to other entities, etc. While most
Entities will be closed types, meaning that the list of properties is known and
defined in the schema, there can also be open types, which allow an arbitrary
number of properties to be additionally attached on them.

### Entity Types

The Entity Type is the definition of the shape of entities, similar to a class
definition in an object oriented programming language.

An Entity Type is made of a unique name and a list of properties. Each property
has a name, its corresponding type, and a set of attributes such as whether or
not it's a collection and whether or not it is nullable. However, a property
cannot be both a collection and nullable.

An Entity Type also defines an ordered subset of its properties as its key,
which should uniquely identify each instance.

Lastly, an Entity Type can be marked as open, which allows arbitrary,
potentially different properties to be set on each instance. These properties
extend the ones concretely defined in the model. The types of the open
properties and whether they are stored is not yet defined.


### Entity Sets

An entity set represents a subset of all the entities of a particular entity
type. The most trivial entityset is the one that contains all the instances of
a type but there can be more specialised ones. As an example, imagine a model
defining a Product entity type and then exposing an entityset named Products,
which contains all the products and an entityset named DiscountedProducts which
only contains Products for which there is a discount.

An entityset also represents an entrypoint for a potential query. This means
that if an entity type has no entity set defined then there is no way to access
the entity type directly. For example a model could define a CartEntry entity
that has no corresponding entity set. Neverthelss, CartEntry entities could be
accessible by navigating through the current user.

### Singletons

A singleton also represents an entrypoint to the entity graph but is a single
entity instance. An example of that could a be CurrentUser singleton.

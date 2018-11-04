# Purescript QueryDsl [![Build Status](https://travis-ci.org/Dretch/purescript-querydsl.svg?branch=master)](https://travis-ci.org/Dretch/purescript-querydsl) [![Documentation](https://pursuit.purescript.org/packages/purescript-querydsl/badge)](https://pursuit.purescript.org/packages/purescript-querydsl)

A SQL query builder for Purescript, very loosely based on Java's [Querydsl](http://www.querydsl.com/).

## Goals
 - Support standard SQL insert/update/delete/select queries.
 - Allow building queries in a mostly type-safe and composable way.
 - Generate reasonably readable SQL for monitoring and debugging.
 - Support multiple underlying databases platforms.

## Non-Goals
 - Support create table syntax: these tend to be very database specific.

## Status
- Experimental, pre-alpha, full of bugs, lacking in features, unstable, don't rely on this, etc.
- Currently only SQLite is supported.

## Quick Example

```purescript
import Prelude
import Effect.Aff (Aff)
import QueryDsl (Column, Table, makeTable, from, select, where_, orderBy, limit, asc)
import QueryDsl.Expressions ((:==))
import QueryDsl.SQLite3 (runSelectManyQuery)
import SQLite3 (DBConnection)
import Type.Data.Boolean (False, True)

customer = makeTable "customer" :: Table (
  id :: Column Int False,
  firstName :: Column String True,
  lastName :: Column String True
)

getLastNames :: DBConnection -> Aff (Array { lastName :: String })
getLastNames conn = do
  runSelectManyQuery conn do
    c <- from customer
    pure $ select { lastName: c.lastName }
      `where_` (c.firstName :== "Bob")
      `orderBy` [asc c.id]
      `limit` 10
```

## Longer Example

### Defining Tables

Querydsl requires that you define a value of type `Table r` for each table in your database. The row parameter `r` holds the types of the columns in the table. E.g.

```purescript
customer = makeTable "customer" :: Table (
  id :: Column Int False,
  firstName :: Column String True,
  lastName :: Column String True
)
```

The first parameter to each `Column` is the Purescript version of the database type, and the second parameter says whether this column is required for inserts or not. In this example `id` is not required because we know it is an auto-generated primary key column.

### Select Statements

A monad is used to build the from-clause, and the value returned by the monad says what columns to select, and what where-clause/order-by/limit/etc to use.

```purescript
runSelectManyQuery db do
  c <- from customer
  o <- join order (\o -> o.customer :== c.id)
  pure $ select {name: c.firstName, total: o.total} `where_` (o.total :>= 50)
```

### Insert Statements

For an insert you must supply a record containing a value of the correct type for each non-optional column in the table, and possibly also values for the optional columns.

```purescript
runQuery db $ insertInto customer { firstName: "Jim", lastName: "Smith" }
```

### Update Statements

To update a table you need a record of expressions of the correct types for each column you want to update and a filtering expression that limits which rows are updated. If you want to update all rows then use `alwaysTrue` as the filter.

The `columns` function gives you access to expressions representing the columns of the table, if you need to refer to them in your update or filter expressions.

```purescript
let c = columns customer in
runQuery db $ update customer { lastName: "Smythe" } (c.lastName :== "Smith")
```

### Delete Statements

To delete rows from a table you must provide a filtering expression that selects the rows to delete.

```purescript
let c = columns customer in
deleteFrom customer (c.firstName :== "Paulo" :&& c.lastName :== "Coelho")
```

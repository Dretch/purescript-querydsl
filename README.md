# Purescript QueryDsl [![Build Status](https://travis-ci.org/Dretch/purescript-querydsl.svg?branch=master)](https://travis-ci.org/Dretch/purescript-querydsl) [![Documentation](https://pursuit.purescript.org/packages/purescript-querydsl/badge)](https://pursuit.purescript.org/packages/purescript-querydsl)

A SQL query builder for Purescript, loosely based on Java's [Querydsl](http://www.querydsl.com/).

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
The first parameter to each `Column` is the Purescript version of the database type, and the second parameter is whether this column is required for inserts (`True`) or not (`False`). In this example `id` is not required because it is an auto-generated primary key column.

### Insert Statements
For an insert you must supply a record containing a value of the correct type for each non-optional column in the table, and possibly also values for the optional columns. E.g.
```purescript
-- where db is a DBConnection
runQuery db $ insertInto customer { firstName: "Jim", lastName: "Smith" }
```

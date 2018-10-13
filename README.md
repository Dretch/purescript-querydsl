An unfinished experimental SQL query builder for Purescript, loosely based on Java's Querydsl (http://www.querydsl.com/).

The goal is to allow building SQL in a type-safe and composable way, ideally with comprehensible types signatures.

# Features
 - Various combinators for creating type-checked predicates for selects/deletes.
 - Column names and types checked for insertions.
 - Generates reasonably readable SQL for monitoring and debugging.

# Example
```purescript
import Prelude
import Effect.Aff (Aff)
import QueryDsl (Column, Table, makeTable, from, select, where_, orderBy, limit, asc)
import QueryDsl.Expressions ((:==))
import QueryDsl.SQLite (runSelectManyQuery)
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

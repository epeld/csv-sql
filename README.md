# CSV-SQL

This project aims to support a subset of the MySQL SQL-syntax for querying CSV data.

The data can be supplied either from a file or from standar input.

There are plans to support
- Joins
- Different CSV column separators

..but right now the functionality is pretty basic.

## Try It Out
Run e.g

    bash sql.sh "select c,b,a from file('abc.csv')"


which should produce the contents of the file `abc.csv` but with its columns reversed

## SQL Subset
As mentioned, the tool aims to support a subset of the MySQL SQL-syntax.

Here are some facts about the current state of support:
- Only `SELECT`s are supported
- `ORDER BY`s are supported *but only for a single field* and the field must be specified *by name*.
- `WHERE` is supported, but currently only in this form: `WHERE mycolumn LIKE '%foo%'`


# omlOrchestrations 0.2.0

* Calculated values no longer lose digits. Before, `calculate_quantity()`
  added the result with `c()`, which turned the value into a 15-digit string,
  so the next call received a rounded double.
* The `value` column of a vdf is now a list column (`I(list(...))`). Each
  element is one R value, so a call can return a vector or `NULL`
  (before, `rbind()` stopped with an error). Use `result$value[[i]]`, or
  `unlist(result$value)` when every value is a scalar.
* `asserted_quantity_values()` converts the SPARQL literal to a double once,
  with jsonlite's correctly rounded parser. `as.numeric()` can be off by one
  ulp on platforms without a long double (e.g. Apple Silicon).
* A vdf with a character or numeric `value` column is still accepted and is
  converted to a list column.
* jsonlite is a new dependency.

# omlOrchestrations 0.1.5

* First Release

# omlOrchestrations 0.1.0

* Initial CRAN submission.

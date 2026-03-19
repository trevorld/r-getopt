getopt 1.21.0 (development)
===========================
* New ``getfile()`` function returns name of calling script, it is an alias of pre-existing `get_Rscript_filename()`.
* New `getusage()` function generates a usage string from a getopt `spec` matrix.
* The `spec` argument of `getopt()` and `getusage()` may now also be a 4-5 column data frame
  (which will be silently coerced to a matrix).
* `getopt()` and `getusage()` now support action strings in column 3 of `spec`:

  + `"append"` appends each argument to a vector each time the flag is used.
  + `"count"` stores integer count of each time the flag is used.
  + `"store"` stores argument value, legacy `1` supported as an alias.
  + `"store_false"` stores `FALSE`.
  + `"store_optional"` stores argument value if present otherwise stores `TRUE`,  legacy `2` supported as an alias.
  + `"store_true"` stores `TRUE`, legacy `0` supported as an alias.

* For long flags the storage mode column of `spec` now only casts any taken argument values from to the requested storage mode.
  In particular `store_true` now never casts to the requested storage mode and `store_optional` only casts to the requested storage mode when the optional argument value is present.
  This was the documented behaviour and the previous/current behavior for short flags.
* You can now store negative numbers when storage mode is not "double" or "integer" (e.g. "character").

getopt 1.20.4
=============
* Documentation tweaks eliminating a CRAN check NOTE.
* `{covr}` is no longer a "suggested" package in the `DESCRIPTION` file.

getopt 1.20.3
=============
* Now by default ``getopt()`` won't override a user specified ``opt`` argument if ``argv`` is in the global environment.
  Will continue to use ``argv`` as a default for ``opt`` if it is in the global environment and the user does not specify an ``opt`` argument (for ``littler`` compatibility).

getopt 1.20.2
=============
* Now allows one to pass an empty string to a character option.
  Thanks Matthew Flickinger for bug report.

getopt 1.20.1
=============
* Now explicitly imports the ``na.omit()`` method from the ``stats`` package.
  Thanks Derrick Oswald for bug report.
* Improved parsing for negative numbers preceded by a space instead of a '=' sign.
  Thanks Roman Zenka for improved regular expression.
* Slightly more informative error message if `storage.mode()` coercion results in an `NA`.
  Thanks Roman Zenka for suggestion.

getopt 1.20.0
=============
* Type of "numeric" in spec automatically cast to "double".
  Previously users might have had an error passing negative numbers if they
  accidentally specified "numeric" instead of "double".
* Project website moved to https://github.com/trevorld/getopt
* Exports new function ``sort_list()``.

getopt 1.19.1
=============
* If a passed in option matches multiple options in the getopt specification but matches one exactly
  then `getopt` now uses that value instead of throwing a "long flag is ambiguous" error.

getopt 1.19.0
=============
* Exports new function ``get_Rscript_filename()`` that returns name of calling script,
  `{getopt}` now uses this function value as default for ``command`` argument.
* Documentation improved and now highlights differences
  between `{getopt}` and `{optparse}` packages for new undecided users.

# The aqueduct R Package

This package is inspired by [Will Landau's outstanding `drake` package](https://github.com/ropensci/drake),
with the following "improvements." Most of the modfications in the `aqueduct` package are not improvements per se,
but rather simplifications. That's why the package is named `aqueduct`. It ain't the best,
but it's an improvement over what the barbarians are doing.

1. **Workflows are more interpretably integrated with antiquated, hierarchical file structures.**
   The `drake` package works best when each component file
   contains a single function. The `aqueduct` package does not assume such an intelligent structure,
   and is more easily interpretable for individuals accustomed to creating linear workflows where each of the component files consists of many (or no) user-made
   functions.
2. **Simple option to save intermediate datasets as either CSVs or R objects in memory.**
   The `drake` package
   has an `autoclean` option to limit memory use and save datasets in long-term memory. The option to save
   CSVs in the `aqueduct` package has the benefit of knowing exactly where, and having other projects access
   those intermediate datasets on the fly. This is especially helpful if the other projects are written using
   another statistical software (e.g. Stata)

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Package Overview

dplyr is a tidyverse R package providing a grammar of data manipulation.

## Key development commands

### General

- When running R from the console, always run it with `--quiet --vanilla`
- Always run `air format .` after generating code

### Testing

- Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`
- Use `devtools::test(reporter = "check")` to run all tests
- Use `devtools::test(filter = "{name}", reporter = "check")` to run tests for `R/{name}.R`
- DO NOT USE `devtools::test_active_file()`
- All testing functions automatically load code; you don't need to
- All new code should have an accompanying test
- If there are existing tests, place new tests next to similar existing tests

### Documentation

- Run `devtools::document()` after changing any roxygen2 docs
- Every user facing function should be exported and have roxygen2 documentation
- Whenever you add a new documentation file, make sure to also add the topic name to `_pkgdown.yml`
- Run `pkgdown::check_pkgdown()` to check that all topics are included in the reference index
- Use sentence case for all headings
- Any user facing changes should be briefly described in a bullet point at the top of `NEWS.md`, following the tidyverse style guide (https://style.tidyverse.org/news.html)

### Code style

- Use newspaper style/high-level first function organisation. Main logic at the top and helper functions should come below
- Don't define functions inside of functions unless they are very brief
- Error messages should use `cli::cli_abort()` and follow the tidyverse style guide (https://style.tidyverse.org/errors.html)

## Architecture

### Key Design Patterns

#### Standalone Imports
- Self-contained utility functions
- `import-standalone-*.R` files reduce dependencies
- Imported from other tidyverse packages

## Key Files

### Core Implementation
- `R/` - R implementation files
- `src/` - C/C++ implementation files

### Testing and Quality
- `tests/testthat/` - Test suite
- `vignettes/` - Documentation and examples
- `.github/workflows/` - CI/CD with R CMD check

## Development Notes

### Testing Strategy
- Snapshot testing for output validation
- Separate test files for each major component

### Code Organization
- Utility functions grouped by purpose (`utils-*.R`)
- Standalone imports minimize external dependencies

### Documentation
- Roxygen2 comments for all exported functions
- Vignettes demonstrate key use cases
- pkgdown site provides comprehensive documentation
- Examples use realistic but safe API interactions

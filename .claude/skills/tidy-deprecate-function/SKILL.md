---
name: tidy-deprecate-function
description: Guide for deprecating R functions/arguments. Use when a user asks to deprecate a function or parameter, including adding lifecycle warnings, updating documentation, adding NEWS entries, and updating tests.
---

# Deprecate functions and function arguments

Use this skill when deprecating functions or function parameters in this package.

## Overview

This skill guides you through the complete process of deprecating a function or parameter, ensuring all necessary changes are made consistently:

1. Add deprecation warning using `lifecycle::deprecate_warn()`.
2. Silence deprecation warnings in existing tests.
3. Add lifecycle badge to documentation.
4. Add bullet point to NEWS.md.
5. Create test for deprecation warning.

## Workflow

### Step 1: Determine deprecation version

Read the current version from DESCRIPTION and calculate the deprecation version:

- Current version format: `MAJOR.MINOR.PATCH.9000` (development).
- Deprecation version: Next minor release `MAJOR.(MINOR+1).0`.
- Example: If current version is `2.5.1.9000`, deprecation version is `2.6.0`.

### Step 2: Add `lifecycle::deprecate_warn()` call

Add the deprecation warning to the function:

```r
# For a deprecated function:
function_name <- function(...) {
  lifecycle::deprecate_warn("X.Y.0", "function_name()", "replacement_function()")
  # rest of function
}

# For a deprecated parameter:
function_name <- function(param1, deprecated_param = deprecated()) {
  if (lifecycle::is_present(deprecated_param)) {
    lifecycle::deprecate_warn("X.Y.0", "function_name(deprecated_param)")
  }
  # rest of function
}
```

Key points:

- First argument is the deprecation version string (e.g., "2.6.0").
- Second argument describes what is deprecated (e.g., "function_name(param)").
- Optional third argument suggests replacement.
- Use `lifecycle::is_present()` to check if a deprecated parameter was supplied.

### Step 3: Update tests

Find all existing tests that use the deprecated function or parameter and silence lifecycle warnings. Add at the beginning of test blocks that use the deprecated feature:

```r
test_that("existing test with deprecated feature", {
  withr::local_options(lifecycle_verbosity = "quiet")

  # existing test code
})
```

Then add a new test to verify the deprecation message in the appropriate test file (usually `tests/testthat/test-{name}.R`):

```r
test_that("function_name(deprecated_param) is deprecated", {
  expect_snapshot(. <- function_name(deprecated_param = value))
})
```

You'll need to supply any additional arguments to create a valid call.

Then run the tests and verify they pass.

### Step 4: Update documentation

For function deprecation, add to the description section:

```r
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Please use [replacement_function()] instead.
```

If the documentation does not already contain `@description`, you will need to add it.

For argument deprecation, add to the appropriate `@param` tag:

```r
#' @param deprecated_param `r lifecycle::badge("deprecated")`
```

When deprecating a function or parameter in favor of a replacement, add old/new examples to the `@examples` section to help users migrate. These should relace all existing examples.

```r
#' @examples
#' # Old:
#' old_function(arg1, arg2)
#' # New:
#' replacement_function(arg1, arg2)
#'
#' # Old:
#' x <- "value"
#' old_function("prefix", x, "suffix")
#' # New:
#' replacement_function("prefix {x} suffix")
```

Key points:

- Use "# Old:" and "# New:" comments to clearly show the transition.
- Include 2-3 practical examples covering common use cases.
- Make examples runnable and self-contained.
- Show how the new syntax differs from the old.

Then re-document the package.

### Step 5: Add NEWS entry

Add a bullet point to the top of the "# packagename (development version)" section in NEWS.md:

```markdown
# packagename (development version)

* `function_name(parameter)` is deprecated and will be removed in a future
  version.
* `function_name()` is deprecated. Use `replacement_function()` instead.
```

Place the entry:

- In the lifecycle subsection if it exists, otherwise at the top level under development version.
- Include the replacement if known.
- Keep entries concise and actionable.

## Implementation checklist

When deprecating a function or parameter, ensure you:

- [ ] Read DESCRIPTION to determine deprecation version.
- [ ] Add `lifecycle::deprecate_warn()` call in the function.
- [ ] Add `withr::local_options(lifecycle_verbosity = "quiet")` to existing tests.
- [ ] Create new test for deprecation warning using `expect_snapshot()`.
- [ ] Run tests to verify everything works.
- [ ] Add lifecycle badge to roxygen documentation.
- [ ] Add migration examples to `@examples` section (for function deprecation).
- [ ] Run `devtools::document()` to update documentation.
- [ ] Add bullet point to NEWS.md.
- [ ] Run `air format .` to format code.

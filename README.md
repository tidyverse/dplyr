
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dplyr <a href="https://dplyr.tidyverse.org"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/dplyr)](https://cran.r-project.org/package=dplyr)
[![R-CMD-check](https://github.com/tidyverse/dplyr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidyverse/dplyr/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tidyverse/dplyr/graph/badge.svg)](https://app.codecov.io/gh/tidyverse/dplyr)
<!-- badges: end -->

## Overview

dplyr is a grammar of data manipulation, providing a consistent set of
verbs that help you solve the most common data manipulation challenges:

- `mutate()` adds new variables that are functions of existing variables
- `select()` picks variables based on their names.
- `filter()` picks cases based on their values.
- `summarise()` reduces multiple values down to a single summary.
- `arrange()` changes the ordering of the rows.

These all combine naturally with `group_by()` which allows you to
perform any operation “by group”. You can learn more about them in
`vignette("dplyr")`. As well as these single-table verbs, dplyr also
provides a variety of two-table verbs, which you can learn about in
`vignette("two-table")`.

If you are new to dplyr, the best place to start is the [data
transformation chapter](https://r4ds.hadley.nz/data-transform) in R for
Data Science.

## Backends

In addition to data frames/tibbles, dplyr makes working with other
computational backends accessible and efficient. Below is a list of
alternative backends:

- [arrow](https://arrow.apache.org/docs/r/) for larger-than-memory
  datasets, including on remote cloud storage like AWS S3, using the
  Apache Arrow C++ engine,
  [Acero](https://arrow.apache.org/docs/cpp/acero/overview.html).

- [dbplyr](https://dbplyr.tidyverse.org/) for data stored in a
  relational database. Translates your dplyr code to SQL.

- [dtplyr](https://dtplyr.tidyverse.org/) for large, in-memory datasets.
  Translates your dplyr code to high performance
  [data.table](https://rdatatable.gitlab.io/data.table/) code.

- [duckplyr](https://duckplyr.tidyverse.org/) for large, in-memory
  datasets. Translates your dplyr code to high performance
  [duckdb](https://duckdb.org) queries with zero extra copies and an
  automatic R fallback when translation isn’t possible.

- [sparklyr](https://spark.posit.co/) for very large datasets stored in
  [Apache Spark](https://spark.apache.org).

## Installation

``` r
# The easiest way to get dplyr is to install the whole tidyverse:
install.packages("tidyverse")

# Alternatively, install just dplyr:
install.packages("dplyr")
```

### Development version

To get a bug fix or to use a feature from the development version, you
can install the development version of dplyr from GitHub.

``` r
# install.packages("pak")
pak::pak("tidyverse/dplyr")
```

<!--
The entire section is hidden by default (`display: none` on `webr-toggle`).
When the JS module loads (pkgdown site), the toggle is flipped and it shows the container.
On GitHub, the JS never runs, so nothing is displayed.
-->

<!--- CSS -->

<style>
/* WebR Editor Styles */
#webr-container {
  border: 1px solid #dee2e6;
  border-radius: 0.375rem;
  padding: 1rem;
  margin: 1rem 0;
  background-color: #f8f9fa;
}
.webr-status {
  margin-bottom: 0.75rem;
  padding: 0.5rem 0.75rem;
  background-color: #e7f1ff;
  border-radius: 0.25rem;
  font-size: 0.9rem;
  color: #0c5460;
}
.webr-editor {
  width: 100%;
  min-height: 100px;
  padding: 0.75rem;
  font-family: 'Source Code Pro', Consolas, 'Liberation Mono', Menlo, monospace;
  font-size: 0.875rem;
  background-color: #ffffff;
  border: 1px solid #ced4da;
  border-radius: 0.25rem;
  box-sizing: border-box;
  resize: vertical;
}
.webr-editor:focus {
  border-color: #86b7fe;
  outline: 0;
  box-shadow: 0 0 0 0.25rem rgba(13, 110, 253, 0.25);
}
.webr-run-btn {
  margin-bottom: 0.75rem;
  padding: 0.5rem 1.5rem;
  font-size: 0.9rem;
  font-weight: 500;
  color: #fff;
  background-color: #447099;
  border: none;
  border-radius: 0.25rem;
  cursor: pointer;
  transition: background-color 0.15s ease-in-out;
}
.webr-run-btn:hover:not(:disabled) {
  background-color: #375a7a;
}
.webr-run-btn:disabled {
  opacity: 0.65;
  cursor: not-allowed;
}
.webr-output {
  width: 100%;
  margin: 0.75rem 0 0 0;
  padding: 0.75rem;
  font-family: 'Source Code Pro', Consolas, 'Liberation Mono', Menlo, monospace;
  font-size: 0.875rem;
  background-color: #ffffff;
  color: #212529;
  border: 1px solid #ced4da;
  border-radius: 0.25rem;
  box-sizing: border-box;
}
.webr-output-plot {
  max-width: 100%;
  margin: 0.75rem auto 0 auto;
  background-color: #ffffff;
  border: 1px solid #ced4da;
  border-radius: 0.25rem;
}
.webr-fallback {
  padding: 1rem;
  text-align: center;
  color: #6c757d;
}
.webr-fallback a {
  color: #0d6efd;
  text-decoration: none;
}
.webr-fallback a:hover {
  text-decoration: underline;
}
</style>

<!--- UI -->

<!--- Can't indent, otherwise it gets treated as markdown -->

<div id="webr-toggle" style="display: none;">

<h3>

Try it
</h3>

<div id="webr-container">

<div class="webr-status">

</div>

<button class="webr-run-btn" disabled>

Run Code
</button>

<textarea class="webr-editor">starwars |>
  filter(species == "Human") |>
  select(name, homeworld) |>
  arrange(name)</textarea>

<pre class="webr-output" style="display: none;"></pre>

<canvas class="webr-output-plot" style="display: none;">

</canvas>

</div>

</div>

<!--- JavaScript -->

<script type="module">
  // This module only runs on the pkgdown site (not GitHub) because GitHub
  // doesn't execute JS. The container is hidden by default, so on GitHub
  // nothing is displayed. This script shows the container on pkgdown.
  import { WebR } from 'https://webr.r-wasm.org/latest/webr.mjs';
  document.addEventListener('DOMContentLoaded', async () => {
    // Toggle the container on (hidden by default for GitHub)
    const toggle = document.getElementById('webr-toggle');
    if (!toggle) {
      return;
    }
    toggle.style.display = 'block';
    const container = document.getElementById('webr-container');
    if (!container) {
      return;
    }
    const statusEl = container.querySelector('.webr-status');
    const editorEl = container.querySelector('.webr-editor');
    const outputEl = container.querySelector('.webr-output');
    const outputPlotEl = container.querySelector('.webr-output-plot');
    const runBtn = container.querySelector('.webr-run-btn');
    const updateStatus = (msg, loading = true) => {
      if (statusEl) {
        statusEl.innerHTML = loading
        ? '<span class="spinner-border spinner-border-sm me-2"></span>' + msg
        : msg;
      }
    };
    updateStatus('Initializing WebR...');
    try {
      const webR = new WebR();
      await webR.init();
      updateStatus('Installing dplyr (this may take a moment)...');
      await webR.installPackages(['dplyr'], { quiet: true });
      updateStatus('Loading dplyr...');
      await webR.evalRVoid('library(dplyr)');
      if (statusEl) {
        // Finished with status updates
        statusEl.style.display = 'none';
      }
      runBtn.disabled = false;
      runBtn.addEventListener('click', async () => {
        runBtn.disabled = true;
        // About to run code, show `Running...` to the user
        outputEl.style.display = 'block';
        outputEl.textContent = 'Running...';
        // Clear previous plots and hide the plot canvas
        outputPlotEl.style.display = 'none';
        outputPlotEl.textContent = null;
        const code = editorEl.value;
        let shelter = await new webR.Shelter();
        try {
          let capture = await shelter.captureR(
            code,
            {
              withAutoprint: true,
              // This gives us an R object in `val.data` when `val.type` is `"error"` or `"warning"`
              // so that we can recall `conditionMessage()`
              throwJsException: false
            }
          );
          // Extract output divs
          const elements = capture.output.map(async (val) => {
            // webr doesn't use `captureCondition()` correctly yet, so we manually
            // handle error and warning cases by calling `conditionMessage()` ourselves
            // https://github.com/r-wasm/webr/issues/281
            const element = document.createElement('div');
            if (val.type === 'stdout') {
              element.textContent = val.data;
            } else if (val.type === 'stderr') {
              element.style.color = 'red';
              element.textContent = val.data;
            } else if (val.type === 'error') {
              element.style.color = 'red';
              const message = await webR.evalRString(`conditionMessage(cnd)`, { env: { cnd: val.data }});
              element.textContent = message.includes("\n") ? `Error:\n${message}` : `Error: ${message}`;
            }else if (val.type === 'warning') {
              element.style.color = 'orange';
              const message = await webR.evalRString(`conditionMessage(cnd)`, { env: { cnd: val.data }});
              element.textContent = message.includes("\n") ? `Warning:\n${message}` : `Warning: ${message}`;
            }
            return element;
          });
          // Clear last output, or initial `Running...`
          outputEl.textContent = null;
          // Show output if we have any, or remove output div entirely if no output
          if (elements.length > 0) {
            const outputs = await Promise.all(elements);
            outputEl.append(...outputs);
          } else {
            outputEl.style.display = 'none';
          }
          // Show last image if we have any.
          // Doesn't make much sense to try and show multiple.
          if (capture.images.length > 0) {
            outputPlotEl.style.display = 'block';
            // Use the last image (most recent plot)
            const image = capture.images[capture.images.length - 1];
            // Set canvas dimensions to match the image
            outputPlotEl.width = image.width;
            outputPlotEl.height = image.height;
            outputPlotEl.getContext('2d').drawImage(image, 0, 0);
          }
        } finally {
          runBtn.disabled = false;
          shelter.purge();
        }
      });
    } catch (e) {
      updateStatus('Failed to initialize WebR: ' + e.message, false);
      console.error('WebR initialization failed:', e);
    }
  });
</script>

## Cheat Sheet

<a href="https://github.com/rstudio/cheatsheets/blob/main/data-transformation.pdf"><img src="https://raw.githubusercontent.com/rstudio/cheatsheets/main/pngs/thumbnails/data-transformation-cheatsheet-thumbs.png" width="630" height="252"/></a>

## Usage

``` r
library(dplyr)

starwars |>
  filter(species == "Droid")
#> # A tibble: 6 × 14
#>   name   height  mass hair_color skin_color  eye_color birth_year sex   gender  
#>   <chr>   <int> <dbl> <chr>      <chr>       <chr>          <dbl> <chr> <chr>   
#> 1 C-3PO     167    75 <NA>       gold        yellow           112 none  masculi…
#> 2 R2-D2      96    32 <NA>       white, blue red               33 none  masculi…
#> 3 R5-D4      97    32 <NA>       white, red  red               NA none  masculi…
#> 4 IG-88     200   140 none       metal       red               15 none  masculi…
#> 5 R4-P17     96    NA none       silver, red red, blue         NA none  feminine
#> # ℹ 1 more row
#> # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
#> #   vehicles <list>, starships <list>

starwars |>
  select(name, ends_with("color"))
#> # A tibble: 87 × 4
#>   name           hair_color skin_color  eye_color
#>   <chr>          <chr>      <chr>       <chr>    
#> 1 Luke Skywalker blond      fair        blue     
#> 2 C-3PO          <NA>       gold        yellow   
#> 3 R2-D2          <NA>       white, blue red      
#> 4 Darth Vader    none       white       yellow   
#> 5 Leia Organa    brown      light       brown    
#> # ℹ 82 more rows

starwars |>
  mutate(name, bmi = mass / ((height / 100)^2)) |>
  select(name:mass, bmi)
#> # A tibble: 87 × 4
#>   name           height  mass   bmi
#>   <chr>           <int> <dbl> <dbl>
#> 1 Luke Skywalker    172    77  26.0
#> 2 C-3PO             167    75  26.9
#> 3 R2-D2              96    32  34.7
#> 4 Darth Vader       202   136  33.3
#> 5 Leia Organa       150    49  21.8
#> # ℹ 82 more rows

starwars |>
  arrange(desc(mass))
#> # A tibble: 87 × 14
#>   name      height  mass hair_color skin_color eye_color birth_year sex   gender
#>   <chr>      <int> <dbl> <chr>      <chr>      <chr>          <dbl> <chr> <chr> 
#> 1 Jabba De…    175  1358 <NA>       green-tan… orange         600   herm… mascu…
#> 2 Grievous     216   159 none       brown, wh… green, y…       NA   male  mascu…
#> 3 IG-88        200   140 none       metal      red             15   none  mascu…
#> 4 Darth Va…    202   136 none       white      yellow          41.9 male  mascu…
#> 5 Tarfful      234   136 brown      brown      blue            NA   male  mascu…
#> # ℹ 82 more rows
#> # ℹ 5 more variables: homeworld <chr>, species <chr>, films <list>,
#> #   vehicles <list>, starships <list>

starwars |>
  group_by(species) |>
  summarise(
    n = n(),
    mass = mean(mass, na.rm = TRUE)
  ) |>
  filter(
    n > 1,
    mass > 50
  )
#> # A tibble: 9 × 3
#>   species      n  mass
#>   <chr>    <int> <dbl>
#> 1 Droid        6  69.8
#> 2 Gungan       3  74  
#> 3 Human       35  81.3
#> 4 Kaminoan     2  88  
#> 5 Mirialan     2  53.1
#> # ℹ 4 more rows
```

## Getting help

If you encounter a clear bug, please file an issue with a minimal
reproducible example on
[GitHub](https://github.com/tidyverse/dplyr/issues). For questions and
other discussion, please use [forum.posit.co](https://forum.posit.co/).

## Code of conduct

Please note that this project is released with a [Contributor Code of
Conduct](https://dplyr.tidyverse.org/CODE_OF_CONDUCT). By participating
in this project you agree to abide by its terms.

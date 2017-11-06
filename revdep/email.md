Hi,

This is an automated email to let you know about the release of {{{ my_package }}}, which will be submitted to CRAN in the near future (on {{{ date }}}).

The major change in this version is that dplyr now depends on the selecting
backend of the tidyselect package. If you have been linking to
`dplyr::select_helpers` documentation topic, you should update the link to point
to `tidyselect::select_helpers`.

Another change that causes warnings is that dplyr now exports the `exprs()`
function. This causes a collision with `Biobase::exprs()`. Either import
functions from dplyr selectively rather than in bulk, or do not import
`Biobase::exprs()` and refer to it with a namespace qualifier.

To check for potential problems, I ran `R CMD check` on your package {{{your_package}}} ({{{your_version}}}).

I found: {{{your_summary}}}.

{{#you_have_problems}}
{{{your_results}}}

If I got an ERROR because I couldn't install your package (or one of its dependencies), my apologies. You'll have to run the checks yourself (unfortunately I don't have the time to diagnose installation failures as I have to run checks on hundreds of packages).

Otherwise, please carefully look at the results. If you think I've introduced a bug in dplyr, please file a reprex at <https://github.com/tidyverse/dplyr/issues>. Otherwise, you'll need to prepare an update to your package following the advice in <http://dplyr.tidyverse.org/articles/compatibility.html>.

To get the development version of {{{ my_package }}} so you can run the checks yourself, you can run:

    # install.packages("devtools")
    devtools::install_github("{{my_github}}")

{{/you_have_problems}}
{{^you_have_problems}}
It looks like everything is ok with your package, so no action is necessary at this time.
{{/you_have_problems}}

If you have any questions about this email, please feel free to respond directly.

Regards,

{{{ me }}}

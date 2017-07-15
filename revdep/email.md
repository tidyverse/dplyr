Hi,

This is an automated email to let you know about the release of {{{ my_package }}}, which will be submitted to CRAN in the near future (on {{{ date }}}). 

* Major changes: <https://blog.rstudio.org/2017/04/13/dplyr-0-6-0-coming-soon/>
* All changes: <https://github.com/tidyverse/dplyr/releases/tag/v0.6.0-rc>
* Backward compatibility guide: <http://dplyr.tidyverse.org/articles/compatibility.html>

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

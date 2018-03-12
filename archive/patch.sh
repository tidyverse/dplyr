#!/bin/sh
#
# Tested on Ubuntu.

set -e

dir=$1
cd $(dirname $0)
echo ${dir}
cd ${dir}

R -q -e 'install.packages(c("testthat", "roxygen2", "devtools"))'
R -q -e 'packrat::snapshot()'
exit 0

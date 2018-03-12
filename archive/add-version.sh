#!/bin/sh
#
# Tested on Ubuntu.

set -e

version=$1
date=$2

if [ "${version}" = "" ] || [ "${date}" = "" ]; then
  echo "Usage: $0 <version> <YYYY-MM-DD>" 1>&2
  exit 1
fi

cd $(dirname $0)
rm -rf ${version}
mkdir ${version}
cd ${version}

R -q -e 'options(repos = "https://cran.microsoft.com/snapshot/'${date}'"); packrat::init(options = list(vcs.ignore.src = TRUE))'
git add .

sed -r -i".bak" "s#Repos: CRAN=.*\$#Repos: CRAN=https://cran.microsoft.com/snapshot/${date}#" packrat/packrat.lock
if diff packrat/packrat.lock packrat/packrat.lock.bak; then
  echo "Fatal: sed hasn't changed anything." 1>&2
  exit 2
fi
rm packrat/packrat.lock.bak

R -q -e 'install.packages("dplyr")'
R -q -e 'packrat::snapshot()'
git add .

#!/bin/sh
#
# Intended usage:
#
# ls -d 0* | parallel -I"{}" -q ./run-in.sh "{}" -e 'packageVersion("dplyr")'
#
# Tested on Ubuntu.


set -e

dir=$1
shift

echo "#" ${dir}
cd ${dir}

"$@"

#!/bin/sh
#
# Tested on Ubuntu.

set -e

dir=$(dirname $0)

ls -d ${dir}/?.*.*-* | sort | parallel --keep-order --gnu -I"{}" -q ${dir}/run-in.sh "{}" "$@"

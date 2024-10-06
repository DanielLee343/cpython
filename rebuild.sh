#!/bin/bash

make clean
rm -rf build
rm -rf python

# ./configure
# ./configure --with-trace-refs
# sleep 1
echo "starting building..."
# make -j regen-cases
# assign default values if arguments are not provided
DO_MIGRATION="${1:-1}"
DEMO_MODE="${2:-3}"
HOTNESS_THRESH="${3:-0}"

# echo "DO_MIGRATION: $DO_MIGRATION"
# echo "DEMO_MODE: $DEMO_MODE"
# echo "HOTNESS_THRESH: $HOTNESS_THRESH"
# exit 0
make -j48 DO_MIGRATION=$DO_MIGRATION DEMO_MODE=$DEMO_MODE HOTNESS_THRESH=$HOTNESS_THRESH
# this will only build python interpreter, no shared mod, etc, for faster test
# make only_intp -j48

# optional, if want to test sqlalchemy
# cd ../sqlalchemy
# $HOME/workspace/cpython/python -m pip install -e . --no-build-isolation
# cd ../cpython

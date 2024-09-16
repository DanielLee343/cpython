#!/bin/bash

make clean
rm -rf build
rm -rf python

# ./configure
# ./configure --with-trace-refs
# sleep 1
echo "starting building..."
# make -j regen-cases
make -j48 DO_MIGRATIOIN=$1 DEMO_MODE=$2 HOTNESS_THRESH=$3
# this will only build python interpreter, no shared mod, etc, for faster test
# make only_intp -j48

# optional, if want to test sqlalchemy
# cd ../sqlalchemy
# $HOME/workspace/cpython/python -m pip install -e . --no-build-isolation
# cd ../cpython

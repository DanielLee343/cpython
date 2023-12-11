#!/bin/bash

make clean
rm -rf build
rm -rf python

# ./configure
# sleep 1

echo "starting building..."
make -j48
# this will only build python interpreter, no shared mod, etc, for faster test
# make only_intp -j48

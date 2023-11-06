#!/bin/bash

make clean
rm -rf build
rm -rf python

make -j48
# this will only build python interpreter, no shared mod, etc, for faster test
# make only_intp -j48
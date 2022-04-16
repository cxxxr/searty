#!/bin/bash

export SEARTY_INDEX_DIR=$(realpath ../index/)

./gen-makefile.ros ~/quicklisp-dist/systems.txt ~/quicklisp-dist/2022-04-01/ $(realpath ../index/) Makefile
rm -rf ../index/
time make -j24 -k 2>&1 | tee log
time ./searty-merge.ros $(realpath ../index/) searty.db

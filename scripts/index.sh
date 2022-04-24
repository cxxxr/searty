#!/bin/bash

./gen-makefile.ros ~/quicklisp-dist/systems.txt ~/quicklisp-dist/2022-04-01/ $(realpath ../build/) $(realpath ../index/) Makefile

rm -rf ../build ../index
mkdir ../build ../index

make build -j24 -k 2>&1 | tee make.log

ls ../build/*.out | parallel ./searty-index.sh

./searty-merge.sh $(realpath ../index) searty.db

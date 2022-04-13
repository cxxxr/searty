#!/bin/bash

./gen-makefile.ros ~/quicklisp-dist/systems.txt ~/quicklisp-dist/2022-04-01/ $(realpath ../index/) Makefile
rm -rf success.txt failure.txt ../index/
time make -j24 -k 2>&1 | tee log

#!/bin/bash

./gen-makefile.ros ~/quicklisp-dist/systems.txt ~/quicklisp-dist/2022-04-01/ $(realpath ../build/analyze/) $(realpath ../build/db/) Makefile

rm -rf ../build
mkdir ../build
mkdir ../build/analyze ../build/db

make analyze -j24 -k

cd ../cmd/searty-index
go build
ls ../../build/analyze/*.json | parallel ./searty-index -o ../../build/db
cd -

rm -rf ../db
mkdir ../db
cd ../cmd/searty-merge
go run . -o ../../db/quicklisp.db ../../build/db/*.sqlite3

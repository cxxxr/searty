#!/bin/bash

./gen-makefile.ros ~/quicklisp-dist/systems.txt ~/quicklisp-dist/2022-04-01/ $(realpath ../build/) $(realpath ../index/) Makefile

rm -rf ../build ../index
mkdir ../build ../index

make build -j24 -k 2>&1 | tee make.log

cd ../cmd/searty-index
go build
ls ../build/*.out | parallel ./searty-index -o ../../index
cd -

rm -rf ../db
mkdir ../db
cd ../cmd/searty-merge
go run . -o ../db/quicklisp.db ../index/*.sqlite3

#!/bin/sh

rm -rf ../index
mkdir ../index

ls ../build/*.out | parallel ./searty-index.sh

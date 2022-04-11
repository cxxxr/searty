#!/bin/bash

system_name=$1
dist_dir=$2
target=$3

./searty-index.ros $1 $2 2>&1 | tee $target
if [ $? -eq 0 ]; then
    echo $1 >> success.txt
else
    echo $1 >> failure.txt
fi

#!/bin/bash

./searty-index.ros $@
if [ $? -eq 0 ]; then
    echo $1 >> success.txt
else
    echo $1 >> failure.txt
fi

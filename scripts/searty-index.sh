#!/bin/bash

./searty-index.ros $@
if [ $? -eq 0 ]; then
    echo $1 >> $SEARTY_INDEX_DIR/success.txt
else
    echo $1 >> $SEARTY_INDEX_DIR/failure.txt
fi

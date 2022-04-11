#!/bin/bash

./initdb.ros
rm -rf /tmp/searty
mkdir /tmp/searty

time make -j24 -k

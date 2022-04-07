#!/bin/bash

system_index_url=$(curl http://beta.quicklisp.org/dist/quicklisp.txt | grep 'system-index-url: ' | sed 's/system-index-url:\s*//')
curl $system_index_url > $(basename $system_index_url)

#!/bin/bash

for f in $(find ./src -type f -name "*.ts") ; do
  if ! grep "$f" "Copyright 2020 Florian Kramer" ; then 
    echo "$f is missing the copyright statement" 
  fi 
done

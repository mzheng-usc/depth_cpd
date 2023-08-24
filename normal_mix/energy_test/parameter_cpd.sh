#!/bin/bash
for a in `seq 0 0.1 1`; 
  do 
    for b in {1..3};
      do
        echo "$a $b"
      done;
  done;


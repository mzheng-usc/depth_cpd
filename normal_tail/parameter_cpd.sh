#!/bin/bash
for a in `seq 2 2 22`; 
  do 
    for b in {1..500};
      do
        echo "$a $b"
      done;
  done;


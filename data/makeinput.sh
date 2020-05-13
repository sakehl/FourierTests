#!/bin/bash

for n in "1000"
do
  for i in "1" "100" "1000" "2000" "5000" "10000" "20000"
  # for i in "1"
  do
    python3 input_gen.py $n $i
  done
done
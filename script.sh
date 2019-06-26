#!/bin/bash

BENCHS="Regular"

for i in "Regular" "Irregular" "Normal" "Foreign"
do
  stack run -- --output bench_20_06_2019_$i.html "main/$i/10000"
done

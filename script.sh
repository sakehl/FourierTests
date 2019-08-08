#!/bin/bash

#BENCHS="Regular"
#CPU stuff
# for i in "Regular" "Irregular" "Foreign"
# do
#   for j in "1" "100" "1000" "2000" "5000" "10000"
#   do
#     time stack run --verbosity silent -- --output results/CPU_bench_20_06_2019_$j_$i.html "CPU/$i/$j" -m "glob"
#   done
# done

#GPU stuff
for i in "Regular" "Irregular" "Normal"
do
  for j in "1" "1000" "10000" "20000" "50000" "100000"
  do
    time stack run --verbosity silent -- --output results/GPU_bench_20_06_2019_$j_$i.html "GPU/$i/$j" -m "glob"
  done
done

#Normal stuff
for i in "Normal"
do
  for j in "1" "100" "1000" "2000"
  do
    time stack run --verbosity silent -- --output results/CPU_bench_20_06_2019_$j_$i.html "CPU/$i/$j" -m "glob"
  done
done

#Normal stuff GPU
for i in "Normal"
do
  for j in "1" "100" "1000" "2000"
  do
    time stack run --verbosity silent -- --output results/GPU_bench_20_06_2019_$j_$i.html "GPU/$i/$j" -m "glob"
  done
done
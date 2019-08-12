#!/bin/bash
#SBATCH -t 2:00:00
#SBATCH -N 1
#SBATCH -p gpu

#BENCHS="Regular"
date=$(date '+%Y_%m_%d')
# run="time stack run --verbosity silent -- --output results"
# module switch CUDA/10.0.130 CUDA/9.0.176

#GPU stuff
for i in "Foreign" "Regular" "Irregular"
do
  for j in "1" "1000" "2000" "5000" "10000" "20000"
  do
    time stack run --verbosity silent -- --output results/GPU_bench_$date\_$i\_$j.html "GPU/$i/$j" -m "glob"
  done
done

# CPU stuff
for i in "Regular" "Irregular" "Foreign"
do
  for j in "1" "100" "1000" "2000" "5000" "10000"
  do
    time stack run --verbosity silent -- --output results/CPU_bench_$date\_$i\_$j.html "CPU/$i/$j" -m "glob"
  done
done

#Normal stuff
for i in "Normal"
do
  for j in "1" "100" "1000"
  do
    time stack run --verbosity silent -- --output results/CPU_bench_$date\_$i\_$j.html "CPU/$i/$j" -m "glob"
  done
done

#Normal stuff GPU
for i in "Normal"
do
  for j in "1" "100" "1000"
  do
    time stack run --verbosity silent -- --output results/GPU_bench_$date\_$i\_$j.html "GPU/$i/$j" -m "glob"
  done
done
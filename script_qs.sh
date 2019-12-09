#!/bin/bash
#SBATCH -t 2:00:00
#SBATCH -N 1
#SBATCH -p gpu
cd $HOME/lvandenhaak/FourierTests
#BENCHS="Regular"
date=$(date '+%Y_%m_%d')
# run="time stack run --verbosity silent -- --output results"
. init.sh
#GPU stuff
for n in "1000" #"1000"
do
    for i in "Regular"
    do
        for j in "1" "100" "1000" "2000" "5000" "10000"
        do
            stack run --verbosity silent -- --output results/qs_GPU_bench_$date\_$i\_$j.html --csv results/qs_$date.csv "QuickSort/GPU/$i/$n/$j" -m "glob"
        done
    done
done
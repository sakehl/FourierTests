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
#"FFTFuthark"
#for i in "Regular" "Lifted" "cuFFT" "Irregular" FFTFuthark
for i in "FFTFuthark"
do
  for j in "1" "100" "1000" "5000" "10000" "20000"
  do
    nvprof --profile-child-processes -u ms --trace gpu --continuous-sampling-interval 1 stack --verbosity silent run -- $i $j | tee -a results/$date
  done
done

exit 1

#Normal stuff GPU
for i in "Normal"
do
  for j in "1" "100" "1000"
  do
    nvprof --profile-child-processes -u ms --trace gpu --continuous-sampling-interval 1 stack --verbosity silent run -- $i $j | tee -a results/$date
  done
done

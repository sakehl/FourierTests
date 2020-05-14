#!/bin/bash

echo "Making the input data"
for n in "1" "100" # "1000" "5000" "10000" "20000"
do
	python3 input_gen.py 32 32 $n
done

for v in "cuFFT" "Regular" "Irregular"
do
	echo "Running Accelerate benchmarks for $v"
	for n in "1" "100" #"1000" "5000" "10000" "20000"
	do
		echo "n=$n"
		nvprof --csv -f -u ms --trace gpu --log-file data/result_fourier_$v\_$n.csv stack run fourier $v $n
	done
done

for v in "Futhark"
do
	echo "Running Futhark benchmarks"
	echo "Futhark says it does 9 runs, but actually it does one warmup run extra, which we do measure with nvprof"
	futhark cuda Futhark/fft-lib.fut
	for n in "1" "100" #"1000" "5000" "10000" "20000"
	do
		echo "n=$n"

		nvprof --csv -f --profile-child-processes -u ms --trace gpu --continuous-sampling-interval 1 --log-file data/result_fourier_$v\_$n\_%p.csv \
		  futhark bench -r 9 --skip-compilation --backend=cuda --exclude=$n Futhark/fft-lib.fut
		export filename=$(ls data/result_fourier_$v\_$n\_*)
		cat $filename > data/result_fourier_$v\_$n.csv
		rm $filename
	done
done

for v in "Normal"
do
	echo "Running Accelerate benchmarks for $v"
	for n in "1" "100" #"1000"
	do
		echo "n=$n"
		nvprof --csv -f -u ms --trace gpu --continuous-sampling-interval 1 --log-file data/result_fourier_$v\_$n.csv stack run fourier $v $n
	done
done

python3 process_csv.py

gnuplot fourier32x32.gnuplot
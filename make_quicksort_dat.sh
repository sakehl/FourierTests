#!/bin/bash


nums_m=("100" "1000" "10000")
#nums_m=("100")
nums_n=("1" "100" "1000" "2000" "5000" "10000")
#nums_n=("1")


function accelerate {
    v=$1
    m=$2
    n=$3
    echo "Running Accelerate benchmarks for $v and m=$m n=$n"
    nvprof --csv -f -u ms --trace gpu --log-file data/result_quicksort_$v\_$m\_$n.csv stack run quicksort $v $m $n
    #stack run quicksort $v $m $n
}

function futharkbench {
    v=$1
    m=$2
    n=$3
    echo "Running Futhark benchmarks for $v and m=$m n=$n"
    nvprof --csv -f --profile-child-processes -u ms --trace gpu --continuous-sampling-interval 1 --log-file data/result_quicksort_$v\_$m\_$n\_%p.csv \
		  futhark bench -r 9 --skip-compilation --backend=cuda --exclude=n$n --exclude=m$m Futhark/quicksort.fut
    #futhark bench -r 9 --skip-compilation --backend=cuda --exclude=n$n --exclude=m$m Futhark/quicksort.fut
    export filename=$(ls data/result_quicksort_$v\_$m\_$n\_*)
    cat $filename > data/result_quicksort_$v\_$m\_$n.csv
    rm $filename
}

echo "Making the input data"
for m in "${nums_m[@]}"
do
    for n in "${nums_n[@]}"
    do
        echo "m=$m, n=$n"
	    python3 input_gen.py $m $n
    done
done

for m in "${nums_m[@]}"
do
    for v in "Regular" "Irregular"
    do
	    for n in "${nums_n[@]}"
        do
		    accelerate $v $m $n
        done
	done
done

echo "Running Futhark benchmarks"
echo "Futhark says it does 9 runs, but actually it does one warmup run extra, which we do measure with nvprof"
futhark cuda Futhark/quicksort.fut
for m in "${nums_m[@]}"
do
    for v in "Futhark"
    do
	    
	    for n in "${nums_n[@]}"
	    do
		    futharkbench $v $m $n
	    done
    done
done

for m in "${nums_m[@]}"
do
    python3 process_csv.py quicksort $m
    gnuplot quicksort-$m.gnuplot
done
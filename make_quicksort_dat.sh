#!/bin/bash


nums_m=("100" "1000" "10000")
nums_n=("1" "100" "1000" "2000" "5000" "10000")


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

noinput="UNSET"
short="UNSET"
onlyn="UNSET"
onlym="UNSET"
while :; do
    case $1 in
		--no-input) noinput="SET"            
        ;;
        -s|--short) short="SET"
		;;
        -n|--n)
            if [ "$2" ]; then
                onlyn=$2
                shift
                re='^[0-9]+$'
				if ! [[ $onlyn =~ $re ]] ; then
				echo "error: $onlyn is Not a number" >&2; exit 1
				fi
            else
                die 'ERROR: "-n" requires a non-empty option argument.'
            fi
        ;;
        -m|--m)
            if [ "$2" ]; then
                onlym=$2
                shift
                re='^[0-9]+$'
				if ! [[ $onlym =~ $re ]] ; then
				echo "error: $onlym is Not a number" >&2; exit 1
				fi
            else
                die 'ERROR: "-m" requires a non-empty option argument.'
            fi
        ;;
        *) break
    esac
    shift
done

if [ $short = "SET" ]
then
nums_m=("100" "1000")
nums_n=("1" "100" "1000" "2000")
fi

if [ $onlyn != "UNSET" ]
then
nums_n=($onlyn)
fi

if [ $onlym != "UNSET" ]
then
nums_m=($m)
fi

if [ $noinput != "SET" ]
then
echo "Making the input data"
for m in "${nums_m[@]}"
do
    for n in "${nums_n[@]}"
    do
        echo "m=$m, n=$n"
	    python3 input_gen.py $m $n
    done
done
fi

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
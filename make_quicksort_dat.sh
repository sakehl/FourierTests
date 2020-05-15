#!/bin/bash


nums_m=("100" "1000" "10000")
nums_m_short=("100" "1000")
nums_n=("1" "100" "1000" "2000" "5000" "10000")
nums_n_short=("1" "100" "1000" "2000")
versions=("Regular" "Irregular")

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
onlyn="UNSET"
onlym="UNSET"
onlyfuthark="UNSET"
accelerateversion="UNSET"
short="UNSET"
while :; do
    case $1 in
		--no-input) noinput="SET"            
        ;;
        -s|--short) short="SET"
            nums_m=("${nums_m_short[@]}")
            nums_n=("${nums_n_short[@]}")
		;;
        -f|--futhark) onlyfuthark="SET"
		;;
        --regular) accelerateversion="Regular"
            versions=("Regular")
		;;
        --irregular) accelerateversion="Irregular"
            versions=("Irregular")
		;;
        -n|--n)
            if [ "$2" ]; then
                re='^[0-9]+$'
				if ! [[ $2 =~ $re ]] ; then
				echo "error: $2 is Not a number" >&2; exit 1
				fi
                nums_n=($2)
                shift
            else
                die 'ERROR: "-n" requires a non-empty option argument.'
            fi
        ;;
        -m|--m)
            if [ "$2" ]; then
                re='^[0-9]+$'
				if ! [[ $2 =~ $re ]] ; then
				echo "error: $2 is Not a number" >&2; exit 1
				fi
                nums_m=($2)
                shift
            else
                die 'ERROR: "-m" requires a non-empty option argument.'
            fi
        ;;
        *) break
    esac
    shift
done

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

################################################# Actual tests
if [ $accelerateversion = "UNSET" ]
then
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
fi


if [ $onlyfuthark = "UNSET" ]
then
for m in "${nums_m[@]}"
do
    for v in "${versions[@]}"
    do
	    for n in "${nums_n[@]}"
        do
            # Don't do irregular ones with n > 100 for short version, and for n> 1000 in general
            if [ $short != "SET" -o $v != "Irregular" -o $n -le 100  ] && [ $v != "Irregular" -o $n -le 1000 ]; then
		        accelerate $v $m $n
            fi
        done
	done
done
fi

for m in "${nums_m[@]}"
do
    if [ $short != "SET" ]; then
        python3 process_csv.py quicksort $m
    else
        python3 process_csv.py quicksort $m short
    fi
    gnuplot quicksort-$m.gnuplot
done
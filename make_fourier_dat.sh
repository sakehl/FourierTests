#!/bin/bash

nums=("1" "100" "1000" "5000" "10000" "20000")
numsshort=("1" "100" "1000")
versions=("cuFFT" "Regular" "Irregular")

function accelerate {
    v=$1
    n=$2
    echo "Running Accelerate benchmarks for $v and n=$n"
    nvprof --csv -f -u ms --trace gpu --log-file data/result_fourier_$v\_$n.csv stack run fourier $v $n
}

function futharkbench {
    v=$1
    n=$2
    echo "Running Futhark benchmarks for $v and n=$n"
    nvprof --csv -f --profile-child-processes -u ms --trace gpu --continuous-sampling-interval 1 --log-file data/result_fourier_$v\_$n\_%p.csv \
		  futhark bench -r 9 --skip-compilation --backend=cuda --exclude=$n Futhark/fft-lib.fut
    export filename=$(ls data/result_fourier_$v\_$n\_*)
    cat $filename > data/result_fourier_$v\_$n.csv
    rm $filename
}

noinput="UNSET"
short="UNSET"
onlyn="UNSET"
onlyfuthark="UNSET"
accelerateversion="UNSET"
while :; do
    case $1 in
		--no-input) noinput="SET"            
        ;;
        -s|--short) short="SET"
		;;
		--futhark) onlyfuthark="SET"
			versions=()
		;;
        --regular) accelerateversion="Regular"
			versions=("Regular")
		;;
        --irregular) accelerateversion="Irregular"
			versions=("Irregular")
		;;
		--cufft) accelerateversion="cuFFT"
			versions=("cuFFT")
		;;
		--normal) accelerateversion="Normal"
			versions=()
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
        *) break
    esac
    shift
done

if [ $short = "SET" ]
then
nums=("1" "100" "1000" "5000")
fi

if [ $onlyn != "UNSET" ]
then
nums=($onlyn)
	if [ $onlyn -gt 1000 ]
	then
		numsshort=()
	else
		numsshort=($onlyn)
	fi
fi

############################### ACTUAL TESTS

if [ $noinput != "SET" ]
then
echo "Making the input data"
for n in "${nums[@]}"
do
    echo "n=$n"
	python3 input_gen.py 32 32 $n
done
fi

if [ $onlyfuthark = "UNSET" ]
then
for v in "${versions[@]}"
do
	for n in "${nums[@]}"
	do
		accelerate $v $n
	done
done
fi

if [ $onlyfuthark = "UNSET" ] && ([ $accelerateversion = "UNSET" ] || [ $accelerateversion = "Normal" ])
then
for v in "Normal"
do
	for n in "${numsshort[@]}"
	do
		accelerate $v $n
	done
done
fi

if [ $accelerateversion = "UNSET" ]
then
for v in "Futhark"
do
	echo "Running Futhark benchmarks"
	echo "Futhark says it does 9 runs, but actually it does one warmup run extra, which we do measure with nvprof"
	futhark cuda Futhark/fft-lib.fut
	for n in "${nums[@]}"
	do
		futharkbench $v $n
	done
done
fi

exit 0

python3 process_csv.py fourier

gnuplot fourier32x32.gnuplot
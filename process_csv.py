#!/usr/bin/env python3

import csv
import sys
#from colorama import Fore, Style
from typing import Optional

ns = {"fourier": [1, 100, 1000, 5000, 10000, 20000], "quicksort" : [1, 100, 1000, 2000, 5000, 10000]}

ns_short = {"fourier": [1, 100, 1000, 5000], "quicksort" : [1, 100, 1000, 2000]}

#ns = [1,100]
versions = {"fourier": ["Regular", "cuFFT", "Irregular", "Normal", "Futhark"], "quicksort": ["Futhark", "Regular", "Irregular"]}


def process_time(algo: str, version: str, m: Optional[int], n: int) -> Optional[float]:
    perc = 0.0
    time = 0.0

    try:
        if algo == "fourier":
            fn = f'data/result_{algo}_{version}_{n}.csv'
        else:
            fn = f'data/result_{algo}_{version}_{m}_{n}.csv'
        with open(fn) as csvfile:
            timings = csv.reader(csvfile)
            for row in timings:
                if len(row) > 2:
                    # Only the GPU activities are the actual kernel times
                    if row[0].startswith("GPU activities"):
                        perc += float(row[1])
                        time += float(row[2])

        if abs(perc - 100.0) > 0.1:
            print(f"WARNING: total percentage ({perc:f}) isn't close to 100% for {version} with {n}")
            print(f"WARNING: not including the result for {version} with {n}. \n Inspect {fn} or rerun the specific benchmark ")
            return None

        return time
    except FileNotFoundError:
        print(f"WARNING: file for {version} with {n} iterations not found")
        return None



def make_dat_file(algo: str, m: Optional[int] = None):
    results = dict()
    for i in ns[algo]:
        results[i] = dict()
        for v in versions[algo]:
            # We don't run normal above 1000, since it will take to long
            if algo == "fourier" and v == "Normal" and i > 1000:
                results[i][v] = "DNF"
            #Same for short runs for n >100 for irregular
            elif algo == "fourier" and v == "Irregular" and short and i > 100:
                results[i][v] = "DNF"
            #Same for quicksort for n >1000 for irregular
            elif algo == "quicksort" and v == "Irregular" and (i > 1000 or (short and i > 100)) :
                results[i][v] = "DNF"
            else:
                t = process_time(algo,v, m, i)
                # Each experiment is ran exactly 10 times, so we take the average of 10 runs
                if t == None:
                    results[i][v] = "DNF"
                else:
                    results[i][v] = t / 10
    #Write output to be used in gnuplot
    if algo == "fourier":
        fn = "data/fourier32x32.dat"
    else:
        fn = f"data/quicksort-{m}.dat"
    with open(fn,'w') as f:
        if algo == "fourier":
            f.write("# Arrays of size 32 x 32\n")
        else:
            f.write(f"# List of size {m}\n")
        f.write("# n\t")
        f.write('\t'.join(versions[algo]) + '\n')
        for i in ns[algo]:
            f.write(str(i) + "\t")
            for v in versions[algo]:
                f.write(str(results[i][v]) + "\t")
            f.write("\n")


short = False
algorithm = sys.argv[1]

if algorithm == "fourier":
    if len(sys.argv) > 2:
        if sys.argv[2] == "short":
            ns=ns_short
            short=True
    make_dat_file(algorithm)
elif algorithm == "quicksort":
    if len(sys.argv) > 3:
        if sys.argv[3] == "short":
            ns=ns_short
            short=True
    m = int(sys.argv[2])
    make_dat_file(algorithm, m)
else:
    print(f"Name '{algorithm}' is not recognized as an option to process'")


#!/usr/bin/env python3

import csv
#from colorama import Fore, Style

#ns = [1, 100, 1000, 5000, 10000, 20000]
ns = [1,100]
versions = ["Regular", "cuFFT", "Irregular", "Normal", "Futhark"]


def process_time(version: str, n: int) -> float:
	perc = 0.0
	time = 0.0

	try:
		with open(f'data/result_fourier_{version}_{n}.csv') as csvfile:
		    timings = csv.reader(csvfile)
		    for row in timings:
		    	if len(row) > 2:
		    		# Only the GPU activities are the actual kernel times
		    		if row[0].startswith("GPU activities"):
		    			perc += float(row[1])
		    			time += float(row[2])

		if abs(perc - 100.0) > 0.1:
			print(f"WARNING: total percentage ({perc:f}) isn't close to 100% for {version} with {n}")

		return time
	except FileNotFoundError:
		print(f"WARNING: file for {version} with {n} iterations not found")
		return None

results = dict()
for i in ns:
	results[i] = dict()
	for v in versions:
		# We don't run normal above 1000, since it will take to long
		if v == "Normal" and i > 1000:
			results[i][v] = "DNF"
		# Irregular was't working for 10000
		elif v =="Irregular" and i == 10000:
			results[i][v] = "DNF"
		else:
			t = process_time(v, i)
			# Each experiment is ran exactly 10 times, so we take the average of 10 runs
			if t == None:
				results[i][v] = "DNF"
			else:
				results[i][v] = t / 10

#Write output to be used in gnuplot
with open("data/fourier32x32.dat",'w') as f:
	f.write("# Arrays of size 32 x 32\n")
	f.write("# n\t")
	f.write('\t'.join(versions) + '\n')
	for i in ns:
		f.write(str(i) + "\t")
		for v in versions:
			f.write(str(results[i][v]) + "\t")
		f.write("\n")
# vim: filetype=gnuplot tw=0 nospell

set title "Quicksort (m = 10000)"

set terminal pdf size 4,3
set output "data/quicksort-10000.pdf"

set key on
set key bottom right invert

set xlabel "# Arrays (n)"
set logscale x
set format x "10^{%L}"
# set xrange [1:25]

# set xtics (4, 8, 12, 16, 20, 24)
# set xtics (2, 4, 6, 8, 10, 12, 14, 16, 18, 20)

# set colorsequence classic   # gnuplot >= 5
set ylabel "Time (ms)"
set logscale y
set format y "10^{%L}"
# set yrange [1:10000]

plot  'data/quicksort-10000.dat' using 1:3 title "Accelerate, Regular" ls 1 lw 3 with linespoints, \
      'data/quicksort-10000.dat' using 1:4 title "Accelerate"          ls 2 lw 3 with linespoints, \
      'data/quicksort-10000.dat' using 1:2 title "Futhark"             ls 3 lw 3 with linespoints


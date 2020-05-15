# vim: filetype=gnuplot tw=0 nospell

set title "Fourier transform (32 x 32)"

set terminal pdf size 4,3
set output "fourier32x32.pdf"

set key on
set key bottom right

set xlabel "# Arrays"
set logscale x
set format x "10^{%L}"
# set xrange [1:25]
# set xrange [1:40000]

# set xtics (4, 8, 12, 16, 20, 24)
# set xtics (2, 4, 6, 8, 10, 12, 14, 16, 18, 20)

# set colorsequence classic   # gnuplot >= 5
set ylabel "Time (ms)"
set logscale y
set format y "10^{%L}"
# set yrange [0.4:150]

plot  'fourier32x32.dat' using 1:3 title "cuFFT"              ls 4 lw 3 with linespoints, \
      'fourier32x32.dat' using 1:6 title "Futhark"            ls 3 lw 3 with linespoints, \
      'fourier32x32.dat' using 1:5 title "Normal"             ls 5 lw 3 with linespoints, \
      'fourier32x32.dat' using 1:4 title "Accelerate"         ls 2 lw 3 with linespoints, \
      'fourier32x32.dat' using 1:2 title "Acelerate, Regular" ls 1 lw 3 with linespoints, \


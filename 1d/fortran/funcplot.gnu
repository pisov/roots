f(x) = x**2 + 200 * x - 0.000015
set grid
set xlabel "x"
set ylabel "f(x)"
set size square
plot [-210:10] f(x) w l
replot 0
pause -1

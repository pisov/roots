set size square

set xrange [ -1 : 1 ]
set yrange [ -4 : 1 ]


set view map
set contour
unset surface
set cntrparam order 8
set cntrparam bspline
set cntrparam levels incremental 0,1,8 
set cntrparam points 5

set xlabel "x"
set ylabel "y"
set key outside

f1(x1,x2) =  x1+x2-x1*x2+2
f2(x1,x2) = x1*exp(-x2)-1
f(x1,x2) = sqrt(f1(x1,x2)**2+f2(x1,x2)**2)

splot f(x,y) 

pause -1

1. Compile

gfortran -O3 newton.f90 -o newton.x -llapack

2. Execute x1 = 0.5, x2 = 0.4

./newton.x > points.dat

3. Plot the minimization steps

gnuplot plot.gnu

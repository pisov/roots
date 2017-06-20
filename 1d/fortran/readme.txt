#Example equation

        2
F(x) = x  + 200 x - 0.000015

Roots: x1 = 7.5e-8
       x2 = -200.000000075


# Task 0 Plot the function first and try to localize the roots

gnuplot funcplot.gnu

# Task 1 Compile Brent example

1.1 Compile the example

gfortran rootfind.f90 zbrac.f90 zbrent.f90 -o rootfind.x

1.2 Execute the code with input values x1 = -1. x2 = 0. tol = 1.e-5 and checkout the result

./rootfind.x

1.3 Try to improve the accuracy changing the tol variable

1.4 Change the initial boundaries in order to search second root x2 of equiation F(x)


#Task 2 Newton-Raphson metod

2.1 Use template file rootfind_newrft.f90 and modify subroutine quaddev in order to implement the Newton-Raphson algorithm

Try tasks 1.3 and 1.4

3. Can we improve the accuracy? How? 


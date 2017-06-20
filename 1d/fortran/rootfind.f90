module functions
implicit none
contains
  function quad(x)
    real :: quad
    real, intent(in) :: x

    quad = x ** 2 + 200.e0 * x - .15e-4
  end function quad
end module functions
program rootfind
use functions
implicit none
interface
  FUNCTION zbrent(func,x1,x2,tol)
    IMPLICIT NONE
    REAL, INTENT(IN) :: x1,x2,tol
    REAL :: zbrent
    INTERFACE
      FUNCTION func(x)
      IMPLICIT NONE
      REAL, INTENT(IN) :: x
      REAL :: func
      END FUNCTION func
    END INTERFACE
  END FUNCTION zbrent
end interface

real :: x1, x2, x, tol
logical :: success

write(0,'(A33)',advance='no')'Please enter left point  x1 = '
read(*,*)x1

write(0,'(A33)',advance='no')'Please enter right point x2 = '
read(*,*)x2

write(0,'(A33)',advance='no')'Please enter tolerance  tol = '
read(*,*)tol

if ( (x2 <= x1).or.(abs(tol)>abs(x2-x1)).or.(tol <= 0.e0)) then
  write(0,*)'Incorect input values !!!'
  stop
end if

call zbrac(quad, x1, x2, success)

if ( success.eqv..true. ) then
  write(0,'(A45,E25.15,A1,E25.15,A1)')'Function has been bracketed between [',x1,',',x2,']'
else
  write(0,*)'Root was not found'
  stop 
end if

x = zbrent(quad, x1, x2, tol)

write(0,'(A45,E25.15,A1,E25.15,A1,E25.15,A1)')'Brent has found [',x1,',',x,',',x2,']'

end program

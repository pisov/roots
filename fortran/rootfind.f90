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

x1 = -1.e0
x2 = 0.e0

tol = 1.e-5

call zbrac(quad, x1, x2, success)

if ( success.eqv..true. ) then
  write(0,'(A45,F7.3,A1,F7.3,A1)')'Function has been bracketed between [',x1,',',x2,']'
else
  write(0,*)'Root was not found'
  stop 
end if

x = zbrent(quad, x1, x2, tol)

write(0,'(A45,F7.3,A1,E15.5,A1,F7.3,A1)')'Brent has found [',x1,',',x,',',x2,']'

end program

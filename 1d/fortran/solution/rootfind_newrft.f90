module functions
implicit none
contains
  function quad(x)
    real :: quad
    real, intent(in) :: x

    quad = x ** 2 + 200.e0 * x - .15e-4
  end function quad
  subroutine quaddev(x, fval,fderiv)
    double precision, intent(in)  :: x
    double precision, intent(out) :: fval,fderiv

!   Encode the actual calculation of f(x)
    fval = x ** 2 + 200.d0 * x - .15d-4 
!   Encode the first derivative of f'(x)
    fderiv = 2 * x + 200.d0

  end subroutine quaddev
end module functions
program rootfind
use functions
implicit none
interface
  FUNCTION rtnewt(funcd,x1,x2,xacc)
    IMPLICIT NONE
    DOUBLE PRECISION, INTENT(IN) :: x1,x2,xacc
    DOUBLE PRECISION :: rtnewt
    INTERFACE
      SUBROUTINE funcd(x,fval,fderiv)
        IMPLICIT NONE
        DOUBLE PRECISION, INTENT(IN) :: x
        DOUBLE PRECISION, INTENT(OUT) :: fval,fderiv
      END SUBROUTINE funcd
    END INTERFACE
  END FUNCTION rtnewt
end interface

double precision :: x1, x2, x, tol
real :: x1sp, x2sp
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

x1sp = REAL(x1)
x2sp = REAL(x2)

call zbrac(quad, x1sp, x2sp, success)

x1 = DBLE(x1sp)
x2 = DBLE(x2sp)

if ( success.eqv..true. ) then
  write(0,'(A45,D25.15,A1,D25.15,A1)')'Function has been bracketed between [',x1,',',x2,']'
else
  write(0,*)'Root was not found'
  stop 
end if

!
! Here you should add call to rtnewt
!

x = rtnewt(quaddev, x1, x2, tol)

write(0,'(A45,D25.15,A1,D25.15,A1,D25.15,A1)')'Brent has found [',x1,',',x,',',x2,']'

end program

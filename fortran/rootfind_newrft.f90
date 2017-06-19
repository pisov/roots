module functions
implicit none
contains
  function quad(x)
    real :: quad
    real, intent(in) :: x

    quad = x ** 2 + 200.e0 * x - .15e-5
  end function quad
  subroutine quaddev(x, fval,fderiv)
    real, intent(in)  :: x
    real, intent(out) :: fval,fderiv

    fval = .0e0
    fderiv = .0e0

  end subroutine quaddev
end module functions
program rootfind
use functions
implicit none
interface
  FUNCTION rtnewt(funcd,x1,x2,xacc)
    IMPLICIT NONE
    REAL, INTENT(IN) :: x1,x2,xacc
    REAL :: rtnewt
    INTERFACE
      SUBROUTINE funcd(x,fval,fderiv)
        IMPLICIT NONE
        REAL, INTENT(IN) :: x
        REAL, INTENT(OUT) :: fval,fderiv
      END SUBROUTINE funcd
    END INTERFACE
  END FUNCTION rtnewt
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


end program

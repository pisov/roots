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

    fval = x ** 2 + 200.e0 * x - .15e-5 
    fderiv = 2 * x + 200.e0

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

x1 = -210.e0
x2 = -190.e0

tol = 1.e-8

call zbrac(quad, x1, x2, success)

if ( success.eqv..true. ) then
  write(0,'(A45,F7.3,A1,F7.3,A1)')'Function has been bracketed between [',x1,',',x2,']'
else
  write(0,*)'Root was not found'
  stop 
end if

x = rtnewt(quaddev,x1,x2,tol); 

write(0,'(A45,F7.3,A1,E15.5,A1,F7.3,A1)')'Brent has found [',x1,',',x,',',x2,']'

end program

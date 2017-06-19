                SUBROUTINE zbrac(func,x1,x2,succes)
                
                IMPLICIT NONE
                REAL, INTENT(INOUT) :: x1,x2
                LOGICAL, INTENT(OUT) :: succes
                INTERFACE
                        FUNCTION func(x)
                        IMPLICIT NONE
                        REAL, INTENT(IN) :: x
                        REAL :: func
                        END FUNCTION func
                END INTERFACE
                INTEGER, PARAMETER :: NTRY=50
                REAL, PARAMETER :: FACTOR=1.6e0
                INTEGER :: j
                REAL :: f1,f2
                if (x1 == x2) then
                        write(0,*)'zbrac: you have to guess an initial range'
                        stop 
                end if
		f1=func(x1)
                f2=func(x2)
                succes=.true.
                do j=1,NTRY
                        if ((f1 > 0.0 .and. f2 < 0.0) .or. &
                                (f1 < 0.0 .and. f2 > 0.0)) RETURN
                        if (abs(f1) < abs(f2)) then
                                x1=x1+FACTOR*(x1-x2)
                                f1=func(x1)
                        else
                                x2=x2+FACTOR*(x2-x1)
                                f2=func(x2)
                        end if
                end do
                succes=.false.
                END SUBROUTINE zbrac

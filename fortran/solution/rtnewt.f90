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
                INTEGER, PARAMETER :: MAXIT=20
                INTEGER :: j
                REAL :: df,dx,f
                rtnewt=0.5*(x1+x2)
                do j=1,MAXIT
                        call funcd(rtnewt,f,df)
                        dx=f/df
                        rtnewt=rtnewt-dx
                        if ((x1-rtnewt)*(rtnewt-x2) < 0.0) then
                                write(0,*)'rtnewt: values jumped out of brackets'
                                STOP
                        end if
                        if (abs(dx) < xacc) RETURN
                end do
                write(0,*)'rtnewt exceeded maximum iterations'
                STOP
                END FUNCTION rtnewt

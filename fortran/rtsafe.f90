        FUNCTION rtsafe(funcd,x1,x2,xacc)
                IMPLICIT NONE
                REAL, INTENT(IN) :: x1,x2,xacc
                REAL :: rtsafe
                INTERFACE
                        SUBROUTINE funcd(x,fval,fderiv)
                        IMPLICIT NONE
                        REAL, INTENT(IN) :: x
                        REAL, INTENT(OUT) :: fval,fderiv
                        END SUBROUTINE funcd
                END INTERFACE
                INTEGER, PARAMETER :: MAXIT=100
                INTEGER :: j
                REAL :: df,dx,dxold,f,fh,fl,temp,xh,xl
                call funcd(x1,fl,df)
                call funcd(x2,fh,df)
                if ((fl > 0.0 .and. fh > 0.0) .or. &
                        (fl < 0.0 .and. fh < 0.0)) then
                        write(0,*)'root must be bracketed in rtsafe'
                        STOP
                end if
                if (fl == 0.0) then
                        rtsafe=x1
                        RETURN
                else if (fh == 0.0) then
                        rtsafe=x2
                        RETURN
                else if (fl < 0.0) then
                        xl=x1
                        xh=x2
                else
                        xh=x1
                        xl=x2
                end if
                rtsafe=0.5*(x1+x2)
                dxold=abs(x2-x1)
                dx=dxold
                call funcd(rtsafe,f,df)
                do j=1,MAXIT
                        if (((rtsafe-xh)*df-f)*((rtsafe-xl)*df-f) >= 0.0 .or. &
                                abs(2.0*f) > abs(dxold*df) ) then
                                dxold=dx
                                dx=0.5*(xh-xl)
                                rtsafe=xl+dx
                                if (xl == rtsafe) RETURN
                        else
                                dxold=dx
                                dx=f/df
                                temp=rtsafe
                                rtsafe=rtsafe-dx
                                if (temp == rtsafe) RETURN
                        end if
                        if (abs(dx) < xacc) RETURN
                        call funcd(rtsafe,f,df)
                        if (f < 0.0) then
                                xl=rtsafe
                        else
                                xh=rtsafe
                        end if
                end do
                WRITE(0,*)'rtsafe: exceeded maximum iterations'
                STOP
                END FUNCTION rtsafe

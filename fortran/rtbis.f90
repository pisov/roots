                FUNCTION rtbis(func,x1,x2,xacc)
                IMPLICIT NONE
                REAL, INTENT(IN) :: x1,x2,xacc
                REAL :: rtbis
                INTERFACE
                        FUNCTION func(x)
                        IMPLICIT NONE
                        REAL, INTENT(IN) :: x
                        REAL :: func
                        END FUNCTION func
                END INTERFACE
                INTEGER, PARAMETER :: MAXIT=40
                INTEGER :: j
                REAL :: dx,f,fmid,xmid
                fmid=func(x2)
                f=func(x1)
                if (f*fmid >= 0.0) then
                        write(0,*)'rtbis: root must be bracketed'
                        stop
                end if
                if (f < 0.0) then
                        rtbis=x1
                        dx=x2-x1
                else
                        rtbis=x2
                        dx=x1-x2
                end if
                do j=1,MAXIT
                        dx=dx*0.5e0
                        xmid=rtbis+dx
                        fmid=func(xmid)
                        if (fmid <= 0.0) rtbis=xmid
                        if (abs(dx) < xacc .or. fmid == 0.0) RETURN
                end do
                write(0,*) 'rtbis: too many bisections'
                END FUNCTION rtbis

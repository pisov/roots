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
                INTEGER, PARAMETER :: ITMAX=100
                REAL, PARAMETER :: EPS=epsilon(x1)
                INTEGER :: iter
                REAL :: a,b,c,d,e,fa,fb,fc,p,q,r,s,tol1,xm
                a=x1
                b=x2
                fa=func(a)
                fb=func(b)
                if ((fa > 0.0 .and. fb > 0.0) .or. (fa < 0.0 .and. fb < 0.0)) then
                        write(0,*)'root must be bracketed for zbrent'
                        STOP
                end if
                c=b
                fc=fb
                do iter=1,ITMAX
                        if ((fb > 0.0 .and. fc > 0.0) .or. (fb < 0.0 .and. fc < 0.0)) then
                                c=a
                                fc=fa
                                d=b-a
                                e=d
                        end if
                        if (abs(fc) < abs(fb)) then
                                a=b
                                b=c
                                c=a
                                fa=fb
                                fb=fc
                                fc=fa
                        end if
                        tol1=2.0*EPS*abs(b)+0.5*tol
                        xm=0.5*(c-b)
                        if (abs(xm) <= tol1 .or. fb == 0.0) then
                                zbrent=b
                                RETURN
                        end if
                        if (abs(e) >= tol1 .and. abs(fa) > abs(fb)) then
                                s=fb/fa
                                if (a == c) then
                                        p=2.0*xm*s
                                        q=1.0-s
                                else
                                        q=fa/fc
                                        r=fb/fc
                                        p=s*(2.0*xm*q*(q-r)-(b-a)*(r-1.0))
                                        q=(q-1.0)*(r-1.0)*(s-1.0)
                                end if
                                if (p > 0.0) q=-q
                                p=abs(p)
                                if (2.0*p  < min(3.0*xm*q-abs(tol1*q),abs(e*q))) then
                                        e=d
                                        d=p/q
                                else
                                        d=xm
                                        e=d
                                end if
                        else
                                d=xm
                                e=d
                        end if
                        a=b
                        fa=fb
                        b=b+merge(d,sign(tol1,xm), abs(d) > tol1 )
                        fb=func(b)
                end do
                write(0,*)'zbrent: exceeded maximum iterations'
                zbrent=b
                END FUNCTION zbrent

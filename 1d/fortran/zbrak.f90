                SUBROUTINE zbrak(func,x1,x2,n,xb1,xb2,nb)               
                IMPLICIT NONE
                INTEGER, INTENT(IN) :: n
                INTEGER, INTENT(OUT) :: nb
                REAL, INTENT(IN) :: x1,x2
                REAL, DIMENSION(:), POINTER :: xb1,xb2
                INTERFACE
                        FUNCTION func(x)                
                        IMPLICIT NONE
                        REAL, INTENT(IN) :: x
                        REAL :: func
                        END FUNCTION func
                END INTERFACE
                INTEGER :: i
                REAL :: dx
                REAL, DIMENSION(0:n) :: f,x
                LOGICAL, DIMENSION(1:n) :: mask
                LOGICAL, SAVE :: init=.true.
                if (init) then
                        init=.false.
                        nullify(xb1,xb2)
                end if
                if (associated(xb1)) deallocate(xb1)
                if (associated(xb2)) deallocate(xb2)
                dx=(x2-x1)/n
 !               x=x1+dx*arth(0,1,n+1)
                do i = 0, n
                  x(i) = x1 + dx*i
                end do
                do i=0,n
                        f(i)=func(x(i))
                end do
                mask=f(1:n)*f(0:n-1) <= 0.0
                nb=count(mask)
                allocate(xb1(nb),xb2(nb))
                xb1(1:nb)=pack(x(0:n-1),mask)
                xb2(1:nb)=pack(x(1:n),mask)
                END SUBROUTINE zbrak

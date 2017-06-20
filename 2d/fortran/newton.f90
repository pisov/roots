program newton
  implicit none

  interface
    function func(x)
       double precision, dimension(:), intent(in) :: x
       double precision, dimension(size(x)) :: func
    end function func
    subroutine calc_jacobi(x, jmat)
      double precision, dimension(:), intent(in) :: x
      double precision, dimension(:,:), intent(inout) :: jmat
    end subroutine calc_jacobi
  end interface

  integer, parameter :: n = 2, maxit = 10
  double precision, parameter :: eps = 1.d-5

  double precision, dimension(n) :: x, d, f
  integer, dimension(n) :: work
  double precision, dimension(n, n) :: jmat
  integer :: i, j, it, info

  do i = 1, n
    write(0,'(A2,I1,A3)')'x(',i,')='
    read(*,*)x(i)
  end do

  it = 1
  do
    d(:) = func(x)
    call calc_jacobi(x, jmat)
    !do i = 1, n
    !  write(0,'(2F20.7)')(jmat(i,j),j=1,n)
    !end do
    call dgesv(n, 1, jmat, n, work, d, n, info)
    write(0,'(4F20.7)')(x(i),i=1,n),(d(i),i=1,n)
    x(:) = x(:) + d(:)
    f(:) = func(x)
    if ((it.gt.maxit).or.(info.ne.0)) exit
!    write(0,'(4F20.7)')(x(i),i=1,n),(f(i),i=1,n)
    it = it + 1
  end do
  
  if (info.ne.0) then
    write(0,*)'Failed to find new step: ',info
  end if
end program newton

function func(x)
  implicit none
  double precision, dimension(:), intent(in) :: x
  double precision, dimension(size(x)) :: func

  double precision :: x1, x2

  func(1) = x(1) + x(2) - x(1)*x(2) + 2.d0
  func(2) = x(1)*exp(-x(2)) - 1.d0
end function func

subroutine calc_jacobi(x, jmat)
  implicit none
  double precision, dimension(:), intent(in) :: x
  double precision, dimension(:,:), intent(inout) :: jmat

  integer :: i, n

  n = size(x)

  jmat(1, 1) = 1.d0 - x(1) 
  jmat(1, 2) = 1.d0 - x(2)
  jmat(2, 1) = exp(-x(2))
  jmat(2, 2) = -x(1)*exp(-x(2))
end subroutine calc_jacobi

module libMath

  implicit none
  integer, parameter :: dp = kind(1.d0)
  real(dp), parameter :: pi = atan(1._dp)*4._dp
  real(dp), parameter :: eps = epsilon(1._dp)

contains

  ! -------------------------------------------------
  !                cross3
  ! -------------------------------------------------
  function cross3(avec,bvec)
    real(dp), intent(in), dimension(3) :: avec,bvec
    real(dp), dimension(3) :: cross3

    cross3(1) = avec(2)*bvec(3)-avec(3)*bvec(2)
    cross3(2) = avec(3)*bvec(1)-avec(1)*bvec(3)
    cross3(3) = avec(1)*bvec(2)-avec(2)*bvec(1)

  end function cross3

  ! -------------------------------------------------
  !                linspace
  ! -------------------------------------------------
  function linspace(xstart,xend,nx) result(xout)
    real(dp), intent(in) :: xstart, xend
    integer , intent(in) :: nx
    real(dp), dimension(nx) :: xout
    integer :: i
    real(dp) :: dx

    dx = (xend-xstart)/(nx-1)

    xout = (/((i*dx),i=0,nx-1)/)
    xout = xout+xstart

  end function linspace

  ! -------------------------------------------------
  !                cosspace
  ! -------------------------------------------------
  function cosspace(xstart,xend,nx) result(xout)
    real(dp), intent(in) :: xstart, xend
    integer , intent(in) :: nx
    real(dp), dimension(nx) :: xout
    real(dp), dimension(nx) :: theta_spacing

    theta_spacing=linspace(0._dp,pi,nx)
    xout=xstart+(xend-xstart)*0.5_dp*(1._dp-cos(theta_spacing))

  end function cosspace

  ! -------------------------------------------------
  !                halfsinspace
  ! -------------------------------------------------
  function halfsinspace(xstart,xend,nx) result(xout)
    real(dp), intent(in) :: xstart, xend
    integer , intent(in) :: nx
    real(dp), dimension(nx) :: xout
    real(dp), dimension(nx) :: theta_spacing

    theta_spacing=linspace(0._dp,pi*0.5_dp,nx)
    xout=xstart+(xend-xstart)*sin(theta_spacing)

  end function halfsinspace

end module libMath

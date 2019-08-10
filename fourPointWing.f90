program fourPointWing
  use libMath
  implicit none
  integer, parameter :: nc = 15
  integer, parameter :: ns = 40
  real(dp), dimension(3,nc+1,2*ns+1) :: PC

  integer :: i,j,ic,is
  real(dp), dimension(3) :: P1,P2,P3,P4

  ! Schematic of wing
  !     
  !   O----------------> Y
  !   |
  !   |  P1--------P4
  !   |  |          |
  !   |  |          |
  !   |  |          |
  !   |  P2--------P3
  !   |
  ! X V 

  ! Input corners of wing
  P1=(/0.0_dp,0.0_dp,0.0_dp/)
  P2=(/0.0_dp,0.0_dp,0.0_dp/)
  P3=(/0.0_dp,0.0_dp,0.0_dp/)
  P4=(/0.0_dp,0.0_dp,0.0_dp/)

  ! Construct outer mesh of right wing
  do i=1,3
    PC(i,:,ns+1)   = linspace(P1(i),P2(i),nc+1)
    PC(i,nc+1,:)   = linspace(P2(i),P3(i),ns+1)
    PC(i,:,2*ns+1) = linspace(P4(i),P3(i),nc+1)
    PC(i,1,:)      = linspace(P1(i),P4(i),ns+1)
  enddo

  ! Construct inner mesh right wing
  do i=1,3
    do is=ns+1,2*ns+1
      PC(i,:,is) = linspace(PC(i,1,j),PC(i,nc+1,j),nc+1)
    enddo
  enddo

  ! Write to file in PLOT3D format
  !open(unit=11,file='rotor01.xyz')
  !write(11,*) nc+1,ns+1,1
  !write(11,'(3E15.7)') &
  !  ((PC(1,ic,is),ic=1,nc+1),is=5,2*ns+1), &
  !  ((PC(2,ic,is),ic=1,nc+1),is=5,2*ns+1), &
  !  ((PC(3,ic,is),ic=1,nc+1),is=5,2*ns+1)
  !close(11)

  ! Write to file in PLOT3D format
  open(unit=11,file='rotor01.xyz')
  write(11,*) nc+1,2*ns+1,1
  write(11,'(3E15.7)') &
    ((PC(1,ic,is),ic=1,nc+1),is=1,2*ns+1), &
    ((PC(2,ic,is),ic=1,nc+1),is=1,2*ns+1), &
    ((PC(3,ic,is),ic=1,nc+1),is=1,2*ns+1)
  close(11)
end program fourPointWing

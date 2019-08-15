program fourPointWing
  use libMath
  implicit none
  integer, parameter :: nc = 10  ! per semispan
  integer, parameter :: ns = 16  ! per semispan
  real(dp), dimension(3,nc+1,2*ns+1) :: PC

  integer :: i,ic,is
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
  ! Warren-12
  P1=(/00.00000_dp,00.00000_dp,00.000_dp/)
  P2=(/01.50000_dp,00.00000_dp,00.000_dp/)
  P3=(/02.41421_dp,01.41421_dp,00.000_dp/)
  P4=(/01.91421_dp,01.41421_dp,00.000_dp/)

  ! TR-1208
  !P1=(/00.000_dp,00.000_dp,00.000_dp/)
  !P2=(/21.941_dp,00.000_dp,00.000_dp/)
  !P3=(/76.520_dp,63.630_dp,00.000_dp/)
  !P4=(/66.647_dp,63.630_dp,00.000_dp/)

  ! Construct LE and TE of right wing
  do i=1,3
    PC(i,nc+1,ns+1:2*ns+1)   = linspace(P2(i),P3(i),ns+1)
    PC(i,1,ns+1:2*ns+1)      = linspace(P1(i),P4(i),ns+1)
  enddo

  ! Construct inner mesh right wing
  do i=1,3
    do is=ns+1,2*ns+1
      PC(i,:,is) = linspace(PC(i,1,is),PC(i,nc+1,is),nc+1)
    enddo
  enddo

  ! Mirror right wing to left wing
    do is=1,ns
      PC(1,:,is) = PC(1,:,(2*ns+1)-is+1)
      PC(2,:,is) = PC(2,:,(2*ns+1)-is+1)*-1_dp
      PC(3,:,is) = PC(3,:,(2*ns+1)-is+1)
    enddo

  ! Write right wing to file in PLOT3D format
  !open(unit=11,file='rotor01.xyz')
  !write(11,*) nc+1,ns+1,1
  !write(11,'(3E15.7)') &
  !  ((PC(1,ic,is),ic=1,nc+1),is=ns+1,2*ns+1), &
  !  ((PC(2,ic,is),ic=1,nc+1),is=ns+1,2*ns+1), &
  !  ((PC(3,ic,is),ic=1,nc+1),is=ns+1,2*ns+1)
  !close(11)

  ! Write both wings to file in PLOT3D format
  open(unit=11,file='output.xyz')
  write(11,*) nc+1,2*ns+1,1
  write(11,'(3E15.7)') &
    ((PC(1,ic,is),ic=1,nc+1),is=1,2*ns+1), &
    ((PC(2,ic,is),ic=1,nc+1),is=1,2*ns+1), &
    ((PC(3,ic,is),ic=1,nc+1),is=1,2*ns+1)
  close(11)
end program fourPointWing

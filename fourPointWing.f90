program fourPointWing
  use libMath
  implicit none
  integer, parameter :: nc = 12  ! No. of chordwise panels per semispan
  integer, parameter :: ns = 30  ! No. of spanwise panels per semispan

  integer :: nx, ny, nyStart, nz  ! No. of grid points
  real(dp), dimension(3,nc+1,2*ns+1) :: PC
  integer :: i,ic,is
  real(dp), dimension(3) :: P1,P2,P3,P4
  real(dp) :: sweep_rad, semispan, rootChord, tipChord
  character(len=4) :: airfoil
  real(dp) :: camberM  ! Max camber
  real(dp) :: camberP  ! Position of max camber
  real(dp) :: localChord

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
  sweep_rad = 00._dp*pi/180._dp
  semispan = 6.0_dp
  rootChord = 1.0_dp
  tipChord = 1.0_dp

  ! Input four digit airfoil
  ! NACA MPXX
  airfoil = "6409"

  read(airfoil(1:1),*) camberM
  camberM = camberM/100._dp
  read(airfoil(2:2),*) camberP
  camberP = camberP/10._dp

  ! Rectangular swept wing
  P1=(/0.0000_dp,0.0000_dp,0.0000_dp/)
  P2=(/rootChord,0.0000_dp,0.0000_dp/)
  P3=(/tipChord+semispan*tan(sweep_rad),semispan,0.0000_dp/)
  P4=(/0.0000_dp+semispan*tan(sweep_rad),semispan,0.0000_dp/)

  ! Rectangular wing
  !P1=(/0.0000_dp,0.0000_dp,0.0000_dp/)
  !P2=(/0.3048_dp,0.0000_dp,0.0000_dp/)
  !P3=(/0.3048_dp,0.4313_dp,0.0000_dp/)
  !P4=(/0.0000_dp,0.4313_dp,0.0000_dp/)

  ! Warren-12
  !P1=(/0.0000_dp,0.0000_dp,0.0000_dp/)
  !P2=(/0.4572_dp,0.0000_dp,0.0000_dp/)
  !P3=(/0.7346_dp,0.4313_dp,0.0000_dp/)
  !P4=(/0.5822_dp,0.4313_dp,0.0000_dp/)

  ! TR-1208
  !P1=(/00.000_dp,00.000_dp,00.000_dp/)
  !P2=(/21.941_dp,00.000_dp,00.000_dp/)
  !P3=(/76.520_dp,63.630_dp,00.000_dp/)
  !P4=(/66.647_dp,63.630_dp,00.000_dp/)

  ! Print out area
  print*,'Full span Area = ',norm2(cross3(P3-P1,P4-P2))

  ! Construct LE and TE of right wing
  do i=1,3
    PC(i,nc+1,ns+1:2*ns+1)   = cosspace(P2(i),P3(i),ns+1)
    PC(i,1,ns+1:2*ns+1)      = cosspace(P1(i),P4(i),ns+1)
  enddo

  ! Construct inner mesh of right wing
  do i=1,3
    do is=ns+1,2*ns+1
      PC(i,:,is) = cosspace(PC(i,1,is),PC(i,nc+1,is),nc+1)
    enddo
  enddo
  if (camberM > 0._dp) then
    do is=ns+1,2*ns+1
      localChord = PC(1,nc+1,is)-PC(1,1,is)
      do ic=1,nc+1
        if (PC(1,ic,is)/localChord .le. camberP) then
          PC(3,ic,is) = localChord*camberM/camberP**2._dp &
            *(2._dp*camberP*PC(1,ic,is)-PC(1,ic,is)**2._dp)
        else 
          PC(3,ic,is) = localChord*camberM/(1._dp-camberP)**2._dp &
            *(1._dp-2._dp*camberP+2._dp*camberP*PC(1,ic,is)-PC(1,ic,is)**2._dp)
        endif
      enddo
    enddo
  endif

  ! Mirror right wing to left wing
  do is=1,ns
    PC(1,:,is) = PC(1,:,(2*ns+1)-is+1)
    PC(2,:,is) = PC(2,:,(2*ns+1)-is+1)*(-1_dp)
    PC(3,:,is) = PC(3,:,(2*ns+1)-is+1)
  enddo

  ! No. of grid points to write to file
  nx = nc+1
  nyStart = ns+1  ! Uncomment to write out only semispan
  !nyStart = 1     ! Uncomment to write out full span wing
  ny = 2*ns+1
  nz = 1

  ! Write to output file
  open(unit=11,file='output.xyz')
  write(11,*) nx, ny-nyStart+1, nz
  write(11,*) &
    ((PC(1,ic,is),ic=1,nx),is=nyStart,ny), &
    ((PC(2,ic,is),ic=1,nx),is=nyStart,ny), &
    ((PC(3,ic,is),ic=1,nx),is=nyStart,ny)
  close(11)

end program fourPointWing

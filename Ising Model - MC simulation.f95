


module PARAMETERS
 implicit none
 integer, parameter :: kr = selected_real_kind(7)
 integer, public :: N=40 ! N=10 or 40 in Sheet12

!Nils: spin should be allocatable
 integer, public, dimension(40,40) :: spin
 integer, public :: x, y, M
 real(kind=kr) :: E, rnd,rnd2, deltaE, T, Cv
 integer, public, dimension(15) :: seed

end module PARAMETERS

module INPUT
 implicit none
 integer, public ::  Nm=100000, Nequil  ! N is a size of the grid.
 !Nm = the total number of Metropolis,
 !Mequil = the number of equilibration steps

end module INPUT



module SPINS
 use PARAMETERS
 use INPUT
 implicit none




contains

subroutine Observables
!Nils: The random initial configuration should be a seperate subroutine, otherwise you always create a new random configuration when you call Observables.
call random_seed(put=seed)
   M = 0.0
   E = 0.0
!  random initial configuration
 do y = 1,N
  do x = 1,N
   call random_number(rnd)
   if (rnd < 0.5) then
    spin(x,y) = 1

   else
    spin(x,y) = -1

   end if
    M = M + spin(x,y)! M is just the sum of all spins.

  end do
 end do


 do x=1,N
  do y=1,N   ! E begins with E=0.0

  E=E+dE(x,y)  ! The function dE(x,y) is defined below
  end do
 end do  ! The total energy(eq.228) is caculated. J=1

end subroutine Observables



 function dE(i,j)
   integer, intent (in) :: i, j
   integer :: dE
   integer :: left
   integer :: right
   integer :: up
   integer :: down
   if (i == 1) then
      left = spin(N,j)
      right = spin(2,j)
   else if (i == N) then
      left = spin(N-1,j)
      right = spin(1,j)
   else
      left = spin(i-1,j)
      right = spin(i+1,j)
   end if
   if (j == 1) then
      up = spin(i,2)
      down = spin(i,N)
   else if (j == N) then
      up = spin(i,1)
      down = spin(i,N-1)
   else
      up = spin(i,j+1)
      down = spin(i,j-1)
   end if

   dE = -0.5*spin(i,j)*(left + right + up + down)   ! all interaction with four neighbor
end function dE

function P(s,r)  ! s is the temperature, it is not T here to avoid overlap
                 ! r is deltaE in this ising model
   real(kind=kr) :: P, s,r


   P=exp(-r/s)  ! P is probablity

end function P

end module SPINS

module METROPOLIS
 use PARAMETERS
 use INPUT
 use SPINS
 implicit none



 contains

 subroutine metro()

 integer :: i
 real(kind=kr) :: pro   ! Probablity value
 !open(1, file='logfile.txt', status='new')
!Nils: You don't have to call observables all the time, because once we initially calculated the energy, all subsequent changes in the energy can be realized by the formula below.


   call random_number(rnd)
   call random_number(rnd2)

   x = int(N*rnd) + 1
   y = int(N*rnd2) + 1   ! x and y are distinguished by rnd and rnd2


   deltaE = dE(x,y)
!Nils: should be P(deltaE,T)
   pro=P(T,deltaE)

   if (deltaE.le.0) then
    spin(x,y) = -spin(x,y)

    M = M + 2*spin(x,y)
    E = E + 4*deltaE
    write(1,*), M, E
   else if (pro.gt.rnd) then
   spin(x,y) = -spin(x,y)

    M = M + 2*spin(x,y)
!Nils: should be E+4.0*deltaE, the 1/2 in deltaE is only for the double counting. Therefore we need to cancel the 1/2.
    E = E + 4*deltaE

   end if

 !close(1)

 end subroutine metro
end module METROPOLIS

module STATISTICS
 use PARAMETERS
 use INPUT
 use SPINS
 use METROPOLIS
 implicit none
contains

subroutine accumulate(accum)
 real (kind=kr), dimension(:), intent (inout) :: accum
 integer :: i


accum(1) = accum(1) + E
accum(2) = accum(2) + M
accum(3) = accum(3) + E*E ! This is E**2 for calculation of specific heat

end subroutine accumulate



end module STATISTICS

program main
 use PARAMETERS
 use INPUT
 use SPINS
 use METROPOLIS
 use STATISTICS

 implicit none
 integer :: k, i
 real (kind=kr), dimension(3) :: accum
 real (kind=kr) :: ss, avM, avE,avEE  ! ss is <s> in eq(256)

 call Observables
!Nils: You have to initialize T, and don't start with 0 because of the specific heat.
 T=0.01
 do k=1, 50

  do i=1,3
   accum(i)=0.0
  end do

  do i=1,Nm   ! Nm is the total number of metropolis steps
   call metro
   call accumulate(accum)
  end do

  avM=accum(2)/Nm  ! avM is average of M = avM
  ss= avM/(N**2)

  avE= accum(1)/(Nm*(N**2))
  avEE= accum(3)/Nm ! avEE is the average of square energy
  Cv= (avEE-avE**2)/T**2  ! Cv is the specific heat

!Nils: T is also important for the Metropolis algorithm, I think this loop should be around everything else in main, so that you do
!Nils: the simulation for every T specified by the loop.


  T=T+0.08

  print *, T, ss
 end do




end program main

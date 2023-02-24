! A fortran95 program for G95
! By WQY
program main
  implicit none
  real :: x = -10-0.4
  real :: dx = 0.4 ! dx is a equidistant mesh
  real :: g, f, dff

!Nils: You should print the results in a file. Do that via:
!Nils: open(unit=20, file="out.dat", status="unknown")
!Nils: Then in the do while loop you can use 
!Nils: write(20,*) x, g, f, dff
!Nils: After the loop you just close the file via:
!Nils: close(20)

  do while (x.le.10-0.4)
   x = x+dx
   g = (1-exp(-x-dx)-(1-exp(-x+dx)))/2*dx ! g is numerical derivative
   f = (exp(-x)-1+exp(-x))/x**2 ! f is analytic derivative
   dff = g - f  ! dff is difference between analytic and numerical result
   print *, x, g, f, dff  ! all of g at x=-10,-10+0.4,.....,10
  end do
!Nils: so for x=0 or also x+dx=0 and x-dx=0 we need to have some analytical expression. Otherwise the calculation would give us either inf or NaN
  ! we got x=0, when x=-10-0.4+25*dx
  ! The biggest difference(error) is 3178.44604 at x = -10.

end program main

!Nils: Please use .f90 as file extension and gfortran as the compiler.

!Nils: 4/7

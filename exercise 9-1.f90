



program main
  implicit none
  real :: C, x=-10-0.04, dx=0.04, simpson, simpson2, I, y, u, I0, lambda, z, dy, t
  C = simpson(x)

  do while (x.lt.10-0.04)   ! The number of datas is 500 in [-10:10]
   x=x+dx                  ! dx=0.04
   C = simpson(x)
   print *, x, C            ! get data of C(x) for x between -10 and 10
  end do


  I0=10  ! 10 is random value of I0
  lambda=0.0000004        ! we can repeat lambda= 400,500,600, ..., 800nm
  z = 384000000           ! unit of distance : meter

  u = y*sqrt(2/(lambda*z))
  I = (I0/8)*((2*simpson(u)+1)**2 + (2*simpson2(u)+1)**2)
  y=-10-0.1
  dy=0.1
  do while (y.lt.50-0.1)
   y=y+dy
   u = y*sqrt(2/(lambda*z))
   I = (I0/8)*((2*simpson(u)+1)**2 + (2*simpson2(u)+1)**2)
   print *, y, I
  end do


end program main



module integration
 implicit none
 integer, parameter :: kr = selected_real_kind(7)
 real :: dx, x
 real(kind=kr), parameter :: pi = 3.141592_kr

contains
 function f(r)   ! r is dummy variable to avoid overlap
  implicit none
  real :: r, f
  f=cos((pi/2)*r**2)
 end function f

 function g(t)
  implicit none
  real :: t, g
  g=sin((pi/2)*t**2)
 end function g

end module integration




function simpson(b)  ! to define C(x)
 use integration
 implicit none
 real :: h, simpson, s, b   ! b is limit of integral
 integer :: k
 h=b/1000            !limits of integral a=0, b
 s=0.0

 do k=1, 500   ! from k=1 to k=1000/2=500
  s=s+f((2*k-2)*h)+4*f((2*k-1)*h)+f(2*k*h)
 end do        ! Sum 's' is calculated
 simpson = (h/3)*s

end function simpson

function simpson2(p) ! to define S(x)
 use integration
 implicit none
 real :: h, simpson2, s, p
 integer :: k
 h=p/1000            !limits of integral a=0, p
 s=0.0

 do k=1, 500   ! from k=1 to k=1000/2=500
  s=s+g((2*k-2)*h)+4*g((2*k-1)*h)+g(2*k*h)
 end do        ! Sum 's' is calculated
 simpson2 = (h/3)*s

end function simpson2

!Nils: Ok. 6/6

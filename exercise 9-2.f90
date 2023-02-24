
program main
  use integration
  ! Part(i) : value of the integral = 1-1=0, because integral of G is 1 and f=G(x,1,20)-G(x,0.009,20)
  implicit none
  real :: integral, simpson
  integer :: n
  integral = simpson(40.0)
  ! Part (ii) : if n=500, we get simpson(40.0) = -1.36410236
  ! Part (iii): lager n makes smaller error, for example n=20000

  ! Part (iv) : We change border of integration. and simpson(25000.0) is smaller than 10E-05
  print *, simpson (10.0), simpson(40.0), simpson(4000.0), simpson(25000.0)






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
  f=(0.39894228)*exp((-(r-20)**2)/2) - (44.32692)*exp((-(r-20)**2)/0.000162)
  ! Constants are calculated myself in f
 end function f
end module integration




 function simpson(b)  ! to define C(x)
  use integration
  implicit none
  real :: h, simpson, s, b   ! b is limit of integral
  integer :: k
                      ! n=500 for part (ii), n=1000 for part(iv)
   h=b/1000           ! h=b/500 for part (ii)
   s=0.0

   do k=1, 500  ! from k=1 to k=n/2
    s=s+f((2*k-2)*h)+4*f((2*k-1)*h)+f(2*k*h)
   end do        ! Sum 's' is calculated
   simpson = (h/3)*s


 end function simpson


!Nils: 4/6


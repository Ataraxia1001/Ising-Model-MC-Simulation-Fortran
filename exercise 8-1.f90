
program sss
  implicit none
  integer, parameter :: kr = selected_real_kind(7), imax = 100
  integer :: i, n
  real(kind=kr) :: f, T=0.0_kr, x
  real(kind=kr) ::  a=-100.0_kr,  b=104.0_kr,  c=2.0_kr
  real(kind=kr) :: fa, fb, fc
  real(kind=kr), parameter :: zero = 0.0_kr
  real(kind=kr), parameter :: onehlf = 0.5_kr
  real(kind=kr), parameter :: one = 1.0_kr
  real(kind=kr), parameter :: two = 2.0_kr
  real(kind=kr), parameter :: ee = 1.e-5_kr

  fa = f(a)
  fb = f(b)
 
  do i=1,imax
      c = onehlf*(a+b)
      fc = f(c)
      if( fa*fc.lt.zero ) then
         b = c
         fb = fc
      else if( fa*fc.gt.zero ) then
         a = c
         fa = fc
      else
      print *, i, c, fc
      stop
      end if

      if( abs(b-a)/(abs(a)+abs(b)).lt.ee ) then
      print *, i, c, fc
      end if
  end do

end program sss

!Ergebnis
! T=1 : i = 56    , i is the number of iteration
! T=1.4 : i = 59
! T=1.8 : i = 55
function f(x)
  implicit none
  integer, parameter :: kr = selected_real_kind(7), imax = 100
  real(kind=kr) :: f, T=0.0_kr, x
  f=x-tanh((2*x)/T)
end function f

!Nils: Ok.
!Nils: 3/3

!Nils: for part a): T=0? -> 0.5/1 

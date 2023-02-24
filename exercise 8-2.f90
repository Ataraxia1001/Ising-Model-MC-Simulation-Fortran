
program main
  implicit none
  integer, parameter :: kr = selected_real_kind(7), imax = 100
  real(kind=kr) :: f, fi, f1,f2,drfi, x, x1, x2, xi, T=0.0_kr
  integer :: i, n
  real(kind=kr), parameter :: ee = 1.e-5_kr

  x1=100.0_kr
  x2=10.0_kr

!Nils: f needs real as input argument, also i is at this point not defined
  f1=f(1)
  f2=f(2)
  fi=f(i)

  do n=1, 200
   T=T+0.02
   do i=1, imax

   drfi = (f2-fi)/(x2-xi)            ! drfi = drf(xi)= derivative of f at xi
   xi = x2-f2*drfi
   if (fi.lt.ee) then
    print *, i, xi, fi
    stop
   else
    print *, i, xi, fi
   end if
   xi=x2
   x2=x1
   end do
  end do


end program main


!Nils: your function needs a second input argument, for example: f(x,T) so that your T-loop has meaning
function f(x)
  implicit none
  integer, parameter :: kr = selected_real_kind(7), imax = 100
  real(kind=kr) :: T=0.0_kr, f, x, x1, x2, xi
  f=x-tanh((2*x)/T)

end function f

!Nils: 3/6

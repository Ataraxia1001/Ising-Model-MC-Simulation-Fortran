
program main
  implicit none
  integer :: i, T, Fibonacci


  do i=1,10
   T= Fibonacci (i)
   print *, T
  end do

end program main

recursive function Fibonacci (n) result (f)
 implicit none
 integer :: f
 integer, intent(in) :: n

 select case (n)

  case (1:2)
   f=1
  case default
   f= Fibonacci(n-1)+Fibonacci(n-2)

 end select



end function Fibonacci

!Nils: +3

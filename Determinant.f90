

program main
 integer :: n  ! n is arbitrary dimension of matrix
 real, dimension(:,:), allocatable :: mat
 real :: determinant


!Nils: You have to read in n first, otherwise n is unknown
 allocate(mat(n,n))
!Nils: You also have to assign some values to the matrix

if(n==1) then
 determinant=det11(mat)

else if(n==2) then
 determinant=det22(mat)

else if(n==3) then

 determinant=det33(mat)

else
 determinant=0 ! we don't handle this case in sheet13

end if


print *, determinant ! we get the determinant of mat

end program main





function det11(A) ! case : 1x1 matrix

  real, dimension(1,1), intent(in) :: A
  real :: det11

  det11=A(1,1)


end function det11

function det22(A) ! case : 2x2 matrix

 real, dimension(2,2), intent(in) :: A
 real :: det22

 det22=A(1,1)*A(2,2)-A(1,2)*A(2,1)


end function det22


function det33(A) ! case : 3x3 matrix
 real, dimension(3,3), intent(in) :: A
 real :: det33

 det33=A(1,1)*(A(2,2)*A(3,3) - A(3,2)*A(2,3)) &
       + A(1,2)*(A(3,1)*A(2,3) - A(2,1)*A(3,3)) &
       + A(1,3)*(A(2,1)*A(3,2) - A(3,1)*A(2,2))

end function det33

!Nils: +4

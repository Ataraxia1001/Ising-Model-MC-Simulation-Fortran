! A fortran95 program for G95
! By WQY
!Nils: Programm kompiliert nicht, daher 0/5
!Nils: Auch hier: Endung .f90 statt .f95
program exercise2
  implicit none
  !This code is finding the solution of cardano`s formula as follows
  !A*x**3+B*x**2+C*x+D=0
!Nils: Alle Deklarationen vor allen Anweisungen
  real :: A,B,C,D,p,q,R
  a = B/A
  b = C/A
  c = D/A
  p = b-a**2/3
  q = 2*a**3/27-a*b/3+c

  R = (q/2)**2+(p/3)**3


  complex :: u,v,s,t,za,zb,zc,xa,xb,xc
  u = (-q/2+sqrt(R))**(1/3)
  v = (-q/2-sqrt(R))**(1/3)
  s = cmplx(-0.5,0.5*sqrt(3))
  t = s**2
  za=u+v
  zb=u*s+v*t
  zc=u*t+v*s
  p>0
  v**3<0
  !

  ! f(x)=x**3-27 has A=1,B=0,C=0,D=-27 as a special case.
  xa=za-a/3
  xb=zb-b/3
  xc=zc-c/3
  Print *, xa,xb,xc
  write(*,*) xa,xb,xc
  !we can change the precision, as we rewrite of xa,xb,xc.
  !real(kind=4) :: xa,xb,xc
  !real(kind=8) :: xa,xb,xc ---more precise
  !real(kind=16) :: xa,xb,xc ---more precise
end program exercise2

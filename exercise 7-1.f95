! A fortran95 program for G95
! By WQY
program main
  implicit none
  ! Da ich leider in lezten Woche komplette Übung total gescheitert habe, mache ich nicht alles wieder,
  ! sondern nur Teil von sheet7 für 3 Punkten.

  real :: q,p,t,x,a, y, b
  complex :: u,v, uu, vv
  p = 10
  q = 10
  t = (q/2)**2 + (p/3)**3   ! t is lambda in Blatt
!Nils: Hier solltest du komplexe Exponenten verwenden, zum Beispiel mit cmplx(1./3.)
  u = ((-q/2)+t**(1./2.))**(1./3.)
  v = ((-q/2)-t**(1./2.))**(1./3.)

  if (t.lt.0) then
  ! u is complex
  x=realpart(u) ! Taking real part of u
  y=AIMAG(u)    ! Taking complex part of u
  a=realpart(v) ! Taking real part of v
  b=AIMAG(v)    ! Taking complex part of v
  uu = cmplx(x,y)
  vv = cmplx(a,b) ! From now on we use uu, vv instead of using u, v
  end if


end program main

!Nils: 2/3

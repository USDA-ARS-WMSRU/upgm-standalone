subroutine sdst(x,dg,dg1,i)
!
implicit none
!
! Dummy arguments
!
real :: dg,dg1
integer :: i
real,dimension(*) :: x
!
!     + + + purpose + + +
!     this subroutine initializes residue, nitorgen, and phosphorous amounts in
!     layers
 
!     + + + keywords + + +
!     initialization
 
!     + + + local variables + + +
 
!     + + + local definitions + + +
!     dg    -
!     dg1   -
!     i     -
!     x     -
 
!      dimension x(10)
 
!     + + + end specifications + + +
 
!     data e/'e'/
if (x(i)<=0.) then
  if (i>1) then
     x(i) = x(i-1)*dg*exp(-.01*dg)/dg1
  else
     x(1) = 1.
  end if
end if
!   3 est(j,i)=e
!
end subroutine sdst

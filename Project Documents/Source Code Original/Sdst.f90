SUBROUTINE sdst(x,dg,dg1,i)
!
IMPLICIT NONE
!
! Subroutine arguments
!
REAL :: dg,dg1
INTEGER :: i
REAL,DIMENSION(*) :: x
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
IF (x(i).LE.0.) THEN
  IF (i.GT.1) THEN
     x(i) = x(i-1)*dg*exp(-.01*dg)/dg1
  ELSE
     x(1) = 1.
  END IF
END IF
!   3 est(j,i)=e
!
END SUBROUTINE sdst

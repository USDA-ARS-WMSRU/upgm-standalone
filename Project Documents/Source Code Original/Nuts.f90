SUBROUTINE nuts(y1,y2,uu)
!
IMPLICIT NONE
!
INCLUDE 'cparm.inc'
!
! Subroutine arguments
!
REAL :: uu,y1,y2
!
! Local variables
!
REAL :: yy
!
!     + + + purpose + + +
!     this subroutine calculates a nutrient stress factor caused by limited
!     supply of n or p.
 
!     + + + keywords + + +
!     nutrient stress
 
!     + + + common blocks + + +
 
 
!     + + + local variables + + +
 
!     + + + local variable definitions + + +
!     y1 - cummulative amount of n or p taken by the plant (kg/ha) - supply
!     y2 - potential amount of n or p needed by the plant (kg/ha) - demand
!     yy - scaled ratio of supply over demand
!     uu - n or p stress factor
!     yy replaces uu where appropriate to minimize confusion
!     a_s8,b_s8 are used instead of scrp(8,1) and scrp(8,2)
 
!     + + + end of specifications + + +
 
IF (y2.NE.0.) THEN
  yy = 200.*(y1/y2-.5)
  IF (yy.GT.0.) THEN
     uu = yy/(yy+exp(a_s8-b_s8*yy))
  ELSE
     uu = 0.
  END IF
END IF
! 
END SUBROUTINE nuts

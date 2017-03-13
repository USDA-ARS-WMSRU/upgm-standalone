FUNCTION julday(dd,mm,yyyy)
!
IMPLICIT NONE
!
! PARAMETER definitions
!
INTEGER,PARAMETER :: igreg = 15 + 31*(10+12*1582)
!
! Function arguments
!
INTEGER :: dd,mm,yyyy
INTEGER :: julday
!
! Local variables
!
REAL*8 :: dble
INTEGER :: int
INTEGER :: ja,jm,jy,y_nw
!
!     + + + purpose + + +
!     in this routine julday returns the julian day number which begins at
!     noon of the calendar date specified by day "dd", month "mm", & year "yyyy"
!     all are integer variables. positive year signifies a.d.; negative, b.c.
!     remember that the year after 1 b.c. was 1 a.d.
 
!     the following invalid calendar dates are checked for and reported:
!     1. zero year
!     2. dates between 4/10/1582 and 15/10/1582 are specified.
!
!     additional checking for other invalid dates could be added
!     in the future if necessary.
!
!     + + + keywords + + +
!     date, utility
!
!     + + + argument declarations + + +
!
!     + + + argument definitions + + +
!     dd - integer value of day in the range 1-31
!     julday - value returned by the julday function. debe added 09/09/09
!     mm - integer value of month in the range 1-12
!     yyyy - integer value of year (negative a.d., positive b.c.)
!
!     + + + parameters + + +
!     gregorian calendar was adopted on oct. 15, 1582.
!
!     + + + local variable definitions + + +
!     dble - 
!     int - 
!     ja - 
!     jm - 
!     jy - 
!     y_nw - 

!     + + + end specifications + + +
!
IF (yyyy.EQ.0) WRITE (*,*) 'there is no year zero'
IF ((yyyy.EQ.1582).AND.(mm.EQ.10).AND.(dd.LT.15).AND.(dd.GT.4)) WRITE (*,*)     &
   & 'this is an invalid date'
IF (yyyy.LT.0) THEN
  y_nw = yyyy + 1
ELSE
  y_nw = yyyy
END IF
IF (mm.GT.2) THEN
  jy = y_nw
  jm = mm + 1
ELSE
  jy = y_nw - 1
  jm = mm + 13
END IF
julday = int(365.25*jy) + int(30.6001*jm) + dd + 1720995
IF (dd+31*(mm+12*y_nw).GE.igreg) THEN
  ja = jy/100
!         ja=int(dble(0.01)*dble(jy))
  julday = julday + 2 - ja + int(dble(0.25)*dble(ja))
END IF
!
END FUNCTION julday

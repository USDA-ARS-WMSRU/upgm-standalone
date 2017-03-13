SUBROUTINE mvdate(delta,dd,mm,yyyy,nday,nmonth,nyear)
!
IMPLICIT NONE
!
! Subroutine arguments
!
INTEGER :: dd,delta,mm,nday,nmonth,nyear,yyyy
!
! Local variables
!
INTEGER :: julday
!
!     + + + purpose + + +
!     compute a date which is delta number of days before or after the
!     date that is passed.
!
!     + + + keywords + + +
!     date, utility
!
!     + + + argument declarations + + +
!
!     + + + argument definitions + + +
!     delta  - positive or negative integer indicating number of days
!     dd     - day   -\
!     mm     - month   >-- passed in parameters. will not change
!     yyyy   - year  -/
!
!     nday   - day   -\
!     nmonth - month   >-- results are shipped out in here
!     nyear  - year  -/
!
!     + + + subroutines called + + +
!     caldat
!
!     + + + function declarations + + +
!
!     + + + end specifications + + +
!
CALL caldat((julday(dd,mm,yyyy)+delta),nday,nmonth,nyear)
!
END SUBROUTINE mvdate

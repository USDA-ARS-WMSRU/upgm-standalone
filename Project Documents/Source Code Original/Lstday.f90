FUNCTION lstday(mm,yyyy)
!
IMPLICIT NONE
!
! Function arguments
!
INTEGER :: mm,yyyy
INTEGER :: lstday
!
! Local variables
!
INTEGER :: julday
INTEGER :: julian,ld,lm,ly
!
!     + + + purpose + + +
!     given a date in mm/yyyy format, lstday will return the last day
!     of that month.
!
!     + + + keywords + + +
!     date, utility
!
!     + + + argument declarations + + +
!
!     + + + argument definitions + + +
!     lstday - the last day of the submitted month
!     mm - month
!     yyyy - year
!
!     + + + local variables + + +
!
!     + + + local definitions + + +
!     julday - value returned from the Julday function
!     julian - julian day value
!     ld - day
!     lm - month
!     ly - year
!
!     + + + subroutines called + + +
!     caldat, julday
!
!     + + + function declarations + + +
!
!     + + + end specifications + + +
!
!     go to the first day of the next month
!     (this is exactly one day after the day we want to find)
lm = mm + 1
ld = 1
ly = yyyy
IF (lm.EQ.13) THEN
  lm = 1
  ly = yyyy + 1
END IF
!     we simply find the julian day and subtract 1 day to get the last
!     day of the previous month
julian = julday(ld,lm,ly) - 1
!     now convert back to gregorian calendar to get the actual day
CALL caldat(julian,ld,lm,ly)
lstday = ld
!      return
!
END FUNCTION lstday

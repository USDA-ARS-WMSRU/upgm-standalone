FUNCTION wkday(dd,mm,yyyy)
!
IMPLICIT NONE
!
! Function arguments
!
INTEGER :: dd,mm,yyyy
INTEGER :: wkday
!
! Local variables
!
INTEGER :: julday
!
!     + + + purpose + + +
!     given a date in dd/mm/yyyy format
!     wkday will give the day of the week.
!     0 = monday
!     1 = tuesday
!     2 = wednesday
!     3 = thursday
!     4 = friday
!     5 = saturday
!     6 = sunday
!
!     + + + keywords + + +
!     date, utility
!
!     + + + argument declarations + + +
!
!     + + + argument definitions + + +
!     dd     - day
!     mm     - month
!     yyyy   - year
!
!     + + + local definitions + + +
!     julian - julian day value
!
!     + + + function declarations + + +
!
!     + + + end specifications + + +
!
!     we simply find the julian day and do a modulo of the value
!
wkday = mod((julday(dd,mm,yyyy)),7)
!
END FUNCTION wkday

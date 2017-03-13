FUNCTION wkjday(jday)
!
IMPLICIT NONE
!
! Function arguments
!
INTEGER :: jday
INTEGER :: wkjday
!
!     + + + purpose + + +
!     given a date in julian day format
!     wkjday will give the day of the week.
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
!     jday   - julian day
!
!     + + + end specifications + + +
!
!     we simply take the julian day and do a modulo of the value
!
wkjday = mod(jday,7)
!
END FUNCTION wkjday

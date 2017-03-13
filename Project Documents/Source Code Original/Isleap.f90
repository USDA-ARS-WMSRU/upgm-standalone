FUNCTION isleap(yyyy)
!
IMPLICIT NONE
!
! Function arguments
!
INTEGER :: yyyy
LOGICAL :: isleap
!
! Local variables
!
INTEGER :: ld
INTEGER :: lstday
!
!     + + + purpose + + +
!     given a year in yyyy format,
!     isleap will return a .true. if it is a leap year
!     or a .false. if it is not a leap year.
!
!     + + + keywords + + +
!     date, utility, leap year
!
!     + + + argument declarations + + +
!
!     + + + argument definitions + + +
!     isleap - result of this function. Returns TRUE if the year is a leap
!              year and FALSE if not.
!     yyyy - year
!
!     + + + local variables + + +
!
!     + + + local definitions + + +
!     ld - day
!     lstday - value returned by the Lstday function
!
!     + + + function declarations + + +
!
!     + + + end specifications + + +
!
!     go to the last day of february and see if the 29th exists
ld = lstday(2,yyyy)
IF (ld.EQ.29) THEN
  isleap = .TRUE.
ELSE
  isleap = .FALSE.
END IF
!
END FUNCTION isleap

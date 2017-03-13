FUNCTION dayear(dd,mm,yyyy)
!
IMPLICIT NONE
!
! Function arguments
!
INTEGER :: dd,mm,yyyy
INTEGER :: dayear
!
! Local variables
!
INTEGER :: difdat
!
!     + + + purpose + + +
!     given a date in dd/mm/yyyy format,
!     dayear will return the number of days
!     from the first of that year.
!
!     + + + keywords + + +
!     date, utility
!
!     + + + argument declarations + + +
!
!     + + + argument definitions + + +
!     dayear - returns the number of days from the first of that year
!     dd     - day
!     mm     - month
!     yyyy   - year
!
!     + + + local variable definitions + + +
!     difdat - the number of days between two dates. a function. This 
!              variable holds the value returned by the Diffdat function.
!              Debe assumed this definition

!     + + + function declarations + + +
!
!     + + + end specifications + + +
!
!     get the difference in days + 1
!
dayear = difdat(1,1,yyyy,dd,mm,yyyy) + 1
!
END FUNCTION dayear

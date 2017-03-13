function dayear(dd,mm,yyyy)
!
implicit none
!
! Dummy arguments
!
integer :: dd,mm,yyyy
integer :: dayear
!
! Local variables
!
integer :: difdat
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
!     difdat - the number of days between two dates. a function. this
!              variable holds the value returned by the diffdat function.
!              debe assumed this definition
 
!     + + + function declarations + + +
!
!     + + + end specifications + + +
!
!     get the difference in days + 1
!
dayear = difdat(1,1,yyyy,dd,mm,yyyy) + 1
!
end function dayear

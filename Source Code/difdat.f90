function difdat(d1,m1,yyy1,d2,m2,yyy2)
!
implicit none
!
! Dummy arguments
!
integer :: d1,d2,m1,m2,yyy1,yyy2
integer :: difdat
!
! Local variables
!
integer :: julday
!
!     + + + purpose + + +
!     two dates are passed to this function and the number of days between
!     them is returned. the important thing to remember here is that the
!     first date is subtracted _from_ the second.
!     example:
!        d1 m1 yyy1    d2 m2 yyy2   returns   meaning
!        01 01 1992    02 01 1992   1         1 day from 01/01/1992 it will
!                                             be 02/01/1992
!        02 01 1992    01 01 1992   -1        -1 day from 02/01/1992 (or 1
!                                             day ago) it was 01/01/1992
!
!     + + + keywords + + +
!     date, utility
!
!     + + + argument declarations + + +
!
!     + + + argument definitions + + +
!     d1     - day
!     difdat - holds the result of the difdat function. debe added 9/9/09
!     m1     - month
!     yyy1   - year
!     d2     - day
!     m2     - month
!     yyy2   - year
 
!     + + + local variable definitions + + +
!     julday - value returned by the julday function. debe added 09/09/09
!
!     + + + function declarations + + +
!
!     + + + end specifications + + +
!
difdat = julday(d2,m2,yyy2) - julday(d1,m1,yyy1)
!
end function difdat

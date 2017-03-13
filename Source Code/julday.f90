function julday(dd,mm,yyyy)
!
implicit none
!
! PARAMETER definitions
!
integer,parameter :: igreg = 15 + 31*(10+12*1582)
!
! Dummy arguments
!
integer :: dd,mm,yyyy
integer :: julday
!
! Local variables
!
real*8 :: dble
integer :: int
integer :: ja,jm,jy,y_nw
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
if (yyyy==0) write (*,*) 'there is no year zero'
if ((yyyy==1582).and.(mm==10).and.(dd<15).and.(dd>4)) write (*,*)               &
   & 'this is an invalid date'
if (yyyy<0) then
  y_nw = yyyy + 1
else
  y_nw = yyyy
end if
if (mm>2) then
  jy = y_nw
  jm = mm + 1
else
  jy = y_nw - 1
  jm = mm + 13
end if
julday = int(365.25*jy) + int(30.6001*jm) + dd + 1720995
if (dd+31*(mm+12*y_nw)>=igreg) then
  ja = jy/100
!         ja=int(dble(0.01)*dble(jy))
  julday = julday + 2 - ja + int(dble(0.25)*dble(ja))
end if
!
end function julday

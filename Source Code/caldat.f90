subroutine caldat(ijulian,dd,mm,yyyy)
!
implicit none
!
include 'm1sim.inc'
!
! PARAMETER definitions
!
integer,parameter :: igreg = 2299161
!
! Dummy arguments
!
integer :: dd,ijulian,mm,yyyy
!
! Local variables
!
real :: alpha,c,e
real*8 :: dble
integer :: int
integer :: ja,jalpha,jb,jc,jd,je,julian
!
!     + + + purpose + + +
!     inverse of the function julday. here 'julian' is input as a julian day
!     number, and the routine outputs the dd, mm, and yyyy on which the
!     specified julian day started at noon.
!
!     + + + keywords + + +
!     date, utility
!
!     + + + argument declarations + + +
!
!     + + + argument definitions + + +
!     mm     - integer value of mm in the range 1-12
!     dd     -                  dd in the range 1-31
!     yyyy   -                  yyyy (negative a.d., positive b.c.)
!     ijulian - julian day value (this is the value passed into caldat.
!               definition taken from lstday)
!     julian - integer value equal to julian day number
!
!     + + + parameters + + +
!     gregorian calendar was adopted on oct. 15, 1582.
!     igreg - parameter
 
!     + + + parameter definitions + + +
 
!     + + + local variables + + +
 
!     + + + local variable definitions + + +
!     alpha -
!     c -
!     dble - double
!     e -
!     int -
!     ja -
!     jalpha -
!     jb -
!     jc -
!     jd -
!     je -
 
!     + + + common block variable definitions + + +
!     am0jd - current julian day of simulation
 
!     + + + end specifications + + +
!
 
!     if the date is -1 then use the simulation date.

julian = ijulian
if (julian==-1) julian = am0jd
 
if (julian>=igreg) then
  alpha = (dble(julian-1867216)-dble(0.25))/dble(36524.25)
  jalpha = int(alpha)
  ja = julian + 1 + jalpha - int(dble(0.25)*jalpha)
else
  ja = julian
end if
jb = ja + 1524
c = dble(6680.0) + ((jb-2439870)-dble(122.1))/dble(365.25)
!c = dble(6680.0) + ((jb-2439870)-dble(122.1))/dble(365.00)!de changed
jc = int(c)
jd = 365*jc + int(dble(0.25)*jc)
e = (jb-jd)/dble(30.6001)
je = int(e)
dd = jb - jd - int(dble(30.6001)*je)
mm = je - 1
if (mm>12) mm = mm - 12
yyyy = jc - 4715
if (mm>2) yyyy = yyyy - 1
if (yyyy<=0) yyyy = yyyy - 1
!
end subroutine caldat

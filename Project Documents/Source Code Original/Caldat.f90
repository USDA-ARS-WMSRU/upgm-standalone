SUBROUTINE caldat(ijulian,dd,mm,yyyy)
!
IMPLICIT NONE
!
INCLUDE 'm1sim.inc'
!
! PARAMETER definitions
!
INTEGER,PARAMETER :: igreg = 2299161
!
! Subroutine arguments
!
INTEGER :: dd,ijulian,mm,yyyy
!
! Local variables
!
REAL :: alpha,c,e
REAL*8 :: dble
INTEGER :: int
INTEGER :: ja,jalpha,jb,jc,jd,je,julian
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
!     ijulian - julian day value (This is the value passed into Caldat. 
!               Definition taken from Lstday)
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
IF (julian.EQ.-1) julian = am0jd
 
IF (julian.GE.igreg) THEN
  alpha = (dble(julian-1867216)-dble(0.25))/dble(36524.25)
  jalpha = int(alpha)
  ja = julian + 1 + jalpha - int(dble(0.25)*jalpha)
ELSE
  ja = julian
END IF
jb = ja + 1524
c = dble(6680.0) + ((jb-2439870)-dble(122.1))/dble(365.25)
!c = dble(6680.0) + ((jb-2439870)-dble(122.1))/dble(365.00)!de changed
jc = int(c)
jd = 365*jc + int(dble(0.25)*jc)
e = (jb-jd)/dble(30.6001)
je = int(e)
dd = jb - jd - int(dble(30.6001)*je)
mm = je - 1
IF (mm.GT.12) mm = mm - 12
yyyy = jc - 4715
IF (mm.GT.2) yyyy = yyyy - 1
IF (yyyy.LE.0) yyyy = yyyy - 1
!
END SUBROUTINE caldat
FUNCTION equa_time(idoy)
!
IMPLICIT NONE
!
INCLUDE 'p1unconv.inc'
!
! Function arguments
!
INTEGER :: idoy
REAL :: equa_time
!
! Local variables
!
REAL :: b
!
!     + + + purpose + + +
!     this function calculates the declination of the earth with respect
!     the sun based on the day of the year
 
!     + + + keywords + + +
!     solar equation of time
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     idoy   - day of year
 
!     + + + local variables + + +
 
!     + + + local definitions + + +
!     b      - sub calculation (time of year, radians)
 
!     + + + common blocks + + +
 
!     + + + end specifications + + +
 
!     calculate time of year (b)
b = (360.0/365.0)*(idoy-81.25)*degtorad                                 !h-55
equa_time = 9.87*sin(2*b) - 7.53*cos(b) - 1.5*sin(b)            !h-54
! 
END FUNCTION equa_time

FUNCTION declination(idoy)
!
IMPLICIT NONE
!
INCLUDE 'p1unconv.inc'
!
! Function arguments
!
INTEGER :: idoy
REAL :: declination
!
! Local variables
!
REAL :: b
!
!     + + + purpose + + +
!     this function calculates the declination of the earth with respect
!     the sun based on the day of the year
 
!     + + + keywords + + +
!     solar declination
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     idoy   - day of year
 
!     + + + local varaibles + + +
 
!     + + + local definitions + + +
!     b      - sub calculation (time of year, radians)
 
!     + + + common blocks + + +
 
!     + + + end specifications + + +
 
!     calculate declination angle (dec)
!
b = (360.0/365.0)*(idoy-81.25)*degtorad                                
declination = 23.45*sin(b)                                                              
! 
END FUNCTION declination

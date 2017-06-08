function declination(idoy)
!
    use constants, only : degtorad
implicit none
!
!
! Dummy arguments
!
integer :: idoy
real :: declination
!
! Local variables
!
real :: b
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
end function declination

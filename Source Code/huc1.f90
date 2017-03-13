function huc1(bwtdmx,bwtdmn,bctmax,bctmin)
!
implicit none
!
! Dummy arguments
!
real :: bctmax,bctmin,bwtdmn,bwtdmx
real :: huc1
!
! Local variables
!
real :: heatunit
!
!     author : amare retta
!     + + + purpose + + +
!     calculate single day heat units for given temperatures
 
!     + + + keywords + + +
!     heat units, daylength
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     bctmax - maximum crop growth temperature. debe assumed this definition
!     bctmin - minimum crop growth temperature
!     bctopt - optimum crop growth temperature. not currently used.
!     bwtdmn - daily minimum air temperature
!     bwtdmx - daily maximum air temperature
!     huc1 - returns the value of the function. debe assumed this definition
 
!     + + + local variable definitions + + +
!     heatunit - a function. this variable holds the value returned by
!                the heatunit function. debe assumed this definition
 
!     + + + output formats + + +
!2000 format('+',109x,2(f8.2,1x))
 
!     + + + function declarations + + +
 
!     + + + end of specifications + + +
 
huc1 = heatunit(bwtdmx,bwtdmn,bctmin) - heatunit(bwtdmx,bwtdmn,bctmax)
if (huc1<0.) huc1 = 0.
!
end function huc1

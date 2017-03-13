FUNCTION huc1(bwtdmx,bwtdmn,bctmax,bctmin)
!
IMPLICIT NONE
!
! Function arguments
!
REAL :: bctmax,bctmin,bwtdmn,bwtdmx
REAL :: huc1
!
! Local variables
!
REAL :: heatunit
!
!     author : amare retta
!     + + + purpose + + +
!     calculate single day heat units for given temperatures
 
!     + + + keywords + + +
!     heat units, daylength
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     bctmax - maximum crop growth temperature. Debe assumed this definition
!     bctmin - minimum crop growth temperature
!     bctopt - optimum crop growth temperature. Not currently used.
!     bwtdmn - daily minimum air temperature
!     bwtdmx - daily maximum air temperature
!     huc1 - returns the value of hte function. Debe assumed this definition

!     + + + local variable definitions + + +
!     heatunit - a function. This variable holds the value returned by 
!                the Heatunit function. Debe assumed this definition

!     + + + output formats + + +
!2000 format('+',109x,2(f8.2,1x))
 
!     + + + function declarations + + +
 
!     + + + end of specifications + + +
 
huc1 = heatunit(bwtdmx,bwtdmn,bctmin) - heatunit(bwtdmx,bwtdmn,bctmax)
IF (huc1.LT.0.) huc1 = 0.
! 
END FUNCTION huc1

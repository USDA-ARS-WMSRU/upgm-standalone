FUNCTION radext(idoy,bmalat)
!
IMPLICIT NONE
!
INCLUDE 'p1const.inc'
INCLUDE 'p1unconv.inc'
INCLUDE 'p1solar.inc'
!
! PARAMETER definitions
!
REAL,PARAMETER :: gsc = 0.08202
!
! Function arguments
!
REAL :: bmalat
INTEGER :: idoy
REAL :: radext
!
! Local variables
!
REAL :: dec,dr,ra1,ra2,rdec,rlat,ws
REAL :: declination,hourangle
!
!     + + + purpose + + +
!     this subroutine estimates the incoming extraterrestial radiation
!     for a given location (mj/m^2/day)
 
!     + + + key words + + +
!     radiation, solar, extraterrestrial
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     idoy     - julian day of year, 1-366
!     bmalat   - latitude of the site, degrees
 
!     + + + common block + + +
 
!     + + + local variables + + +
 
!     + + + local definitions + + +
!     dec    - declination of the earth with respect to the sun (degrees)
!     dr     - direct radiation variation with distance from sun of earth orbit
!     ra1, ra2 - intermediate calculations
!     rlat   - latitude (radians)
!     rdec   - declination (radians)
!     ws     - sunset hour angle (radians)
 
!     + + + parameters + + +
 
 
 
!     gsc - solar_constant in mj/m^2-min (0.08202 mj/m^2-min = 1367 w/m^2)
 
!     + + + data initialization + + +
 
!     + + + function declarations + + +
 
!     + + + end specifications + + +
 
!     convert to radians for trig functions
rlat = bmalat*degtorad
dec = declination(idoy)
rdec = dec*degtorad
 
!     compute factor for variable distance from sun along orbital path
dr = 1 + 0.033*cos(2*pi*idoy/365)                                                       
 
ws = hourangle(bmalat,dec,beamrise)*degtorad
ra1 = ((24.0*60.0)/pi)*gsc*dr                                                                          
ra2 = (ws*sin(rlat)*sin(rdec)) + (cos(rlat)*cos(rdec)*sin(ws))      
radext = ra1*ra2                                                                                               
! 
END FUNCTION radext

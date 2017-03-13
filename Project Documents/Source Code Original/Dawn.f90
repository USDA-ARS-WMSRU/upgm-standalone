FUNCTION dawn(dlat,dlong,idoy,riseangle)
!
IMPLICIT NONE
!
! Function arguments
!
REAL :: dlat,dlong,riseangle
INTEGER :: idoy
REAL :: dawn
!
! Local variables
!
REAL :: dec,e,h,sn
REAL :: declination,equa_time,hourangle
!
!     + + + purpose + + +
!     this function calculates the time of sunrise (hours) for any simulation
!     site based on the global position of the site, and day of the
!     year.  the inputs for the function are day of the year, latitude
!     of the site, and longitude of the site.
 
!     + + + keywords + + +
!     sunrise
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     idoy   - day of year
!     dlat   - latitude of the site, degrees (north > 0, south < 0)
!     dlong  - longitude of the site, degrees (east > 0, west < 0)
!     riseangle - angle of earths rotation where sunrise occurs
!                 this varies depending on whether you are calculating
!                 direct beam, civil twilight, nautical twilight or
!                 astronomical twilight hourangle
 
!     + + + local variables + + +
 
!     + + + local definitions + + +
!     dec    - declination of earth with respect to the sun (degrees)
!     e      - equation of time (minutes)
!     h      - hour angle (degrees)
!     sn     - solar noon (hour of the day, midnight = 0.0)
 
!     + + + common blocks + + +
 
!     include 'p1const.inc'
!     include 'p1unconv.inc'
 
!     + + + function declarations + + +
 
!     + + + end specifications + + +
 
!     declination angle (dec)
dec = declination(idoy)
 
!     sunset hour angle (noon is zero degrees, sunset (+), sunrise (-))
h = hourangle(dlat,dec,riseangle)
 
!     equation of time (e)
e = equa_time(idoy)
 
!     calculate solar noon (sn)
sn = 12.0 - e/60.0 - 4.0*mod(dlong,15.0)/60.0                          
 
!     calculate the time of sunrise (rise)
dawn = sn - h/15.0                                                                             
 
!     to prevent errors of bleed over into previous day where
!     where daylength is 24 hours, limit time of sunrise
dawn = max(0.0,dawn)
! 
END FUNCTION dawn

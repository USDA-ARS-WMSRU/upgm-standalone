function daylen(dlat,idoy,riseangle)
!
implicit none
!
! Dummy arguments
!
real :: dlat,riseangle
integer :: idoy
real :: daylen
!
! Local variables
!
real :: dec,h
real :: declination,hourangle
!
!     + + + purpose + + +
!     this function calculates the daylength (hours) for any simulation
!     site based on the global position of the site, and day of the
!     year.  the inputs for the function are day of the year, and latitude
!     of the site.
 
!     + + + keywords + + +
!     day length
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     idoy - day of year
!     daylen - the length of the day. returns the value of this function.
!     dlat - latitude of the site, degrees (north > 0, south < 0)
!     riseangle - angle of earths rotation where sunrise occurs.
!                 this varies depending on whether you are calculating
!                 direct beam, civil twilight, nautical twilight or
!                 astronomical twilight daylength
 
!     + + + local variables + + +
 
!     + + + local variable definitions + + +
!     dec - declination of earth with respect to the sun (degrees)
!     declination -
!     h - hour angle (degrees)
!     hourangle - a function. this variable holds the value returned by
!                 the hourangle function. debe assumed this definition
 
!     + + + common blocks + + +
!     include 'p1unconv.inc'
 
!     + + + function declarations + + +
 
!     + + + end specifications + + +
 
!     declination angle (dec)
!
dec = declination(idoy)
 
!     sunrise or sunset hour angle
h = hourangle(dlat,dec,riseangle)
 
!     calculate the length of the day
daylen = 2.0*h/15.0
!
end function daylen

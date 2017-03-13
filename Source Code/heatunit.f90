function heatunit(tmax,tmin,thres)
!
implicit none
!
! PARAMETER definitions
!
real,parameter :: pi = 3.1415927
!
! Dummy arguments
!
real :: thres,tmax,tmin
real :: heatunit
!
! Local variables
!
real :: range,theta,tmean
!
!     calculates the amount of heat units in degree-days above a
!     threshold temperature assuming a fully sinusoidal daily
!     temperature cycle with maximum and minimum 12 hours apart.
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     tmax   - maximum daily air temperature
!     tmin   - minimum daily air temperature
!     thres  - threshold temperature (such as minimum temperature for growth)
 
!     + + + local variables + +
 
!     + + + local variable definitions + + +
!     tmean  - arithmetic average of tmax and tmin
!     range  - daily range of maximum and minimum temperature
!     theta  - point where threshold and air temperature are equal
!              defines integration limits
 
!     + + + parameters + + +
 
!     + + + end initializations + + +
 
if (thres>=tmax) then
  heatunit = 0.0
else if ((thres<=tmin).or.(tmax<=tmin)) then
  tmean = (tmax+tmin)/2.0
  heatunit = tmean - thres
else
  tmean = (tmax+tmin)/2.0
  range = (tmax-tmin)/2.0
  theta = asin((thres-tmean)/range)
  heatunit = ((tmean-thres)*(pi/2.0-theta)+range*cos(theta))/pi
end if
!
end function heatunit

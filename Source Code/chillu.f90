subroutine chillu(bctchillucum,day_max_temp,day_min_temp)
!
implicit none
!
!
! PARAMETER definitions
!
real,parameter :: tmin = 0.0,tmax = 18.0,topt = 7.0,tdev = 30.0,daylim = 10.0,  &
                & daydev = -0.5
!
! Dummy arguments
!
real :: bctchillucum,day_max_temp,day_min_temp
!
! Local variables
!
real :: relvern,tavg
!
!     + + + purpose + + +
!     calculates the vernalization effectiveness of a day. for fully
!     effective tmeperatures, a full day is returned. for temperatures
!     that are less than fully effective, a partial day or zero is
!     returned. if temperatures are above the upper temperature
!     threshold and insufficient chill days are accumulated, devernalization
!     occurs.
 
!     method taken from: ritchie, j.t. 1991. wheat phasic development in:
!     hanks, j. and ritchie, j.t. eds. modeling plant and soil systems.
!     agronomy monograph 31, pages 34-36.
 
!     + + + keywords + + +
!     vernalization chill units
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     bctchillucum - accumulated chilling units (days)
!     day_max_temp - daily maximum temperature (deg.c)
!     day_min_temp - daily minimum temperature (deg.c)
 
!     + + + local variables + + +
 
 
!     + + + local variable definitions + + +
!     tavg     - daily average temperature  (deg.c)
!     relvern  - relative vernalization effectiveness
!     tmin     - minimum temperature in vernalization function (deg.c)
!     tmax     - maximum temperature in vernalization function (deg.c)
!     topt     - optimum temperature in vernalization function (deg.c)
!     tdev     - temperature above which devernalization can occur (deg.c)
!     daylim   - vernalization days beyond which no devernalization can
!                occur (days)
!     daydev   - devernalization days subtracted for each degree c above
!                tdev (days)
 
      ! find average temperature
tavg = 0.5*(day_max_temp+day_min_temp)
 
if ((tavg>=tmin).and.(tavg<=tmax)) then
  if (tavg<=topt) then
              ! full vernalization effectiveness
     relvern = 1.0
  else
              ! reduced vernalization effectiveness
     relvern = (tmax-tavg)/(tmax-topt)
  end if
else if ((day_max_temp>tdev).and.(bctchillucum<daylim)) then
          ! devernalization
  relvern = daydev*(day_max_temp-tdev)
else
  relvern = 0.0
end if
!
! bctchillucum = bctchillucum + relvern
bctchillucum = max(0.0,bctchillucum+relvern)    ! fred fox change 3/23/2012
!
end subroutine chillu

SUBROUTINE gddcalc(bctmin,tmin,tmax,gddday,gmethod,tbase,toptlo,toptup,tupper)
!
! the code from the gddcalc subroutine in phenologymms was used to begin
! this subroutine.
!
!pass both bctmin and tbase to compare values
! 
!now topt is the average of toptlo and toptup. the upgm variable bctopt is
!no longer passed into topt.
! 
IMPLICIT NONE
!
! Subroutine arguments
!
REAL :: bctmin,gddday,tbase,tmax,tmin,toptlo,toptup,tupper
INTEGER :: gmethod
!
! Local variables
!
REAL :: m,tavg,tavgtemp,tf,tmaxtemp,tmintemp,topt,x,x1,x2,x3,x4,y,y1,y2,y3,y4
!
! debe added variables to read in lower and upper optimum temperatures
!  and maximum upper temperature. later added toptlo, toptup, and tbase
!  which are now read in from the upgm_crop.dat file.
! 
!     + + + purpose + + +
!     to calculate the daily gdd based on the chosen method of calculation. 
!
!     + + + argument definitions + + +
!   bctmin - bctmin is defined as the 'base temperature' (deg. c) in crop.
!            in huc1 and temps it is defined as the minimum crop growth
!            temperature.  it is used here as the 'base temperature'.
!   gddday - the number of gdd with 0°c base temperature for that day.
!   gmethod - selects the method whereby gdd will be calculated.  a value of
!             1 corresponds to method 1 in phenologymms and is used for crops
!             such as winter wheat, winter barley and proso millet. a
!             value of 2 corresponds to method 2 in phenologymms and is used
!             for crops such as corn, sorghum and sunflower.  a value of 3 is
!             the way that weps/upgm calculated ggd for the day.
!   tbase - lowest temperature below which no growth occurs (deg.c).
!   tmax - daily maximum air temperature (deg.c)
!   tmin - daily minimum air temperature (deg.c)
!   topt - optimum temperature (deg. c). now calculated as the average of
!          toptlo and toptup.
!   toptlo - the lower temperature in the optimum range for plant
!            growth (deg.c).
!   toptup - the upper temperature in the optimum range for plant
!            growth (deg.c).
!   tupper - upper/maximum temperature for plant growth (deg.c).
!            no growth with temperatures above this point.
 
!     + + + local variable definitions + + +
!   tavg - the average daily temperature.
!   tavgtemp - the variable that holds the average of tmaxtemp and
!              tmintemp.
!   tf - temperature factor that is used to adjust the gddday value based
!        on where the tavg temperature falls on the temperature model
!        for that method.
!   tmaxtemp - the variable that is set equal to bctmin if the day's maximum
!              air temperature is less than the base temperature.
!              also, it is set equal to tupper if the day's maximum
!              air temperature is greater than the upper/maximum
!              temperature.
!   tmintemp - the variable that is set equal to bctmin if the day's
!              minimum air temperature is less than the base temperature.
!              also, it is set equal to tupper if the day's minimum air
!              temperature is greater than the upper/maximum temperature.
!   x - the x coordinate of the point to be found
!   x1 - the x coordinate of tbase in methods 3 and 4
!   x2 - the x coordinate of the topt point in method 3 and the toptlo
!        point in method 4.
!   x3 - the x coordinate of the toptup point in method 4.
!   x4 - the x coordiante of the tupper point in methods 3 and 4.
!   y - the y coordinate of the point to be found
!   y1 - the y coordinate of tbase in methods 3 and 4
!   y2 - the y coordinate of the topt point in method 3 and the toptlo
!        point in method 4.
!   y3 - the y coordinate of the toptup point in method 4.
!   y4 - the y coordiante of the tupper point in methods 3 and 4.
 
! initialize local variables
tavg = 0.0                  !huc1 - in case we keep the weps/upgm way
tavgtemp = 0.0
tmaxtemp = tmax       ! bwtdmx = daily tmax
tmintemp = tmin       ! bwtdmn = daily tmin
topt = (toptlo+toptup)/2
      !tbase point coordinates
x1 = tbase
y1 = 0.0
      !toplo point ccordinates
x2 = toptlo
y2 = 1.0       ! maximum development rate
      !toptup point coordinates
x3 = toptup
y3 = 1.0
      !tupper point coordinates
x4 = tupper
y4 = 0.0
 
! calculate the current day's gdd (gddday) according to the method of
!   choice.
! gmethod = 1 ! use for wheats, barleys, millets
! gmethod = 2 ! use for corn, sorghum, sunflower, dry beans
! gmethod = 3
! gmethod = 4
 
!debe added bctmin in place of tbase (tbase is a phenologymms variable).
!later changed to using phenologymms variable names
 
! calculate gddday using method 1:
IF (gmethod.EQ.1) THEN
  tavg = (tmax+tmin)/2.0
  gddday = tavg - tbase
  IF (gddday.LT.0.) gddday = 0.
 
! calculate using method 2:
 
!      elseif (gmethod .eq. 2) then
!! adjust tmax and tmin if need be and store values in tmaxtemp and tmintemp
!         if (tmax .lt. tbase) tmaxtemp = tbase
!         if (tmax .gt. toptup) tmaxtemp = toptup
!         if (tmin .lt. tbase) tmintemp = tbase
!         if (tmin .gt. toptup) tmintemp = toptup
!
!         tavgtemp = (tmaxtemp + tmintemp)/2.
!         gddday = tavgtemp - tbase
!
!         if (gddday .lt. 0.) gddday = 0.
!         if (gddday .gt. toptup) gddday = toptup
!
ELSE IF (gmethod.EQ.2) THEN
! a value for toptup is read in from upgm_crop.dat in main.
  IF (tmax.LT.tbase) tmaxtemp = tbase
  IF (tmax.GT.toptup) tmaxtemp = toptup
  IF (tmin.LT.tbase) tmintemp = tbase
  IF (tmin.GT.toptup) tmintemp = toptup
  
 ! print*, 'tbase = ', tbase, 'toptup = ', toptup
 
  tavgtemp = (tmaxtemp+tmintemp)/2.
  gddday = tavgtemp - tbase
!
  IF (gddday.LT.0.) gddday = 0.
  IF (gddday.GT.toptup) gddday = toptup
!
!! calculate using method 3: this is the original weps/upgm way:
!      else if (gmethod .eq. 3) then
!         gddday = huc1(bwtdmx,bwtdmn,bctopt,bctmin)
!      endif
 
!   calculate using method 3:
ELSE IF (gmethod.EQ.3) THEN
         !from phenologymms:
  tavg = (tmax+tmin)/2.
 
! use the slope equation to determine a temperature factor (tf) by which
! the day's gdd is adjusted.
  IF ((tavg.GE.tbase).AND.(tavg.LE.topt)) THEN
     m = (y2-y1)/(x2-x1)       ! find the slope of the line tbase -topt
      !next find the point on the first segment of the linear model where
      !a line drawn perpendicular from the tavg point on the x axis will
      !interesect the first segment of the linear model. the y part of that
      !point will be the tf factor or the y axis portion of the intersected
      !point.  know the slope of the line, now find the point of intersection.
      !use the point-slope equation
      !  y-y1 = m(x-x1) ! x2 and y2 could also be used in place of x1 and y1
     x = tavg
     y = m*x - m*x1 + y1
     tf = y
     gddday = tf*(tavg-tbase)
  ELSE IF ((tavg.GT.topt).AND.(tavg.LE.tupper)) THEN
     m = (y4-y2)/(x4-x2)      ! find the slope of the line topt-tupper
     x = tavg
     y = m*x - m*x4 + y4
     tf = y
     gddday = tf*(tavg-tbase)
  ELSE IF ((tavg.LT.tbase).OR.(tavg.GT.tupper)) THEN
     gddday = 0.0
  END IF
 
! calculate using method 4:
! this is a three segmented linear model
ELSE IF (gmethod.EQ.4) THEN        !this is method 3 w/another line segment
  tavg = (tmax+tmin)/2.
 
  IF ((tavg.GE.tbase).AND.(tavg.LT.toptlo)) THEN
     m = (y2-y1)/(x2-x1)       !find the slope of the line tbase -toptlo
      !  y-y1 = m(x-x1) ! x2 and y2 could also be used in place of x1 and y1
     x = tavg
     y = m*x - m*x1 + y1
     tf = y
     gddday = tf*(tavg-tbase)
  ELSE IF ((tavg.GE.toptlo).AND.(tavg.LE.toptup)) THEN
     m = (y3-y2)/(x3-x2)      ! find the slope of the line toptlo-toptup
     x = tavg             ! this slope should be 0
     y = m*x - m*x2 + y2
     tf = y
     gddday = tf*(tavg-tbase)
  ELSE IF ((tavg.GT.toptup).AND.(tavg.LE.tupper)) THEN
     m = (y4-y3)/(x4-x3)      ! find the slope of the line toptup-tupper
     x = tavg
     y = m*x - m*x3 + y3
     tf = y
     gddday = tf*(tavg-tbase)
! no growth when tavg is lower than tbase or greater than tupper.
  ELSE IF ((tavg.LT.tbase).OR.(tavg.GT.tupper)) THEN
     gddday = 0.0
  END IF
!      count = count + 1
END IF
 
!      if(gmethod .eq. 2) then
!        print *, 'tavgtemp = ', tavgtemp, 'method # ', gmethod,                &
!        &'gddday = ', gddday
!      elseif (gmethod .ne. 2) then
!        print *, 'tavg = ',tavg, 'method # ', gmethod, 'gddday = ', gddday
!      endif
! 
END SUBROUTINE gddcalc

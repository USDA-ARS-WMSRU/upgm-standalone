SUBROUTINE emerge(cliname,cropname,dap,daynum,ddap,dgdds,egdd,elong,emrgflg,ems,&
                & ergdd,gddday,gddtbg,germgdd,germs,ggdd,pd,pdepth,pm,py,seedsw,&
                & soilwat,tempsw,year)
!                
!  debe added the emerge subroutine from phenologymms to begin implementing
!  emergence in upgm. after getting information from fred fox it appears
!  that it is best to call emerge from shoot_grow.for. 07/23/2008
!
!  upgm original pdepth (=growdepth, becomes bc0growdepth) was 2.5cm and
!  phenologymms was 5cm. however, upgm seems to work better with a 4 cm
!  depth, so crop parameter files set to 4 cm pdepth.
!
!  the emerge subroutine calculates the thermal time for germination
!  (germgdd)and elongation rate in mm/gdd (ergdd) for different crops
!  based on the soil water content of the seedbed (seedsw). this
!  subroutine is based on the shootgro model (wilhelm et al., 1993.
!  ecological modelling 68:183-203).
!
!  the soil water level must be input to the emerge subroutine. currently it
!  is read in from the upgm_crop.dat file. 1 = optimum, 2 = medium, 3 = dry,
!  and 4 = planted in dust. the parameters for each crop are also in the
!  upgm_crop.dat file.
!
!  the gdd required for germination increases and elongation rate
!  decreases as water content decreases.  after planting, precipitation
!  can shift the level towards optimum conditions, but there is no
!  provision for reducing the soil level based on evaporation.
! 
!  inputs:  dap(r), daynum(r), ddap(20)(c), dgdds(20)(c), elong(c,r),
!           ems(c,r), ergdd(4)(r), gddday(r), gdds(r), germgdd(4)(r),
!           germs(c,r), pdepth(r), precip(r), seedsw(c,r)
! 
!  outputs: ddap(20)(c), dgdds(20)(c), elong(c,r), ems(c,r),
!           germs(c,r), seedsw(c,r)
! 
!  debe 081308 added the following comments:
!     weps/upgm variables passed into emerge subroutine variables:
!     doy into daynum
!     bc0growdepth into pdepth  note: must convert from meters (bc0growdepth)
!         to cm (pdepth)!!!
!               
!  debe replaced 'bc0growdepth' with 'pdepth' in the passing arguments in the subroutine
!  list above. 'bc0growdepth' (upgm variable name) is passed into 'pdepth' (phenologymms
!  variable) name.
!  debe changed the upgm variable name 'yy' to the phenologymms variable name 'year'.
!  the upgm value for year is passed into the phenologymms variable 'year'.
!  the upgm variables pd, pm, py and cliname are passed to emerge to allow printing these
!  values to the output file emerge.out.
!  debe added two new six element arrays ggdd and egdd with two positions for intermediate values
!  at intermediate soil moisture levels in positions 2 and 4. Later passed in tempsw, the array 
!  index value for these two arrays. It is initialized in Cinit.
!
!     + + + purpose + + +
!     to calculate the day of emergence based on soil moisture and accumulated thermal time.
! 
IMPLICIT NONE
!
INCLUDE 'file.fi'
INCLUDE 'w1clig.inc'
!
! Subroutine arguments
!
CHARACTER(128) :: cliname
CHARACTER(40) :: cropname
INTEGER :: dap,daynum,emrgflg,pd,pm,py,seedsw,tempsw, year 
REAL :: elong,gddday,gddtbg,pdepth
INTEGER,DIMENSION(20) :: ddap
REAL,DIMENSION(20) :: dgdds
REAL,DIMENSION(6) :: egdd,ggdd
INTEGER,DIMENSION(4) :: ems,germs
REAL,DIMENSION(4) :: ergdd,germgdd
CHARACTER(80),DIMENSION(4) :: soilwat
!
! Local variables
!
INTEGER :: i,row
REAL :: pdepthnew,precip
CHARACTER(40) :: seedbed
!
!debe made dap an integer, (it was a real in phenologymms).
!debe added yy from crop for the year. later changed this so that the 'yy' value is
!passed into the phenologymms variable 'year'.
!debe added six element arrays to handle two intermediate soil moisture levels: ggdd, egdd.
 
!debe made pdepth a real, (it was an integer in phenologymms).
!debe changed seedsw from integer to real to allow moving half a soil moisture level with precip.
! later changed back to an integer because array subscripts must be integers or constants.
!debe made soilwat character* (80) to agree with the other subroutines in which it is used.
!debe changed character size to 40 from 80. seedbed and cropname are 40.
!debe added seedbed, emrgflg, pd, pm, py to print out the values.
 
!debe added adjgermgdd and adjergdd variables to adjust the germgdd and ergdd values when moving
! only half a soil moisture level. later these were not needed because of adding the six element
! array and these two variables were no longer needed to hold the intermediate values. they were
! calculated and stored in elements 2 and 4.
  
!debe added pdepthnew to allow converting pdepth which receives the planting depth
!from upgm in meters and must be converted to cm. the converted value is held in
!pdepthnew.
 
!     + + + argument definitions + + +
!     cliname - the name of the location for the climate file. used to
!               write out the name of the climate file location in emerge.out.
!     cropname - name of the current crop.
!     dap - days after planting.
!     daynum - day number of the year - doy is passed into it from weps.
!       doy - day of year. calculated in shoot_grow.
!     ddap - array holding the number of days after planting for up
!            to 20 different stages.
!     dgdds - array holding the number of gdd after seeding for up
!            to 20 different stages.
!     elong - total elongation of the emerging seedling based on the
!             day's gdd (mm)
!     emrgflg - a flag to determine if the new emerge subroutine should be
!               called (emrgflg=1) or to proceed with the weps/upgm method
!               of achieving emergence (emrgflg=0).
!     ems - simulated day of emergence.
!     egdd - a 6 element array that holds the ergdd values plus calculated values
!            for two intermediate soil moisture level values in elements 2 and 4.
!     ergdd - an array holding 4 elongation rates in mm per gdd
!             based on each soil moisture description.
!     gddday - the number of gdd with 0°c base temperature for that day.
!     gddtbg - used to accumulate gdd for seeds planted in dust after a
!              rainfall event has moved the soil moisture condition to
!              dry. then the seed can begin to accumulate gdd to begin
!              germination.
!     germgdd - an array holding 4 germination times in gdd at base 0°c for
!               the soil moisture levels.
!     germs - simulated day that germination occurred.
!     ggdd - a 6 element array that holds the germgdd values plus calculated values for
!           two intermediate soil moisture level values in elements 2 and 4.
!     pd - planting day.
!     pdepth - planting depth; cm.  variable bc0growdepth (in meters)is
!              passed into pdepth from weps.
!     pm - planting month.
!     py - planting year.  currently, not the calendar year.
!     seedsw - soil water content at seed depth.  it is read in as
!              optimum, medium, dry or planted in dust and converted
!              to an integer.	 1 = optimum, 2 = medium, 3 = dry and
!              4 = planted in dust. This becomes the array index for soilwat.
!     soilwat - aan array holding the four soil moisture content at seed depth descriptions 
!     tempsw - a new variable to designate the array subscripts for the new 6 element
!              arrays: egdd, ggdd
 
!     + + + local variable definitions + + +
!     pdepthnew - holds the converted pdepth value to cm.
!     precip - the amount of precipitation read from the weather file for
!              a particular day.  variable precip is set equal to awzdpt
!              which is found in the include file - w1clig.inc.
!     row - the row of soil moisture information.
!     seedbed - description of the soil moisture condition. used to
!       convert the character to an integer.
!     year - variable yy from weps/upgm is used for year. this is the year i of the 
!            rotation and not the calendar year.
 
!     + + + common block variables definitions + + +
!     awzdpt - daily precipitation (mm). awzdpt is found in the include
!              file: w1clig.inc.
 
!     initialize variables
row = 6  
 
!debe added two 6 element arrays to hold two half steps between dry and medium
! and medium and optimum soil water.
! create the new arrays: ggdd for germgdd and egdd for ergdd.
 
DO i = 1,row
  IF (i.EQ.1) THEN      !Optimum
     ggdd(i) = germgdd(i)
     egdd(i) = ergdd(i)
  ELSE IF (i.EQ.2) THEN !between Medium and Optimum
     ggdd(i) = ((germgdd(1)+germgdd(2))/2)
     egdd(i) = ((ergdd(1)+ergdd(2))/2)
  ELSE IF (i.EQ.3) THEN !Medium
     ggdd(i) = germgdd(2)
     egdd(i) = ergdd(2)
  ELSE IF (i.EQ.4) THEN !between Dry and Medium
     ggdd(i) = ((germgdd(2)+germgdd(3))/2)
     egdd(i) = ((ergdd(2)+ergdd(3))/2)
  ELSE IF (i.EQ.5) THEN !Dry
     ggdd(i) = germgdd(3)
     egdd(i) = ergdd(3)
  ELSE IF (i.EQ.6) THEN !Planted in Dust
     ggdd(i) = germgdd(4)
     egdd(i) = ergdd(4)
  END IF
END DO

 
! need to convert planting depth (pdepth) value from weps/upgm in meters
! to cm for pdepth used in emerge.
pdepthnew = pdepth*100.
precip = awzdpt       !awzdpt comes from the common block w1clig.inc
 
!debe added the following code for precip .ge. than 7 mm. a 6 element array was needed to hold the
! half steps between dry and medium and medium and optimum.
IF ((precip.GE.7).AND.(tempsw.LT.6)) THEN !move up to next level if NOT Planted in Dust and precip > 7
                                          !tempsw must be less than 6 to move up.
  tempsw = tempsw - 1
END IF
 
IF (tempsw.EQ.6) THEN !if Planted in Dust move to next levels based on amount of precip
  IF ((precip.GE.7.0).AND.(precip.LT.12.0)) THEN
     tempsw = tempsw - 1 !Dry
  ELSE IF ((precip.GE.12.0).AND.(precip.LT.20.0)) THEN
     tempsw = tempsw - 3 !Medium
  ELSE IF (precip.GE.20.0) THEN
     tempsw = tempsw - 5 ! Optimum
  END IF
END IF
 
! reset tempsw to 1 if it becomes less than 1
IF (tempsw.LT.1) tempsw = 1
 
! seeds planted in dust cannot germinate as though they were planted in
! one of the other soil moisture levels just because a significant
! rainfall event occurs.  the seeds planted in one of the other soil
! moisture levels have already begun the germination process.
! therefore, seeds planted in dust receiving a significant rainfall
! event should be moved up only to the level of planted in dry conditions
! and then begin to accumulate enough growing degree days to emerge.
 
IF (tempsw.LT.6) THEN
  gddtbg = gddtbg + gddday
ELSE
  gddtbg = 0.
END IF
 
!enable seedbed value to be written out to emerge.out
seedbed = soilwat(seedsw)
 
! check if germination can occur
IF ((germs(1).EQ.999).AND.(gddtbg.GE.ggdd(tempsw))) THEN
  germs(1) = daynum
  germs(2) = year
  CALL date1(germs)
  PRINT *,'germination = ',germs
END IF
 
! if germination has occurred then check if elongation is sufficient to
!  allow emergence to occur.
IF ((germs(1).NE.999).AND.(ems(1).EQ.999)) THEN
  elong = elong + (egdd(tempsw)*gddday)
  IF (elong.GE.pdepthnew*10.) THEN  !elong (mm),pdepth (cm)
     ems(1) = daynum
     ems(2) = year
     CALL date1(ems)
     ddap(1) = dap
     dgdds(1) = gddtbg   !debe changed gdds to gddbtg
     PRINT *,'ems = ',ems 
  END IF
END IF
 
 ! this gets written from the day of planting until emergence occurs.
WRITE (luoemerge,1000) cropname,daynum,dap,pd,pm,py,pdepth,ems(1),ems(4),ems(3),&
                     & ems(2),seedbed,emrgflg,gddday,gddtbg,cliname !, 'in Emerge'

!from shoot_grow
! 1000 FORMAT (1x,a15,2x,i3,5x,i3,6x,i2,2x,i2,3x,i1,3x,f5.3,4x,i3,2x,i2,2x,i2,1x,&
!            & i2,2x,a15,7x,i1,3x,f8.2,4x,f8.2,5x,a40) causes the line to go farther: 4x before f8.2


 
 1000 FORMAT (1x,a15,2x,i3,5x,i3,6x,i2,2x,i2,3x,i1,3x,f5.3,4x,i3,2x,i2,2x,i2,1x,&
            & i2,2x,a15,7x,i1,3x,f8.2,1x,f8.2,5x,a40)
! 
END SUBROUTINE emerge 
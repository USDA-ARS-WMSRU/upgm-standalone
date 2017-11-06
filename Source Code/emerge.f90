subroutine emerge(ctrl, clidat, cliname, cropname,dap,daynum,ddap,dgdds,egdd,elong,emrgflg,ems,&
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
!  at intermediate soil moisture levels in positions 2 and 4. later passed in tempsw, the array
!  index value for these two arrays. it is initialized in cinit.
!
!     + + + purpose + + +
!     to calculate the day of emergence based on soil moisture and accumulated thermal time.
    use climate, only : climate_data
    use upgm_simdata, only : controls
implicit none
!
! Dummy arguments
!
    type(climate_data) :: clidat
    type(controls) :: ctrl
character(80) :: cliname
character(80) :: cropname
integer :: dap,daynum,emrgflg,pd,pm,py,seedsw,tempsw,year
real :: elong,gddday,gddtbg,pdepth
integer,dimension(20) :: ddap
real,dimension(20) :: dgdds
real,dimension(6) :: egdd,ggdd
integer,dimension(4) :: ems,germs
real,dimension(4) :: ergdd,germgdd
character(80),dimension(4) :: soilwat
!
! Local variables
!
integer :: i,row
real :: pdepthnew,precip
character(40) :: seedbed
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
!     doy - day of year. calculated in shoot_grow.
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
!     gddday - the number of gdd with 0°C base temperature for that day.
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
!              4 = planted in dust. this becomes the array index for soilwat.
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
 
do i = 1,row
  if (i==1) then        !optimum
     ggdd(i) = germgdd(i)
     egdd(i) = ergdd(i)
  else if (i==2) then   !between medium and optimum
     ggdd(i) = ((germgdd(1)+germgdd(2))/2)
     egdd(i) = ((ergdd(1)+ergdd(2))/2)
  else if (i==3) then   !medium
     ggdd(i) = germgdd(2)
     egdd(i) = ergdd(2)
  else if (i==4) then   !between dry and medium
     ggdd(i) = ((germgdd(2)+germgdd(3))/2)
     egdd(i) = ((ergdd(2)+ergdd(3))/2)
  else if (i==5) then   !dry
     ggdd(i) = germgdd(3)
     egdd(i) = ergdd(3)
  else if (i==6) then   !planted in dust
     ggdd(i) = germgdd(4)
     egdd(i) = ergdd(4)
  end if
end do
 
 
! need to convert planting depth (pdepth) value from weps/upgm in meters
! to cm for pdepth used in emerge.
pdepthnew = pdepth*100.
precip = clidat%awzdpt       !awzdpt comes from the common block w1clig.inc
 
!debe added the following code for precip .ge. than 7 mm. a 6 element array was needed to hold the
! half steps between dry and medium and medium and optimum.
                                          !tempsw must be less than 6 to move up.
if ((precip>=7).and.(tempsw<6)) tempsw = tempsw - 1
                                          !move up to next level if not planted in dust and precip > 7
 
if (tempsw==6) then   !if planted in dust move to next levels based on amount of precip
  if ((precip>=7.0).and.(precip<12.0)) then
     tempsw = tempsw - 1 !dry
  else if ((precip>=12.0).and.(precip<20.0)) then
     tempsw = tempsw - 3 !medium
  else if (precip>=20.0) then
     tempsw = tempsw - 5 ! optimum
  end if
end if
 
! reset tempsw to 1 if it becomes less than 1
if (tempsw<1) tempsw = 1
 
! seeds planted in dust cannot germinate as though they were planted in
! one of the other soil moisture levels just because a significant
! rainfall event occurs.  the seeds planted in one of the other soil
! moisture levels have already begun the germination process.
! therefore, seeds planted in dust receiving a significant rainfall
! event should be moved up only to the level of planted in dry conditions
! and then begin to accumulate enough growing degree days to emerge.
 
if (tempsw<6) then
  gddtbg = gddtbg + gddday
else
  gddtbg = 0.
end if
 
!enable seedbed value to be written out to emerge.out
!de added to prevent soilwat getting an array index of 0
if (seedsw .NE. 0) then
    seedbed = soilwat(seedsw)
else 
    seedbed = ""    
end if  

! check if germination can occur
if ((germs(1)==999).and.(gddtbg>=ggdd(tempsw))) then
  germs(1) = daynum
  germs(2) = year
  call date1(germs)
  print *,'germination = ',germs
end if
 
! if germination has occurred then check if elongation is sufficient to
!  allow emergence to occur.
if ((germs(1)/=999).and.(ems(1)==999)) then
  elong = elong + (egdd(tempsw)*gddday)
  if (elong>=pdepthnew*10.) then    !elong (mm),pdepth (cm)
     ems(1) = daynum
     ems(2) = year
     call date1(ems)
     ddap(1) = dap
     dgdds(1) = gddtbg   !debe changed gdds to gddbtg
     print *,'ems = ',ems
  end if
end if
 
 ! this gets written from the day of planting until emergence occurs.
write (ctrl%handles%luoemerge,1000) cropname,daynum,dap,pd,pm,py,pdepth,ems(1),ems(4),ems(3),&
                     & ems(2),seedbed,emrgflg,gddday,gddtbg,cliname 
 
!from shoot_grow
! 1000 format (1x,a15,2x,i3,5x,i3,6x,i2,2x,i2,3x,i1,3x,f5.3,4x,i3,2x,i2,2x,i2,1x,&
!            & i2,2x,a15,7x,i1,3x,f8.2,4x,f8.2,5x,a40) causes the line to go farther: 4x before f8.2
 
 
 
 1000 format (1x,a15,2x,i3,5x,i3,6x,i2,2x,i2,3x,i1,3x,f5.3,4x,i3,2x,i2,2x,i2,1x,&
            & i2,2x,a15,7x,i1,3x,f8.2,1x,f8.2,5x,a40)
!
end subroutine emerge

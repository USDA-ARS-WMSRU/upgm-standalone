subroutine canopyht(antss,canht,cname,dayhtinc,dummy2,ecanht,ems,gddday,gdde,   &
                  & stemelong,maxht)
!
!debe changed the subroutine arguments to make canopyht applicapable to all crops
!and remove the need for a separate subroutine for each crop
!     
!debe added a generic stage - stemelong to use for the middle stage because
! the name for this stage varies among crops. hay millet, proso millet, spring
! wheat and winter wheat use tss, corn and sorghum use ies, spring and winter
! barley use aifs, dry beans use cots and sunflower uses lf4s.
!debe added ecanht so that it can be read in instead of set in the code for each crop.
!debe took out gddwsf as it is not used.
!
!  the canopyht subroutine calculates the height of the canopy for
!  different crops by calling the appropriate subroutine for each crop.
 
!  inputs:  antss(r), canht(r), cname(r), dummy2(r), ems(r), gddday(r),
!           gdde(r), joints(r), maxht(r)
 
!  outputs: canht(c,r)
!
implicit none
!
! Dummy arguments
!
real :: canht,dayhtinc,ecanht,gddday,gdde,maxht
character(80) :: cname
integer,dimension(4) :: antss,ems,stemelong
real,dimension(32) :: dummy2
!
! Local variables
!
real :: gdds1,gdds2,hrate1,hrate2
!
! subroutine arguments
!
! local variables
!
!debe use gddwsf in place of dummy2. gddwsf column 5 has water stress adjusted
! gdd values. went back to dummy2.
!debe added dayhtinc to be able to pass the daily increase in height to growth
! for the ht_dia_sai subroutine in place of the weps/upgm variable dht when
! canopyflg = 1.
!
!     + + + argument definitions + + +
!     antss - start of anthesis growth stage. this array includes daynum,
!             year, month and day of when this stage was reached.
!     canht - the cumulative height of the plant canopy.
!     cname - name of the crop.
!     dayhtinc - the increase in plant height for today.
!     dummy2 - an array to hold the gdd values, both under stressed
!              and non- stressed conditions,required to reach each growth
!              stage of the current crop.
!     ecanht - this is the maximum canopy height of the crop in phase 1 of
!              the canopy height growth.  this is usually from emergence to
!              when the plant begins elongating stems but this stage varies
!              among crops. it is an input parameter and is read in from upgm_crop.dat.
!     ems - day when emergence occurred in all crops. this array includes
!           daynum, year, month and day of when this event occurred.
!     gddday - the number of gdd with 0°C base temperature for that day.
!     gdde - accumulation of growing degree days since emergence.
!     gdds1 = the sum of ggd needed from emergence to the stage that is equivalent to stemelong.
!     maxht - the maximum height of the plant canopy.
!     stemelong - is a generic stage that holds the middle stage passed in by a crop.
!                 this stage is variable in name among crops so in canopyht,
!                 stemelong holds the stage that is the ending of phase 1 canopy
!                 growth and the beginning of phase2 canopy growth.
 
 
!debe copy the canopycn code as part of making canopyht more generic
! and to be used for all crops. then individual subroutines will not be
! needed for each crop.
 
if (cname=='corn') then
  gdds1 = (dummy2(2)+dummy2(5))
else if (cname=='dry beans') then
  gdds1 = dummy2(2)
else if (cname=='hay millet') then
  gdds1 = (dummy2(3)+dummy2(4)+dummy2(5))
else if (cname=='proso millet') then
  gdds1 = (dummy2(3)+dummy2(4)+dummy2(5))
else if (cname=='sorghum') then
  gdds1 = dummy2(3)
else if (cname=='soybean') then
  gdds1 = dummy2(2)+dummy2(3)+dummy2(4)
else if (cname=='spring barley') then
  gdds1 = (dummy2(3)+dummy2(4)+dummy2(5))
else if (cname=='spring wheat') then
  gdds1 = (dummy2(3)+dummy2(4)+dummy2(5))
else if (cname=='sunflower') then
  gdds1 = dummy2(2)
else if (cname=='winter barley') then 
  gdds1 = (dummy2(2)+dummy2(3)+dummy2(4)+dummy2(5))
else if (cname=='winter wheat') then
  gdds1 = (dummy2(2)+dummy2(3)+dummy2(4)+dummy2(5))
else
  gdds1 = 0.
end if
 
gdds2 = 0.
 
!phase 1 - emergence to 'stemelong'
if (ems(1)/=999.and.gdde<gdds1) then
!  calculate the growth rate for phase 1
  hrate1 = ecanht/gdds1
!debe added a new variable dayhtinc to hold the daily increase in height.
! this is needed so that this value can be passed to the growth subroutine
! and passed to ht_dia_sai when canopyflg = 1. this is used in place of
! the weps/upgm variable dht and is comparable to dht.
  dayhtinc = hrate1*gddday
  canht = canht + dayhtinc !adding today's height incr. to canopy height
!  don't allow canopy to be greater than
!    maximum canopy height for emergence growth stage.
  if (canht>ecanht) canht = ecanht
!
!  phase 2 - from 'stemelong' to the start of anthesis
else if (stemelong(1)/=999.and.antss(1)==999) then
! with corn's correct growth stages: de changed 8/15/08, 10/27/08, 3/20/09
! add the dummy2 elements for the appropriate stages. using gddwsf resulted in the sum
! to be divided in the hrate calculations being too large because the value for antss
! position 7 was 0.0 until the stage was reached.
 
 ! add the growth stage's gdd values for this interval
  if (cname=='corn') then
     gdds2 = (dummy2(6)+dummy2(7)) - (dummy2(5))
  else if (cname=='dry beans') then
     gdds2 = (dummy2(3)+dummy2(4)+dummy2(5)+dummy2(6)+dummy2(7)+dummy2(8)       &
           & +dummy2(9))
  else if (cname=='hay millet') then
     gdds2 = (dummy2(6)+dummy2(7)+dummy2(8)) + (dummy2(9))
  else if (cname=='proso millet') then
     gdds2 = (dummy2(6)+dummy2(7)+dummy2(8)) + (dummy2(9))
  else if (cname=='sorghum') then
     gdds2 = (dummy2(5)+dummy2(6)+dummy2(7)-dummy2(3))    ! equals 215
  else if (cname=='soybean') then
     gdds2 = (dummy2(5)+dummy2(6)+dummy2(7)+dummy2(8))
  else if (cname=='spring barley') then
     gdds2 = (dummy2(6)+dummy2(7)+dummy2(8)) + (dummy2(9))
  else if (cname=='spring wheat') then
     gdds2 = (dummy2(6)+dummy2(7)+dummy2(8)) + (dummy2(9))
  else if (cname=='sunflower') then    !check if this is when max sunflower canopy height is reached (stage8)
     gdds2 = (dummy2(3)+dummy2(4)+dummy2(5)+dummy2(6)+dummy2(7)+dummy2(8)       &
           & +dummy2(9))
  else if (cname=='winter barley') then
     gdds2 = (dummy2(6)+dummy2(7)+dummy2(8)) + (dummy2(9))
  else if (cname=='winter wheat') then
     gdds2 = (dummy2(6)+dummy2(7)+dummy2(8)) + (dummy2(9))
  end if
 
!  calculate the growth rate for this phase of canopy ht. growth
  if (gdds2>0.0) then            !debe added to insure gdds2 is > 0.0
     hrate2 = (maxht-ecanht)/gdds2
     dayhtinc = hrate2*gddday
     canht = canht + dayhtinc
  end if
!         don't let canopy height exceed maximum potential height:
  if (canht>maxht) canht = maxht
 
end if
!
end subroutine canopyht

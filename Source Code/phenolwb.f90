subroutine phenolwb(ctrl, aepa,aifs,antes,antss,pdepth,bhfwsf,boots,cliname,cname,daa,dae,dap,dav,daynum,ddae,ddap,ddav,    &
                  & dgdde,dgdds,dgddv,drs,dummy2,ems,emrgflg,endphenol,first7,fps,gdda,gdde,gdds,gddv,gddwsf,gmethod,heads, &
                  & hrs,ies,joints,lnpout,mats,partcoefleaf,partcoefstem,partcoefrepro,pchron,pdate,seedbed,srs,tis,        &
                  & useupgmpart,year)
!
!  this subroutine simulates phenological growth stages for winter barley.
!
!  note:  have a subroutine to convert input values all to gdd and pass
!         the gdd values into each phenol subroutine.
!
!  note:  can also have a subroutine calculating the stress factor (rai)
!         for use in each phenol subroutine.  if for nothing else base
!         rai on dryland/irrigated and dry/wet years.
!
!
!  inputs: aepa(c,r), aifs(c,r), antes(c,r), antss(c,r), boots(c,r),
!          dae(r), dap(r), dav(r), daynum(r), ddae(20)(c), ddap(20)(c),
!          ddav((20)c), dgdde(20)(c), dgdds(20)(c), dgddv(20)(c),
!          drs(c,r), dummy2(15)(r), first7(c,r), fps(c,r),
!          gdde(r), gdds(r), gddv(r), heads(c,r), hrs(c,r), ies(c,r),
!          joints(c,r), mats(c,r), nolvs(c), pchron(r),
!          srs(c,r), tis(c,r)
 
! outputs: aifs(c,r), antes(c,r), antss(c,r), boots(c,r), ddae(20)(c),
!          ddap(20)(c), ddav((20)c), dgdde(20)(c), dgdds(20)(c),
!          dgddv(20)(c), drs(c,r), first7(c,r), fps(c,r), heads(c,r),
!          hrs(c,r), ies(c,r), joints(c,r), mats(c,r), nolvs(c),
!          srs(c,r), tis(c,r)
!
!
    use upgm_simdata, only : controls
implicit none
!
! Dummy arguments
!
    type(controls) :: ctrl
real :: aepa,bhfwsf,gdda,gdde,gdds,gddv,pchron,pdepth
real :: partcoefleaf,partcoefstem,partcoefrepro
character(80) :: cliname
character(80) :: cname,seedbed
integer :: daa,dae,dap,dav,daynum,emrgflg,first7,gmethod,pdate,year
logical :: endphenol,useupgmpart
integer,dimension(4) :: aifs,antes,antss,boots,drs,ems,fps,heads,hrs,ies,joints,&
                      & mats,srs,tis
integer,dimension(20) :: ddae,ddap,ddav
real,dimension(20) :: dgdde,dgdds,dgddv
real,dimension(32) :: dummy2
real,dimension(16,5) :: gddwsf
real,dimension(100,2) :: lnpout
!
! Local variables
!
real :: adjgdd
integer :: i,j,row
integer,dimension(4) :: pdatearr
!
!debe changed upgm variable name 'bc0growdepth' to phenologymms variable name
!of 'pdepth' to maintain consistency with phenologymms code.
!debe added for writing out phenology info to a file.
!
!debe changed dimensions of dummy 2 array for stressed and non-stressed values.
!debe added bhfwsf to the call to each crop's phenol subroutine to
! implement daily water stress effect on time of reaching a growth stage
!debe added initialized gddwsf array
!!debe added adjgdd value for use in returning the adjusted gdd value
!! from the water_stress subroutine used in determining when a growth stage
!! can be entered.
 
!debe added cliname to write the climate location name to phenol.out
 
!debe added this variable to stop the call to phenol
!DE added variables to enable partitioning at growth stages 5/16/18.
 
!debe added local variable's declaration and initialization
!   local variables
 
!     + + + argument definitions + + +
!     adjgdd - the adjusted gdd required to enter the growth stage.
!     aepa - the parameter for duration of anthesis (i.e., gdd from start
!            to end of anthesis
!     aifs - awns initials formed growth stage for spring barley and winter
!            barley. this array includes daynum, year, month and day of when
!            this stage was reached.
!     antes - end of anthesis growth stage. this array includes daynum, year,
!             month and day of when this stage was reached.
!     antss - start of anthesis growth stage. this array includes daynum, year,
!             month and day of when this stage was reached.
!     bhfwsf - water stress factor ratio (0-1).  this is read in daily.
!     boots - booting growth stage. this array includes daynum, year,
!             month and day of when this stage was reached.  booting is
!            defined as flag leaf has completed its growth.
!     cliname - the name of the location for the climate data.
!     cname - crop name.
!     daa - days after anthesis.
!     dae - days after emergence.
!     dap - days after planting.
!     dav - days after vernalization.
!     daynum - the current day numbered from jan 1.
!     ddae - an array holding the dae for each growth stage.
!     ddap - an array holding the dap for each growth stage.
!     ddav - an array holding the dav for each growth stage.
!     dgdde - an array holding the gdde for each growth stage.
!     dgdds - an array holding the gdds for each growth stage.
!     dgddv -  an array holding the gddv for each growth stage.
!     drs - double ridge growth stage. this array includes daynum, year,
!             month and day of when this stage was reached.
!     dummy2 - an array to hold the gdd values, both under stressed
!              and non- stressed conditions,required to reach each growth
!              stage of the current crop.
!     emrgflg - a flag to determine if the new emerge subroutine should be
!               called (emrgflg=1) or to proceed with the weps/upgm method
!               of achieving emergence (emrgflg=0).
!     ems - day when emergence occurred. this array includes daynum, year,
!             month and day of when this event occurred.
!     endphenol - a flag to indicate if this subroutine should be called
!                 again on the next day.
!     first7 - used to set the value of aepa the first time phenolwb is called.
!     fps - flower primordium initiation growth stage. this array includes
!           daynum, year, month and day of when this stage was reached.
!     gdda - growing degree days from anthesis.
!     gdde - growing degree days from emergence.
!     gdds - growing degree days from seeding.
!     gddv - growing degree days from vernalization.
!     gddwsf - an array to hold the gn and gs gdd values plus the high and
!              low water stress factor values.  these are used in calculating
!              the slope of the line for each growth stage and this is then
!              used to calculate the adjusted gdd value for the current
!              growth stage.
!              column one contains the gn values and is y2.
!              column two contains the gs value and is y1.
!              column three contains wsfhi (high water stress) and is x1.
!              column four contains wsflo (low water stress) and is x2.
!              column five contains the adjgdd value for the stage.
!     gmethod - selects the method whereby gdd will be calculated.  a value
!               of 1 corresponds to method 1 in phenologymms and is used
!               for crops such as winter wheat, winter barley and proso
!               millet. a value of 2 corresponds to method 2 in
!               phenologymms and is used for crops such as corn, sorghum
!               and sunflower.  a value of 3 is the way that weps/upgm
!               calculated ggd for the day.
!     heads - heading growth stage. this array includes daynum, year,
!             month and day of when this stage was reached.
!     hrs - time to harvest ripe growth stage. this array includes daynum,
!           year, month and day of when this stage was reached.
!     ies - start of internode elongation growth stage. this array includes
!           daynum, year, month and day of when this stage was reached.
!     joints - jointing growth stage. this array includes daynum, year,
!             month and day of when this stage was reached.
!     lnpout - an array used in writing out daynum and the number of leaves
!              on that day.  the values are written each time a new leaf has
!              appeared.
!     mats - physiological maturity growth stage. this array includes daynum,
!            year, month and day of when this stage was reached.
!     partcoefleaf - the partitioning coefficient for leaves
!     partcoefrepro - the partitioning coefficient for reproductive parts
!     partcoefstem - the partitioning coefficient for stems
!     pchron - phyllochron value which is the number of gdd per leaf.
!     pdate - planting date.
!     pdepth - depth of growing point at time of planting (m).
!              bc0growdepth is assed into pdepth.
!     seedbed - contains the soil moisture condition of the seedbed.
!     srs - single ridge growth stage. this array includes daynum, year,
!           month and day of when this stage was reached.
!     tis - start of tillering growth stage. this array includes daynum, year,
!           month and day of when this stage was reached.
!     uesupgmpart - flag when TRUE tells model to use the partitioning coefficients.
!     year - year.
 
!     + + + local variable definitions + + +
!     i - this tells which row is to be read in from the dummy2 array.
!     j - a counter variable for outputting the leaf number array.
!     pdatearr - the planting date array. it contains the daynum,
!                year, month and day that planting occurred.
!     row - this holds the row to be filled in the gddwsf array in the
!           water_stress subroutine.
 
! initialize local variables
j = 1
useupgmpart = .true.   ! flag set to true to use the new partitioning coefficients

!debe initialize planting date array
do i = 1,4
  pdatearr(i) = 0
end do
pdatearr(2) = year
 
if (first7==0) then
  aepa = 120.        ! this is the value for crops which use method 1
                     ! for gdd calculation.
  first7 = 1
end if
 
!debe added call to the water_stress subroutine which is called
! only for the current growth stage.
 
! emergence has occurred so fill the first row in the gddwsf array (only
! print for the first period from e to tiller initiation):
if ((ems(1)/=999).and.(tis(1)==999)) then
  row = 1
  i = 1
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
end if
!
! *******   fill growth stage arrays   *******
 
!debe added code to change the adjusted value in gddwsf only when
! the growth stage has been reached.
 
 
!  start of tillering:
if (tis(1)==999) then
  row = 2
  i = 2
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gdde>=gddwsf(2,5)) then          !if gdde is greater than the
                                !adjusted gdd in gddwsf, tillering has begun.
     tis(1) = daynum
     tis(2) = year
     call date1(tis)
     ddap(2) = dap
     ddae(2) = dae
     ddav(2) = dav
     dgdds(2) = gdds
     dgdde(2) = gdde
     dgddv(2) = gddv
     print *,'tis = ',tis
     if (tis(1).ne.999) then
         partcoefleaf = 0.8
         partcoefstem = 0.2
         partcoefrepro = 0.0
     endif
  end if
 
!  single ridge growth stage:
!  single ridge occurs dummy2(3) gdd after vernalization, which is
!  assumed to have occurred by midwinter (jan. 1 or july 1) for winter
!  wheat, or after emergence for spring wheat and spring barley.
else if (srs(1)==999) then
  row = 3
  i = 3
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gddv>=gddwsf(3,5)) then
     srs(1) = daynum
     srs(2) = year
     call date1(srs)
     ddap(3) = dap
     ddae(3) = dae
     ddav(3) = dav
     dgdds(3) = gdds
     dgdde(3) = gdde
     dgddv(3) = gddv
     print *,'srs = ',srs
     if (srs(1).ne.999) then
         partcoefleaf = 0.7
         partcoefstem = 0.3
         partcoefrepro = 0.0
     endif
  end if
 
!  double ridge growth stage:
!  double ridge occurs dummy2(4) gdd phyllochrons after single ridge growth
!  stage.  do not allow additional stages to occur on same day dr
!  is reached.
else if (drs(1)==999) then
  row = 4
  i = 4
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gddv>=(gddwsf(3,5)+gddwsf(4,5))) then
     drs(1) = daynum
     drs(2) = year
     call date1(drs)
     ddap(4) = dap
     ddae(4) = dae
     ddav(4) = dav
     dgdds(4) = gdds
     dgdde(4) = gdde
     dgddv(4) = gddv
     print *,'drs = ',drs
     if (drs(1).ne.999) then
         partcoefleaf = 0.6
         partcoefstem = 0.3
         partcoefrepro = 0.1
     endif
  end if
 
!  awn initials formed stage:
else if ((aifs(1)==999).and.(ies(1)==999)) then
!debe changed this to row 7 to agree with the heading awn initials formed
! in gddwsf.
  row = 7
  i = 5
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
! debe skip empty fps row in gddwsf so use row 7
  if (gddv>=(gddwsf(3,5)+gddwsf(4,5)+gddwsf(7,5))) then
     aifs(1) = daynum
     aifs(2) = year
     call date1(aifs)
     ddap(5) = dap
     ddae(5) = dae
     ddav(5) = dav
     dgdds(5) = gdds
     dgdde(5) = gdde
     dgddv(5) = gddv
     print *,'aifs = ',aifs
  end if
 
!  start of internode elongation:
!  internode elongation begins dummy2(5) gdd phyllochrons after double ridge
!  growth stage.
  row = 6
  i = 5
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gddv>=(gddwsf(3,5)+gddwsf(4,5)+gddwsf(6,5))) then
     ies(1) = daynum
     ies(2) = year
     call date1(ies)
     ddap(6) = dap
     ddae(6) = dae
     ddav(6) = dav
     dgdds(6) = gdds
     dgdde(6) = gdde
     dgddv(6) = gddv
     print *,'ies = ',ies
     if ((aifs(1).ne.999).and.(ies(1).ne.999)) then ! is this the right place for this if block?
         partcoefleaf = 0.5
         partcoefstem = 0.3
         partcoefrepro = 0.2
     endif
  end if
 
!  jointing growth stage prediction:
else if (joints(1)==999) then
  row = 8
  i = 6
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gddv>=(gddwsf(3,5)+gddwsf(4,5)+gddwsf(6,5)+gddwsf(8,5))) then
     joints(1) = daynum
     joints(2) = year
     call date1(joints)
     ddap(8) = dap
     ddae(8) = dae
     ddav(8) = dav
     dgdds(8) = gdds
     dgdde(8) = gdde
     dgddv(8) = gddv
     print *,'joints = ',joints
     if (joints(1).ne.999) then
         partcoefleaf = 0.4
         partcoefstem = 0.3
         partcoefrepro = 0.3
     endif
  end if
 
!  booting growth stage: this is defined as flag leaf has
!  completed its growth.
else if (boots(1)==999) then
  row = 9
  i = 7
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gddv>=(gddwsf(3,5)+gddwsf(4,5)+gddwsf(6,5)+gddwsf(8,5)+gddwsf(9,5))) then
     boots(1) = daynum
     boots(2) = year
     call date1(boots)
     ddap(9) = dap
     ddae(9) = dae
     ddav(9) = dav
     dgdds(9) = gdds
     dgdde(9) = gdde
     dgddv(9) = gddv
     print *,'boots = ',boots
     if (boots(1).ne.999) then
         partcoefleaf = 0.0
         partcoefstem = 0.4
         partcoefrepro = 0.6
     endif
  end if
 
!  heading growth stage:
!  if enough gdd have passed, then heading begins.  go to
!  code for next stage since anthesis is allowed to occur on same day
!  as heading if enough degree-days accumulated today.
else if (heads(1)==999) then
  row = 10
  i = 8
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gddv>=(gddwsf(3,5)+gddwsf(4,5)+gddwsf(6,5)+gddwsf(8,5)+gddwsf(9,5)        &
    & +gddwsf(10,5))) then
     heads(1) = daynum
     heads(2) = year
     call date1(heads)
     ddap(10) = dap
     ddae(10) = dae
     ddav(10) = dav
     dgdds(10) = gdds
     dgdde(10) = gdde
     dgddv(10) = gddv
     print *,'heads = ',heads
     if (tis(1).ne.999) then
         partcoefleaf = 0.0
         partcoefstem = 0.3
         partcoefrepro = 0.7
     endif
  end if
 
!  beginnning of anthesis:
!  allow end of anthesis to occur on same day if enough degree-days have
!  accumulated today.
else if (antss(1)==999) then
  row = 11
  i = 9
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gddv>=(gddwsf(3,5)+gddwsf(4,5)+gddwsf(6,5)+gddwsf(8,5)+gddwsf(9,5)        &
    & +gddwsf(10,5)+gddwsf(11,5))) then
     antss(1) = daynum
     antss(2) = year
     call date1(antss)
     ddap(11) = dap
     ddae(11) = dae
     ddav(11) = dav
     dgdds(11) = gdds
     dgdde(11) = gdde
     dgddv(11) = gddv
     print *,'antss = ',antss
     if (tis(1).ne.999) then
         partcoefleaf = 0.0
         partcoefstem = 0.0
         partcoefrepro = 1.0
     endif
  end if
 
!  physiological maturity:
else if (mats(1)==999) then
  row = 13
  i = 10
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gddv>=(gddwsf(3,5)+gddwsf(4,5)+gddwsf(6,5)+gddwsf(8,5)+gddwsf(9,5)        &
    & +gddwsf(10,5)+gddwsf(11,5)+gddwsf(13,5))) then
     mats(1) = daynum
     mats(2) = year
     call date1(mats)
     ddap(13) = dap
     ddae(13) = dae
     ddav(13) = dav
     dgdds(13) = gdds
     dgdde(13) = gdde
     dgddv(13) = gddv
     print *,'mats = ',mats
  end if
 
! time to harvest ripe:
else if (hrs(1)==999) then
  row = 14
  i = 11
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gddv>=(gddwsf(3,5)+gddwsf(4,5)+gddwsf(6,5)+gddwsf(8,5)+gddwsf(9,5)        &
    & +gddwsf(10,5)+gddwsf(11,5)+gddwsf(13,5)+gddwsf(14,5))) then
     hrs(1) = daynum
     hrs(2) = year
     call date1(hrs)
     ddap(14) = dap
     ddae(14) = dae
     ddav(14) = dav
     dgdds(14) = gdds
     dgdde(14) = gdde
     dgddv(14) = gddv
     print *,'hrs = ',hrs
  end if
end if

!  flower primordium initiation:
!  flower primordium initiation begins 0.3 phyllochrons after
!  double ridge.
if ((drs(1)/=999) .and. (fps(1)==999)) then
  if (gddv>=(gddwsf(3,5)+gddwsf(4,5)+(0.3*pchron))) then
     fps(1) = daynum
     fps(2) = year
     call date1(fps)
     ddap(7) = dap
     ddae(7) = dae
     ddav(7) = dav
     dgdds(7) = gdds
     dgdde(7) = gdde
     dgddv(7) = gddv
     print *,'fps = ',fps
  end if
end if

!  end of anthesis:
if ((antss(1)/=999) .and. (antes(1)==999)) then
! don't need to call water_stress subroutine because antes uses the same
!  gddwsf array locations as antss. to signal antes stage aepa is added to
!  the sum. aepa = anthesis end parameter
  if (gddv>=(gddwsf(3,5)+gddwsf(4,5)+gddwsf(6,5)+gddwsf(8,5)+gddwsf(9,5)        &
    & +gddwsf(10,5)+gddwsf(11,5)+aepa)) then
     antes(1) = daynum
     antes(2) = year
     call date1(antes)
     ddap(12) = dap
     ddae(12) = dae
     ddav(12) = dav
     dgdds(12) = gdds
     dgdde(12) = gdde
     dgddv(12) = gddv
     print *,'antes = ',antes
  end if
end if

!
! *******   output data   *******
! output information from each crop's phenol subroutine.
!
if (hrs(1)/=999) then
  print *,'crop is: ',cname
  print *,'gdds = ',gdds
  print *,'dap = ',dap
  print *,'gdde = ',gdde
  print *,'dae = ',dae
  print *,'gddv = ',gddv
  print *,'dav = ',dav
  print *,'gdda = ',gdda
  print *,'daa = ',daa
  print *,'year = ',year
 
! fill pdatearr
  pdatearr(1) = pdate
  call date1(pdatearr)
 
!  heading for leaf number table
  write (ctrl%handles%luophenol,1000) cname
 
!  write out a table with leaf numbers by doy
  do while (lnpout(j,2)<dgdde(9)/pchron)
     write (ctrl%handles%luophenol,1100) lnpout(j,1),lnpout(j,2)
     j = j + 1
  end do
 
! convert integer boots(1) to a real number
  write (ctrl%handles%luophenol,1100) real(boots(1)),dgdde(9)/pchron
 
!debe add items to print to the output file phenol.out
  write (ctrl%handles%luophenol,1200) cname,cliname,pdepth,pdatearr(1),pdatearr(3),          &
                       & pdatearr(4),gmethod,emrgflg,seedbed,bhfwsf
 
  write (ctrl%handles%luophenol,1300) pdatearr(1),pdatearr(3),pdatearr(4),ems(1),ems(3),     &
                       & ems(4),ddap(1),dgdds(1),tis(1),tis(3),tis(4),ddap(2),  &
                       & ddae(2),dgdds(2),dgdde(2),dgdde(2)/pchron,srs(1),srs(3)&
                       & ,srs(4),ddap(3),ddae(3),ddav(3),dgdds(3),dgdde(3),     &
                       & dgddv(3),dgdde(3)/pchron,drs(1),drs(3),drs(4),ddap(4), &
                       & ddae(4),ddav(4),dgdds(4),dgdde(4),dgddv(4),dgdde(4)    &
                       & /pchron,fps(1),fps(3),fps(4),ddap(7),ddae(7),ddav(7),  &
                       & dgdds(7),dgdde(7),dgddv(7),dgdde(7)/pchron,ies(1),     &
                       & ies(3),ies(4),ddap(6),ddae(6),ddav(6),dgdds(6),dgdde(6)&
                       & ,dgddv(6),dgdde(6)/pchron,aifs(1),aifs(3),aifs(4),     &
                       & ddap(5),ddae(5),ddav(5),dgdds(5),dgdde(5),dgddv(5),    &
                       & dgdde(5)/pchron,joints(1),joints(3),joints(4),ddap(8), &
                       & ddae(8),ddav(8),dgdds(8),dgdde(8),dgddv(8),dgdde(8)    &
                       & /pchron,boots(1),boots(3),boots(4),ddap(9),ddae(9),    &
                       & ddav(9),dgdds(9),dgdde(9),dgddv(9),dgdde(9)/pchron,    &
                       & heads(1),heads(3),heads(4),ddap(10),ddae(10),ddav(10), &
                       & dgdds(10),dgdde(10),dgddv(10),dgdde(9)/pchron,antss(1),&
                       & antss(3),antss(4),ddap(11),ddae(11),ddav(11),dgdds(11),&
                       & dgdde(11),dgddv(11),dgdde(9)/pchron,antes(1),antes(3), &
                       & antes(4),ddap(12),ddae(12),ddav(12),dgdds(12),dgdde(12)&
                       & ,dgddv(12),dgdde(9)/pchron,mats(1),mats(3),mats(4),    &
                       & ddap(13),ddae(13),ddav(13),dgdds(13),dgdde(13),        &
                       & dgddv(13),dgdde(9)/pchron,hrs(1),hrs(3),hrs(4),ddap(14)&
                       & ,ddae(14),ddav(14),dgdds(14),dgdde(14),dgddv(14),      &
                       & dgdde(9)/pchron
 
! debe added writing out and formatting of the gddwsf array
  write (ctrl%handles%luophenol,1400) gddwsf(1,1),gddwsf(1,2),gddwsf(1,3),gddwsf(1,4),       &
                       & gddwsf(1,5),gddwsf(2,1),gddwsf(2,2),gddwsf(2,3),       &
                       & gddwsf(2,4),gddwsf(2,5),gddwsf(3,1),gddwsf(3,2),       &
                       & gddwsf(3,3),gddwsf(3,4),gddwsf(3,5),gddwsf(4,1),       &
                       & gddwsf(4,2),gddwsf(4,3),gddwsf(4,4),gddwsf(4,5),       &
                       & gddwsf(5,1),gddwsf(5,2),gddwsf(5,3),gddwsf(5,4),       &
                       & gddwsf(5,5),gddwsf(6,1),gddwsf(6,2),gddwsf(6,3),       &
                       & gddwsf(6,4),gddwsf(6,5),gddwsf(7,1),gddwsf(7,2),       &
                       & gddwsf(7,3),gddwsf(7,4),gddwsf(7,5),gddwsf(8,1),       &
                       & gddwsf(8,2),gddwsf(8,3),gddwsf(8,4),gddwsf(8,5),       &
                       & gddwsf(9,1),gddwsf(9,2),gddwsf(9,3),gddwsf(9,4),       &
                       & gddwsf(9,5),gddwsf(10,1),gddwsf(10,2),gddwsf(10,3),    &
                       & gddwsf(10,4),gddwsf(10,5),gddwsf(11,1),gddwsf(11,2),   &
                       & gddwsf(11,3),gddwsf(11,4),gddwsf(11,5),gddwsf(12,1),   &
                       & gddwsf(12,2),gddwsf(12,3),gddwsf(12,4),gddwsf(12,5),   &
                       & gddwsf(13,1),gddwsf(13,2),gddwsf(13,3),gddwsf(13,4),   &
                       & gddwsf(13,5),gddwsf(14,1),gddwsf(14,2),gddwsf(14,3),   &
                       & gddwsf(14,4),gddwsf(14,5),gddwsf(15,1),gddwsf(15,2),   &
                       & gddwsf(15,3),gddwsf(15,4),gddwsf(15,5),gddwsf(16,1),   &
                       & gddwsf(16,2),gddwsf(16,3),gddwsf(16,4),gddwsf(16,5)
 
  endphenol = .true.
 
end if
 1000 format (42x,a14)
 1100 format (40x,f5.1,6x,f4.1)
 
 1200 format ('crop name =',2x,a15,/x,'climate location =',2x,a128,/x,          &
             &'planting depth =',2x,f5.3,'(m)',/x,'planting date =',2x,i3,':',  &
            & i2,'/',i2,/x,'gdd method =',2x,i1,/x,'emergence method used =',2x,&
            & i1,/x,'seedbed soil moisture =',2x,a17,/x,'water stress factor =',&
            & 2x,f3.1,/x)
 
 1300 format (' phenological event',7x,'day of year',2x,'date',2x,'dap',5x,     &
            & 'dae',5x,'dav',5x,'gdd ap',5x,'gdd ae',5x,'gdd av',5x,'nolvs',    &
             &/1x'planting date',18x,i4,2x,i2,'/',i2,/1x,'emergence',22x,i4,2x, &
            & i2,'/',i2,1x,i4,21x,f6.1,/1x,'first tiller',19x,i4,2x,i2,'/',i2,  &
            & 1x,i4,4x,i4,13x,f6.1,5x,f6.1,15x,f6.1,/1x,'single ridge',19x,i4,  &
            & 2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,   &
            & /1x,'double ridge',19x,i4,2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1, &
            & 5x,f6.1,5x,f6.1,4x,f6.1,/1x,'floret primordia init begins',3x,i4, &
            & 2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,   &
            & /1x,'stem elongation begins',9x,i4,2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,&
            & 5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'awn initials formed',12x,i4, &
            & 2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,   &
            & /1x,'jointing',23x,i4,2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,  &
            & f6.1,5x,f6.1,4x,f6.1,/1x,'booting',24x,i4,2x,i2,'/',i2,1x,i4,4x,  &
            & i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'heading',24x,i4,2x, &
            & i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x,  &
             &'anthesis starts',16x,i4,2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x,f6.1,  &
            & 5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis ends',18x,i4,2x,i2,'/',i2,  &
            & 1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x,            &
             &'physiological maturity',9x,i4,2x,i2,'/',i2,1x,i4,4x,i4,4x,i4,5x, &
            & f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'harvest ready',18x,i4,2x,i2,'/',&
            & i2,1x,i4,4x,i4,4x,i4,5x,f6.1,5x,f6.1,5x,f6.1,4x,f6.1,/1x)
 
!  heading for gddwsf array
 1400 format (/2x,39x,'gddwsf array',/1x,'phenological event',12x,'gn gdd',2x,  &
             &'gs gdd',2x,'wsfhi',2x,'wsflo',2x,'adjgdd',/1x,'emergence',22x,   &
            & f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'first tiller',19x,f5.1, &
            & 3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'single ridge',19x,f5.1,3x,   &
            & f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'double ridge',19x,f5.1,3x,f5.1, &
            & 3x,f3.1,4x,f3.1,4x,f5.1,/1x,'floret primordia init begins',3x,    &
            & f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'stem elongation begins',&
            & 9x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'awn initials formed',&
            & 12x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'jointing',23x,f5.1, &
            & 3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'booting',24x,f5.1,3x,f5.1,3x,&
            & f3.1,4x,f3.1,4x,f5.1,/1x,'heading',24x,f5.1,3x,f5.1,3x,f3.1,4x,   &
            & f3.1,4x,f5.1,/1x,'anthesis starts',16x,f5.1,3x,f5.1,3x,f3.1,4x,   &
            & f3.1,4x,f5.1,/1x,'anthesis ends',18x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,&
            & 4x,f5.1,/1x,'physiological maturity',9x,f5.1,3x,f5.1,3x,f3.1,4x,  &
            & f3.1,4x,f5.1,/1x,'harvest ready',18x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,&
            & 4x,f5.1,/1x,'row 15',25x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,6x,f3.1,/1x)
!
end subroutine phenolwb

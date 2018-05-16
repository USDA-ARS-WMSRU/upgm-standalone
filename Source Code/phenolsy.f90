subroutine phenolsy(ctrl,aepa,antss,bmats,pdepth,bhfwsf,cots,cliname,cname,daa,dae,dap,daynum,ddae,ddap,dgdde,&
                  & dgdds,dummy2,emrgflg,ems,endphenol,epods,eseeds,first7,gdda,gdde,gdds,gddwsf,gmethod,hrs, &
                  & lf1s,lf2s,lf3s,lf4s,lf5s,lnpout,mats,mffls,mpods,mseeds,partcoefleaf,partcoefstem,        &
                  & partcoefrepro,pchron,pdate,seedbed,useupgmpart,year) 
!
!  the phenolsy subroutine ... finish description here.
!
!  inputs: aepasf(r), antes(c,r), antss(c,r), aspasf(r),
!          boots(c,r), btpasf(r), dae(r), dap(r), dav(r), daynum(r),
!          ddae(c), ddap(c), ddav(c), dgdde(c), dgdds(c), dgddv(c),
!          drpasf(r), drs(c,r), empasf(r), ems(c), first7(c,r),
!          fps(c,r), gdde(r), gdds(r), gddv(r), hdpasf(r), heads(c,r),
!          hrpasf(r), hrs(c,r), iepasf(r), ies(c,r), joints(c,r),
!          jtpasf(r), mats(c,r), mtpasf(r), pchron(r), srpasf(r),
!          srs(c,r), tipasf(r), tis(c,r), tspasf(r), tss(c,r)
 
! outputs: antes(c,r), antss(c,r), boots(c,r), ddae((20)c), ddap(20)(c),
!          ddav(20)(c), dgdde(20)(c), dgdds(20)(c), dgddv(20)(c),
!          drs(c,r), first7(c,r), fps(c,r), heads(c,r), hrs(c,r),
!          ies(c,r), joints(c,r), mats(c,r), srs(c,r), tis(c,r),
!          tss(c,r)
!
    use upgm_simdata, only : controls
    use biomaterial
implicit none
!
! Dummy arguments
!
    type(controls) :: ctrl
!    type(biomatter),    intent(inout) :: bio
real :: aepa,bhfwsf,gdda,gdde,gdds,pchron,pdepth
real :: partcoefleaf,partcoefstem,partcoefrepro
character(80) :: cliname
character(80) :: cname,seedbed
integer :: daa,dae,dap,daynum,emrgflg,first7,gmethod,pdate,year
logical :: endphenol,useupgmpart
integer,dimension(4) :: antss,bmats,cots,ems,epods,eseeds,hrs,lf1s,lf2s,lf3s,lf4s,lf5s,    &
                      & mats,mffls,mpods,mseeds
integer,dimension(20) :: ddae,ddap
real,dimension(20) :: dgdde,dgdds
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
!debe changed dimensions of dummy 2 arrays for stressed and non-stressed
! values.
!debe added bhfwsf to the call to each crop's phenol subroutine to
! implement daily water stress effect on time of reaching a growth stage
!debe added initialized gddwsf array.
!
!debe added the variable endphenol to stop the call to phenol
!debe added cliname to write the climate location name to phenol.out
!DE added variables to enable partitioning at growth stages 5/16/18.
!
!     + + + argument definitions + + +
!     adjgdd - the adjusted gdd required to enter a growth stage.
!     aepa - the parameter for duration of anthesis (i.e., gdd from start
!            to end of anthesis.
!     antss - the start of anthesis growth stage, beginning bloom. one open flower
!             at any node. this array includes daynum, year, month
!             and day of when this stage was reached.
!     bmats - the beginning of maturity. There is one pod anywhere with its mature color.
!             this array includes daynum, year, month and day of when this stage was reached
!     bhfwsf - water stress factor ratio (0-1).  this is read in daily.
!     cliname - the name of the location for the climate data.
!     cname - crop name.
!     cots - cotyledonary and unifoliolate leaves are visible in 
!            soybean. this array includes daynum, year, month and day
!            of when this stage was reached.
!     daa - days after anthesis.
!     dae - days after emergence.
!     dap - days after planting.
!     daynum - the current day numbered from jan 1.
!     ddae - an array holding the dae for each growth stage.
!     ddap - an array holding the dap for each growth stage.
!     dgdde - an array holding the gdde for each growth stage.
!     dgdds - an array holding the gdds for each growth stage.
!     dummy2 - an array to hold the gdd values, both under stressed
!              and non- stressed conditions,required to reach each growth
!              stage of the current crop.
!     emrgflg - a flag to determine if the new emerge subroutine should be
!               called (emrgflg=1) or to proceed with the weps/upgm method
!               of achieving emergence (emrgflg=0).
!     ems - day when emergence occurred. this array includes daynum, year,
!           month and day of when this event occurred.
!     endphenol - a flag to indicate if this subroutine should be called
!                 again on the next day.
!     epods - beginning pod. pod is 3/16" long at one of the four uppermost nodes.
!             this array includes daynum,year, month and day of when this stage was reached.            
!     eseeds - beginning seed. seed is 1/8" long in the pod at one of the 
!              four uppermost nodes. at R5.5, VN occurs and vegetative growth ceases.
!              this array includes daynum, year, month and day of when this stage was reached.
!     first7 - used to set the value of aepa the first time phenolbn is called.
!     gdda - growing degree days from anthesis.
!     gdde - growing degree days from emergence.
!     gdds - growing degree days from seeding.
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
!               phenologymms and is used for crops such as corn, dry beans,
!               sorghum, soybean and sunflower.  a value of 3 is the way that weps/upgm
!               calculated ggd for the day.
!     hrs - harvest ready stage. Plants have dried to less than 15% moisture.
!           this array includes daynum, year, month and day of when this stage was reached.
!     lf1s - stage when the first trifoliolate leaf is unfolded in
!            soybean. this array includes daynum, year, month and day of when
!            this stage was reached.
!     lf2s - stage when the second trifoliolate leaf is unfolded in
!            soybean. this is when internode elongatin begins. this array includes daynum, 
!            year, month and day of when this stage was reached.
!     lf3s - stage when the third trifoliolate leaf is unfolded in
!            soybean. this is when growing point differentition occurs. 
!            this array includes daynum, year, month and day of when this stage was reached.
!     lf4s - the stage when the fourth trifoliolate leaf is unfolded in 
!            soybean.  this array includes daynum, year, month and day of
!            when this stage was reached.
!     lf5s - the stage when the fifth trifoliolate leaf is unfolded in 
!            soybean.  this array includes daynum, year, month and day of
!            when this stage was reached.
!     lnpout - an array used in writing out daynum and the number of leaves
!              on that day.  the values are written each time a new leaf has
!              appeared.
!     mats - full maturity. 95% of the pods have reached their mature color. 
!            physiological maturity. this array includes daynum, year, month and day of
!            when this stage was reached.
!     mffls - full bloom in soybean. one open flower at one of the two uppermost nodes.
!             this array includes daynum, year, month and day of when this stage was reached.
!     mpods - full pod. pod is 3/4" long at one of the four uppermost nodes.
!             this array includes daynum, year, month and day of when this stage was reached.
!     mseeds - full seed. pod containing a green seed that fills the pod cavity at one of the four uppermost nodes.
!              this array includes daynum, year, month and day of when this stage was reached.
!     pchron - phyllochron value which is the number of gdd per leaf.
!     pdate - planting date.
!     pdepth - depth of growing point at time of planting (m).
!              bc0growdepth is assed into pdepth.
!     seedbed - contains the soil moisture condition of the seedbed.
!     year - year.
 
 
!     + + + local variable definitions + + +
!     i - this tells which row is to be read in from the dummy2 array.
!     j - a counter variable for outputting the leaf number array.
!     pdatearr - the planting date array. it contains the daynum,
!                year, month and day that planting occurred.
!     row - this holds the row to be filled in the gddwsf array in the
!           water_stress subroutine.
 
! note: check what aepa should be set to for dry beans.
!****   it is currently not used - is it needed? ****
!  aepa = anthesis end parameter
if (first7==0) then
  aepa = 120.        ! this is the value for crops which use method 1
                     ! for gdd calculation.
                ! note: need to change this value because dry beans uses method 2.
  first7 = 1
end if

useupgmpart = .true.   ! flag set to true to use the new partitioning coefficients

 !debe initialize planting date array
do i = 1,4
  pdatearr(i) = 0
end do
pdatearr(2) = year
 
      j = 1
      
! emergence has occurred so fill the first row in the gddwsf array (only
! print for the first period from e to cotylendoary leaf stage):
if ((ems(1)/=999).and.(cots(1)==999)) then
  row = 1
  i = 1
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
end if
 
!
! *******   fill growth stage arrays   *******
 
!debe added code to change the adjusted value in gddwsf only when
! the growth stage has been reached.
 
!debe adjusted the code for stages that needed to be accumulating
! gdd's simultaneously. that is they are not dependent on the previous
! stage's completion before the stage can begin.
!note: use 'elseif' only when the stage is dependent upon the completion
! of the previous stage before it can begin. otherwise use 'endif' and
! end the 'if' block for that stage.
 
 
!  cotyledonary leaf stage - vc:
if (cots(1)==999) then
  row = 2
  i = 2
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gdde>=gddwsf(2,5)) then
     cots(1) = daynum
     cots(2) = year
     call date1(cots)
     ddap(2) = dap
     ddae(2) = dae
     dgdds(2) = gdds
     dgdde(2) = gdde
     print *,'cots = ',cots
     if (cots(1).ne.999) then
         partcoefleaf = 0.8
         partcoefstem = 0.2
         partcoefrepro = 0.0
     endif
  end if
 
!  1st trifoliolate leaf stage - v1:
else if (lf1s(1)==999) then
  row = 3
  i = 3
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gdde>=(gddwsf(2,5)+gddwsf(3,5))) then
     lf1s(1) = daynum
     lf1s(2) = year
     call date1(lf1s)
     ddap(3) = dap
     ddae(3) = dae
     dgdds(3) = gdds
     dgdde(3) = gdde
     print *,'lf1s = ',lf1s
     if (lf1s(1).ne.999) then
         partcoefleaf = 0.7
         partcoefstem = 0.2
         partcoefrepro = 0.1
     endif
  end if

 
!  2nd trifoliolate leaf stage - v2; start of internode elongation:
else if (lf2s(1)==999) then
  row = 4
  i = 4
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gdde>=(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5))) then
     lf2s(1) = daynum
     lf2s(2) = year
     call date1(lf2s)
     ddap(4) = dap
     ddae(4) = dae
     dgdds(4) = gdds
     dgdde(4) = gdde
     print *,'lf2s = ',lf2s
  end if

 
!  3rd trifoliolate leaf stage - v3; start of growing point differentiation:
else if (lf3s(1)==999) then
  row = 5
  i = 5
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if ((gdde>=gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5))) then
     lf3s(1) = daynum
     lf3s(2) = year
     call date1(lf3s)
     ddap(5) = dap
     ddae(5) = dae
     dgdds(5) = gdds
     dgdde(5) = gdde
     print *,'lf3s = ',lf3s
  end if

 
!  4th trifoliolate leaf stage - v4:
else if (lf4s(1)==999) then
  row = 6
  i = 6
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gdde>=(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5))) then
     lf4s(1) = daynum
     lf4s(2) = year
     call date1(lf4s)
     ddap(6) = dap
     ddae(6) = dae
     dgdds(6) = gdds
     dgdde(6) = gdde
     print *,'lf4s = ',lf4s
  end if

 
!  5th trifoliolate leaf stage - v5:
 else if (lf5s(1)==999) then
  row = 7
  i = 7
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gdde>=(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)+gddwsf(7,5))) then
     lf5s(1) = daynum
     lf5s(2) = year
     call date1(lf5s)
     ddap(7) = dap
     ddae(7) = dae
     dgdds(7) = gdds
     dgdde(7) = gdde
     print *,'lf5s = ',lf5s
  end if
    
! beginning bloom - one open flower at any node - r1
else if (antss(1)==999) then
  row = 8
  i = 8
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gdde>=(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)        &
    & +gddwsf(7,5)+gddwsf(8,5))) then
     antss(1) = daynum
     antss(2) = year
     call date1(antss)
     ddap(8) = dap
     ddae(8) = dae
     dgdds(8) = gdds
     dgdde(8) = gdde
     print *,'antss = ',antss
     if (antss(1).ne.999) then
         partcoefleaf = 0.5
         partcoefstem = 0.3
         partcoefrepro = 0.2
     endif
  end if
 
!  full bloom - one open floer at one of the two uppermost nodes - r2.
else if (mffls(1)==999) then
  row = 9
  i = 9
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gdde>=(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)        &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5))) then
     mffls(1) = daynum
     mffls(2) = year
     call date1(mffls)
     ddap(9) = dap
     ddae(9) = dae
     dgdds(9) = gdds
     dgdde(9) = gdde
     print *,'mffls = ',mffls
     if (mffls(1).ne.999) then
         partcoefleaf = 0.4
         partcoefstem = 0.2
         partcoefrepro = 0.4
     endif
  end if
 
!  beginning pod - pod is 3/16" long at one of the four uppermost nodes - r3
else if (epods(1)==999) then
  row = 10
  i = 10
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gdde>=(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)        &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)+gddwsf(10,5))) then
     epods(1) = daynum
     epods(2) = year
     call date1(epods)
     ddap(10) = dap
     ddae(10) = dae
     dgdds(10) = gdds
     dgdde(10) = gdde
     print *,'epods = ',epods
     if (epods(1).ne.999) then
         partcoefleaf = 0.3
         partcoefstem = 0.1
         partcoefrepro = 0.6
     endif
  end if
 
!  full pod - pod is 3/4" long at one of the four uppermost nodes - r4
elseif (mpods(1)==999) then
  row = 11
  i = 11
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gdde>=(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)        &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)+gddwsf(10,5)+gddwsf(11,5))) then
     mpods(1) = daynum
     mpods(2) = year
     call date1(mpods)
     ddap(11) = dap
     ddae(11) = dae
     dgdds(11) = gdds
     dgdde(11) = gdde
     print *,'mpods = ',mpods
     if (mpods(1).ne.999) then
         partcoefleaf = 0.2
         partcoefstem = 0.1
         partcoefrepro = 0.7
     endif
  end if
 
!  beginning seed - seed is 1/8" long in pod at one of the four uppermost nodes - r5
!  VN occurs at R5.5 
else if (eseeds(1)==999) then
  row = 12
  i = 12
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gdde>=(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)        &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)+gddwsf(10,5)+gddwsf(11,5)+gddwsf(12,5))) then
     eseeds(1) = daynum
     eseeds(2) = year
     call date1(eseeds)
     ddap(12) = dap
     ddae(12) = dae
     dgdds(12) = gdds
     dgdde(12) = gdde
     print *,'eseeds = ',eseeds
     if (eseeds(1).ne.999) then
         partcoefleaf = 0.1
         partcoefstem = 0.1
         partcoefrepro = 0.8
     endif
  end if
 
!  full seed - pod containing a green seed that fills the pod cavity 
!  at one of the four uppermost nodes - r6
else if (mseeds(1)==999) then
  row = 13
  i = 13
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gdde>=(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)        &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)+gddwsf(10,5)+gddwsf(11,5)            &
    & +gddwsf(12,5)+gddwsf(13,5))) then
     mseeds(1) = daynum
     mseeds(2) = year
     call date1(mseeds)
     ddap(13) = dap
     ddae(13) = dae
     dgdds(13) = gdds
     dgdde(13) = gdde
     print *,'mseeds = ',mseeds
     if (mseeds(1).ne.999) then
         partcoefleaf = 0.1
         partcoefstem = 0.0
         partcoefrepro = 0.9
     endif
  end if
 
! beginning maturity - one pod anywhere with its mature color - r7
else if (bmats(1)==999) then
  row = 14
  i = 14
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gdde>=(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)        &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)+gddwsf(10,5)+gddwsf(11,5)            &
    & +gddwsf(12,5)+gddwsf(13,5)+gddwsf(14,5))) then
     bmats(1) = daynum
     bmats(2) = year
     call date1(bmats)
     ddap(14) = dap
     ddae(14) = dae
     dgdds(14) = gdds
     dgdde(14) = gdde
     print *,'bmats = ',bmats
     if (bmats(1).ne.999) then
         partcoefleaf = 0.0
         partcoefstem = 0.0
         partcoefrepro = 1.0
     endif
  end if
    
    ! full maturity - 95% of the pods have reached their mature color - r8
else if (mats(1)==999) then
  row = 15
  i = 15
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gdde>=(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)        &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)+gddwsf(10,5)+gddwsf(11,5)            &
    & +gddwsf(12,5)+gddwsf(13,5)+gddwsf(14,5)+gddwsf(15,5))) then
     mats(1) = daynum
     mats(2) = year
     call date1(mats)
     ddap(15) = dap
     ddae(15) = dae
     dgdds(15) = gdds
     dgdde(15) = gdde
     print *,'mats = ',mats
  end if
 
! harvest ready - plants have dried to less than 15% moisture - r8
else if (hrs(1)==999) then
  row = 16
  i = 16
  call water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  if (gdde>=(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)        &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)+gddwsf(10,5)+gddwsf(11,5)            &
    & +gddwsf(12,5)+gddwsf(13,5)+gddwsf(14,5)+gddwsf(15,5)+gddwsf(16,5))) then
     hrs(1) = daynum
     hrs(2) = year
     call date1(hrs)
     ddap(16) = dap
     ddae(16) = dae
     dgdds(16) = gdds
     dgdde(16) = gdde
     print *,'hrs = ',hrs
  end if
end if

! *******   output data   *******
! output information from each crop's phenol subroutine.
! print to the screen:
if (hrs(1)/=999) then
  print *,'crop is: ',cname
  print *,'gdds = ',gdds
  print *,'dap = ',dap
  print *,'gdde = ',gdde
  print *,'dae = ',dae
  print *,'gdda = ',gdda
  print *,'daa = ',daa
  print *,'year = ',year
!
! fill pdatearr
  pdatearr(1) = pdate
  call date1(pdatearr)
 
!  heading for leaf number table
  write (ctrl%handles%luophenol,1000) cname
  
! The following is the old way of writing out the leaf numbers which allowed &
! the discrepancy in the maximum leaf number in the leaf number table versus &
! that in the phenol table.
  
!  write out a table with leaf numbers by doy
!  do j = 1,60
!!  write only the integer values that are greater than 0 
!     if ((lnpout(j,2)<dgdde(9)/pchron).and.(lnpout(j,2)>0.0))                 &                
!       & write (ctrl%handles%luophenol,1100) lnpout(j,1),lnpout(j,2)
!  end do
  
      do while (lnpout(j,2) .le. dgdde(12)/pchron)
        write (ctrl%handles%luophenol,1100) lnpout(j,1),lnpout(j,2)
        j = j + 1
      end do 
      
!! convert integer epods(1) to a real number
      write(ctrl%handles%luophenol,1100) real (eseeds(1)), dgdde(12)/pchron

      write (ctrl%handles%luophenol,1200)
                   ! leaf number table
!
!debe add items to print to the output file phenol.out
  write (ctrl%handles%luophenol,1300) cname,cliname,pdepth,pdatearr(1),pdatearr(3),          &
                       & pdatearr(4),gmethod,emrgflg,seedbed,bhfwsf
 
  write (ctrl%handles%luophenol,1400) pdatearr(1),pdatearr(3),pdatearr(4),      &
                       & ems(1),ems(3),ems(4),ddap(1),dgdds(1),                 &
                       & cots(1),cots(3),cots(4),ddap(2),ddae(2),dgdds(2),      &
                       & dgdde(2),dgdde(2)/pchron,                              &
                       & lf1s(1),lf1s(3),lf1s(4),ddap(3),ddae(3),dgdds(3),      &
                       & dgdde(3),dgdde(3)/pchron,                              &
                       & lf2s(1),lf2s(3),lf2s(4),ddap(4),ddae(4),dgdds(4),      &
                       & dgdde(4),dgdde(4)/pchron,                              &
                       & lf3s(1),lf3s(3), lf3s(4),ddap(5),ddae(5),dgdds(5),     &
                       & dgdde(5),dgdde(5)/pchron,                              &
                       & lf4s(1),lf4s(3),lf4s(4),ddap(6),ddae(6),dgdds(6),      &
                       & dgdde(6),dgdde(6)/pchron,                              &
                       & lf5s(1),lf5s(3),lf5s(4),ddap(7),ddae(7), dgdds(7),     &
                       & dgdde(7),dgdde(7)/pchron,                              &
                       & antss(1),antss(3),antss(4),ddap(8),ddae(8),dgdds(8),   &
                       & dgdde(8),dgdde(8)/pchron,                              &
                       & mffls(1),mffls(3),mffls(4),ddap(9),ddae(9),dgdds(9),   &
                       & dgdde(9),dgdde(9)/pchron,                              &
                       & epods(1),epods(3),epods(4),ddap(10),ddae(10),dgdds(10),&
                       & dgdde(10),dgdde(10)/pchron,                            &
                       & mpods(1),mpods(3),mpods(4),ddap(11),ddae(11),dgdds(11),&
                       & dgdde(11),dgdde(11)/pchron,                            &
                       & eseeds(1),eseeds(3),eseeds(4),ddap(12),ddae(12),       &
                       & dgdds(12),dgdde(12),dgdde(12)/pchron,                  &
                       & mseeds(1),mseeds(3),mseeds(4),ddap(13),ddae(13),       &
                       & dgdds(13),dgdde(13),dgdde(12)/pchron,                  &
                       & bmats(1),bmats(3),bmats(4),ddap(14),ddae(14),dgdds(14),&
                       & dgdde(14),dgdde(12)/pchron,                            &
                       & mats(1),mats(3),mats(4),ddap(15),ddae(15),dgdds(15),   &
                       & dgdde(15),dgdde(12)/pchron,                            &
                       & hrs(1),hrs(3),hrs(4),ddap(16),ddae(16),dgdds(16),      &
                       & dgdde(16),dgdde(12)/pchron
 
! debe added writing out and formatting of the gddwsf array
  write (ctrl%handles%luophenol,1500) gddwsf(1,1),gddwsf(1,2),gddwsf(1,3),gddwsf(1,4),       &
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
 1000 format (46x,a14)
!
 1100 format (40x,f5.1,6x,f4.1)
 1200 format (/1x) ! write a blank line after outputting the
 
1300  format ('crop name =',2x,a15,/x,'climate location =',2x,a30,/x,          &
             &'planting depth =',2x,f5.3,'(m)',/x,'planting date =',2x,i3,':',  &
            & i2,'/',i2,/x,'gdd method =',2x,i1,/x,'emergence method used =',2x,&
            & i1,/x,'seedbed soil moisture =',2x,a17,/x,'water stress factor =',&
            & 2x,f3.1,/x)

 
 1400 format (' Phenological Event', 7x, 'Day of Year', 2x, 'Date', 2x, & 
     & 'DAP', 5x, 'DAE', 5x, 'GDD AP', 5x, 'GDD AE', 5x, 'NOLVS', /1x,  &
     & 'Planting Date', 18x, i4, 2x, i2, '/', i2, /1x,                  &
     & 'Emergence', 22x, i4, 2x, i2, '/', i2, 1x, i4, 13x, f6.1, /1x,   & 
     & 'Cotyledonary lvs (VC)', 10x, i4, 2x, i2, '/', i2, 1x, i4, 4x,   &
     &    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,                        &
     & '1st trifoliolate lf (V1)', 7x, i4, 2x, i2, '/', i2, 1x, i4, 4x, &
     &    i4, 5x, f6.1, 5x,f6.1, 4x, f6.1, /1x,                         &
     & '2nd trifoliolate lf (V2)', 7x, i4, 2x, i2, '/', i2, 1x, i4, 4x, &
     &    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,                        &
     & '3rd trifoliolate lf (V3)', 7x, i4, 2x, i2, '/', i2, 1x, i4, 4x, &
     &    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,                        &
     & '4th trifoliolate lf (V4)', 7x, i4, 2x, i2, '/', i2, 1x, i4, 4x, &
     &    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,                        &
     & '5th trifoliolate lf (V5)', 7x, i4, 2x, i2, '/', i2, 1x, i4, 4x, &
     &    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,                        &
     & 'Beginning Bloom (R1)', 11x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4,&
     &    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,                            &
     & 'Full Bloom (R2)', 16x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4,     &
     &    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,                            &
     & 'Beginning Pod (R3)', 13x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4,  &
     &    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,                            &
     & 'Full Pod (R4)', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, 5x,   &
     &    f6.1, 5x, f6.1, 4x, f6.1, /1x,                                &
     & 'Beginning Seed (R5)', 12x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4, &
     &    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,                            &
     & 'Full Seed (R6)', 17x, i4, 2x, i2, '/', i2, 1x, i4, 4x,          &
     &    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,                        &
     & 'Beginning Maturity (R7)', 8x, i4, 2x, i2, '/', i2, 1x, i4, 4x,  &
     &    i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,                        &
     & 'Full Maturity (R8)',  13x, i4, 2x, i2, '/', i2, 1x, i4,         &
     &    4x, i4, 5x, f6.1, 5x, f6.1, 4x, f6.1, /1x,                    &
     & 'Harvest Ready', 18x, i4, 2x, i2, '/', i2, 1x, i4, 4x, i4,       &
     &    5x, f6.1, 5x, f6.1, 4x, f6.1, /1x)

 
!  heading for gddwsf array
1500 format (/2x,39x,'gddwsf array',/1x,'phenological event',12x,'gn gdd',2x,  &
             &'gs gdd',2x,'wsfhi',2x,'wsflo',2x,'adjgdd',/1x,'emergence',22x,   &
            & f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'coyyledonary leaves',   &
            & 12x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,                     &
             &'first trifoliolate leaf',8x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,&
            & /1x,'second trifoliolate leaf',7x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,&
            & f5.1,/1x,'third trifoliolate leaf',8x,f5.1,3x,f5.1,3x,f3.1,4x,    &
            & f3.1,4x,f5.1,/1x,'fourth trifoliolate leaf',7x,f5.1,3x,f5.1,3x,   &
            & f3.1,4x,f3.1,4x,f5.1,/1x,'fifth trifoliolate leaf',8x,f5.1,3x,f5.1,3x,   &
            & f3.1,4x,f3.1,4x,f5.1,/1x,'bloom',26x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,&
            & 4x,f5.1,/1x,'mid-full flower',16x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,&
            & f5.1,/1x,'early pod set',18x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,&
            & /1x,'mid pod set',20x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,   &
             &'early seed fill',16x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,   &
             &'mid seed fill',18x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,     &
             &'beginning maturity',13x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1, &
            & /1x,'full maturity',18x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1, &
            & /1x,'harvest ready',18x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x)
             
!
end subroutine phenolsy

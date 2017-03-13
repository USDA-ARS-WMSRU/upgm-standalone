SUBROUTINE phenolbn(aepa,antss,pdepth,bhfwsf,cots,cliname,cname,daa,dae,dap,    &
                  & daynum,ddae,ddap,dgdde,dgdds,dummy2,emrgflg,ems,endphenol,  &
                  & epods,eseeds,first7,gdda,gdde,gdds,gddwsf,gmethod,hrs,lf1s, &
                  & lf2s,lf3s,lf4s,lnpout,mats,mffls,mpods,mseeds,pchron,pdate, &
                  & seedbed,year)
!                  
!  the phenolbn subroutine ... finish description here.
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
IMPLICIT NONE
!
INCLUDE 'file.fi'
!
! Subroutine arguments
!
REAL :: aepa,bhfwsf,gdda,gdde,gdds,pchron,pdepth
CHARACTER(128) :: cliname
CHARACTER(80) :: cname,seedbed
INTEGER :: daa,dae,dap,daynum,emrgflg,first7,gmethod,pdate,year
LOGICAL :: endphenol
INTEGER,DIMENSION(4) :: antss,cots,ems,epods,eseeds,hrs,lf1s,lf2s,lf3s,lf4s,    &
                      & mats,mffls,mpods,mseeds
INTEGER,DIMENSION(20) :: ddae,ddap
REAL,DIMENSION(20) :: dgdde,dgdds
REAL,DIMENSION(30) :: dummy2
REAL,DIMENSION(15,5) :: gddwsf
REAL,DIMENSION(100,2) :: lnpout
!
! Local variables
!
REAL :: adjgdd
INTEGER :: i,j,row
INTEGER,DIMENSION(4) :: pdatearr
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
!
!     + + + argument definitions + + +
!     adjgdd - the adjusted gdd required to enter a growth stage.
!     aepa - the parameter for duration of anthesis (i.e., gdd from start
!            to end of anthesis.
!     antss - the start of anthesis growth stage. one open flower per
!             plant =100% bloom. this array includes daynum, year, month
!             and day of when this stage was reached.
!     bhfwsf - water stress factor ratio (0-1).  this is read in daily.
!     cliname - the name of the location for the climate data.
!     cname - crop name.
!     cots - cotyledonary and unifoliolate leaves are visible in dry
!            beans. this array includes daynum, year, month and day
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
!     epods - one pod has reached the maximum length in dry beans (early
!             pod set). this array includes daynum,year, month and day of
!             when this stage was reached.
!     eseeds - there is one pod with fully developed seeds in dry
!              beans (early seed fill). this array includes daynum, year,
!              month and day of when this stage was reached.
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
!               sorghum and sunflower.  a value of 3 is the way that weps/upgm
!               calculated ggd for the day.
!     hrs - 80% of pods are at the mature color in dry beans. time to
!           harvest ripe growth stage. this array includes daynum, year,
!           month and day of when this stage was reached.
!     lf1s - stage when the first trifoliolate leaf is unfolded in dry
!            beans. this array includes daynum, year, month and day of when
!            this stage was reached.
!     lf2s - stage when the second trifoliolate leaf is unfolded in dry
!            beans. this array includes daynum, year, month and day of
!            when this stage was reached.
!     lf3s - stage when the third trifoliolate leaf is unfolded in dry
!            beans. this array includes daynum, year, month and day of
!            when this stage was reached.
!     lf4s - the stage when the fourth trifoliolate leaf is unfolded in dry
!            beans.  this array includes daynum, year, month and day of
!            when this stage was reached.
!     lnpout - an array used in writing out daynum and the number of leaves
!              on that day.  the values are written each time a new leaf has
!              appeared.
!     mats - one pod has changed color/striped. physiological maturity growth
!            stage. this array includes daynum, year, month and day of
!            when this stage was reached.
!     mffls - the stage of mid to full flower in dry beans. this array
!             includes daynum, year, month and day of when this stage was
!             reached.
!     mpods - the stage when 50% of the pods are at the maximum length
!             (mid pod set). this array includes daynum, year, month and
!             day of when this stage was reached.
!     mseeds - the stage when 50% of the pods have fully developed seeds
!              in dry beans (mid seed fill). this array includes daynum,
!              year, month and day of when this stage was reached.
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
IF (first7.EQ.0) THEN
  aepa = 120.        ! this is the value for crops which use method 1
                     ! for gdd calculation.
                ! note: need to change this value because dry beans uses method 2.
  first7 = 1
END IF
 
 !debe initialize planting date array
DO i = 1,4
  pdatearr(i) = 0
END DO
pdatearr(2) = year
 
! emergence has occurred so fill the first row in the gddwsf array (only
! print for the first period from e to cotylendoary leaf stage):
IF ((ems(1).NE.999).AND.(cots(1).EQ.999)) THEN
  row = 1
  i = 1
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
END IF
 
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
IF (cots(1).EQ.999) THEN
  row = 2
  i = 2
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.gddwsf(2,5)) THEN
     cots(1) = daynum
     cots(2) = year
     CALL date1(cots)
     ddap(2) = dap
     ddae(2) = dae
     dgdds(2) = gdds
     dgdde(2) = gdde
     PRINT *,'cots = ',cots
  END IF
 
!  1st trifoliolate leaf stage - v1:
ELSE IF (lf1s(1).EQ.999) THEN
  row = 3
  i = 3
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5))) THEN
     lf1s(1) = daynum
     lf1s(2) = year
     CALL date1(lf1s)
     ddap(3) = dap
     ddae(3) = dae
     dgdds(3) = gdds
     dgdde(3) = gdde
     PRINT *,'lf1s = ',lf1s
  END IF
        !endif
 
!  2nd trifoliolate leaf stage - v2:
ELSE IF (lf2s(1).EQ.999) THEN
  row = 4
  i = 4
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5))) THEN
     lf2s(1) = daynum
     lf2s(2) = year
     CALL date1(lf2s)
     ddap(4) = dap
     ddae(4) = dae
     dgdds(4) = gdds
     dgdde(4) = gdde
     PRINT *,'lf2s = ',lf2s
  END IF
       ! endif
 
!  3rd trifoliolate leaf stage - v3:
ELSE IF (lf3s(1).EQ.999) THEN
  row = 5
  i = 5
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF ((gdde.GE.gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5))) THEN
     lf3s(1) = daynum
     lf3s(2) = year
     CALL date1(lf3s)
     ddap(5) = dap
     ddae(5) = dae
     dgdds(5) = gdds
     dgdde(5) = gdde
     PRINT *,'lf3s = ',lf3s
  END IF
      !  endif
 
!  4th trifoliolate leaf stage - v4:
ELSE IF (lf4s(1).EQ.999) THEN
  row = 6
  i = 6
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)))    &
    & THEN
     lf4s(1) = daynum
     lf4s(2) = year
     CALL date1(lf4s)
     ddap(6) = dap
     ddae(6) = dae
     dgdds(6) = gdds
     dgdde(6) = gdde
     PRINT *,'lf4s = ',lf4s
  END IF
END IF
 
! one open flower per plant=100% bloom - r1
IF (antss(1).EQ.999) THEN
  row = 7
  i = 7
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)      &
    & +gddwsf(7,5))) THEN
     antss(1) = daynum
     antss(2) = year
     CALL date1(antss)
     ddap(7) = dap
     ddae(7) = dae
     dgdds(7) = gdds
     dgdde(7) = gdde
     PRINT *,'antss = ',antss
  END IF
END IF
 
!  mid to full flower growth stage - r2.
IF (mffls(1).EQ.999) THEN
  row = 8
  i = 8
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)      &
    & +gddwsf(7,5)+gddwsf(8,5))) THEN
     mffls(1) = daynum
     mffls(2) = year
     CALL date1(mffls)
     ddap(8) = dap
     ddae(8) = dae
     dgdds(8) = gdds
     dgdde(8) = gdde
     PRINT *,'mffls = ',mffls
  END IF
END IF
 
!  one pod is at maximum length (early pod set) - r3
IF (epods(1).EQ.999) THEN
  row = 9
  i = 9
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)      &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5))) THEN
     epods(1) = daynum
     epods(2) = year
     CALL date1(epods)
     ddap(9) = dap
     ddae(9) = dae
     dgdds(9) = gdds
     dgdde(9) = gdde
     PRINT *,'epods = ',epods
  END IF
END IF
 
!  50% of the pods are at maximum length (mid pod set) - r4
IF (mpods(1).EQ.999) THEN
  row = 10
  i = 10
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)      &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)+gddwsf(10,5))) THEN
     mpods(1) = daynum
     mpods(2) = year
     CALL date1(mpods)
     ddap(10) = dap
     ddae(10) = dae
     dgdds(10) = gdds
     dgdde(10) = gdde
     PRINT *,'mpods = ',mpods
  END IF
END IF
 
!  one pod has fully developed seeds (early seed fill) - r5
IF (eseeds(1).EQ.999) THEN
  row = 11
  i = 11
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)      &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)+gddwsf(10,5)+gddwsf(11,5))) THEN
     eseeds(1) = daynum
     eseeds(2) = year
     CALL date1(eseeds)
     ddap(11) = dap
     ddae(11) = dae
     dgdds(11) = gdds
     dgdde(11) = gdde
     PRINT *,'eseeds = ',eseeds
  END IF
END IF
 
!  50% of pods have fully developed seeds (mid seed fill) - r6
IF (mseeds(1).EQ.999) THEN
  row = 12
  i = 12
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)      &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)+gddwsf(10,5)+gddwsf(11,5)            &
    & +gddwsf(12,5))) THEN
     mseeds(1) = daynum
     mseeds(2) = year
     CALL date1(mseeds)
     ddap(12) = dap
     ddae(12) = dae
     dgdds(12) = gdds
     dgdde(12) = gdde
     PRINT *,'mseeds = ',mseeds
  END IF
END IF
 
! one pod has changed color/striped physiological maturity growth stage - r7
IF (mats(1).EQ.999) THEN
  row = 13
  i = 13
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)      &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)+gddwsf(10,5)+gddwsf(11,5)            &
    & +gddwsf(12,5)+gddwsf(13,5))) THEN
     mats(1) = daynum
     mats(2) = year
     CALL date1(mats)
     ddap(13) = dap
     ddae(13) = dae
     dgdds(13) = gdds
     dgdde(13) = gdde
     PRINT *,'mats = ',mats
  END IF
END IF
 
! 80% of the pods are at mature harvest color. harvest ready - r8
IF (hrs(1).EQ.999) THEN
  row = 14
  i = 14
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)      &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)+gddwsf(10,5)+gddwsf(11,5)            &
    & +gddwsf(12,5)+gddwsf(13,5)+gddwsf(14,5))) THEN
     hrs(1) = daynum
     hrs(2) = year
     CALL date1(hrs)
     ddap(14) = dap
     ddae(14) = dae
     dgdds(14) = gdds
     dgdde(14) = gdde
     PRINT *,'hrs = ',hrs
  END IF
END IF
 
! *******   output data   *******
! output information from each crop's phenol subroutine.
! print to the screen:
IF (hrs(1).NE.999) THEN
  PRINT *,'crop is: ',cname
  PRINT *,'gdds = ',gdds
  PRINT *,'dap = ',dap
  PRINT *,'gdde = ',gdde
  PRINT *,'dae = ',dae
  PRINT *,'gdda = ',gdda
  PRINT *,'daa = ',daa
  PRINT *,'year = ',year
!
! fill pdatearr
  pdatearr(1) = pdate
  CALL date1(pdatearr)
 
!  heading for leaf number table
  WRITE (luophenol,1000) cname
 
!  write out a table with leaf numbers by doy
  DO j = 1,60
!
!  write only the integer values that are greater than 0
     IF ((lnpout(j,2).LT.dgdde(9)/pchron).AND.(lnpout(j,2).GT.0.0))             &
       & WRITE (luophenol,1100) lnpout(j,1),lnpout(j,2)
  END DO
 
  WRITE (luophenol,1200)
                   ! leaf number table
!
!debe add items to print to the output file phenol.out
  WRITE (luophenol,1300) cname,cliname,pdepth,pdatearr(1),pdatearr(3),          &
                       & pdatearr(4),gmethod,emrgflg,seedbed,bhfwsf
 
  WRITE (luophenol,1400) pdatearr(1),pdatearr(3),pdatearr(4),ems(1),ems(3),     &
                       & ems(4),ddap(1),dgdds(1),cots(1),cots(3),cots(4),ddap(2)&
                       & ,ddae(2),dgdds(2),dgdde(2),dgdde(2)/pchron,lf1s(1),    &
                       & lf1s(3),lf1s(4),ddap(3),ddae(3),dgdds(3),dgdde(3),     &
                       & dgdde(3)/pchron,lf2s(1),lf2s(3),lf2s(4),ddap(4),ddae(4)&
                       & ,dgdds(4),dgdde(4),dgdde(4)/pchron,lf3s(1),lf3s(3),    &
                       & lf3s(4),ddap(5),ddae(5),dgdds(5),dgdde(5),dgdde(5)     &
                       & /pchron,lf4s(1),lf4s(3),lf4s(4),ddap(6),ddae(6),       &
                       & dgdds(6),dgdde(6),dgdde(6)/pchron,antss(1),antss(3),   &
                       & antss(4),ddap(7),ddae(7),dgdds(7),dgdde(7),dgdde(7)    &
                       & /pchron,mffls(1),mffls(3),mffls(4),ddap(8),ddae(8),    &
                       & dgdds(8),dgdde(8),dgdde(8)/pchron,epods(1),epods(3),   &
                       & epods(4),ddap(9),ddae(9),dgdds(9),dgdde(9),dgdde(9)    &
                       & /pchron,mpods(1),mpods(3),mpods(4),ddap(10),ddae(10),  &
                       & dgdds(10),dgdde(10),dgdde(9)/pchron,eseeds(1),eseeds(3)&
                       & ,eseeds(4),ddap(11),ddae(11),dgdds(11),dgdde(11),      &
                       & dgdde(9)/pchron,mseeds(1),mseeds(3),mseeds(4),ddap(12),&
                       & ddae(12),dgdds(12),dgdde(12),dgdde(9)/pchron,mats(1),  &
                       & mats(3),mats(4),ddap(13),ddae(13),dgdds(13),dgdde(13), &
                       & dgdde(9)/pchron,hrs(1),hrs(3),hrs(4),ddap(14),ddae(14),&
                       & dgdds(14),dgdde(14),dgdde(9)/pchron
 
! debe added writing out and formatting of the gddwsf array
  WRITE (luophenol,1500) gddwsf(1,1),gddwsf(1,2),gddwsf(1,3),gddwsf(1,4),       &
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
                       & gddwsf(15,3),gddwsf(15,4),gddwsf(15,5)
 
  endphenol = .TRUE.
 
END IF
 1000 FORMAT (46x,a14)
!
!! convert integer antss(1) to a real number
!      write(luophenol,57) real (antss(1)), dgdde(9)/pchron
 1100 FORMAT (40x,f5.1,6x,f4.1)
 1200 FORMAT (/1x) ! write a blank line after outputting the
 
 1300 FORMAT ('crop name =',2x,a15,/x,'climate location =',2x,a128,/x,          &
             &'planting depth =',2x,f5.3,'(m)',/x,'planting date =',2x,i3,':',  &
            & i2,'/',i2,/x,'gdd method =',2x,i1,/x,'emergence method used =',2x,&
            & i1,/x,'seedbed soil moisture =',2x,a17,/x,'water stress factor =',&
            & 2x,f3.1,/x)
 
 1400 FORMAT (' phenological event',7x,'day of year',2x,'date',2x,'dap',5x,     &
            & 'dae',5x,'gdd ap',5x,'gdd ae',5x,'nolvs',/1x,'planting date',18x, &
            & i4,2x,i2,'/',i2,/1x,'emergence',22x,i4,2x,i2,'/',i2,1x,i4,13x,    &
            & f6.1,/1x,'coyyledonary leaves (vc)',7x,i4,2x,i2,'/',i2,1x,i4,4x,  &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'first trifoliolate leaf (v1)',3x, &
            & i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,          &
             &'second trifoliolate leaf (v2)',2x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,&
            & f6.1,5x,f6.1,4x,f6.1,/1x,'third trifoliolate leaf (v3)',3x,i4,2x, &
            & i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,                &
             &'fourth trifoliolate leaf (v4)',2x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,&
            & f6.1,5x,f6.1,4x,f6.1,/1x,'bloom (r1)',21x,i4,2x,i2,'/',i2,1x,i4,  &
            & 4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'mid-full flower (r2)',11x,i4,  &
            & 2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,             &
             &'early pod set (r3)',13x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,  &
            & f6.1,4x,f6.1,/1x,'mid pod set (r4)',15x,i4,2x,i2,'/',i2,1x,i4,4x, &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'early seed fill (r5)',11x,i4,2x,  &
            & i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,                &
             &'mid seed fill (r6)',13x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,  &
            & f6.1,4x,f6.1,/1x,'maturity (r6)',18x,i4,2x,i2,'/',i2,1x,i4,4x,i4, &
            & 5x,f6.1,5x,f6.1,4x,f6.1,/1x,'harvest ready',18x,i4,2x,i2,'/',i2,  &
            & 1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1)
 
!  heading for gddwsf array
 1500 FORMAT (/2x,39x,'gddwsf array',/1x,'phenological event',12x,'gn gdd',2x,  &
             &'gs gdd',2x,'wsfhi',2x,'wsflo',2x,'adjgdd',/1x,'emergence',22x,   &
            & f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'coyyledonary leaves',   &
            & 12x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,                     &
             &'first trifoliolate leaf',8x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,&
            & /1x,'second trifoliolate leaf',7x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,&
            & f5.1,/1x,'third trifoliolate leaf',8x,f5.1,3x,f5.1,3x,f3.1,4x,    &
            & f3.1,4x,f5.1,/1x,'fourth trifoliolate leaf',7x,f5.1,3x,f5.1,3x,   &
            & f3.1,4x,f3.1,4x,f5.1,/1x,'bloom',26x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,&
            & 4x,f5.1,/1x,'mid-full flower',16x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,&
            & f5.1,/1x,'early pod set',18x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,&
            & /1x,'mid pod set',20x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,   &
             &'early seed fill',16x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,   &
             &'mid seed fill',18x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,     &
             &'physiological maturity',9x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1, &
            & /1x,'harvest ready',18x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x, &
             &'row 15',25x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,6x,f3.1,/1x)
! 
END SUBROUTINE phenolbn
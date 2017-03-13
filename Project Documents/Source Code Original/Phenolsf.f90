SUBROUTINE phenolsf(aepa,antes,antss,pdepth,bhfwsf,browns,cliname,cname,daa,dae,&
                  & dap,daynum,ddae,ddap,dgdde,dgdds,dummy2,emrgflg,ems,        &
                  & endphenol,first7,gdda,gdde,gdds,gddwsf,gmethod,hrs,ies,ies2,&
                  & infls,lf12s,lf4s,lf8s,lnpout,mats,opens,pchron,pdate,       &
                  & seedbed,year,yelows)
!                  
!  the phenolsf subroutine ... finish description here.
!
! upgm original tbase (=bctmin) was 6c, phenologymms assumes 7c, so crop
!      parameter files changed to be 7c.
!
!  the growth stage descriptions are taken from schneiter, a.a. and j.e.
!  miller. 1981. description of sunflower growth stages. crop sci.
!  21:901-903.
! 
!  inputs: antes(c,r), antss(c,r),
!           dae(r), dap(r), daynum(r),
!          ddae(c), ddap(c), dgdde(c), dgdds(c),
!          ems(c), first7(c,r),
!          gdde(r), gdds(r),
!           hrs(c,r), ies(c,r),
!           mats(c,r),  pchron(r),
!          tss(c,r)
! 
! outputs: antes(c,r), antss(c,r), (c,r), ddae((20)c), ddap(20)(c),
!          dgdde(20)(c), dgdds(20)(c),
!          first7(c,r), hrs(c,r),
!          ies(c,r), mats(c,r),
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
INTEGER,DIMENSION(4) :: antes,antss,browns,ems,hrs,ies,ies2,infls,lf12s,lf4s,   &
                      & lf8s,mats,opens,yelows
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
REAL :: real
!
!debe changed upgm variable name 'bc0growdepth' to phenologymms variable name
!of 'pdepth' to maintain consistency with phenologymms code.
!debe added for writing out phenology info to a file.
! 
!debe changed dimensions of dummy 2 array for stressed and non-stressed values.
!debe added bhfwsf to the call to each crop's phenol subroutine to
! implement daily water stress effect on time of reaching a growth stage
!debe added initialized gddwsf array
 
!debe added this variable to stop the call to phenol
!debe added cliname to write the climate location name to phenol.out
! 
!     + + + argument definitions + + +
!     adjgdd - the adjusted gdd required to enter the growth stage.
!     aepa - the parameter for duration of anthesis (i.e., gdd from start
!            to end of anthesis
!     antes - end of anthesis growth stage. this array includes daynum, year,
!             month and day of when this stage was reached.
!     antss - start of anthesis growth stage. this array includes daynum, year,
!             month and day of when this stage was reached.
!     bhfwsf - water stress factor ratio (0-1).  this is read in daily.
!     browns - when the back of the sunflower head is yellow and there may be
!              some brown spotting. this array includes daynum, year, month
!              and day of when this stage was reached.
!     cliname - the name of the location for the climate data.
!     cname - crop name.
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
!             month and day of when this event occurred.
!     endphenol - a flag to indicate if this subroutine should be called
!                 again on the next day.
!     first7 - used to set the value of aepa the first time phenolsf is called.
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
!               phenologymms and is used for crops such as corn, sorghum
!               and sunflower.  a value of 3 is the way that weps/upgm
!               calculated ggd for the day.
!     hrs - time to harvest ripe growth stage. this array includes daynum,
!           year, month and day of when this stage was reached.
!     ies - start of internode elongation growth stage. this array includes
!           daynum, year, month and day of when this stage was reached.
!     ies2 - for sunflower, this is when the internode below the inflorescence
!            continues lengthening and lifts the head above the surrounding
!            leaves more than 2 cm. this array includes daynum, year,
!            month and day of when this stage was reached.
!     infls - the sunflower inflorescence becomes visible. this array includes
!             daynum, year, month and day of when this stage was reached.
!     lf12s - the 12 leaf growth stage. this array includes daynum,
!             year, month and day of when this stage was reached.
!     lf4s - the 4 leaf growth stage. this array includes daynum, year,
!            month and day of when this stage was reached.
!     lf8s - the 8 leaf growth stage. this array includes daynum, year,
!            month and day of when this stage was reached.
!     lnpout - an array used in writing out daynum and the number of leaves
!              on that day.  the values are written each time a new leaf has
!              appeared.
!     mats - physiological maturity growth stage. this array includes daynum,
!            year, month and day of when this stage was reached.
!     opens - the sunflower inflorescence begins to open. this array includes
!             daynum, year, month and day of when this stage was reached.
!     pchron - phyllochron value which is the number of gdd per leaf.
!     pdate - planting date.
!     pdepth - depth of growing point at time of planting (m).
!              bc0growdepth is assed into pdepth.
!     seedbed - contains the soil moisture condition of the seedbed.
!     year - year.
!     yelows - back of the sunflower head is a light yellow. this array
!              includes daynum, year, month and day of when this stage was
!              reached.
 
!     + + + local variable definitions + + +
!     i - this tells which row is to be read in from the dummy2 array.
!     j - a counter variable for outputting the leaf number array.
!     pdatearr - the planting date array. it contains the daynum,
!                year, month and day that planting occurred.
!     row - this holds the row to be filled in the gddwsf array in the
!           water_stress subroutine.
 
! initialize local variables
j = 1
!debe initialize planting date array
DO i = 1,4
  pdatearr(i) = 0
END DO
pdatearr(2) = year
 
! is aepa needed for sunflower and if so what should be the value?
!  aepa = anthesis end parameter
IF (first7.EQ.0) THEN
  aepa = 120.        ! this is the value for crops which use method 1
                     ! for gdd calculation.
  first7 = 1
END IF
 
! emergence has occurred so fill the first row in the gddwsf array (only
! print for the first period from e to 4th leaf stage):
IF ((ems(1).NE.999).AND.(lf4s(1).EQ.999)) THEN
  row = 1
  i = 1
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
END IF
!
! *******   fill growth stage arrays   *******
 
!debe added code to change the adjusted value in gddwsf only when
! the growth stage has been reached.
 
 
!  4th leaf stage - v4
IF (lf4s(1).EQ.999) THEN
  row = 2
  i = 2
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.gddwsf(2,5)) THEN
     lf4s(1) = daynum
     lf4s(2) = year
     CALL date1(lf4s)
     ddap(2) = dap
     ddae(2) = dae
     dgdds(2) = gdds
     dgdde(2) = gdde
     PRINT *,'lf4s = ',lf4s
  END IF
 
! 8th leaf stage - v8
ELSE IF (lf8s(1).EQ.999) THEN
  row = 3
  i = 3
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5))) THEN
     lf8s(1) = daynum
     lf8s(2) = year
     CALL date1(lf8s)
     ddap(3) = dap
     ddae(3) = dae
     dgdds(3) = gdds
     dgdde(3) = gdde
     PRINT *,'lf8s = ',lf8s
  END IF
 
! 12th leaf stage - v12
ELSE IF (lf12s(1).EQ.999) THEN
  row = 4
  i = 4
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5))) THEN
     lf12s(1) = daynum
     lf12s(2) = year
     CALL date1(lf12s)
     ddap(4) = dap
     ddae(4) = dae
     dgdds(4) = gdds
     dgdde(4) = gdde
     PRINT *,'lf12s = ',lf12s
  END IF
 
!  inflorescence becomes visible - r1
ELSE IF (infls(1).EQ.999) THEN
  row = 5
  i = 5
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5))) THEN
     infls(1) = daynum
     infls(2) = year
     CALL date1(infls)
     ddap(5) = dap
     ddae(5) = dae
     dgdds(5) = gdds
     dgdde(5) = gdde
     PRINT *,'infls = ',infls
  END IF
 
!  start of internode elongation directly below the base of the
!  inflorescence - r2
ELSE IF (ies(1).EQ.999) THEN
  row = 6
  i = 6
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)))    &
    & THEN
     ies(1) = daynum
     ies(2) = year
     CALL date1(ies)
     ddap(6) = dap
     ddae(6) = dae
     dgdds(6) = gdds
     dgdde(6) = gdde
     PRINT *,'ies = ',ies
  END IF
 
!  internode elongation continues.  the internode below the
!  reproductive bud continues to elongate and the inflorescence is lifted
!  more than 2 cm above the surrounding leaves. - r3
ELSE IF (ies2(1).EQ.999) THEN
  row = 7
  i = 7
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)      &
    & +gddwsf(7,5))) THEN
     ies2(1) = daynum
     ies2(2) = year
     CALL date1(ies2)
     ddap(7) = dap
     ddae(7) = dae
     dgdds(7) = gdds
     dgdde(7) = gdde
     PRINT *,'ies2 = ',ies2
  END IF
 
!  the inflorescence begins to open and small ray flowers are visible - r4
ELSE IF (opens(1).EQ.999) THEN
  row = 8
  i = 8
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)      &
    & +gddwsf(7,5)+gddwsf(8,5))) THEN
     opens(1) = daynum
     opens(2) = year
     CALL date1(opens)
     ddap(8) = dap
     ddae(8) = dae
     dgdds(8) = gdds
     dgdde(8) = gdde
     PRINT *,'opens = ',opens
  END IF
 
!  anthesis begins.  the mature ray flowers are fully extended
!  and the disk flowers are visible - r5.
ELSE IF (antss(1).EQ.999) THEN
  row = 9
  i = 9
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)      &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5))) THEN
     antss(1) = daynum
     antss(2) = year
     CALL date1(antss)
     ddap(9) = dap
     ddae(9) = dae
     dgdds(9) = gdds
     dgdde(9) = gdde
     PRINT *,'antss = ',antss
  END IF
 
!  anthesis is complete.  the ray flowers are wilting. - r6
ELSE IF (antes(1).EQ.999) THEN
  row = 10
  i = 10
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)      &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)+gddwsf(10,5))) THEN
     antes(1) = daynum
     antes(2) = year
     CALL date1(antes)
     ddap(10) = dap
     ddae(10) = dae
     dgdds(10) = gdds
     dgdde(10) = gdde
     PRINT *,'antes = ',antes
  END IF
 
!  the back of the inflorescence is turning to a light yellow color. - r7
ELSE IF (yelows(1).EQ.999) THEN
  row = 11
  i = 11
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)      &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)+gddwsf(10,5)+gddwsf(11,5))) THEN
     yelows(1) = daynum
     yelows(2) = year
     CALL date1(yelows)
     ddap(11) = dap
     ddae(11) = dae
     dgdds(11) = gdds
     dgdde(11) = gdde
     PRINT *,'yelows = ',yelows
  END IF
 
!  the back of the inflorescence is yellow but the bracts are still green.
!  there may be some brown spotting on the back of the head. - r8
ELSE IF (browns(1).EQ.999) THEN
  row = 12
  i = 12
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(2,5)+gddwsf(3,5)+gddwsf(4,5)+gddwsf(5,5)+gddwsf(6,5)      &
    & +gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)+gddwsf(10,5)+gddwsf(11,5)            &
    & +gddwsf(12,5))) THEN
     browns(1) = daynum
     browns(2) = year
     CALL date1(browns)
     ddap(12) = dap
     ddae(12) = dae
     dgdds(12) = gdds
     dgdde(12) = gdde
     PRINT *,'browns = ',browns
  END IF
 
!  physiological maturity.  the bracts are yellow and brown and much of the
!  back of the head may be brown. - r9
ELSE IF (mats(1).EQ.999) THEN
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
 
!  harvest ready.
ELSE IF (hrs(1).EQ.999) THEN
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
 
! fill pdatearr
  pdatearr(1) = pdate
  CALL date1(pdatearr)
!
!  heading for leaf number table
  WRITE (luophenol,1000) cname
 
!  write out a table with leaf numbers by doy
  DO j = 1,60
!
!  write only the integer values that are greater than 0
     IF ((lnpout(j,2).LT.dgdde(9)/pchron).AND.(lnpout(j,2).GT.0.0))             &
       & WRITE (luophenol,1100) lnpout(j,1),lnpout(j,2)
  END DO
 
! convert integer antss(1) to a real number
  WRITE (luophenol,1100) real(antss(1)),dgdde(9)/pchron
 
!debe add items to print to the output file phenol.out
  WRITE (luophenol,1200) cname,cliname,pdepth,pdatearr(1),pdatearr(3),          &
                       & pdatearr(4),gmethod,emrgflg,seedbed,bhfwsf
 
  WRITE (luophenol,1300) pdatearr(1),pdatearr(3),pdatearr(4),ems(1),ems(3),     &
                       & ems(4),ddap(1),dgdds(1),lf4s(1),lf4s(3),lf4s(4),ddap(2)&
                       & ,ddae(2),dgdds(2),dgdde(2),dgdde(2)/pchron,lf8s(1),    &
                       & lf8s(3),lf8s(4),ddap(3),ddae(3),dgdds(3),dgdde(3),     &
                       & dgdde(3)/pchron,lf12s(1),lf12s(3),lf12s(4),ddap(4),    &
                       & ddae(4),dgdds(4),dgdde(4),dgdde(4)/pchron,infls(1),    &
                       & infls(3),infls(4),ddap(5),ddae(5),dgdds(5),dgdde(5),   &
                       & dgdde(5)/pchron,ies(1),ies(3),ies(4),ddap(6),ddae(6),  &
                       & dgdds(6),dgdde(6),dgdde(6)/pchron,ies2(1),ies2(3),     &
                       & ies2(4),ddap(7),ddae(7),dgdds(7),dgdde(7),dgdde(7)     &
                       & /pchron,opens(1),opens(3),opens(4),ddap(8),ddae(8),    &
                       & dgdds(8),dgdde(8),dgdde(8)/pchron,antss(1),antss(3),   &
                       & antss(4),ddap(9),ddae(9),dgdds(9),dgdde(9),dgdde(9)    &
                       & /pchron,antes(1),antes(3),antes(4),ddap(10),ddae(10),  &
                       & dgdds(10),dgdde(10),dgdde(9)/pchron,yelows(1),yelows(3)&
                       & ,yelows(4),ddap(11),ddae(11),dgdds(11),dgdde(11),      &
                       & dgdde(9)/pchron,browns(1),browns(3),browns(4),ddap(12),&
                       & ddae(12),dgdds(12),dgdde(12),dgdde(9)/pchron,mats(1),  &
                       & mats(3),mats(4),ddap(13),ddae(13),dgdds(13),dgdde(13), &
                       & dgdde(9)/pchron,hrs(1),hrs(3),hrs(4),ddap(14),ddae(14),&
                       & dgdds(14),dgdde(14),dgdde(9)/pchron
 
! debe added writing out and formatting of the gddwsf array
  WRITE (luophenol,1400) gddwsf(1,1),gddwsf(1,2),gddwsf(1,3),gddwsf(1,4),       &
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
 1000 FORMAT (43x,a14)
 1100 FORMAT (40x,f5.1,6x,f4.1)
 
 1200 FORMAT ('crop name =',2x,a15,/x,'climate location =',2x,a128,/x,          &
             &'planting depth =',2x,f5.3,'(m)',/x,'planting date =',2x,i3,':',  &
            & i2,'/',i2,/x,'gdd method =',2x,i1,/x,'emergence method used =',2x,&
            & i1,/x,'seedbed soil moisture =',2x,a17,/x,'water stress factor =',&
            & 2x,f3.1,/x)
 
 1300 FORMAT (' phenological event',7x,'day of year',2x,'date',2x,'dap',5x,     &
            & 'dae',5x,'gdd ap',5x,'gdd ae',5x,'nolvs',/1x'planting date',18x,  &
            & i4,2x,i2,'/',i2,/1x,'emergence',22x,i4,2x,i2,'/',i2,1x,i4,13x,    &
            & f6.1,/1x,'leaf 4',25x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,&
            & 4x,f6.1,/1x,'leaf 8',25x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,  &
            & f6.1,4x,f6.1,/1x,'leaf 12',24x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,    &
            & f6.1,5x,f6.1,4x,f6.1,/1x,'inflorescence visible',10x,i4,2x,i2,'/',&
            & i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'internode elongation',&
            & 11x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,      &
             &'internode elongation > 2',7x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,&
            & 5x,f6.1,4x,f6.1,/1x,'inflorescence opens',12x,i4,2x,i2,'/',i2,1x, &
            & i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis starts',16x,i4,2x, &
            & i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis ends',&
            & 18x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,      &
             &'head back yellow',15x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,    &
            & f6.1,4x,f6.1,/1x,'head yellow brown',14x,i4,2x,i2,'/',i2,1x,i4,4x,&
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'physiological maturity',9x,i4,2x, &
            & i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'harvest ready',&
            & 18x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x)
 
!  heading for gddwsf array
 1400 FORMAT (/2x,39x,'gddwsf array',/1x,'phenological event',12x,'gn gdd',2x,  &
             &'gs gdd',2x,'wsfhi',2x,'wsflo',2x,'adjgdd',/1x,'emergence',22x,   &
            & f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'4th leaf',23x,f5.1,3x,  &
            & f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'8th leaf',23x,f5.1,3x,f5.1,3x,  &
            & f3.1,4x,f3.1,4x,f5.1,/1x,'12th leaf',22x,f5.1,3x,f5.1,3x,f3.1,4x, &
            & f3.1,4x,f5.1,/1x,'inflorescence visible',10x,f5.1,3x,f5.1,3x,f3.1,&
            & 4x,f3.1,4x,f5.1,/1x,'internode elongation',11x,f5.1,3x,f5.1,3x,   &
            & f3.1,4x,f3.1,4x,f5.1,/1x,'internode elongation > 2',7x,f5.1,3x,   &
            & f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'inflorescence opens',12x,f5.1,  &
            & 3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'anthesis starts',16x,f5.1,3x,&
            & f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'anthesis ends',18x,f5.1,3x,f5.1,&
            & 3x,f3.1,4x,f3.1,4x,f5.1,/1x,'head back yellow',15x,f5.1,3x,f5.1,  &
            & 3x,f3.1,4x,f3.1,4x,f5.1,/1x,'head yellow brown',14x,f5.1,3x,f5.1, &
            & 3x,f3.1,4x,f3.1,4x,f5.1,/1x,'physiological maturity',9x,f5.1,3x,  &
            & f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'harvest ready',18x,f5.1,3x,f5.1,&
            & 3x,f3.1,4x,f3.1,4x,f5.1,/1x,'row 15',25x,f5.1,3x,f5.1,3x,f3.1,4x, &
            & f3.1,6x,f3.1,/1x)
! 
END SUBROUTINE phenolsf
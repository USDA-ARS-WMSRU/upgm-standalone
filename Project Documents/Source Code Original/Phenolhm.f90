SUBROUTINE phenolhm(aepa,antes,antss,pdepth,bhfwsf,boots,cliname,cname,daa,dae, &
                  & dap,daynum,ddae,ddap,dgdde,dgdds,drs,dummy2,emrgflg,ems,    &
                  & endphenol,first7,fps,gdda,gdde,gdds,gddwsf,gmethod,heads,   &
                  & hrs,ies,joints,lnpout,mats,pchron,pdate,seedbed,srs,tis,tss,&
                  & year)
!
!  the phenolhm subroutine ... finish description here.
! 
!  inputs:  aepa(c), antes(c,r), antss(c,r), boots(c,r), dae(r),
!           dap(r), daynum(r), ddae(20)(c), ddap(20)(c),
!           dgdde(20)(c), dgdds(20)(c),
!           drs(c,r), dummy2(15)(r), first7(c,r), fps(c,r),
!           gdde(r), gdds(r), heads(c,r), hrs(c,r), ies(c,r),
!           joints(c,r), mats(c,r), nolvs(c), pchron(r), srs(c,r),
!           tis(c,r), tss(c,r)
 
!  outputs: antes(c,r), antss(c,r), boots(c,r), ddae(20)(c),
!          ddap(20)(c), dgdde(20)(c), dgdds(20)(c),
!          drs(c,r), first7(c,r), fps(c,r), heads(c,r),
!          hrs(c,r), ies(c,r), joints(c,r), mats(c,r), nolvs(c),
!          srs(c,r), tis(c,r), tss(c,r)                  
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
INTEGER,DIMENSION(4) :: antes,antss,boots,drs,ems,fps,heads,hrs,ies,joints,mats,&
                      & srs,tis,tss
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
!
!debe added the variable 'endphenol' to stop the call to phenol
!debe added cliname to write the climate location name to phenol.out
 
!   local variables
 
!     + + + argument definitions + + +
!     adjgdd - the adjusted gdd required to enter the growth stage.
!     aepa - the parameter for duration of anthesis (i.e., gdd from start
!            to end of anthesis
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
!     daynum - the current day numbered from jan 1.
!     ddae - an array holding the dae for each growth stage.
!     ddap - an array holding the dap for each growth stage.
!     dgdde - an array holding the gdde for each growth stage.
!     dgdds - an array holding the gdds for each growth stage.
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
!     first7 - used to set the value of aepa the first time phenolhm is called.
!     fps - flower primordium initiation growth stage. this array includes
!           daynum, year, month and day of when this stage was reached.
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
!     pchron - phyllochron value which is the number of gdd per leaf.
!     pdate - planting date.
!     pdepth - depth of growing point at time of planting (m).
!              bc0growdepth is assed into pdepth.
!     seedbed - contains the soil moisture condition of the seedbed.
!     srs - single ridge growth stage. this array includes daynum, year,
!           month and day of when this stage was reached.
!     tis - start of tillering growth stage. this array includes daynum, year,
!           month and day of when this stage was reached.
!     tss - terminal spikelet growth stage. this array includes daynum, year,
!           month and day of when this stage was reached.
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
!debe initialize planting date array
DO i = 1,4
  pdatearr(i) = 0
END DO
pdatearr(2) = year
 
 
IF (first7.EQ.0) THEN
  aepa = 120.        ! this is the value for crops which use method 1
                     ! for gdd calculation.
  first7 = 1
END IF
 
! emergence has occurred so fill the first row in the gddwsf array (only
! print for the first period from e to tiller initiation):
IF ((ems(1).NE.999).AND.(tis(1).EQ.999)) THEN
  row = 1
  i = 1
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
END IF
!
! *******   fill growth stage arrays   *******
 
!debe added code to change the adjusted value in gddwsf only when
! the growth stage has been reached.
 
!  start of tillering:
IF (tis(1).EQ.999) THEN
  row = 2
  i = 2
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.gddwsf(2,5)) THEN
     tis(1) = daynum
     tis(2) = year
     CALL date1(tis)
     ddap(2) = dap
     ddae(2) = dae
     dgdds(2) = gdds
     dgdde(2) = gdde
     PRINT *,'tis = ',tis
  END IF
END IF
 
!  single ridge growth stage:
!debe accumulate gdd simultaneously with that accumulated for tis stage.
IF (srs(1).EQ.999) THEN
  row = 3
  i = 3
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.gddwsf(3,5)) THEN
     srs(1) = daynum
     srs(2) = year
     CALL date1(srs)
     ddap(3) = dap
     ddae(3) = dae
     dgdds(3) = gdds
     dgdde(3) = gdde
     PRINT *,'srs = ',srs
  END IF
 
!  double ridge growth stage:
!  double ridge occurs dummy2(4) gdd phyllochrons after single ridge growth
!  stage.  do not allow additional stages to occur on same day dr
!  is reached.
ELSE IF (drs(1).EQ.999) THEN
  row = 4
  i = 4
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(3,5)+gddwsf(4,5))) THEN
     drs(1) = daynum
     drs(2) = year
     CALL date1(drs)
     ddap(4) = dap
     ddae(4) = dae
     dgdds(4) = gdds
     dgdde(4) = gdde
     PRINT *,'drs = ',drs
  END IF
 
!  flower primordium initiation:
!  flower primordium initiation begins 0.3 phyllochrons after
!  double ridge.
ELSE IF (fps(1).EQ.999) THEN
  IF (gdde.GE.(gddwsf(3,5)+gddwsf(4,5)+(0.3*pchron))) THEN
     fps(1) = daynum
     fps(2) = year
     CALL date1(fps)
     ddap(7) = dap
     ddae(7) = dae
     dgdds(7) = gdds
     dgdde(7) = gdde
     PRINT *,'fps = ',fps
  END IF
 
!  terminal spikelet stage:
!debe combine tss and ies stage because they can occur on the same day.
ELSE IF ((tss(1).EQ.999).AND.(ies(1).EQ.999)) THEN
!
!debe changed following code so that the correct row in gddwsf array will be
! filled with the terminal spikelet information on the row for terminal
! spikelets (row 7) and row 5 will be skipped because fps is on that row and
! filled with zeros.  need to read in value in row 5 in the dummy2 array.
 
  row = 7
  i = 5
 
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
!debe changed last row to be added to rows 3 and 4 to row 7 in gddwsf because
! row 5 was left filled with zeros in the fps stage and ies stage corresponds
! to row 6.  the tss stage needs to be in row 7.
  IF (gdde.GE.(gddwsf(3,5)+gddwsf(4,5)+gddwsf(7,5))) THEN
     tss(1) = daynum
     tss(2) = year
     CALL date1(tss)
     ddap(5) = dap
     ddae(5) = dae
     dgdds(5) = gdds
     dgdde(5) = gdde
     PRINT *,'tss = ',tss
  END IF
 
!  start of internode elongation:
!  internode elongation begins dummy2(5) gdd phyllochrons after double ridge
!  growth stage.
!
!debe changed the following code so that the correct row in gddwsf array is
! filled with the start of internode elongation information on the row for
! internode elongation and row 5 is skipped because fps is in that row and
! filled with zeros.  need to read in value in row 5 in the dummy2 array.
  row = 6
  i = 5
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
!debe changed last row to be added to rows 3 and 4 to be row 6 in gddwsf
! because row 5 was left filled with zeros in the fps stage.
  IF (gdde.GE.(gddwsf(3,5)+gddwsf(4,5)+gddwsf(6,5))) THEN
     ies(1) = daynum
     ies(2) = year
     CALL date1(ies)
     ddap(6) = dap
     ddae(6) = dae
     dgdds(6) = gdds
     dgdde(6) = gdde
     PRINT *,'ies = ',ies
  END IF
 
!  jointing growth stage prediction:
ELSE IF (joints(1).EQ.999) THEN
  row = 8
  i = 6
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
!debe changed following code to add rows 7 and 8 of gddwsf array to
! rows 3 and 4 because row 7 is for end spikelet initiation and
! row 8 is for jointing.
  IF (gdde.GE.(gddwsf(3,5)+gddwsf(4,5)+gddwsf(7,5)+gddwsf(8,5))) THEN
     joints(1) = daynum
     joints(2) = year
     CALL date1(joints)
     ddap(8) = dap
     ddae(8) = dae
     dgdds(8) = gdds
     dgdde(8) = gdde
     PRINT *,'joints = ',joints
  END IF
 
!  booting growth stage: this is defined as flag leaf has
!  completed its growth.
ELSE IF (boots(1).EQ.999) THEN
  row = 9
  i = 7        ! this is the correct row for booting in dummy2 array.
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
!debe changed following code to add rows 7, 8 and 9 of gddwsf array to
! rows 3 and 4 because row 7 is for end spikelet initiation, row 8 is for
! jointing and row 9 is for booting.
  IF (gdde.GE.(gddwsf(3,5)+gddwsf(4,5)+gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)))    &
    & THEN
     boots(1) = daynum
     boots(2) = year
     CALL date1(boots)
     ddap(9) = dap
     ddae(9) = dae
     dgdds(9) = gdds
     dgdde(9) = gdde
     PRINT *,'boots = ',boots
  END IF
 
!  heading growth stage:
!  if enough gdd have passed, then heading begins.  go on to
!  code for next stage since anthesis is allowed to occur on same day
!  as heading if enough degree-days accumulated today.
ELSE IF (heads(1).EQ.999) THEN
  row = 10
  i = 8
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
!debe changed following code to add rows 7,8,9 and 10 of gddwsf array to
! rows 3 and 4 because row 7 is for end spikelet initiation, row 8 is for
! jointing, row 9 is for booting and 10 is for heading.
  IF (gdde.GE.(gddwsf(3,5)+gddwsf(4,5)+gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)      &
    & +gddwsf(10,5))) THEN
     heads(1) = daynum
     heads(2) = year
     CALL date1(heads)
     ddap(10) = dap
     ddae(10) = dae
     dgdds(10) = gdds
     dgdde(10) = gdde
     PRINT *,'heads = ',heads
  END IF
 
!  beginning of anthesis:
!  allow end of anthesis to occur on same day if enough degree-days have
!  accumulated today.
ELSE IF (antss(1).EQ.999) THEN
  row = 11
  i = 9
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
!debe changed following code to add rows 7, 8, 9, 10 and 11 of gddwsf array
! to rows 3 and 4 because row 7 is for end spikelet initiation, row 8 is for
! jointing, row 9 is for booting, row 10 is for heading and row 11 is for
! anthesis starts.
  IF (gdde.GE.(gddwsf(3,5)+gddwsf(4,5)+gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)      &
    & +gddwsf(10,5)+gddwsf(11,5))) THEN
     antss(1) = daynum
     antss(2) = year
     CALL date1(antss)
     ddap(11) = dap
     ddae(11) = dae
     dgdds(11) = gdds
     dgdde(11) = gdde
     PRINT *,'antss = ',antss
  END IF
 
!  end of anthesis:
ELSE IF (antes(1).EQ.999) THEN
!debe changed following code to add the correct rows in gddwsf array as
! noted above.  anthesis ends uses the same additions as anthesis starts with
! the additional value of the aepa parameter.
  IF (gdde.GE.(gddwsf(3,5)+gddwsf(4,5)+gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)      &
    & +gddwsf(10,5)+gddwsf(11,5)+aepa)) THEN
     antes(1) = daynum
     antes(2) = year
     CALL date1(antes)
     ddap(12) = dap
     ddae(12) = dae
     dgdds(12) = gdds
     dgdde(12) = gdde
     PRINT *,'antes = ',antes
  END IF
 
!  physiological maturity:
ELSE IF (mats(1).EQ.999) THEN
  row = 13
  i = 10
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
!debe changed following code to add rows 7, 8, 9, 10, 11 and 13 of gddwsf
! array to rows 3 and 4 because row 7 is for end spikelet initiation, row 8
! is for jointing, row 9 is for booting, row 10 is for heading, row 11 is for
! anthesis starts and row 13 is for physiological maturity.  row 12 is used
! for anthesis ends and is left filled with zeros in gddwsf array.
  IF (gdde.GE.(gddwsf(3,5)+gddwsf(4,5)+gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)      &
    & +gddwsf(10,5)+gddwsf(11,5)+gddwsf(13,5))) THEN
     mats(1) = daynum
     mats(2) = year
     CALL date1(mats)
     ddap(13) = dap
     ddae(13) = dae
     dgdds(13) = gdds
     dgdde(13) = gdde
     PRINT *,'mats = ',mats
  END IF
 
! time to harvest ripe:
ELSE IF (hrs(1).EQ.999) THEN
  row = 14
  i = 11
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
!debe changed following code to add rows 7, 8, 9, 10, 11, 13 and 14 of gddwsf
! array to rows 3 and 4 because row 7 is for end spikelet initiation, row 8
! is for jointing, row 9 is for booting, row 10 is for heading, row 11 is for
! anthesis starts, row 13 is for physiological maturity and row 14 is for
! harvest ready.  row 12 is used for anthesis ends and is left filled with
! zeros in gddwsf array.
  IF (gdde.GE.(gddwsf(3,5)+gddwsf(4,5)+gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)      &
    & +gddwsf(10,5)+gddwsf(11,5)+gddwsf(13,5)+gddwsf(14,5))) THEN
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
 
!
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
!
!  heading for leaf number table
  WRITE (luophenol,1000) cname
 
!  write out a table with leaf numbers by doy
  DO WHILE (lnpout(j,2).LT.dgdde(9)/pchron)
     WRITE (luophenol,1100) lnpout(j,1),lnpout(j,2)
     j = j + 1
  END DO
 
! convert integer boots(1) to a real number
  WRITE (luophenol,1100) real(boots(1)),dgdde(9)/pchron
 
  WRITE (luophenol,1200)
!      leaf number table
 
!debe add items to print to the output file phenol.out
  WRITE (luophenol,1300) cname,cliname,pdepth,pdatearr(1),pdatearr(3),          &
                       & pdatearr(4),gmethod,emrgflg,seedbed,bhfwsf

 !for the phenology table:
  WRITE (luophenol,1400) pdatearr(1),pdatearr(3),pdatearr(4),ems(1),ems(3),     &
                       & ems(4),ddap(1),dgdds(1),tis(1),tis(3),tis(4),ddap(2),  &
                       & ddae(2),dgdds(2),dgdde(2),dgdde(2)/pchron,srs(1),srs(3)&
                       & ,srs(4),ddap(3),ddae(3),dgdds(3),dgdde(3),dgdde(3)     &
                       & /pchron,drs(1),drs(3),drs(4),ddap(4),ddae(4),dgdds(4), &
                       & dgdde(4),dgdde(4)/pchron,fps(1),fps(3),fps(4),ddap(7), &
                       & ddae(7),dgdds(7),dgdde(7),dgdde(7)/pchron,ies(1),ies(3)&
                       & ,ies(4),ddap(6),ddae(6),dgdds(6),dgdde(6),dgdde(6)     &
                       & /pchron,tss(1),tss(3),tss(4),ddap(5),ddae(5),dgdds(5), &
                       & dgdde(5),dgdde(5)/pchron,joints(1),joints(3),joints(4),&
                       & ddap(8),ddae(8),dgdds(8),dgdde(8),dgdde(8)/pchron,     &
                       & boots(1),boots(3),boots(4),ddap(9),ddae(9),dgdds(9),   &
                       & dgdde(9),dgdde(9)/pchron,heads(1),heads(3),heads(4),   &
                       & ddap(10),ddae(10),dgdds(10),dgdde(10),dgdde(9)/pchron, &
                       & antss(1),antss(3),antss(4),ddap(11),ddae(11),dgdds(11),&
                       & dgdde(11),dgdde(9)/pchron,antes(1),antes(3),antes(4),  &
                       & ddap(12),ddae(12),dgdds(12),dgdde(12),dgdde(9)/pchron, &
                       & mats(1),mats(3),mats(4),ddap(13),ddae(13),dgdds(13),   &
                       & dgdde(13),dgdde(9)/pchron,hrs(1),hrs(3),hrs(4),ddap(14)&
                       & ,ddae(14),dgdds(14),dgdde(14),dgdde(9)/pchron

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
 1000 FORMAT (42x,a14)
 1100 FORMAT (40x,f5.1,6x,f4.1)
 1200 FORMAT (/1x) ! write a blank line after outputting the
 
 1300 FORMAT ('crop name =',2x,a15,/x,'climate location =',2x,a128,/x,          &
             &'planting depth =',2x,f5.3,'(m)',/x,'planting date =',2x,i3,':',  &
            & i2,'/',i2,/x,'gdd method =',2x,i1,/x,'emergence method used =',2x,&
            & i1,/x,'seedbed soil moisture =',2x,a17,/x,'water stress factor =',&
            & 2x,f3.1,/x)
 
 1400 FORMAT (' phenological event',7x,'day of year',2x,'date',2x,'dap',5x,     &
            & 'dae',5x,'gdd ap',5x,'gdd ae',5x,'nolvs',/1x'planting date',18x,  &
            & i4,2x,i2,'/',i2,/1x,'emergence',22x,i4,2x,i2,'/',i2,1x,i4,13x,    &
            & f6.1,/1x,'first tiller',19x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,  &
            & 5x,f6.1,4x,f6.1,/1x,'single ridge',19x,i4,2x,i2,'/',i2,1x,i4,4x,  &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'double ridge',19x,i4,2x,i2,'/',i2,&
            & 1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,                          &
             &'floret primordia init begins',3x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x, &
            & f6.1,5x,f6.1,4x,f6.1,/1x,'stem elongation begins',9x,i4,2x,i2,'/',&
            & i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,                       &
             &'end spikelet initiation',8x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1, &
            & 5x,f6.1,4x,f6.1,/1x,'jointing',23x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,&
            & f6.1,5x,f6.1,4x,f6.1,/1x,'booting',24x,i4,2x,i2,'/',i2,1x,i4,4x,  &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'heading',24x,i4,2x,i2,'/',i2,1x,  &
            & i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis starts',16x,i4,2x, &
            & i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis ends',&
            & 18x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,      &
             &'physiological maturity',9x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,  &
            & 5x,f6.1,4x,f6.1,/1x,'harvest ready',18x,i4,2x,i2,'/',i2,1x,i4,4x, &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x)
 
!  heading for gddwsf array
 1500 FORMAT (/2x,39x,'gddwsf array',/1x,'phenological event',12x,'gn gdd',2x,  &
             &'gs gdd',2x,'wsfhi',2x,'wsflo',2x,'adjgdd',/1x,'emergence',22x,   &
            & f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'first tiller',19x,f5.1, &
            & 3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'single ridge',19x,f5.1,3x,   &
            & f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'double ridge',19x,f5.1,3x,f5.1, &
            & 3x,f3.1,4x,f3.1,4x,f5.1,/1x,'floret primordia init begins',3x,    &
            & f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'stem elongation begins',&
            & 9x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,                      &
             &'end spikelet initiation',8x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,&
            & /1x,'jointing',23x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,      &
             &'booting',24x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'heading', &
            & 24x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'anthesis starts',   &
            & 16x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'anthesis ends',18x, &
            & f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'physiological maturity',&
            & 9x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'harvest ready',18x,  &
            & f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'row 15',25x,f5.1,3x,    &
            & f5.1,3x,f3.1,4x,f3.1,6x,f3.1,/1x)
! 
END SUBROUTINE phenolhm

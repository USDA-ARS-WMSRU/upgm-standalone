SUBROUTINE phenolsg(aepa,antes,antss,pdepth,bhfwsf,cliname,cname,daa,dae,dap,   &
                  & daynum,ddae,ddap,dgdde,dgdds,dummy2,emrgflg,ems,endlgs,     &
                  & endphenol,first7,fullbs,gdda,gdde,gdds,gddwsf,gpds,gmethod, &
                  & halfbs,hrs,ies,joints,lnpout,mats,pchron,pdate,seedbed,tis, &
                  & year)
!                  
!  the phenolsg subroutine ... finish description here.
 
!  inputs: aepa(c,r), antes(c,r), antss(c,r), dae(r), dap(r),
!          daynum(r), ddae(20)(c), ddap(20)(c),
!          dgdde(20)(c), dgdds(20)(c), dummy2(15)(r),
!          endlgs(c,r), first7(c,r), fullbs(c,r), gdde(r),
!          gdds(r), gpds(c,r), halfbs(c,r), hrs(c,r),
!          ies(c,r), joints(c,r), mats(c,r), nolvs(c), tis(c,r)
 
! outputs: antes(c,r), antss(c,r), ddae(20)(c), ddap(20)(c),
!          dgdde(20)(c), dgdds(20)(c),
!          endlgs(c,r), first7(c,r), fullbs(c,r), gpds(c,r),
!          halfbs(c,r), hrs(c,r), ies(c,r), joints(c,r), mats(c,r),
!          nolvs(c), tis(c,r) 
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
INTEGER,DIMENSION(4) :: antes,antss,ems,endlgs,fullbs,gpds,halfbs,hrs,ies,      &
                      & joints,mats,tis
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
!     endlgs - end of leaf growth stage in sorghum. this array includes
!              daynum, year, month and day of when this stage was reached.
!     endphenol - a flag to indicate if this subroutine should be called
!                 again on the next day.
!     first7 - used to set the value of aepa the first time phenolsg is called.
!     fullbs - full bloom growth stage in sorghum. this array includes
!            daynum, year, month and day of when this stage was reached.
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
!     gpds - growing point differentiation growth stage in sorghum. this
!            array includes daynum, year, month and day of when this stage
!            was reached.
!     halfbs - half bloom growth stage in sorghum. this array includes
!            daynum, year, month and day of when this stage was reached.
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
!     tis - start of tillering growth stage. this array includes daynum, year,
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
  !aepa = 260.        !aepa=120 old value de changed to 260 to make
                     ! antes occur after fullbs.
                     
  aepa = 120 !debe try new values. 260 gets antes after fullbs but also later than maturity!                   
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
 
!  start of internode elongation:
!  internode elongation begins dummy2(3) gdd after start of tillering
IF (ies(1).EQ.999) THEN
  row = 3
  i = 3
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.gddwsf(3,5)) THEN
     ies(1) = daynum
     ies(2) = year
     CALL date1(ies)
     ddap(3) = dap
     ddae(3) = dae
     dgdds(3) = gdds
     dgdde(3) = gdde
     PRINT *,'ies = ',ies
  END IF
 
  !  jointing growth stage prediction:
ELSE IF (joints(1).EQ.999) THEN
  row = 4
  i = 4
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(3,5)+gddwsf(4,5))) THEN
     joints(1) = daynum
     joints(2) = year
     CALL date1(joints)
     ddap(4) = dap
     ddae(4) = dae
     dgdds(4) = gdds
     dgdde(4) = gdde
     PRINT *,'joints = ',joints
  END IF
END IF
 
!  growing point differentiation:
IF (gpds(1).EQ.999) THEN
  row = 5
  i = 5
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.gddwsf(5,5)) THEN
     gpds(1) = daynum
     gpds(2) = year
     CALL date1(gpds)
     ddap(5) = dap
     ddae(5) = dae
     dgdds(5) = gdds
     dgdde(5) = gdde
     PRINT *,'gpds = ',gpds
  END IF
 
!  end of leaf growth:
ELSE IF (endlgs(1).EQ.999) THEN
  row = 6
  i = 6
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(5,5)+gddwsf(6,5))) THEN
     endlgs(1) = daynum
     endlgs(2) = year
     CALL date1(endlgs)
     ddap(6) = dap
     ddae(6) = dae
     dgdds(6) = gdds
     dgdde(6) = gdde
     PRINT *,'endlgs = ',endlgs
  END IF
 
!  beginning of anthesis:
!  allow end of anthesis to occur on same day if enough degree-days have
!  accumulated today.
ELSE IF (antss(1).EQ.999) THEN
  row = 7
  i = 7
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(5,5)+gddwsf(6,5)+gddwsf(7,5))) THEN
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
 
!  end of anthesis:
IF (antes(1).EQ.999) THEN
  IF (gdde.GE.(gddwsf(5,5)+gddwsf(6,5)+gddwsf(7,5)+aepa)) THEN
     antes(1) = daynum
     antes(2) = year
     CALL date1(antes)
     ddap(12) = dap
     ddae(12) = dae
     dgdds(12) = gdds
     dgdde(12) = gdde
     PRINT *,'antes = ',antes
  END IF
END IF
 
!  half bloom growth stage:
IF ((antss(1).NE.999).AND.(halfbs(1).EQ.999)) THEN
  row = 9
  i = 8
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(5,5)+gddwsf(6,5)+gddwsf(7,5)+gddwsf(8,5))) THEN !(9,5))) THEN debe corrected this and subsequent stages
     halfbs(1) = daynum
     halfbs(2) = year
     CALL date1(halfbs)
     ddap(8) = dap
     ddae(8) = dae
     dgdds(8) = gdds
     dgdde(8) = gdde
     PRINT *,'halfbs = ',halfbs
  END IF
 
!  full bloom growth stage:
ELSE IF ((halfbs(1).NE.999).AND.(fullbs(1).EQ.999)) THEN
  row = 10
  i = 9
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(5,5)+gddwsf(6,5)+gddwsf(7,5)+gddwsf(8,5)+gddwsf(9,5)))   & !(9,5)+gddwsf(10,5)))
    & THEN
     fullbs(1) = daynum
     fullbs(2) = year
     CALL date1(fullbs)
     ddap(9) = dap
     ddae(9) = dae
     dgdds(9) = gdds
     dgdde(9) = gdde
     PRINT *,'fullbs = ',fullbs
  END IF
 
!  physiological maturity:
ELSE IF ((fullbs(1).NE.999).AND.(mats(1).EQ.999)) THEN
  row = 11
  i = 10
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(5,5)+gddwsf(6,5)+gddwsf(7,5)+gddwsf(10,5))) THEN !(11,5))) THEN
     mats(1) = daynum
     mats(2) = year
     CALL date1(mats)
     ddap(10) = dap
     ddae(10) = dae
     dgdds(10) = gdds
     dgdde(10) = gdde
     PRINT *,'mats = ',mats
  END IF
 
! time to harvest ready:
ELSE IF ((mats(1).NE.999).AND.(hrs(1).EQ.999)) THEN
  row = 12
  i = 11
  CALL water_stress(adjgdd,bhfwsf,dummy2,gddwsf,row,i)
  IF (gdde.GE.(gddwsf(5,5)+gddwsf(6,5)+gddwsf(7,5)+gddwsf(10,5)+gddwsf(11,5)))  & !(11,5)+gddwsf(12,5)))
    & THEN
     hrs(1) = daynum
     hrs(2) = year
     CALL date1(hrs)
     ddap(11) = dap
     ddae(11) = dae
     dgdds(11) = gdds
     dgdde(11) = gdde
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
 
!  heading for leaf number table
  WRITE (luophenol,1000) cname
 
!  write out a table with leaf numbers by doy
  DO j = 1,60
!
!  write only the integer values that are greater than 0
     IF ((lnpout(j,2).LT.dgdde(6)/pchron).AND.(lnpout(j,2).GT.0.0))             &
       & WRITE (luophenol,1100) lnpout(j,1),lnpout(j,2)
  END DO
!
! convert integer endlgs(1) to a real number
  WRITE (luophenol,1100) real(endlgs(1)),dgdde(6)/pchron
 
 
!debe add items to print to the output file phenol.out
  WRITE (luophenol,1200) cname,cliname,pdepth,pdatearr(1),pdatearr(3),          &
                       & pdatearr(4),gmethod,emrgflg,seedbed,bhfwsf
 
 
  WRITE (luophenol,1300) pdatearr(1),pdatearr(3),pdatearr(4),ems(1),ems(3),     &
                       & ems(4),ddap(1),dgdds(1),tis(1),tis(3),tis(4),ddap(2),  &
                       & ddae(2),dgdds(2),dgdde(2),dgdde(2)/pchron,gpds(1),     &
                       & gpds(3),gpds(4),ddap(5),ddae(5),dgdds(5),dgdde(5),     &
                       & dgdde(5)/pchron,ies(1),ies(3),ies(4),ddap(3),ddae(3),  &
                       & dgdds(3),dgdde(3),dgdde(3)/pchron,joints(1),joints(3), &
                       & joints(4),ddap(4),ddae(4),dgdds(4),dgdde(4),dgdde(4)   &
                       & /pchron,endlgs(1),endlgs(3),endlgs(4),ddap(6),ddae(6), &
                       & dgdds(6),dgdde(6),dgdde(6)/pchron,antss(1),antss(3),   &
                       & antss(4),ddap(7),ddae(7),dgdds(7),dgdde(7),dgdde(6)    &
                       & /pchron,halfbs(1),halfbs(3),halfbs(4),ddap(8),ddae(8), &
                       & dgdds(8),dgdde(8),dgdde(6)/pchron,fullbs(1),fullbs(3), &
                       & fullbs(4),ddap(9),ddae(9),dgdds(9),dgdde(9),dgdde(6)   &
                       & /pchron,antes(1),antes(3),antes(4),ddap(12),ddae(12),  &
                       & dgdds(12),dgdde(12),dgdde(6)/pchron,mats(1),mats(3),   &
                       & mats(4),ddap(10),ddae(10),dgdds(10),dgdde(10),dgdde(6) &
                       & /pchron,hrs(1),hrs(3),hrs(4),ddap(11),ddae(11),        &
                       & dgdds(11),dgdde(11),dgdde(6)/pchron
 
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
 1000 FORMAT (44x,a14)
 1100 FORMAT (40x,f5.1,6x,f4.1)
 
 1200 FORMAT ('crop name =',2x,a15,/x,'climate location =',2x,a128,/x,          &
             &'planting depth =',2x,f5.3,'(m)',/x,'planting date =',2x,i3,':',  &
            & i2,'/',i2,/x,'gdd method =',2x,i1,/x,'emergence method used =',2x,&
            & i1,/x,'seedbed soil moisture =',2x,a17,/x,'water stress factor =',&
            & 2x,f3.1,/x)
 
 1300 FORMAT (' phenological event',7x,'day of year',2x,'date',2x,'dap',5x,     &
            & 'dae',5x,'gdd ap',5x,'gdd ae',5x,'nolvs'/1x'planting date',18x,i4,&
            & 2x,i2,'/',i2,/1x,'emergence',22x,i4,2x,i2,'/',i2,1x,i4,13x,f6.1,  &
            & /1x,'first tiller',19x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,    &
            & f6.1,4x,f6.1,/1x,'growing point differentiation',2x,i4,2x,i2,'/', &
            & i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,                       &
             &'internode elongation begins',4x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,  &
            & f6.1,5x,f6.1,4x,f6.1,/1x,'jointing',23x,i4,2x,i2,'/',i2,1x,i4,4x, &
            & i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'end of leaf growth',13x,i4,2x,i2, &
             &'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'anthesis starts', &
            & 16x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,      &
             &'half bloom',21x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,  &
            & f6.1,/1x,'full bloom',21x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x, &
            & f6.1,4x,f6.1,/1x,'anthesis ends',18x,i4,2x,i2,'/',i2,1x,i4,4x,i4, &
            & 5x,f6.1,5x,f6.1,4x,f6.1,/1x,'physiological maturity',9x,i4,2x,i2, &
             &'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x,'harvest ready',   &
            & 18x,i4,2x,i2,'/',i2,1x,i4,4x,i4,5x,f6.1,5x,f6.1,4x,f6.1,/1x)
 
!  heading for gddwsf array
 1400 FORMAT (/2x,39x,'gddwsf array',/1x,'phenological event',12x,'gn gdd',2x,  &
             &'gs gdd',2x,'wsfhi',2x,'wsflo',2x,'adjgdd',/1x,'emergence',22x,   &
            & f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'first tiller',19x,f5.1, &
            & 3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'internode elongation begins',&
            & 4x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'jointing',23x,f5.1,  &
            & 3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,                              &
             &'growing point differentiation',2x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,  &
            & 4x,f5.1,/1x,'end of leaf growth',13x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,&
            & 4x,f5.1,/1x,'anthesis starts',16x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,&
            & f5.1,/1x,'anthesis ends',18x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,&
            & /1x,'half bloom',21x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,    &
             &'full bloom',21x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,        &
             &'physiological maturity',9x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1, &
            & /1x,'harvest ready',18x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x, &
             &'row 13',25x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'row 14',   &
            & 25x,f5.1,3x,f5.1,3x,f3.1,4x,f3.1,4x,f5.1,/1x,'row 15',25x,f5.1,3x,&
            & f5.1,3x,f3.1,4x,f3.1,6x,f3.1,/1x)
! 
END SUBROUTINE phenolsg

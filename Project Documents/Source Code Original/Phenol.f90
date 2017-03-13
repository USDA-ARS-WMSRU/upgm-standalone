SUBROUTINE phenol(aepa,aifs,antes,antss,pdepth,bhfwsf,blstrs,boots,browns,      &
                & cliname,cname,cots,daa,dae,dap,dav,daynum,ddae,ddap,ddav,     &
                & dgdde,dgdds,dgddv,doughs,drs,dummy2,ears,emrgflg,ems,endlgs,  &
                & endphenol,dents,epods,eseeds,first7,fps,fullbs,gdda,gdde,gdds,&
                & gddv,gddwsf,gmethod,gpds,halfbs,heads,hrs,ies,ies2,infls,     &
                & joints,lf1s,lf12s,lf2s,lf3s,lf4s,lf8s,lnpout,mats,mffls,milks,&
                & mpods,mseeds,nolvs,opens,pchron,pdate,seedsw,silks,soilwat,   &
                & srs,tis,tsints,tss,yelows,year)
!
!  the phenol subroutine calls subroutines to calculate the phenology of
!  a specific crop.
! 
!  inputs:  aepa(r), aifs(r), antes(r), antss(r), boots(r), cname(r),
!           dae(r), dap(r), dav(r), daynum(r), ddae(r), ddap(r), ddav(r),
!           dgdde(r), dgdds(r), dgddv(r), drs(r), dummy2(r), ems(r),
!           endlgs(r), first7(r), fps(r), fullbs(r), gdde(r), gdds(r),
!           gddv(r), gpds(r), halfbs(r), heads(r), hrs(r), ies(r),
!           joints(r), mats(r), nolvs(r), pchron(r), srs(r), tis(r),
!           tss(r)
 
!  outputs: aepa(r), aifs(r), antes(r), antss(r), boots(r), cname(r),
!           dae(r), dap(r), dav(r), daynum(r), ddae(r), ddap(r),
!           ddav(r), dgdde(r), dgdds(r), dgddv(r), drs(r), dummy2(r),
!           ems(r), endlgs(r), first7(r), fps(r), fullbs(r), gdde(r),
!           gdds(r), gddv(r), gpds(r), halfbs(r), heads(r), hrs(r),
!           ies(r), joints(r), mats(r), nolvs(r), pchron(r), srs(r),
!           tis(r), tss(r)
!                
!debe added bc0growdepth, emrgflg, gemthod, seedsw, soilwat to pass to
! phenol_cropname subroutines to print out to the output file.  yy was
! passed from crop.
!debe added variables for and call to phenoldb for dry beans
!debe changed bc0growdepth to a phenologymms variable name (pdepth) to
!maintian consistency with phenologymms code. also, changed yy to year for
!the same reason. bhfwsf is an upgm variable name but it is used to pass to
!water_stress subroutine which is not a phenologymms subroutine but one that we
!added to upgm. will leave bhfwsf as is.
! 
IMPLICIT NONE
!
! Subroutine arguments
!
REAL :: aepa,bhfwsf,gdda,gdde,gdds,gddv,nolvs,pchron,pdepth
CHARACTER(128) :: cliname
CHARACTER(80) :: cname
INTEGER :: daa,dae,dap,dav,daynum,emrgflg,first7,gmethod,pdate,seedsw,year
LOGICAL :: endphenol
INTEGER,DIMENSION(4) :: aifs,antes,antss,blstrs,boots,browns,cots,dents,doughs, &
                      & drs,ears,ems,endlgs,epods,eseeds,fps,fullbs,gpds,halfbs,&
                      & heads,hrs,ies,ies2,infls,joints,lf12s,lf1s,lf2s,lf3s,   &
                      & lf4s,lf8s,mats,mffls,milks,mpods,mseeds,opens,silks,srs,&
                      & tis,tsints,tss,yelows
INTEGER,DIMENSION(20) :: ddae,ddap,ddav
REAL,DIMENSION(20) :: dgdde,dgdds,dgddv
REAL,DIMENSION(30) :: dummy2
REAL,DIMENSION(15,5) :: gddwsf
REAL,DIMENSION(100,2) :: lnpout
CHARACTER(80),DIMENSION(4) :: soilwat
!
! Local variables
!
CHARACTER(80) :: seedbed
!
!debe added bhfwsf to implement daily water stress effect on time
! of reaching a growth stage. added dummy1 to determine stress level of
! the crop to be used in the algorithm determining the gdd based
! on the daily water stress factor.
 
!debe changed seedsw from integer to real to allow moving half a soil moisture level with precip.
! later changed it back to an integer because array subscripts must be integers or constants.
 
 
 
!debe changed the dimensions of dummy2 array to include both stressed and
! non-stressed values.
!debe added gddwsf which has been initialized in cinint and is ready to be
! passed to the appropriate phenol_cropname subroutine.
!debe added cliname for writing out the climate file name.
 
 
!     + + + argument definitions + + +
!     aepa - the parameter for duration of anthesis (i.e., gdd from start
!            to end of anthesis.
!     aifs - awns initials formed growth stage for spring barley and winter
!            barley. this array includes daynum, year, month and day of when
!            this stage was reached.
!     antes - end of anthesis growth stage for hay millet, proso millet,
!             spring barley, spring wheat, sunflower, winter barley and winter
!             wheat. this array includes daynum, year, month and day of when
!             this stage was reached.
!     antss - start of anthesis growth stage for corn, dry beans, hay millet,
!             proso millet, sorghum (first bloom), spring barley, spring
!             wheat, sunflower, winter barley and winter wheat. in dry beans,
!             the start of anthesis growth stage and there is one open
!             flower per plant =100% bloom. this array includes daynum,
!             year, month and day of when this stage was reached.
!     bhfwsf - water stress factor ratio (0-1).  this is read in daily.
!     blstrs - blister growth stage in corn. this array includes daynum,
!              year, month and day of when this stage was reached.
!     boots - booting growth stage for hay millet, proso millet, spring
!             barley, spring wheat, winter barley and winter wheat. this array
!             includes daynum, year, month and day of when this stage was
!             reached.  booting is defined as flag leaf has completed its
!             growth.
!     browns - when the back of the sunflower head is yellow and there may be
!              some brown spotting. this array includes daynum, year, month
!              and day of when this stage was reached.
!     cliname - the name of the location for the climate data.
!     cname - crop name.
!     cots - cotyledonary and unifoliolate leaves are visible in dry
!            beans. this array includes daynum, year, month and day
!            of when this stage was reached.
!     daa - days after anthesis.
!     dae - days after emergence.
!     dap - days after planting.
!     dav - days after vernalization.
!     daynum - the current day numbered from jan 1.
!     ddae - an array holding the dae for each growth stage.
!     ddap - an array holding the dap for each growth stage.
!     ddav - an array holding the dav for each growth stage.
!     dents - the dent growth stage in corn. this array includes daynum,
!             year, month and day of when this stage was reached.
!     dgdde - an array holding the gdde for each growth stage.
!     dgdds - an array holding the gdds for each growth stage.
!     dgddv -  an array holding the gddv for each growth stage.
!     doughs - the dough growth stage in corn. this array includes daynum,
!              year, month and day of when this stage was reached.
!     drs - double ridge growth stage for hay millet, proso millet, spring
!           barley, spring wheat, winter barley and winter wheat. this array
!           includes daynum, year, month and day of when this stage was
!           reached.
!     dummy2 - an array to hold the gdd values, both under stressed
!              and non- stressed conditions,required to reach each growth
!              stage of the current crop.
!     ears - the ear initiation stage in corn. this array includes daynum,
!            year, month and day of when this stage was reached.
!     emrgflg - a flag to determine if the new emerge subroutine should be
!               called (emrgflg=1) or to proceed with the weps/upgm method
!               of achieving emergence (emrgflg=0).
!     ems - day when emergence occurred in all crops. this array includes
!           daynum, year, month and day of when this event occurred.
!     endlgs - end of leaf growth stage in sorghum. this array includes
!              daynum, year, month and day of when this stage was reached.
!     endphenol - a flag to indicate if this subroutine should be called
!                 again the next day.
!     epods - one pod has reached the maximum length in dry beans.
!             this array includes daynum, year, month and day of when
!             this stage was reached.
!     eseeds - there is one pod with fully developed seeds in dry
!              beans. this array includes daynum, year, month and day
!              of when this stage was reached.
!     first7 - used to set the value of aepa the first time the crop's phenol 
!              subroutine is called.
!     fps - flower primordium initiation growth stage. this array includes
!           daynum, year, month and day of when this stage was reached.
!     fullbs - full bloom growth stage in sorghum. this array includes
!            daynum, year, month and day of when this stage was reached.
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
!     gpds - growing point differentiation growth stage in sorghum. this
!            array includes daynum, year, month and day of when this stage
!            was reached.
!     halfbs - half bloom growth stage in sorghum. this array includes
!            daynum, year, month and day of when this stage was reached.
!     heads - heading growth stage for hay millet, proso millet, spring
!             barley, spring wheat, winter barley and winter wheat. this
!             array includes daynum, year, month and day of when this stage
!             was reached.
!     hrs - time to harvest ripe growth stage for corn, dry beans, hay
!           millet, proso millet, sorghum, spring barley, spring wheat,
!           sunflower, winter barley and winter wheat. in dry beans, 80%
!           of pods are at the mature color in dry beans. this array
!           includes daynum, year, month and day of when this stage was
!           reached.
!     ies - start of internode elongation growth stage for corn, hay millet,
!           proso millet, sorghum, spring barley, spring wheat, winter barley,
!           and winter wheat. for sunflower, this stage occurs when the
!           internode below the inflorescence elongates 0.5 to 2.0 cm above
!           the nearest leaf on the stem. this array includes daynum, year,
!           month and day of when this stage was reached.
!     ies2 - for sunflower, this is when the internode below the inflorescence
!            continues lengthening and lifts the head above the surrounding
!            leaves more than 2 cm. this array includes daynum, year,
!            month and day of when this stage was reached.
!     infls - the sunflower inflorescence becomes visible. this array includes
!             daynum, year, month and day of when this stage was reached.
!     joints - jointing growth stage for hay millet, proso millet, sorghum,
!              spring barley, spring wheat, winter barley and winter wheat.
!              this array includes daynum, year, month and day of when this
!              stage was reached.
!     lf1s - stage when the first trifoliolate leaf is unfolded in dry
!            beans. this array includes daynum, year, month and day of
!            when this stage was reached.
!     lf12s - the 12 leaf growth stage for corn and sunflower. this array
!             includes daynum, year, month and day of when this stage was
!             reached.
!     lf2s - stage when the second trifoliolate leaf is unfolded in dry
!            beans. this array includes daynum, year, month and day of
!            when this stage was reached.
!     lf3s - stage when the third trifoliolate leaf is unfolded in dry
!            beans. this array includes daynum, year, month and day of
!            when this stage was reached.
!     lf4s - the 4 leaf growth stage for corn and sunflower and the
!            stage when the fourth trifoliolate leaf is unfolded in dry
!            beans. this array includes daynum, year, month and day of
!            when this stage was reached.
!     lf8s - the 8 leaf growth stage for sunflower. this array includes
!            daynum, year, month and day of when this stage was reached.
!     lnpout - an array used in writing out daynum and the number of leaves
!              on that day. the values are written each time a new leaf has
!              appeared.
!     mats - physiological maturity growth stage for corn, dry beans,
!            hay millet, proso millet, sorghum, spring barley, spring
!            wheat, sunflower, winter barley and winter wheat. in dry beans,
!            one pod has changed color/striped. this array includes
!            daynum, year, month and day of when this stage was reached.
!     mffls - the stage of mid to full flower in dry beans. this array
!             includes daynum, year, month and day of when this stage
!             was reached.
!     milks - the milk growth stage in corn. this array includes daynum, year,
!             month and day of when this stage was reached.
!     mpods - the stage when 50% of the pods are at the maximum length.
!             this array includes daynum, year, month and day of when
!             this stage was reached.
!     mseeds - the stage when 50% of the pods have fully developed seeds
!              in dry beans. this array includes daynum, year, month and
!              day of when this stage was reached.
!     nolvs - number of leaves.
!     opens - the sunflower inflorescence begins to open. this array includes
!             daynum, year, month and day of when this stage was reached.
!     pchron - phyllochron value which is the number of gdd per leaf.
!     pdate - planting date.
!     pdepth - depth of growing point at time of planting (m).
!              bc0growthdepth is passed into this variable.
!     seedbed - contains the soil moisture condition of the seedbed.
!     seedsw - soil water content at seed depth.  it is read in as
!              optimum, medium, dry or planted in dust and converted
!              to an integer.	 1 = optimum, 2 = medium, 3 = dry and
!              4 = planted in dust.
!     silks - the silking growth stage in corn. this array includes daynum,
!             year, month and day of when this stage was reached.
!     soilwat - an array holding the swtype for each soil moisture
!               condition.
!     srs - single ridge growth stage for hay millet, proso millet, spring
!           barley, spring wheat, winter barley and winter wheat. this array
!           includes daynum, year, month and day of when this stage was
!           reached.
!     tis - start of tillering growth stage for corn, hay millet, proso
!           millet, sorghum, spring barley, spring wheat, winter barley and
!           winter wheat. this array includes daynum, year, month and day of
!           when this stage was reached.
!     tsints - tassel initiation growth stage in corn. this array includes
!              daynum, year, month and day of when this stage was reached.
!     tss - terminal spikelet growth stage for spring and winter wheat. this
!           array includes daynum, year, month and day of when this stage was
!           reached.
!     year - year. yy is passed into this variable name.
!     yelows - back of the sunflower head is a light yellow. this array
!              includes daynum, year, month and day of when this stage was
!              reached.
!
 
!debe get seedbed moisture condition to be used in printing it in output.
seedbed = soilwat(seedsw)
!debe set year equal to yy
  !    year = yy
 
 
!debe added bhfwsf  to the call to each crop's phenol subroutine
! to implement daily water stress effect on time of reaching a growth stage.
!debe added gddwsf to the passing arguments for each crop below.
 
! call the correct phenology subroutine for the selected crop:
!
! note: if corn: make sure the cropxml.dat file has only 'corn' for the
! crop name. if the variety name is e.g. 'corn, grain, 110' then it must
! be adjusted to read only 'corn'.
IF (cname.EQ.'corn'.AND.endphenol.NE..TRUE.) THEN
!print*, 'in call to phenolcn in phenol. dummy2(5) = ', dummy2(5)
  CALL phenolcn(aepa,antss,pdepth,bhfwsf,blstrs,cliname,cname,daa,dae,dap,      &
              & daynum,ddae,ddap,dents,dgdde,dgdds,doughs,dummy2,ears,emrgflg,  &
              & ems,first7,gdda,gdde,gdds,gddwsf,gmethod,hrs,ies,lf12s,lf4s,    &
              & lnpout,mats,milks,pchron,pdate,seedbed,silks,tsints,year,       &
              & endphenol)
 
! if dry beans (crop name in cropxml.dat needs to read drybeans)
ELSE IF (cname.EQ.'dry beans'.AND.endphenol.NE..TRUE.) THEN
  CALL phenolbn(aepa,antss,pdepth,bhfwsf,cots,cliname,cname,daa,dae,dap,daynum, &
              & ddae,ddap,dgdde,dgdds,dummy2,emrgflg,ems,endphenol,epods,eseeds,&
              & first7,gdda,gdde,gdds,gddwsf,gmethod,hrs,lf1s,lf2s,lf3s,lf4s,   &
              & lnpout,mats,mffls,mpods,mseeds,pchron,pdate,seedbed,year)
 
! if hay millet:
ELSE IF (cname.EQ.'hay millet'.AND.endphenol.NE..TRUE.) THEN
  CALL phenolhm(aepa,antes,antss,pdepth,bhfwsf,boots,cliname,cname,daa,dae,dap, &
              & daynum,ddae,ddap,dgdde,dgdds,drs,dummy2,emrgflg,ems,endphenol,  &
              & first7,fps,gdda,gdde,gdds,gddwsf,gmethod,heads,hrs,ies,joints,  &
              & lnpout,mats,pchron,pdate,seedbed,srs,tis,tss,year)
 
! if proso millet:
ELSE IF (cname.EQ.'proso millet'.AND.endphenol.NE..TRUE.) THEN
  CALL phenolpm(aepa,antes,antss,pdepth,bhfwsf,boots,cliname,cname,daa,dae,dap, &
              & daynum,ddae,ddap,dgdde,dgdds,drs,dummy2,emrgflg,ems,endphenol,  &
              & first7,fps,gdda,gdde,gdds,gddwsf,gmethod,heads,hrs,ies,joints,  &
              & lnpout,mats,pchron,pdate,seedbed,srs,tis,tss,year)
 
! if sorghum:
ELSE IF (cname.EQ.'sorghum'.AND.endphenol.NE..TRUE.) THEN
  CALL phenolsg(aepa,antes,antss,pdepth,bhfwsf,cliname,cname,daa,dae,dap,daynum,&
              & ddae,ddap,dgdde,dgdds,dummy2,emrgflg,ems,endlgs,endphenol,      &
              & first7,fullbs,gdda,gdde,gdds,gddwsf,gpds,gmethod,halfbs,hrs,ies,&
              & joints,lnpout,mats,pchron,pdate,seedbed,tis,year)
 
! if spring barley:
ELSE IF (cname.EQ.'spring barley'.AND.endphenol.NE..TRUE.) THEN
  CALL phenolsb(aepa,aifs,antes,antss,pdepth,bhfwsf,boots,cliname,cname,daa,dae,&
              & dap,daynum,ddae,ddap,dgdde,dgdds,drs,dummy2,emrgflg,ems,        &
              & endphenol,first7,fps,gdda,gdde,gdds,gddwsf,gmethod,heads,hrs,   &
              & ies,joints,lnpout,mats,pchron,pdate,seedbed,srs,tis,year)
 
! if spring wheat:
ELSE IF (cname.EQ.'spring wheat'.AND.endphenol.NE..TRUE.) THEN
  CALL phenolsw(aepa,antes,antss,pdepth,bhfwsf,boots,cliname,cname,daa,dae,dap, &
              & daynum,ddae,ddap,dgdde,dgdds,drs,dummy2,emrgflg,ems,endphenol,  &
              & first7,fps,gdda,gdde,gdds,gddwsf,gmethod,heads,hrs,ies,joints,  &
              & lnpout,mats,pchron,pdate,seedbed,srs,tis,tss,year)
 !
! if sunflower:
ELSE IF (cname.EQ.'sunflower'.AND.endphenol.NE..TRUE.) THEN
  CALL phenolsf(aepa,antes,antss,pdepth,bhfwsf,browns,cliname,cname,daa,dae,dap,&
              & daynum,ddae,ddap,dgdde,dgdds,dummy2,emrgflg,ems,endphenol,      &
              & first7,gdda,gdde,gdds,gddwsf,gmethod,hrs,ies,ies2,infls,lf12s,  &
              & lf4s,lf8s,lnpout,mats,opens,pchron,pdate,seedbed,year,yelows)
 
! if winter barley:
ELSE IF (cname.EQ.'winter barley'.AND.endphenol.NE..TRUE.) THEN
  CALL phenolwb(aepa,aifs,antes,antss,pdepth,bhfwsf,boots,cliname,cname,daa,dae,&
              & dap,dav,daynum,ddae,ddap,ddav,dgdde,dgdds,dgddv,drs,dummy2,ems, &
              & emrgflg,endphenol,first7,fps,gdda,gdde,gdds,gddv,gddwsf,gmethod,&
              & heads,hrs,ies,joints,lnpout,mats,pchron,pdate,seedbed,srs,tis,  &
              & year)
 
! if winter wheat:
ELSE IF (cname.EQ.'winter wheat'.AND.endphenol.NE..TRUE.) THEN
  CALL phenolww(aepa,antes,antss,pdepth,bhfwsf,boots,cliname,cname,daa,dae,dap, &
              & dav,daynum,ddae,ddap,ddav,dgdde,dgdds,dgddv,dummy2,drs,emrgflg, &
              & ems,first7,fps,gdda,gdde,gdds,gddv,gddwsf,gmethod,heads,hrs,ies,&
              & joints,lnpout,mats,pchron,pdate,seedbed,srs,tis,tss,year,       &
              & endphenol)
 
!
END IF
! 
END SUBROUTINE phenol

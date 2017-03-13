SUBROUTINE phenolmms(aepa,aifs,antes,antss,bc0growdepth,bctmin,bhfwsf,blstrs,   &
                   & boots,browns,bwtdmn,bwtdmx,canht,cliname,cname,cots,       &
                   & cropname,daa,dae,dap,dav,dayhtinc,daynum,dd,ddae,ddap,ddav,&
                   & dents,dgdde,dgdds,dgddv,doughs,drs,dummy2,ears,ecanht,     &
                   & emrgflg,ems,endlgs,endphenol,epods,eseeds,first7,fps,      &
                   & fullbs,gdda,gddday,gdde,gdds,gddv,gddwsf,gmethod,gpds,     &
                   & halfbs,heads,hrs,ies,ies2,infls,jan1,joints,lf1s,lf12s,    &
                   & lf2s,lf3s,lf4s,lf8s,lnarray,lncntr,lnpout,mats,maxht,mffls,&
                   & milks,mm,mpods,mseeds,nolvs,opens,pchron,pdate,rowcntr,    &
                   & seedsw,silks,soilwat,srs,tbase,tis,todayln,toptlo,toptup,  &
                   & tsints,tss,tupper,yelows,yestln,yy)
!                   
!debe added this subroutine as a means of keeping the subroutines from
!phenologymms all together. this subroutine will be called from crop.
!phenolmms will then call the appropriate subroutines brought in from
!phenologymms.
! 
IMPLICIT NONE
!
! Subroutine arguments
!
REAL :: aepa,bc0growdepth,bctmin,bhfwsf,bwtdmn,bwtdmx,canht,dayhtinc,ecanht,    &
      & gdda,gddday,gdde,gdds,gddv,maxht,nolvs,pchron,tbase,todayln,toptlo,     &
      & toptup,tupper,yestln
CHARACTER(128) :: cliname
CHARACTER(80) :: cname,cropname
INTEGER :: daa,dae,dap,dav,daynum,dd,emrgflg,first7,gmethod,lncntr,mm,pdate,    &
         & rowcntr,seedsw,yy
LOGICAL :: endphenol,jan1
INTEGER,DIMENSION(4) :: aifs,antes,antss,blstrs,boots,browns,cots,dents,doughs, &
                      & drs,ears,ems,endlgs,epods,eseeds,fps,fullbs,gpds,halfbs,&
                      & heads,hrs,ies,ies2,infls,joints,lf12s,lf1s,lf2s,lf3s,   &
                      & lf4s,lf8s,mats,mffls,milks,mpods,mseeds,opens,silks,srs,&
                      & tis,tsints,tss,yelows
INTEGER,DIMENSION(20) :: ddae,ddap,ddav
REAL,DIMENSION(20) :: dgdde,dgdds,dgddv
REAL,DIMENSION(30) :: dummy2
REAL,DIMENSION(15,5) :: gddwsf
REAL,DIMENSION(400,2) :: lnarray
REAL,DIMENSION(100,2) :: lnpout
CHARACTER(80),DIMENSION(4) :: soilwat
!
!debe added dayhtinc to be able to pass the daily increase in height to growth
! for the ht_dia_sai subroutine in place of the weps/upgm variable dht when
! canopyflg = 1.
!debe added ecanht so that it can be read in instead of set in the code for each crop.
! 

!     + + + argument definitions + + +
!     bc0growdepth - depth of growing point at time of planting (m).
!     bctmin - base temperature (deg. c).
!     bhfwsf - water stress factor ratio (0-1).  this is read in daily.
!     bwtdmn - daily minimum air temperature (deg.c).
!     bwtdmx - daily maximum air temperature (deg.c).
!     cname - crop name.
!     dd - day.
!     mm - month.
!     yy - year.

!     + + + local variable definitions + + +

!     + + + newly added argument definitions + + +
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
!     canht - the cumulative height of the plant canopy.
!     cliname - the name of the location for the climate data.
!     cots - cotyledonary and unifoliolate leaves are visible in dry
!            beans. this array includes daynum, year, month and day
!            of when this stage was reached.
!     cropname - crop name.
!     daa - days after anthesis.
!     dae - days after emergence.
!     dap - days after planting.
!     dav - days after vernalization.
!     dayhtinc - the increase in plant height for today.
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
!     ecanht - this is the maximum canopy height of the crop in phase 1 of 
!              the canopy height growth.  This is usually from emergence to 
!              when the plant begins elongating stems but this stage varies 
!              among crops. it is an input parameter and is read in from upgm_crop.dat.
!     emrgflg - a flag to determine if the new emerge subroutine should be
!               called (emrgflg=1) or to proceed with the weps/upgm method
!               of achieving emergence (emrgflg=0).
!     ems - day when emergence occurred in all crops. this array includes
!           daynum, year, month and day of when this event occurred.
!     endlgs - end of leaf growth stage in sorghum. this array includes
!              daynum, year, month and day of when this stage was reached.
!     endphenol - a flag to indicate if this subroutine should be called
!                 again on the next day.
!     epods - one pod has reached the maximum length in dry beans (early
!             pod set). this array includes daynum,year, month and day of
!             when this stage was reached.
!     eseeds - there is one pod with fully developed seeds in dry
!              beans (early seed fill). this array includes daynum, year,
!              month and day of when this stage was reached.
!     first7 - used to set the value of aepa the first time the crop's phenol 
!              subroutine is called.
!     fps - flower primordium initiation growth stage. this array includes
!           daynum, year, month and day of when this stage was reached.
!     fullbs - full bloom growth stage in sorghum. this array includes
!            daynum, year, month and day of when this stage was reached.
!     gdda - growing degree days from anthesis.
!     gddday - the number of gdd with 0°c base temperature for that day.
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
!               phenologymms and is used for crops such as corn, dry beans,
!               sorghum and sunflower.  a value of 3 is the way that weps/upgm
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
!     jan1 - a flag to test if january 1 has occurred.  if it has passed,
!            then the winter annual crop is assumed to have completed
!            vernalization.
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
!     lnarray - an array to hold the leaf number calculated for each day
!     lncntr - counter for the leafno subroutine
!     lnpout - an array used in writing out daynum and the number of leaves
!              on that day. the values are written each time a new leaf has
!              appeared.
!     mats - physiological maturity growth stage for corn, dry beans,
!            hay millet, proso millet, sorghum, spring barley, spring
!            wheat, sunflower, winter barley and winter wheat. in dry beans,
!            one pod has changed color/striped. this array includes
!            daynum, year, month and day of when this stage was reached.
!     maxht - the maximum height of the plant canopy.
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
!     rowcntr - a counter for the rows in an array
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
!     tbase - lowest temperature below which no growth occurs (deg.c).
!     tis - start of tillering growth stage for corn, hay millet, proso
!           millet, sorghum, spring barley, spring wheat, winter barley and
!           winter wheat. this array includes daynum, year, month and day of
!           when this stage was reached.
!     todayln - the value of the current day's leaf number
!     toptlo - the lower temperature in the optimum range for plant
!              growth (deg.c).
!     toptup - the upper temperature in the optimum range for plant
!              growth (deg.c).
!     tsints - tassel initiation growth stage in corn. this array includes
!              daynum, year, month and day of when this stage was reached.
!     tss - terminal spikelet growth stage for spring and winter wheat. this
!           array includes daynum, year, month and day of when this stage was
!           reached.
!     tupper - upper/maximum temperature for plant growth (deg.c).
!              no growth with temperatures above this point.
!     yelows - back of the sunflower head is a light yellow. this array
!              includes daynum, year, month and day of when this stage was
!              reached.
!     yestln - the value of yesterday's leaf number
 
! ***** calculating gdd *****
!debe added call to new subroutine gddcalc.
!debe added variables: tbase, toptlo, toptup. take out sending bctopt to
!gddcalc for topt. topt is now being calculated in gddcalc as the average
! of toptlo and toptup.
CALL gddcalc(bctmin,bwtdmn,bwtdmx,gddday,gmethod,tbase,toptlo,toptup,tupper)
 
!IF (daynum.GT.pdate) THEN
!    print*, 'daynum =', daynum, 'gddday =', gddday
!ENDIF


!debe moved the next two lines of code out of the 'if (huiy.lt.1.0) then'
! statement so that hrs growth stage will show the amounts for dap and
! gdds. this is because hrs growth stage occurs after huiy.lt.1.0 and
! these 2 values will not be incremented for the hrs stage.  the values
! are available for shoot_grow and emerge because they are saved in the
! save statement above.
 
IF ((daynum.GT.pdate).OR.(yy.GE.2)) THEN
! debe added 'or' so that dap will increment in crops that go into year 2,
! i.e. winter crops.
!debe try ge 2 so that years greater than 2 can be used, i.e. from
!historical weather so that upgm will run.
  gdds = gdds + gddday           !debe moved this inside the if statement
  dap = dap + 1                  !gdds accumulates only after planting day
!  print*, 'in PhenolMMS dap = ', dap
END IF
!debe moved the following section for the accumulation of gdde, gddv
! and gdda and the calls to leafno and phenol to occur only if emergence
! has occurred.
!
! ***** emergence *****
!  accumulate gdd from emergence (gdde) and days after emergence (dae)
!  once emergence has occurred:
IF (ems(1).NE.999) THEN     ! emergence has occurred
!!debe incrementing dae the day after emergence occurs.
!  IF ((daynum.GT.ems(1)).OR.(yy.EQ.2)) THEN
!  IF (ems(1).GT.dap) THEN The crop doesn't finish because dap becomes 
!                          .GT. ems(1) before hrs is reached if this IF statement is used.
     gdde = gdde + gddday    !debe moved here so that gdde is increased after
     dae = dae + 1           !day of emergence.
!  END IF
!
!if emergence has occurred, call phenol and leaf number subroutines.
!debe added the variables to pass to phenol for leaf number and crop
!phenol subroutines.
 
!debe moved call to leafno subroutine before the call to phenol
!subroutine.
 
! ***** leafno *****
!  call the leafno subroutine to calculate the number of leaves once
!  emergence has occurred for the leaf number output table.
  IF (antss(1).EQ.999) THEN 
      !print*, ' before call to leafno antss(1) = ', antss(1)
     CALL leafno(daynum,gdde,lnarray,lncntr,lnpout,pchron,rowcntr,todayln,yestln)
  ENDIF
!
! ***** vernalization *****
!debe corrected when days after vernalization should start accumulating
!calculate growing degree-days (gddv) and number of days after
! vernalization requirement is satisfied (dav) fow winter crops. if
! vernalization has been accomplished add to the gdd after vernalization
! array (gddv) and to the days after vernalization (dav) value. crop id of
! 2 is for cool-season legumes such as peas and 5 is for cool-season
! annuals such as winter wheat, winter canola, etc.
! if jan. 1 has occurred, it is assumed that vernalization has occurred.
! currently (020909) there is no separate vernalization subroutine.
!note: for spring crops, somewhere want to set verns = 1.0, meaning no
! vernalization requirement.
!
 
!******** trying to use upgm/weps method of caclulating vernalization ****
!!!! check if jan. 1 has occurred.
!!!debe commented this if block out to use upgm/weps method of vernalization
!!!           if ((mm .eq. 1) .and. (dd .eq. 1) .and. (yy .eq. 2)) then
!!!            jan1 = .true.
!!!           endif
!!! if vernalization requirement has been satisfied, then start accumulating
!!! gdd from vernalization (gddv). assume this to be jan 1 for northern
!!! hemisphere.
!!!           if (jan1 .eq. .true.) then
!
!!! debe 013009 following line was the original way but gddv did not
!!!  accumulate soon enough.
!!!!           if ((hu_delay .eq. 1.) .and. (jan1 .eq. .true.)) then
!
!
!!! debe copy the following lines to accumulate gddv up to the section where
!!! upgm/weps: "check winter annuals for completion of vernalization,
!!          ! warming and spring day length." about line 712.
!!        didn't get past tis growth stage up there.
!
!
!! !debe with only the next two lines not commented out, hrs occurs 4/29 for
!! ! cimarron weather, wsf 0.9, pdepth 2.5, planting date 9/1
!!!         gddv = gddv + gddday
!!!	        dav = dav + 1
!!!		   endif
 
! *** phenologymms version of vernalization ***
!debe moved next line of Code to Crop so that dav will be incremented truly 
! on the day(S) after vernalization
!  IF ((mm.EQ.1).AND.(dd.EQ.1).AND.(yy.EQ.2)) jan1 = .TRUE.
 
! if vernalization requirement has been satisfied, then start accumulating
! gdd from vernalization (gddv). assume this to be jan 1 for northern
! hemisphere.
  IF (jan1.EQ..TRUE.) THEN
! debe 013009 following line was the original way but gddv did not
! accumulate soon enough. ! if (hu_delay .eq. 1.) then ! .and. (jan1 .eq. .true.)) then
     gddv = gddv + gddday
     dav = dav + 1
  END IF
!
! ***** anthesis *****
! calculate growing degree-days from anthesis (gdda) and number of days
! after anthsis has occurred. just need to know that anthesis has started.
  IF (antss(1).NE.999) THEN
     gdda = gdda + gddday
     daa = daa + 1
  END IF
 
!debe moved call to canopyht above the call to phenol so that dayhtinc will 
! have the height increase value on the day of anthesis. Then on the next day 
! anthesis will have occurred and antss(1) will not equal 999 any more and 
! canopyht will not be called. All other growth stages used in canopyht will 
! have been passed long before anthesis occurs and will have their own daynum 
! and date values.

!debe add phenologymms method of determining canopy height. 
!debe took out passing gddwsf; it is not used in canopyht.
  IF (antss(1).EQ.999) THEN
    IF (cname.EQ.'corn') THEN
        CALL canopyht(antss,canht,cname,dayhtinc,dummy2,ecanht,ems,gddday,      & 
           & gdde,ies,maxht)
    ELSE IF (cname.EQ.'dry beans') THEN
        CALL canopyht(antss,canht,cname,dayhtinc,dummy2,ecanht,ems,gddday,      &
           & gdde,cots,maxht)
    ELSE IF (cname.EQ.'sorghum') THEN
        CALL canopyht(antss,canht,cname,dayhtinc,dummy2,ecanht,ems,gddday,      &
           & gdde,ies,maxht)
    ELSE IF (cname.EQ.'spring barley') THEN
        CALL canopyht(antss,canht,cname,dayhtinc,dummy2,ecanht,ems,gddday,      &
           & gdde,aifs,maxht)
    ELSE IF (cname.EQ.'sunflower') THEN
        CALL canopyht(antss,canht,cname,dayhtinc,dummy2,ecanht,ems,gddday,      &
           & gdde,lf4s,maxht)
    ELSE IF (cname.EQ.'winter barley') THEN
        CALL canopyht(antss,canht,cname,dayhtinc,dummy2,ecanht,ems,gddday,      &
           & gdde,aifs,maxht)
    ELSE    !rest of the crops: hay millet, proso millet, spring wheat, winter wheat
        CALL canopyht(antss,canht,cname,dayhtinc,dummy2,ecanht,ems,gddday,      &
           & gdde,tss,maxht)
    ENDIF !end if for cname
ENDIF !end if for antss

!debe moved call to phenol subroutine after the call to leafno and the gdde,
! gdda and gddv variables have been updated for the day. then pass the
! updated values to phenol.
 
! ***** phenology *****
!phenol calls the appropriate phenol routine for the current crop being
! simulated.
!debe passed dap calculated above to dap in phenol instead of sending it
! bcdayap (the weps variable). also passed gdds to phenol instead
! of bcthucum.
!debe moved the call to phenol before testing if tillering or anthesis
! has occurred so that if either has the accumulation of gdd and days
! after these stages can begin on the same day that it actually occurred.
 
!debe added bhfwsf (weps/upgm variable)to the call to phenol to implement
! daily water stress effect on time of reaching a growth stage.
!debe added gddwsf array to pass to phenol.  the gddwsf array holds the
! number of gdd required to reach each growth stage based on the
! water stress factor.
 
!debe added variables to pass to the phenol_cropname subroutines for
!printing out information: bc0growdepth, gmethod, emrgflg, cliname
!debe added canht to pass to phenol to be written in phenol.out from
!phenol_cropname. debe added variables for dry beans.
 
  CALL phenol(aepa,aifs,antes,antss,bc0growdepth,bhfwsf,blstrs,boots,browns,    &
            & cliname,cname,cots,daa,dae,dap,dav,daynum,ddae,ddap,ddav,dgdde,   &
            & dgdds,dgddv,doughs,drs,dummy2,ears,emrgflg,ems,endlgs,endphenol,  &
            & dents,epods,eseeds,first7,fps,fullbs,gdda,gdde,gdds,gddv,gddwsf,  &
            & gmethod,gpds,halfbs,heads,hrs,ies,ies2,infls,joints,lf1s,lf12s,   &
            & lf2s,lf3s,lf4s,lf8s,lnpout,mats,mffls,milks,mpods,mseeds,nolvs,   &
            & opens,pchron,pdate,seedsw,silks,soilwat,srs,tis,tsints,tss,yelows,&
            & yy)
! 
END IF
! 
END SUBROUTINE phenolmms

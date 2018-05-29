subroutine phenolmms(ctrl, clidat, bio,daa,dae,dap,dav,daynum,dd,ddae,ddap,ddav,dgdde,dgdds,dgddv,endphenol,    &
                  & gdda,gddday,gdde,gdds,gddv,gddwsf,jan1,lnarray,lncntr,lnpout,mm,partcoefleaf,partcoefstem,  &                               
                  & partcoefrepro,pdate,rowcntr,todayln,useupgmpart,yestln,yy,ln)

!debe added this subroutine as a means of keeping the subroutines from
!phenologymms all together. this subroutine will be called from crop.
!phenolmms will then call the appropriate subroutines brought in from
!phenologymms.
!
    use upgm_simdata, only : controls
    use climate, only : climate_data
    use biomaterial
implicit none
!
! Dummy arguments
!
    type(controls),     intent(inout) :: ctrl
    type(biomatter),    intent(inout) :: bio
    type(climate_data), intent(inout) :: clidat
    
real :: gdda,gddday,gdde,gdds,gddv, todayln,yestln,ln
integer :: daa,dae,dap,dav,daynum,dd,lncntr,mm,pdate,    &
         & rowcntr,yy
logical :: endphenol,jan1,useupgmpart
integer,dimension(20) :: ddae,ddap,ddav
real,dimension(20) :: dgdde,dgdds,dgddv
real,dimension(16,5) :: gddwsf
real,dimension(400,2) :: lnarray
real,dimension(100,2) :: lnpout
real :: partcoefleaf, partcoefstem, partcoefrepro
!
!debe added bio%upgm%dayhtinc to be able to pass the daily increase in height to growth
! for the ht_dia_sai subroutine in place of the weps/upgm variable dht when
! canopyflg = 1.
!debe added bio%upgm%ecanht so that it can be read in instead of set in the code for each crop.
!DE added partitioning variables and flag to use the upgm method of partitioning: partcoefleaf, 
! partcoefstem, partcoefrepro, useupgmpart. These are passed to phenol.f90 5/15/18
 
!     + + + argument definitions + + +
!     bio%database%growdepth - depth of growing point at time of planting (m).
!     bio%database%tmin - base temperature (deg. c).
!     ctrl%cropstress%ahfwsf - water stress factor ratio (0-1).  this is read in daily.
!     clidat%awtdmn - daily minimum air temperature (deg.c).
!     clidat%awtdmx - daily maximum air temperature (deg.c).
!     bio%bname - crop name.
!     dd - day.
!     mm - month.
!     yy - year.
 
!     + + + local variable definitions + + +
 
!     + + + newly added argument definitions + + +
!     bio%upgm%aepa - the parameter for duration of anthesis (i.e., gdd from start
!            to end of anthesis.
!     bio%upgm%aifs - awns initials formed growth stage for spring barley and winter
!            barley. this array includes daynum, year, month and day of when
!            this stage was reached.
!     bio%upgm%antes - end of anthesis growth stage for hay millet, proso millet,
!             spring barley, spring wheat, sunflower, winter barley and winter
!             wheat. this array includes daynum, year, month and day of when
!             this stage was reached.
!     bio%upgm%antss - start of anthesis growth stage for corn, dry beans, hay millet,
!             proso millet, sorghum (first bloom), soybean (beginning bloom), spring barley, 
!             spring wheat, sunflower, winter barley and winter wheat. in dry beans,
!             the start of anthesis growth stage and there is one open
!             flower per plant =100% bloom. in soybean, there is one open flower at any node.
!             this array includes daynum, year, month and day of when this stage was reached.
!     bio%upgm%blstrs - blister growth stage in corn. this array includes daynum,
!              year, month and day of when this stage was reached.
!     bio%upgm%boots - booting growth stage for hay millet, proso millet, spring
!             barley, spring wheat, winter barley and winter wheat. this array
!             includes daynum, year, month and day of when this stage was
!             reached.  booting is defined as flag leaf has completed its
!             growth.
!     bio%upgm%bmats - beginning maturity growth stage for soybean. one pod anywhere
!              with its mature color. this array includes daynum, year, month and 
!             day of when this stage was reached.  
!     bio%upgm%browns - when the back of the sunflower head is yellow and there may be
!              some brown spotting. this array includes daynum, year, month
!              and day of when this stage was reached.
!     bio%upgm%canht - the cumulative height of the plant canopy.
!     ctrl%sim%cliname - the name of the location for the climate data.
!     bio%upgm%cots - cotyledonary and unifoliolate leaves are visible in dry
!            beans and soybean. this array includes daynum, year, month and day
!            of when this stage was reached.
!     daa - days after anthesis.
!     dae - days after emergence.
!     dap - days after planting.
!     dav - days after vernalization.
!     bio%upgm%dayhtinc - the increase in plant height for today.
!     daynum - the current day numbered from jan 1.
!     ddae - an array holding the dae for each growth stage.
!     ddap - an array holding the dap for each growth stage.
!     ddav - an array holding the dav for each growth stage.
!     bio%upgm%dents - the dent growth stage in corn. this array includes daynum,
!             year, month and day of when this stage was reached.
!     dgdde - an array holding the gdde for each growth stage.
!     dgdds - an array holding the gdds for each growth stage.
!     dgddv -  an array holding the gddv for each growth stage.
!     bio%upgm%doughs - the dough growth stage in corn. this array includes daynum,
!              year, month and day of when this stage was reached.
!     bio%upgm%drs - double ridge growth stage for hay millet, proso millet, spring
!           barley, spring wheat, winter barley and winter wheat. this array
!           includes daynum, year, month and day of when this stage was
!           reached.
!     bio%upgm%dummy2 - an array to hold the gdd values, both under stressed
!              and non- stressed conditions,required to reach each growth
!              stage of the current crop.
!     bio%upgm%ears - the ear initiation stage in corn. this array includes daynum,
!            year, month and day of when this stage was reached.
!     bio%upgm%ecanht - this is the maximum canopy height of the crop in phase 1 of
!              the canopy height growth.  this is usually from emergence to
!              when the plant begins elongating stems but this stage varies
!              among crops. it is an input parameter and is read in from upgm_crop.dat.
!     bio%upgm%emrgflg - a flag to determine if the new emerge subroutine should be
!               called (bio%upgm%emrgflg=1) or to proceed with the weps/upgm method
!               of achieving emergence (bio%upgm%emrgflg=0).
!     bio%upgm%ems - day when emergence occurred in all crops. this array includes
!           daynum, year, month and day of when this event occurred.
!     bio%upgm%endlgs - end of leaf growth stage in sorghum. this array includes
!              daynum, year, month and day of when this stage was reached.
!     endphenol - a flag to indicate if this subroutine should be called
!                 again on the next day.
!     bio%upgm%epods - one pod has reached the maximum length in dry beans (early
!             pod set). in soybean, it is beginning pod stage and a pod is 3/16" long at one of the four uppermost nodes. 
!             this array includes daynum,year, month and day of when this stage was reached.
!     bio%upgm%eseeds - there is one pod with fully developed seeds in dry
!              beans (early seed fill). in soybean, this is beginning seed stage and seed is 1/8" long in pod at one 
!              of the four uppermost nodes.this array includes daynum, year, month and day of when
!              this stage was reached.
!     bio%upgm%first7 - used to set the value of bio%upgm%aepa the first time the crop's phenol
!              subroutine is called.
!     bio%upgm%fps - flower primordium initiation growth stage. this array includes
!           daynum, year, month and day of when this stage was reached.
!     bio%upgm%fullbs - full bloom growth stage in sorghum. this array includes
!            daynum, year, month and day of when this stage was reached.
!     gdda - growing degree days from anthesis.
!     gddday - the number of gdd with 0°C base temperature for that day.
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
!     bio%upgm%gmethod - selects the method whereby gdd will be calculated.  a value
!               of 1 corresponds to method 1 in phenologymms and is used
!               for crops such as winter wheat, winter barley and proso
!               millet. a value of 2 corresponds to method 2 in
!               phenologymms and is used for crops such as corn, dry beans,
!               sorghum and sunflower.  a value of 3 is the way that weps/upgm
!               calculated ggd for the day.
!     bio%upgm%gpds - growing point differentiation growth stage in sorghum. this
!            array includes daynum, year, month and day of when this stage
!            was reached.
!     bio%upgm%halfbs - half bloom growth stage in sorghum. this array includes
!            daynum, year, month and day of when this stage was reached.
!     bio%upgm%heads - heading growth stage for hay millet, proso millet, spring
!             barley, spring wheat, winter barley and winter wheat. this
!             array includes daynum, year, month and day of when this stage
!             was reached.
!     bio%upgm%hrs - time to harvest ready growth stage for corn, dry beans, hay
!           millet, proso millet, sorghum, soybean, spring barley, spring wheat,
!           sunflower, winter barley and winter wheat. in dry beans, 80%
!           of pods are at the mature color. this array
!           includes daynum, year, month and day of when this stage was
!           reached.
!     bio%upgm%ies - start of internode elongation growth stage for corn, hay millet,
!           proso millet, sorghum, soybean, spring barley, spring wheat, winter barley,
!           and winter wheat. for sunflower, this stage occurs when the
!           internode below the inflorescence elongates 0.5 to 2.0 cm above
!           the nearest leaf on the stem. this array includes daynum, year,
!           month and day of when this stage was reached.
!     bio%upgm%ies2 - for sunflower, this is when the internode below the inflorescence
!            continues lengthening and lifts the head above the surrounding
!            leaves more than 2 cm. this array includes daynum, year,
!            month and day of when this stage was reached.
!     bio%upgm%infls - the sunflower inflorescence becomes visible. this array includes
!             daynum, year, month and day of when this stage was reached.
!     jan1 - a flag to test if january 1 has occurred.  if it has passed,
!            then the winter annual crop is assumed to have completed
!            vernalization.
!     bio%upgm%joints - jointing growth stage for hay millet, proso millet, sorghum,
!              spring barley, spring wheat, winter barley and winter wheat.
!              this array includes daynum, year, month and day of when this
!              stage was reached.
!     bio%upgm%lf1s - stage when the first trifoliolate leaf is unfolded in dry
!            beans and soybean. this array includes daynum, year, month and day of
!            when this stage was reached.
!     bio%upgm%lf12s - the 12 leaf growth stage for corn and sunflower. this array
!             includes daynum, year, month and day of when this stage was
!             reached.
!     bio%upgm%lf2s - stage when the second trifoliolate leaf is unfolded in dry
!            beans and soybean. this array includes daynum, year, month and day of
!            when this stage was reached.
!     bio%upgm%lf3s - stage when the third trifoliolate leaf is unfolded in dry
!            beans and soybean. this array includes daynum, year, month and day of
!            when this stage was reached.
!     bio%upgm%lf4s - the 4 leaf growth stage for corn and sunflower and the
!            stage when the fourth trifoliolate leaf is unfolded in dry
!            beans and soybean. this array includes daynum, year, month and day of
!            when this stage was reached.
!     bio%upgm%lf5s - the 5 leaf growth stage for soybean and the stage when the fifth 
!            trifoliolate leaf is unfolded. this array includes daynum, year, month 
!            and day of when this stage was reached.
!     bio%upgm%lf8s - the 8 leaf growth stage for sunflower. this array includes
!            daynum, year, month and day of when this stage was reached.
!     lnarray - an array to hold the leaf number calculated for each day
!     lncntr - counter for the leafno subroutine
!     lnpout - an array used in writing out daynum and the number of leaves
!              on that day. the values are written each time a new leaf has
!              appeared.
!     bio%upgm%mats - physiological maturity growth stage for corn, dry beans,
!            hay millet, proso millet, sorghum, soybean (full maturity), spring barley, 
!            spring wheat, sunflower, winter barley and winter wheat. in dry beans,
!            one pod has changed color/striped. in soybean, 95% of the pods have 
!            reached their mature color. this array includes daynum, year, month
!            and day of when this stage was reached.
!     bio%upgm%maxht - the maximum height of the plant canopy.
!     bio%upgm%mffls - the stage of mid to full flower in dry beans. in soybean, this
!             is full bloom there is one open flower at one of the two uppermost nodes. 
!             this array includes daynum, year, month and day of when this stage
!             was reached.
!     bio%upgm%milks - the milk growth stage in corn. this array includes daynum, year,
!             month and day of when this stage was reached.
!     bio%upgm%mpods - the stage when 50% of the pods are at the maximum length.
!             this array includes daynum, year, month and day of when
!             this stage was reached.
!     bio%upgm%mseeds - the stage when 50% of the pods have fully developed seeds
!              in dry beans. in soybean, this stage is full seed and is reached when 
!              there is a pod containing a green seed that fills the pod cavity at one 
!              of the four uppermost nodes. this array includes daynum, year, month and
!              day of when this stage was reached.
!     bio%upgm%opens - the sunflower inflorescence begins to open. this array includes
!             daynum, year, month and day of when this stage was reached.
!     partcoefleaf - the partitioning coefficient for leaves
!     partcoefrepro - the partitioning coefficient for reproductive parts
!     partcoefstem - the partitioning coefficient for stems
!     bio%upgm%pchron - phyllochron value which is the number of gdd per leaf.
!     pdate - planting date.
!     rowcntr - a counter for the rows in an array
!     bio%upgm%seedsw - soil water content at seed depth.  it is read in as
!              optimum, medium, dry or planted in dust and converted
!              to an integer.	 1 = optimum, 2 = medium, 3 = dry and
!              4 = planted in dust.
!      bio%upgm%silks - the silking growth stage in corn. this array includes daynum,
!             year, month and day of when this stage was reached.
!     bio%upgm%soilwat - an array holding the swtype for each soil moisture
!               condition.
!     bio%upgm%srs - single ridge growth stage for hay millet, proso millet, spring
!           barley, spring wheat, winter barley and winter wheat. this array
!           includes daynum, year, month and day of when this stage was
!           reached.
!     bio%upgm%tbase - lowest temperature below which no growth occurs (deg.c).
!     bio%upgm%tis - start of tillering growth stage for corn, hay millet, proso
!           millet, sorghum, spring barley, spring wheat, winter barley and
!           winter wheat. this array includes daynum, year, month and day of
!           when this stage was reached.
!     todayln - the value of the current day's leaf number
!     bio%upgm%toptlo - the lower temperature in the optimum range for plant
!              growth (deg.c).
!     bio%upgm%toptup - the upper temperature in the optimum range for plant
!              growth (deg.c).
!     bio%upgm%tsints - tassel initiation growth stage in corn. this array includes
!              daynum, year, month and day of when this stage was reached.
!     bio%upgm%tss - terminal spikelet growth stage for spring and winter wheat. this
!           array includes daynum, year, month and day of when this stage was
!           reached.
!     bio%upgm%tupper - upper/maximum temperature for plant growth (deg.c).
!              no growth with temperatures above this point.
!     uesupgmpart - flag when TRUE tells model to use the partitioning coefficients.
!     bio%upgm%yelows - back of the sunflower head is a light yellow. this array
!              includes daynum, year, month and day of when this stage was
!              reached.
!     yestln - the value of yesterday's leaf number
 
! ***** calculating gdd *****
!debe added call to new subroutine gddcalc.
!debe added variables: bio%upgm%tbase, bio%upgm%toptlo, bio%upgm%toptup. take out sending bctopt to
!gddcalc for topt. topt is now being calculated in gddcalc as the average
! of bio%upgm%toptlo and bio%upgm%toptup.
call gddcalc(bio%database%tmin,clidat%awtdmn,clidat%awtdmx,gddday,bio%upgm%gmethod,bio%upgm%tbase,bio%upgm%toptlo,bio%upgm%toptup,bio%upgm%tupper)
 
 
!debe moved the next two lines of code out of the 'if (huiy.lt.1.0) then'
! statement so that bio%upgm%hrs growth stage will show the amounts for dap and
! gdds. this is because bio%upgm%hrs growth stage occurs after huiy.lt.1.0 and
! these 2 values will not be incremented for the bio%upgm%hrs stage.  the values
! are available for shoot_grow and emerge because they are saved in the
! save statement above.
 
if ((daynum>pdate).or.(yy>=2)) then
! debe added 'or' so that dap will increment in crops that go into year 2,
! i.e. winter crops.
!debe try ge 2 so that years greater than 2 can be used, i.e. from
!historical weather so that upgm will run.
  gdds = gdds + gddday           !debe moved this inside the if statement
  dap = dap + 1                  !gdds accumulates only after planting day
end if
!debe moved the following section for the accumulation of gdde, gddv
! and gdda and the calls to leafno and phenol to occur only if emergence
! has occurred.
!
! ***** emergence *****
!  accumulate gdd from emergence (gdde) and days after emergence (dae)
!  once emergence has occurred:
if (bio%upgm%ems(1)/=999) then       ! emergence has occurred
!!debe incrementing dae the day after emergence occurs.
!  if ((daynum.gt.bio%upgm%ems(1)).or.(yy.eq.2)) then
!  if (bio%upgm%ems(1).gt.dap) then the crop doesn't finish because dap becomes
!                          .gt. bio%upgm%ems(1) before bio%upgm%hrs is reached if this if statement is used.
 
    gdde = gdde + gddday       !debe moved here so that gdde is increased after
  dae = dae + 1              !day of emergence.
!  end if
!
!if emergence has occurred, call phenol and leaf number subroutines.
!debe added the variables to pass to phenol for leaf number and crop
!phenol subroutines.
 
!debe moved call to leafno subroutine before the call to phenol
!subroutine.
 
! ***** leafno *****
!  call the leafno subroutine to calculate the number of leaves once
!  emergence has occurred for the leaf number output table.
! debe added eseeds to be sent to leafno to allow determining leaf number in soybean  
  call leafno(bio%upgm%antss,bio%upgm%boots,bio%bname,daynum,dgdde,bio%upgm%endlgs,bio%upgm%epods,&
             &bio%upgm%eseeds,gdde,ln,lnarray,lncntr,lnpout,bio%upgm%pchron,rowcntr,todayln,yestln)

  !testing values in lnpout coming back from leafno subroutine
  !print *, 'after call to leafno lnpout leaf num = ', lnpout(lncntr,2)
  !print*, 'rowcntr = ', rowcntr

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
!!        didn't get past bio%upgm%tis growth stage up there.
!
!
!! !debe with only the next two lines not commented out, bio%upgm%hrs occurs 4/29 for
!! ! cimarron weather, wsf 0.9, pdepth 2.5, planting date 9/1
!!!         gddv = gddv + gddday
!!!	        dav = dav + 1
!!!		   endif
 
! *** phenologymms version of vernalization ***
!debe moved next line of code to crop so that dav will be incremented truly
! on the day(s) after vernalization
!  if ((mm.eq.1).and.(dd.eq.1).and.(yy.eq.2)) jan1 = .true.
 
! if vernalization requirement has been satisfied, then start accumulating
! gdd from vernalization (gddv). assume this to be jan 1 for northern
! hemisphere.
  if (jan1.eqv..true.) then
! debe 013009 following line was the original way but gddv did not
! accumulate soon enough. ! if (hu_delay .eq. 1.) then ! .and. (jan1 .eq. .true.)) then
     gddv = gddv + gddday
     dav = dav + 1
  end if
!
! ***** anthesis *****
! calculate growing degree-days from anthesis (gdda) and number of days
! after anthsis has occurred. just need to know that anthesis has started.
  if (bio%upgm%antss(1)/=999) then
     gdda = gdda + gddday
     daa = daa + 1
  end if
 
!debe moved call to canopyht above the call to phenol so that bio%upgm%dayhtinc will
! have the height increase value on the day of anthesis. then on the next day
! anthesis will have occurred and bio%upgm%antss(1) will not equal 999 any more and
! canopyht will not be called. all other growth stages used in canopyht will
! have been passed long before anthesis occurs and will have their own daynum
! and date values.
 
!debe add phenologymms method of determining canopy height.
!debe took out passing gddwsf; it is not used in canopyht.
  if (bio%upgm%antss(1)==999) then
     if (bio%bname=='corn') then
        call canopyht(bio%upgm%antss,bio%upgm%canht,bio%bname,bio%upgm%dayhtinc,bio%upgm%dummy2,bio%upgm%ecanht,bio%upgm%ems,gddday,gdde, &
                    & bio%upgm%ies,bio%upgm%maxht)
     else if (bio%bname=='dry beans') then
        call canopyht(bio%upgm%antss,bio%upgm%canht,bio%bname,bio%upgm%dayhtinc,bio%upgm%dummy2,bio%upgm%ecanht,bio%upgm%ems,gddday,gdde, &
                    & bio%upgm%cots,bio%upgm%maxht)
     else if (bio%bname=='soybean') then
        call canopyht(bio%upgm%antss,bio%upgm%canht,bio%bname,bio%upgm%dayhtinc,bio%upgm%dummy2,bio%upgm%ecanht,bio%upgm%ems,gddday,gdde, &
                    & bio%upgm%cots,bio%upgm%maxht)
     else if (bio%bname=='sorghum') then
        call canopyht(bio%upgm%antss,bio%upgm%canht,bio%bname,bio%upgm%dayhtinc,bio%upgm%dummy2,bio%upgm%ecanht,bio%upgm%ems,gddday,gdde, &
                    & bio%upgm%ies,bio%upgm%maxht)
     else if (bio%bname=='spring barley') then
        call canopyht(bio%upgm%antss,bio%upgm%canht,bio%bname,bio%upgm%dayhtinc,bio%upgm%dummy2,bio%upgm%ecanht,bio%upgm%ems,gddday,gdde, &
                    & bio%upgm%aifs,bio%upgm%maxht)
     else if (bio%bname=='sunflower') then
        call canopyht(bio%upgm%antss,bio%upgm%canht,bio%bname,bio%upgm%dayhtinc,bio%upgm%dummy2,bio%upgm%ecanht,bio%upgm%ems,gddday,gdde, &
                    & bio%upgm%lf4s,bio%upgm%maxht)
     else if (bio%bname=='winter barley') then
        call canopyht(bio%upgm%antss,bio%upgm%canht,bio%bname,bio%upgm%dayhtinc,bio%upgm%dummy2,bio%upgm%ecanht,bio%upgm%ems,gddday,gdde, &
                    & bio%upgm%aifs,bio%upgm%maxht)
     else   !rest of the crops: hay millet, proso millet, spring wheat, winter wheat
        call canopyht(bio%upgm%antss,bio%upgm%canht,bio%bname,bio%upgm%dayhtinc,bio%upgm%dummy2,bio%upgm%ecanht,bio%upgm%ems,gddday,gdde, &
                    & bio%upgm%tss,bio%upgm%maxht)
     end if
          !end if for bio%bname
  end if
      !end if for bio%upgm%antss
 
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
 
!debe added ctrl%cropstress%ahfwsf (weps/upgm variable)to the call to phenol to implement
! daily water stress effect on time of reaching a growth stage.
!debe added gddwsf array to pass to phenol.  the gddwsf array holds the
! number of gdd required to reach each growth stage based on the
! water stress factor.
 
!debe added variables to pass to the phenol_cropname subroutines for
!printing out information: bio%database%growdepth, bio%upgm%gmethod, bio%upgm%emrgflg, ctrl%sim%cliname
!debe added bio%upgm%canht to pass to phenol to be written in phenol.out from
!phenol_cropname. debe added variables for dry beans. DE added variables for partitioning.

  call phenol(ctrl,bio,daa,dae,dap,dav,daynum,ddae,ddap,ddav,dgdde,dgdds,dgddv,endphenol,gdda,gdde,gdds,gddv, &
              & gddwsf,lnpout,partcoefleaf,partcoefstem,partcoefrepro,pdate,useupgmpart,yy)
  !
end if
!
end subroutine phenolmms

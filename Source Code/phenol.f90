subroutine phenol(ctrl,bio,daa,dae,dap,dav,daynum,ddae,ddap,ddav,dgdde,dgdds,dgddv,endphenol,gdda,gdde,gdds,gddv,gddwsf, &
                 & lnpout,partcoefleaf,partcoefstem,partcoefrepro,pdate,useupgmpart,year)
!
!  the phenol subroutine calls subroutines to calculate the phenology of
!  a specific crop.
!
!  inputs:  bio%upgm%aepa(r), bio%upgm%aifs(r), bio%upgm%antes(r), bio%upgm%antss(r), bio%upgm%boots(r), bio%bname(r),
!           dae(r), dap(r), dav(r), daynum(r), ddae(r), ddap(r), ddav(r),
!           dgdde(r), dgdds(r), dgddv(r), bio%upgm%drs(r), bio%upgm%dummy2(r), bio%upgm%ems(r),
!           bio%upgm%endlgs(r), bio%upgm%first7(r), bio%upgm%fps(r), bio%upgm%fullbs(r), gdde(r), gdds(r),
!           gddv(r), bio%upgm%gpds(r), bio%upgm%halfbs(r), bio%upgm%heads(r), bio%upgm%hrs(r), bio%upgm%ies(r),
!           bio%upgm%joints(r), bio%upgm%mats(r), nolvs(r), bio%upgm%pchron(r), bio%upgm%srs(r), bio%upgm%tis(r),
!           bio%upgm%tss(r)
 
!  outputs: bio%upgm%aepa(r), bio%upgm%aifs(r), bio%upgm%antes(r), bio%upgm%antss(r), bio%upgm%boots(r), bio%bname(r),
!           dae(r), dap(r), dav(r), daynum(r), ddae(r), ddap(r),
!           ddav(r), dgdde(r), dgdds(r), dgddv(r), bio%upgm%drs(r), bio%upgm%dummy2(r),
!           bio%upgm%ems(r), bio%upgm%endlgs(r), bio%upgm%first7(r), bio%upgm%fps(r), bio%upgm%fullbs(r), gdde(r),
!           gdds(r), gddv(r), bio%upgm%gpds(r), bio%upgm%halfbs(r), bio%upgm%heads(r), bio%upgm%hrs(r),
!           bio%upgm%ies(r), bio%upgm%joints(r), bio%upgm%mats(r), nolvs(r), bio%upgm%pchron(r), bio%upgm%srs(r),
!           bio%upgm%tis(r), bio%upgm%tss(r)
!
!debe added bc0growdepth, bio%upgm%emrgflg, gemthod, bio%upgm%seedsw, bio%upgm%soilwat to pass to
! phenol_cropname subroutines to print out to the output file.  yy was
! passed from crop.
!debe added variables for and call to phenoldb for dry beans
!debe changed bc0growdepth to a phenologymms variable name (bio%database%growdepth) to
!maintian consistency with phenologymms code. also, changed yy to year for
!the same reason. ctrl%cropstress%ahfwsf is an upgm variable name but it is used to pass to
!water_stress subroutine which is not a phenologymms subroutine but one that we
!added to upgm. will leave ctrl%cropstress%ahfwsf as is.
!
    use upgm_simdata, only : controls
    use biomaterial
implicit none
!
! Dummy arguments
!
    type(controls),     intent(inout) :: ctrl
    type(biomatter),    intent(inout) :: bio
real :: gdda,gdde,gdds,gddv
integer :: daa,dae,dap,dav,daynum,pdate,year, rowcntr
logical :: endphenol,useupgmpart
real :: partcoefleaf,partcoefstem,partcoefrepro
integer,dimension(20) :: ddae,ddap,ddav
real,dimension(20) :: dgdde,dgdds,dgddv
real,dimension(16,5) :: gddwsf
real,dimension(100,2) :: lnpout
!
! Local variables
!
character(80) :: seedbed
integer :: j
!
!debe added ctrl%cropstress%ahfwsf to implement daily water stress effect on time
! of reaching a growth stage. added dummy1 to determine stress level of
! the crop to be used in the algorithm determining the gdd based
! on the daily water stress factor.
 
!debe changed bio%upgm%seedsw from integer to real to allow moving half a soil moisture level with precip.
! later changed it back to an integer because array subscripts must be integers or constants.
 
 
 
!debe changed the dimensions of bio%upgm%dummy2 array to include both stressed and
! non-stressed values.
!debe added gddwsf which has been initialized in cinint and is ready to be
! passed to the appropriate phenol_cropname subroutine.
!debe added ctrl%sim%cliname for writing out the climate file name.
 
 
!     + + + argument definitions + + +
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
!             flower per plant =100% bloom. in soybean, tere is one open flower at any 
!             node. this array includes daynum, year, month and day of when this stage was reached.
!     ctrl%cropstress%ahfwsf - water stress factor ratio (0-1).  this is read in daily.
!     bio%upgm%blstrs - blister growth stage in corn. this array includes daynum,
!              year, month and day of when this stage was reached.
!     bio%upgm%bmats - in soybean, beginning maturity. There is one pod anywhere with its mature color.
!             this array includes daynum, year, month and day of when this stage was reached
!     bio%upgm%boots - booting growth stage for hay millet, proso millet, spring
!             barley, spring wheat, winter barley and winter wheat. this array
!             includes daynum, year, month and day of when this stage was
!             reached.  booting is defined as flag leaf has completed its
!             growth.
!     bio%upgm%browns - when the back of the sunflower head is yellow and there may be
!              some brown spotting. this array includes daynum, year, month
!              and day of when this stage was reached.
!     ctrl%sim%cliname - the name of the location for the climate data.
!     bio%bname - crop name.
!     bio%upgm%cots - cotyledonary and unifoliolate leaves are visible in dry
!            beans and soybean. this array includes daynum, year, month and day
!            of when this stage was reached.
!     daa - days after anthesis.
!     dae - days after emergence.
!     dap - days after planting.
!     dav - days after vernalization.
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
!     bio%upgm%emrgflg - a flag to determine if the new emerge subroutine should be
!               called (bio%upgm%emrgflg=1) or to proceed with the weps/upgm method
!               of achieving emergence (bio%upgm%emrgflg=0).
!     bio%upgm%ems - day when emergence occurred in all crops. this array includes
!           daynum, year, month and day of when this event occurred.
!     bio%upgm%endlgs - end of leaf growth stage in sorghum. this array includes
!              daynum, year, month and day of when this stage was reached.
!     endphenol - a flag to indicate if this subroutine should be called
!                 again the next day.
!     bio%upgm%epods - one pod has reached the maximum length in dry beans.
!             in soybean, beginning pod, pod is 3/16" long at one of the four 
!             uppermost nodes. this array includes daynum, year, month and day 
!             of when this stage was reached.
!     bio%upgm%eseeds - there is one pod with fully developed seeds in dry
!              beans. in soybean, beginning seed, seed is 1/8" long in pod 
!              at one of the four uppermost nodes. this array includes daynum, year, 
!              month and dayo f when this stage was reached.
!     bio%upgm%first7 - used to set the value of bio%upgm%aepa the first time the crop's phenol
!              subroutine is called.
!     bio%upgm%fps - flower primordium initiation growth stage. this array includes
!           daynum, year, month and day of when this stage was reached.
!     bio%upgm%fullbs - full bloom growth stage in sorghum. this array includes
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
!     bio%upgm%gmethod - selects the method whereby gdd will be calculated.  a value
!               of 1 corresponds to method 1 in phenologymms and is used
!               for crops such as winter wheat, winter barley and proso
!               millet. a value of 2 corresponds to method 2 in
!               phenologymms and is used for crops such as corn, sorghum
!               and sunflower.  a value of 3 is the way that weps/upgm
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
!           of pods are at the mature color in dry beans. this array
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
!     bio%upgm%joints - jointing growth stage for hay millet, proso millet, sorghum,
!              spring barley, spring wheat, winter barley and winter wheat.
!              this array includes daynum, year, month and day of when this
!              stage was reached.
!     bio%upgm%lf1s - stage when the first trifoliolate leaf is unfolded in dry
!            beans. this array includes daynum, year, month and day of
!            when this stage was reached.
!     bio%upgm%lf12s - the 12 leaf growth stage for corn and sunflower. this array
!             includes daynum, year, month and day of when this stage was
!             reached.
!     bio%upgm%lf2s - stage when the second trifoliolate leaf is unfolded in dry
!            beans. this array includes daynum, year, month and day of
!            when this stage was reached.
!     bio%upgm%lf3s - stage when the third trifoliolate leaf is unfolded in dry
!            beans. this array includes daynum, year, month and day of
!            when this stage was reached.
!     bio%upgm%lf4s - the 4 leaf growth stage for corn and sunflower and the
!            stage when the fourth trifoliolate leaf is unfolded in dry
!            beans. this array includes daynum, year, month and day of
!            when this stage was reached.
!     bio%upgm%lf5s - the stage when the fifth trifoliolate leaf is unfolded in 
!            soybean.  this array includes daynum, year, month and day of
!            when this stage was reached.
!     bio%upgm%lf8s - the 8 leaf growth stage for sunflower. this array includes
!            daynum, year, month and day of when this stage was reached.
!     lnpout - an array used in writing out daynum and the number of leaves
!              on that day. the values are written each time a new leaf has
!              appeared.
!     bio%upgm%mats - physiological maturity growth stage for corn, dry beans,
!            hay millet, proso millet, sorghum, soybean, spring barley, spring
!            wheat, sunflower, winter barley and winter wheat. in dry beans,
!            one pod has changed color/striped. in soybean, full maturity, 95% 
!            of the pods have reached their mature color. this array includes
!            daynum, year, month and day of when this stage was reached.
!     bio%upgm%mffls - the stage of mid to full flower in dry beans. in soybean, 
!             full bloom, one open flower at one of the two uppermost nodes. this array
!             includes daynum, year, month and day of when this stage
!             was reached.
!     bio%upgm%milks - the milk growth stage in corn. this array includes daynum, year,
!             month and day of when this stage was reached.
!     bio%upgm%mpods - the stage when 50% of the pods are at the maximum length in dry beans.
!             in soybean, full pod, pod is 3/4" long at one of the four uppermost nodes. 
!             this array includes daynum, year, month and day of when
!             this stage was reached.
!     bio%upgm%mseeds - the stage when 50% of the pods have fully developed seeds
!              in dry beans. in soybean, full seed, pod contains a green seed that 
!              fills the pod cavity at one of the four uppermost nodes. this array 
!              includes daynum, year, month and day of when this stage was reached.
!     bio%upgm%opens - the sunflower inflorescence begins to open. this array includes
!             daynum, year, month and day of when this stage was reached.
!     partcoefleaf - the partitioning coefficient for leaves
!     partcoefrepro - the partitioning coefficient for reproductive parts
!     partcoefstem - the partitioning coefficient for stems
!     bio%upgm%pchron - phyllochron value which is the number of gdd per leaf.
!     pdate - planting date.
!     bio%database%growdepth - depth of growing point at time of planting (m).
!              bc0growthdepth is passed into this variable.
!     seedbed - contains the soil moisture condition of the seedbed.
!     bio%upgm%seedsw - soil water content at seed depth.  it is read in as
!              optimum, medium, dry or planted in dust and converted
!              to an integer.	 1 = optimum, 2 = medium, 3 = dry and
!              4 = planted in dust.
!     bio%upgm%silks - the silking growth stage in corn. this array includes daynum,
!             year, month and day of when this stage was reached.
!     bio%upgm%soilwat - an array holding the swtype for each soil moisture
!               condition.
!     bio%upgm%srs - single ridge growth stage for hay millet, proso millet, spring
!           barley, spring wheat, winter barley and winter wheat. this array
!           includes daynum, year, month and day of when this stage was
!           reached.
!     bio%upgm%tis - start of tillering growth stage for corn, hay millet, proso
!           millet, sorghum, spring barley, spring wheat, winter barley and
!           winter wheat. this array includes daynum, year, month and day of
!           when this stage was reached.
!     bio%upgm%tsints - tassel initiation growth stage in corn. this array includes
!              daynum, year, month and day of when this stage was reached.
!     bio%upgm%tss - terminal spikelet growth stage for spring and winter wheat. this
!           array includes daynum, year, month and day of when this stage was
!           reached.
!     uesupgmpart - flag when TRUE tells model to use the partitioning coefficients.
!     year - year. yy is passed into this variable name.
!     bio%upgm%yelows - back of the sunflower head is a light yellow. this array
!              includes daynum, year, month and day of when this stage was
!              reached.
!
 
!debe get seedbed moisture condition to be used in printing it in output.
!seedbed = bio%upgm%soilwat(bio%upgm%seedsw)
!de added to prevent bio%upgm%soilwat getting an array index of 0
if (bio%upgm%seedsw .NE. 0) then
    seedbed = bio%upgm%soilwat(bio%upgm%seedsw)
else 
    seedbed = ""    
end if  

 
!debe added ctrl%cropstress%ahfwsf  to the call to each crop's phenol subroutine
! to implement daily water stress effect on time of reaching a growth stage.
!debe added gddwsf to the passing arguments for each crop below.
 
! call the correct phenology subroutine for the selected crop:
!
! note: if corn: make sure the cropxml.dat file has only 'corn' for the
! crop name. if the variety name is e.g. 'corn, grain, 110' then it must
! be adjusted to read only 'corn'.
if ((bio%bname=='corn').and.(endphenol.neqv..true.)) then
!print*, 'in call to phenolcn in phenol. bio%upgm%dummy2(5) = ', bio%upgm%dummy2(5)
  call phenolcn(ctrl,bio%upgm%aepa,bio%upgm%antss,bio%database%growdepth,ctrl%cropstress%ahfwsf,bio%upgm%blstrs,ctrl%sim%cliname,bio%bname,daa,dae,dap, &
              & daynum,ddae,ddap,bio%upgm%dents,dgdde,dgdds,bio%upgm%doughs,bio%upgm%dummy2,bio%upgm%ears,bio%upgm%emrgflg,bio%upgm%ems,bio%upgm%first7,&
              & gdda,gdde,gdds,gddwsf,bio%upgm%gmethod,bio%upgm%hrs,bio%upgm%ies,bio%upgm%lf12s,bio%upgm%lf4s,lnpout,bio%upgm%mats,bio%upgm%milks,      &
              & partcoefleaf,partcoefstem,partcoefrepro,bio%upgm%pchron,pdate,seedbed,bio%upgm%silks,bio%upgm%tsints,useupgmpart,year,endphenol)
 
! if dry beans (crop name in cropxml.dat needs to read drybeans)
else if ((bio%bname=='dry beans').and.(endphenol.neqv..true.)) then
  call phenolbn(ctrl,bio%upgm%aepa,bio%upgm%antss,bio%database%growdepth,ctrl%cropstress%ahfwsf,bio%upgm%cots,ctrl%sim%cliname,bio%bname,daa,dae,dap,daynum, &
              & ddae,ddap,dgdde,dgdds,bio%upgm%dummy2,bio%upgm%emrgflg,bio%upgm%ems,endphenol,bio%upgm%epods,bio%upgm%eseeds,bio%upgm%first7,gdda,gdde,gdds, &
              & gddwsf,bio%upgm%gmethod,bio%upgm%hrs,bio%upgm%lf1s,bio%upgm%lf2s,bio%upgm%lf3s,bio%upgm%lf4s,lnpout,bio%upgm%mats,bio%upgm%mffls,            &
              & bio%upgm%mpods,bio%upgm%mseeds,partcoefleaf,partcoefstem,partcoefrepro,bio%upgm%pchron,pdate,seedbed,useupgmpart,year) 

  ! if soybean (crop name in cropxml.dat needs to read soybean)
else if ((bio%bname=='soybean').and.(endphenol.neqv..true.)) then
  call phenolsy(ctrl,bio%upgm%aepa,bio%upgm%antss,bio%upgm%bmats,bio%database%growdepth,ctrl%cropstress%ahfwsf,bio%upgm%cots,ctrl%sim%cliname,bio%bname,daa, &
              & dae,dap,daynum,ddae,ddap,dgdde,dgdds,bio%upgm%dummy2,bio%upgm%emrgflg,bio%upgm%ems,endphenol,bio%upgm%epods,bio%upgm%eseeds,bio%upgm%first7, &
              & gdda,gdde,gdds,gddwsf,bio%upgm%gmethod,bio%upgm%hrs,bio%upgm%lf1s,bio%upgm%lf2s,bio%upgm%lf3s,bio%upgm%lf4s,bio%upgm%lf5s,lnpout,            &
              & bio%upgm%mats,bio%upgm%mffls,bio%upgm%mpods,bio%upgm%mseeds,partcoefleaf,partcoefstem,partcoefrepro,bio%upgm%pchron,pdate,seedbed,           &
              & useupgmpart,year)
 
! if hay millet:
else if ((bio%bname=='hay millet').and.(endphenol.neqv..true.)) then
  call phenolhm(ctrl,bio%upgm%aepa,bio%upgm%antes,bio%upgm%antss,bio%database%growdepth,ctrl%cropstress%ahfwsf,bio%upgm%boots,ctrl%sim%cliname,bio%bname,  &
              & daa,dae,dap,daynum,ddae,ddap,dgdde,dgdds,bio%upgm%drs,bio%upgm%dummy2,bio%upgm%emrgflg,bio%upgm%ems,endphenol,bio%upgm%first7,bio%upgm%fps,&
              & gdda,gdde,gdds,gddwsf,bio%upgm%gmethod,bio%upgm%heads,bio%upgm%hrs,bio%upgm%ies,bio%upgm%joints,lnpout,bio%upgm%mats,partcoefleaf,         &
              & partcoefstem,partcoefrepro,bio%upgm%pchron,pdate,seedbed,bio%upgm%srs,bio%upgm%tis,bio%upgm%tss,useupgmpart,year)
 
! if proso millet:
else if ((bio%bname=='proso millet').and.(endphenol.neqv..true.)) then
  call phenolpm(ctrl,bio%upgm%aepa,bio%upgm%antes,bio%upgm%antss,bio%database%growdepth,ctrl%cropstress%ahfwsf,bio%upgm%boots,ctrl%sim%cliname,bio%bname,  &
              & daa,dae,dap,daynum,ddae,ddap,dgdde,dgdds,bio%upgm%drs,bio%upgm%dummy2,bio%upgm%emrgflg,bio%upgm%ems,endphenol,bio%upgm%first7,bio%upgm%fps,&
              & gdda,gdde,gdds,gddwsf,bio%upgm%gmethod,bio%upgm%heads,bio%upgm%hrs,bio%upgm%ies,bio%upgm%joints,lnpout,bio%upgm%mats,partcoefleaf,         &
              & partcoefstem,partcoefrepro,bio%upgm%pchron,pdate,seedbed,bio%upgm%srs,bio%upgm%tis,bio%upgm%tss,useupgmpart,year)
 
! if sorghum:
else if ((bio%bname=='sorghum').and.(endphenol.neqv..true.)) then
  call phenolsg(ctrl,bio%upgm%aepa,bio%upgm%antes,bio%upgm%antss,bio%database%growdepth,ctrl%cropstress%ahfwsf,ctrl%sim%cliname,bio%bname,daa,dae,dap,   &
                  & daynum,ddae,ddap,dgdde,dgdds,bio%upgm%dummy2,bio%upgm%emrgflg,bio%upgm%ems,bio%upgm%endlgs,endphenol,bio%upgm%first7,bio%upgm%fullbs,&
                  & gdda,gdde,gdds,gddwsf,bio%upgm%gpds,bio%upgm%gmethod,bio%upgm%halfbs,bio%upgm%hrs,bio%upgm%ies,bio%upgm%joints,lnpout,bio%upgm%mats, &
                  & partcoefleaf,partcoefstem,partcoefrepro,bio%upgm%pchron,pdate,seedbed,bio%upgm%tis,useupgmpart,year)

! if spring barley:
else if ((bio%bname=='spring barley').and.(endphenol.neqv..true.)) then
  call phenolsb(ctrl,bio%upgm%aepa,bio%upgm%aifs,bio%upgm%antes,bio%upgm%antss,bio%database%growdepth,ctrl%cropstress%ahfwsf,bio%upgm%boots,            &
              & ctrl%sim%cliname,bio%bname,daa,dae,dap,daynum,ddae,ddap,dgdde,dgdds,bio%upgm%drs,bio%upgm%dummy2,bio%upgm%emrgflg,bio%upgm%ems,         &
              & endphenol,bio%upgm%first7,bio%upgm%fps,gdda,gdde,gdds,gddwsf,bio%upgm%gmethod,bio%upgm%heads,bio%upgm%hrs,bio%upgm%ies,bio%upgm%joints, &
              & lnpout,bio%upgm%mats,partcoefleaf,partcoefstem,partcoefrepro,bio%upgm%pchron,pdate,seedbed,bio%upgm%srs,bio%upgm%tis,useupgmpart,year)
 
! if spring wheat:
else if ((bio%bname=='spring wheat').and.(endphenol.neqv..true.)) then
  call phenolsw(ctrl,bio%upgm%aepa,bio%upgm%antes,bio%upgm%antss,bio%database%growdepth,ctrl%cropstress%ahfwsf,bio%upgm%boots,ctrl%sim%cliname,bio%bname,    &
              & daa,dae,dap,daynum,ddae,ddap,dgdde,dgdds,bio%upgm%drs,bio%upgm%dummy2,bio%upgm%emrgflg,bio%upgm%ems,endphenol,bio%upgm%first7,bio%upgm%fps,  &
              & gdda,gdde,gdds,gddwsf,bio%upgm%gmethod,bio%upgm%heads,bio%upgm%hrs,bio%upgm%ies,bio%upgm%joints,lnpout,bio%upgm%mats,partcoefleaf,           &
              & partcoefstem,partcoefrepro,bio%upgm%pchron,pdate,seedbed,bio%upgm%srs,bio%upgm%tis,bio%upgm%tss,useupgmpart,year)
 !
! if sunflower:
else if ((bio%bname=='sunflower').and.(endphenol.neqv..true.)) then
  call phenolsf(ctrl,bio%upgm%aepa,bio%upgm%antes,bio%upgm%antss,bio%database%growdepth,ctrl%cropstress%ahfwsf,bio%upgm%browns,ctrl%sim%cliname,bio%bname, &
              & daa,dae,dap,daynum,ddae,ddap,dgdde,dgdds,bio%upgm%dummy2,bio%upgm%emrgflg,bio%upgm%ems,endphenol,bio%upgm%first7,gdda,gdde,gdds,gddwsf,    &
              & bio%upgm%gmethod,bio%upgm%hrs,bio%upgm%ies,bio%upgm%ies2,bio%upgm%infls,bio%upgm%lf12s,bio%upgm%lf4s,bio%upgm%lf8s,lnpout,bio%upgm%mats,   &
              & bio%upgm%opens,partcoefleaf,partcoefstem,partcoefrepro,bio%upgm%pchron,pdate,seedbed,useupgmpart,year,bio%upgm%yelows)
 
! if winter barley:
else if ((bio%bname=='winter barley').and.(endphenol.neqv..true.)) then
  call phenolwb(ctrl,bio%upgm%aepa,bio%upgm%aifs,bio%upgm%antes,bio%upgm%antss,bio%database%growdepth,ctrl%cropstress%ahfwsf,bio%upgm%boots,ctrl%sim%cliname,&
              & bio%bname,daa,dae,dap,dav,daynum,ddae,ddap,ddav,dgdde,dgdds,dgddv,bio%upgm%drs,bio%upgm%dummy2,bio%upgm%ems,bio%upgm%emrgflg,endphenol,      &
              & bio%upgm%first7,bio%upgm%fps,gdda,gdde,gdds,gddv,gddwsf,bio%upgm%gmethod,bio%upgm%heads,bio%upgm%hrs,bio%upgm%ies,bio%upgm%joints,lnpout,    &
              & bio%upgm%mats,partcoefleaf,partcoefstem,partcoefrepro,bio%upgm%pchron,pdate,seedbed,bio%upgm%srs,bio%upgm%tis,useupgmpart,year)
 
! if winter wheat:
else if ((bio%bname=='winter wheat').and.(endphenol.neqv..true.)) then
  call phenolww(ctrl,bio%upgm%aepa,bio%upgm%antes,bio%upgm%antss,bio%database%growdepth,ctrl%cropstress%ahfwsf,bio%upgm%boots,ctrl%sim%cliname,bio%bname, &
              & daa,dae,dap,dav,daynum,ddae,ddap,ddav,dgdde,dgdds,dgddv,bio%upgm%dummy2,bio%upgm%drs,bio%upgm%emrgflg,bio%upgm%ems,bio%upgm%first7,       &
              & bio%upgm%fps,gdda,gdde,gdds,gddv,gddwsf,bio%upgm%gmethod,bio%upgm%heads,bio%upgm%hrs,bio%upgm%ies,bio%upgm%joints,lnpout,bio%upgm%mats,   &
              & bio%upgm%pchron,partcoefleaf,partcoefstem,partcoefrepro,pdate,seedbed,bio%upgm%srs,bio%upgm%tis,bio%upgm%tss,useupgmpart,year,endphenol)
 
!
end if
!
end subroutine phenol

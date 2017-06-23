subroutine callcrop(ctrl,clidat,sppdat,bio,aepa,aifs,daysim,sr,antes,antss,blstrs,boots,browns,callgdd,&
                  & canht,canopyflg,cliname,cots,cropname,dayhtinc,dents,doughs,&
                  & drs,dummy1,dummy2,ears,ecanht,egdd,emrgflg,ems,endlgs,epods,&
                  & ergdd,eseeds,first7,fps,fullbs,gddtbg,germgdd,germs,ggdd,   &
                  & gmethod,gpds,growth_stress,halfbs,heads,hrs,icli,ies,ies2,  &
                  & infls,joints,lf12s,lf1s,lf2s,lf3s,lf4s,lf8s,mats,maxht,     &
                  & mffls,milks,mpods,mseeds,opens,pchron,pd,phenolflg,pm,py,   &
                  & seedsw,silks,soilwat,srs,tbase,tis,toptlo,toptup,tsints,tss,&
                  & tupper,wfpslo,wfpsup,yelows,co2x,co2y,co2atmos)
 
     use upgm_simdata, only : upgm_ctrls, controls
     use climate, only : climate_data
     use soil, only : soil_phys_props
     use constants, only : mnsz, mnsub,mnbpls,mncz,mndk
     use biomaterial
!debe 082508 removed sram0jd-plant_jday+1 from the subroutine argument list
! because this caused many errors. i think a mistake was made in copying in
! the passing arguments from main. the first two arguments are (daysim, sr)
! in the callcrop subroutine in the 081308 archived copy. i put daysim back
! in as the first arguement in place of 'sram0jd-plant_jday+1' and sr in
! place of 1. this fixed the problem.
!debe added growth_stress because it is now read in.
!debe added canopyflg so that it can be passed on to crop.
!
! ***** emergence *****
!
!debe added the emergence arguments so that they can be passed to crop.
! these include seedsw, cropname, soilwat, wfpslo, wfpsup, germgdd,
! ergdd, gddday, gddtbg.  ac0nam is passed into cropname in main to be passed to
! emerge.
!debe added the emergence flag (emrgflg)to determine whether to call the
! emerge subroutine or not.
!debe added icli to pass to emerge to enable printing the weather file name
! in emerge.out. also, added pd,pm,py to print planting date in emerge.out.
! debe changed seedsw to real to allow moving a half soil moisture level.
! later changed back to integer because array subscripts should be integers.
!debe added two new arrays of 6 elements to be used in emerge.for to
! enable adding values for germgdd and ergdd for two soil moisture
! levels that are half steps between dry and medium and medium and optimum.
!
! ***** phenology *****
!
!debe added aepa and first7 because they are initialized in cropinit and need
! to be passed to crop. other phenology variables include: dummy1, dummy2,
! pchron.
!
! ***** gddcalc *****
!
!debe added the variable for the maximum upper temperature (tupper) value
! read in from upgm_crop.dat in main and passed into callcrop which will
! pass it to crop and then to gddcalc.
!debe added callgdd to be passed on to crop where it is used.
!
! ***** canopyht *****
!debe added maxht read in from upgm_crop. also, canht so that crop can pass
! it back to main and print the final canopy height in phenol.out.
!debe added canopyflg to determine which method of determining canopyheight
!will be used. i canoyflg=0, the weps/upgm method will be used, if it
!equals 1, the phenologymms method will be used.
!debe added dayhtinc to be able to pass the daily increase in height to growth
! for the ht_dia_sai subroutine in place of the weps/upgm variable dht when
! canopyflg = 1.
!debe added ecanht for height of phase 1 of canopy growth.
!    
! ***** co2 *****
!debe added co2x, co2y for the co2 arrays and co2atmos for the atmospheric co2 level.
!
implicit none
!
include 'c1db1.inc'
include 'c1db2.inc'
include 'c1db3.inc'
include 'c1info.inc'
include 'c1glob.inc'
include 'c1gen.inc'
include 'd1glob.inc'
include 'd1gen.inc'
include 's1dbc.inc'
include 'h1hydro.inc'
include 'h1et.inc'
include 'prevstate.inc'
include 'decomp.inc'
include 'cenvr.inc'
!
! Dummy arguments
!
    type(controls) :: ctrl
    type(climate_data) :: clidat
    type(soil_phys_props) :: sppdat
    type(biomatter) :: bio
real :: aepa,canht,dayhtinc,ecanht,gddtbg,maxht,pchron,tbase,toptlo,toptup,     &
      & tupper,co2atmos
logical :: callgdd
integer :: canopyflg,daysim,emrgflg,first7,gmethod,growth_stress,icli,pd,       &
         & phenolflg,pm,py,seedsw,sr
character(80) :: cliname
character(80) :: cropname
integer,dimension(4) :: aifs,antes,antss,blstrs,boots,browns,cots,dents,doughs, &
                      & drs,ears,ems,endlgs,epods,eseeds,fps,fullbs,germs,gpds, &
                      & halfbs,heads,hrs,ies,ies2,infls,joints,lf12s,lf1s,lf2s, &
                      & lf3s,lf4s,lf8s,mats,mffls,milks,mpods,mseeds,opens,     &
                      & silks,srs,tis,tsints,tss,yelows
character(5),dimension(30) :: dummy1
real,dimension(30) :: dummy2
real,dimension(6) :: egdd,ggdd
real,dimension(4) :: ergdd,germgdd,wfpslo,wfpsup
character(80),dimension(4) :: soilwat
real,dimension(10) :: co2x,co2y
!
! Local variables
!
integer :: lay
!
! include 'timer.fi'
!
!debe added the following common block
 
!     + + + common blocks + + +
!
! ***** upgm/weps variables *****
!
!debe added growth_stress because it is now read in.
!
! ***** newly added variables *****
!
!debe 110708 changed dimensions of dummy arrays 1 and 2 for stressed and
! non-stressed values.  this allows both stressd and non-stressed values
! to be included in the arrays.
!debe added gmethod read in from upgm_crop.dat in main.
!debe added passing of cliname through callcrop. debe added canopyflg.
!
! ***************************************************************** wjr

!     + + + end of specifications + + +
 
!   + + + argument definitions + + +
!   icli - a flag to determine which type of weather file to read.  a value
!          of 1 indicates that climate data should be read from the cligen
!          weather file.  a value of 0 indicates that a historical climate
!          file will be used.
!   pd - planting day
!   pm - planting month
!   py - planting year. currently, not the calendar year.
!   sr - used to access a cell in an array
!   timcrop - a parameter
!   timstart - a parameter
!   timstop - a parameter
!   yr - year
 
!   + + + local variable definitions + + +
!   lay - counter variable
 
 
!     + + +  newly added arguments definitions + + +
!     aepa - the parameter for duration of anthesis (i.e., gdd from start
!            to end of anthesis.
!     aifs - awn initials formed growth stage for spring barley and winter
!            barley. this array includes daynum, year, month and day of
!            when this stage was reached.
!     antes - end of anthesis growth stage for hay millet, proso millet,
!             spring barley, spring wheat, sunflower, winter barley and
!             winter wheat. this array includes daynum, year, month and
!             day of when this stage was reached.
!     antss - start of anthesis growth stage for hay millet, proso millet,
!             sorghum (first bloom), spring barley, spring wheat,
!             sunflower, winter barley and winter wheat. also, dry beans
!             and corn.this array includes daynum, year, month and day
!             of when this stage was reached.
!     blstrs - blister growth stage in corn. this array includes daynum,
!              year, month and day of when this stage was reached.
!     boots - booting growth stage for hay millet, proso millet, spring
!             barley, spring wheat, winter barley and winter wheat. this
!             array includes daynum, year, month and day of when this
!             stage was reached.  booting is defined as flag leaf has
!             completed its growth.
!     browns - when the back of the sunflower head is yellow and there
!              may be some brown spotting. this array includes daynum,
!              year, month and day of when this stage was reached.
!     callgdd - a flag to switch between methods for determining gdd.
!               if the flag is set to true then gddcalc subroutine is
!               called. otherwise, the code within crop is used.
!     canht - holds the final canopy height of the crop for the current day.
!     canopyflg - a flag to indicate if the weps/upgm method to calculate
!                 plant height will be used. value will then be 0. if using
!                 the phenologymms method, the value will be 1.
!     cliname - the name of the location for the climate data
!     co2atmos - the atmospheric level of CO2.
!     co2x - the CO2 levels in ppm. The x axis on the relationship curve.
!     co2y - the relative effect at different levels of CO2, i.e. co2x.
!     cots - cotyledonary and unifoliolate leaves are visible in dry
!            beans. this array includes daynum, year, month and day
!            of when this stage was reached.
!     cropname - name of the crop
!     dayhtinc - the increase in plant height for today.
!     daysim - day of the simulation
!     dents - the dent growth stage in corn. this array includes daynum,
!             year, month and day of when this stage was reached.
!     doughs - the dough growth stage in corn. this array includes daynum,
!              year, month and day of when this stage was reached.
!     drs - double ridge growth stage for hay millet, proso millet,
!           spring barley, spring wheat, winter barley and winter wheat.
!           this array includes daynum, year, month and day of when this
!           stage was reached.
!     dummy1 - in determining the next phenological growth stage, this
!              holds whether the condition is gn or gs, that is when gdd
!              values are used to advance to the next growth stage is it
!              done under non-stressed or stressed conditions.
!     dummy2 - an array to hold the gdd values, both under stressed
!              and non- stressed conditions,required to reach each growth
!              stage of the current crop.
!     ears - the ear initiation stage in corn. this array includes daynum,
!            year, month and day of when this stage was reached.
!     ecanht - this is the maximum canopy height of the crop in phase 1 of
!              the canoy height growth.  this is usually from emergence to
!              when the plant begins elongating stems but this stage varies
!              among crops. it is an input parameter and is read in from upgm_crop.dat.
!     egdd - a 6 element array that holds the ergdd values plus calculated values
!            for two intermediate soil moisture level values in elements 2 and 4.
!     emrgflg - a flag to determine if the new emerge subroutine should be
!               called (emrgflg=1) or to proceed with the weps/upgm method
!               of achieving emergence (emrgflg=0).
!     ems - day when emergence occurred in all crops. this array includes
!           daynum, year, month and day of when this event occurred.
!     endlgs - end of leaf growth stage in sorghum. this array includes
!              daynum, year, month and day of when this stage was reached.
!     epods - one pod has reached the maximum length in dry beans.
!             this array includes daynum, year, month and day of when
!             this stage was reached.
!     ergdd - an array holding 4 elongation rates in mm per gdd
!             based on each soil moisture description.
!     eseeds - there is one pod with fully developed seeds in dry
!              beans. this array includes daynum, year, month and day
!              of when this stage was reached.
!     first7 - used to set the value of aepa the first time phenolww is
!              called.
!     fps - flower primordium initiation growth stage. this array includes
!           daynum, year, month and day of when this stage was reached.
!     fullbs - full bloom growth stage in sorghum. this array includes
!            daynum, year, month and day of when this stage was reached.
!     gddtbg - used to accumulate gdd for seeds planted in dust after a
!              rainfall event has moved the soil moisture condition to
!              dry.  the seed can begin accumulating gdd's germination.
!     germgdd - an array holding 4 germination times in gdd at base 0°c
!               for the soil moisture levels
!     germs - simulated day that germination occurs
!     ggdd - a 6 element array that holds the germgdd values plus calculated values for
!           two intermediate soil moisture level values in elements 2 and 4.
!     gmethod - selects the method whereby gdd will be calculated.
!               a value of 1 corresponds to method 1 in phenologymms and
!               is used for crops such as winter wheat, winter barley and
!               proso millet. a value of 2 corresponds to method 2 in
!               phenologymms and is used for crops such as corn, sorghum
!               and sunflower.  a value of 3 is the way that weps/upgm
!               calculated ggd for the day.
!     gpds - growing point differentiation growth stage in sorghum. this
!            array includes daynum, year, month and day of when this stage
!            was reached.
!     growth_stress- flag setting which turns on water or temperature
!                   stress (or both)
!                   growth_stress = 0  ! no stress values applied
!                   growth_stress = 1  ! turn on water stress
!                   growth_stress = 2  ! turn on temperature stress
!                   growth_stress = 3  ! turn on both
!     halfbs - half bloom growth stage in sorghum. this array includes
!            daynum, year, month and day of when this stage was reached.
!     heads - heading growth stage for hay millet, proso millet, spring
!             barley, spring wheat, winter barley and winter wheat. this
!             array includes daynum, year, month and day of when this
!             stage was reached.
!     hrs - time to harvest ripe growth stage for corn, hay millet, proso
!           millet, sorghum, spring barley, spring wheat, sunflower,
!           winter barley and winter wheat. 80% of pods are at the mature
!           color in dry beans. this array includes daynum, year, month
!           and day of when this stage was reached.
!     ies - start of internode elongation growth stage for corn, hay
!           millet, proso millet, sorghum, spring barley, spring wheat,
!           winter barley, and winter wheat. for sunflower, this stage
!           occurs when the internode below the inflorescence elongates
!           0.5 to 2.0 cm above the nearest leaf on the stem. this array
!           includes daynum, year, month and day of when this stage was
!           reached.
!     ies2 - for sunflower, this is when the internode below the
!            inflorescence continues lengthening and lifts the head above
!            the surrounding leaves more than 2 cm. this array includes
!            daynum, year, month and day of when this stage was reached.
!     infls - the sunflower inflorescence becomes visible. this array
!             includes daynum, year, month and day of when this stage was
!             reached.
!     jan1 - a flag to test if january 1 has occurred.  if it has passed,
!            then the winter annual crop is assumed to have completed
!            vernalization.
!     joints - jointing growth stage for hay millet, proso millet,
!              sorghum, spring barley, spring wheat, winter barley and
!              winter wheat. this array includes daynum, year, month and
!              day of when this stage was reached.
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
!     mats - physiological maturity growth stage for corn, hay millet,
!            proso millet, sorghum, spring barley, spring wheat,
!            sunflower, winter barley and winter wheat. in dry beans,
!            one pod has changed color/striped. this array includes
!            daynum, year, month and day of when this stage was
!            reached.
!     maxht - this is the maximum canopy height of the crop.  it is an
!             input parameter and is read in from upgm_crop.dat.
!     mffls - the stage of mid to full flower in dry beans. this array
!             includes daynum, year, month and day of when this stage
!             was reached.
!     milks - the milk growth stage in corn. this array includes daynum,
!             year, month and day of when this stage was reached.
!     mpods - the stage when 50% of the pods are at the maximum length.
!             this array includes daynum, year, month and day of when
!             this stage was reached.
!     mseeds - the stage when 50% of the pods have fully developed seeds
!              in dry beans. this array includes daynum, year, month and
!              day of when this stage was reached.
!     opens - the sunflower inflorescence begins to open. this array
!             includes daynum, year, month and day of when this stage
!             was reached.
!     pchron - phyllochron value which is the number of gdd per leaf.
!     pd - planting day
!     pm - planting month
!     py - planting year.  currently, not the calendar year.
!     seedsw - soil water content at seed depth.  it is read in as
!              optimum, medium, dry or planted in dust and converted
!              to an integer.	 1 = optimum, 2 = medium, 3 = dry and
!              4 = planted in dust
!     silks - the silking growth stage in corn. this array includes
!             daynum, year, month and day of when this stage was reached.
!     soilwat - an array holding the swtype for each soil moisture
!               condition
!     srs - single ridge growth stage for hay millet, proso millet,
!           spring barley, spring wheat, winter barley and winter wheat.
!           this array includes daynum, year, month and day of when this
!           stage was reached.
!     tbase - lowest temperature below which no growth occurs (deg.c).
!     tis - start of tillering growth stage for corn, hay millet, proso
!           millet, sorghum, spring barley, spring wheat, winter barley
!           and winter wheat. this array includes daynum, year, month and
!           day of when this stage was reached.
!     toptlo - the lower temperature in the optimum range for plant
!              growth (deg.c).
!     toptup - the upper temperature in the optimum range for plant
!              growth (deg.c).
!     tsints - tassel initiation growth stage in corn. this array
!              includes daynum, year, month and day of when this stage
!              was reached.
!     tss - terminal spikelet growth stage for spring and winter wheat.
!           this array includes daynum, year, month and day of when this
!           stage was reached.
!     tupper - upper/maximum temperature for plant growth (deg.c).
!              no growth with temperatures above this point.
!     wfpslo - an array holding the low values for each soil moisture
!              condition.
!     wfpsup - an array holding the high values for each soil moisture
!              condition.
!     yelows - back of the sunflower head is a light yellow. this array
!              includes daynum, year, month and day of when this stage
!              was reached.
 
!     + + + new local variable definitions + + +
!     phenolflg - a flag that determines if the upgm/weps method of determining maturity
!                 will be used (phenolflg =0) or the phenologymms method will be used (phenolflg = 1).
 
!   + + + common block variables definitions + + +
!   ac0aht - height s-curve parameter
!   ac0alf - leaf partitioning parameter
!   ac0arp - rprd partitioning parameter
!   ac0bceff - biomass conversion efficiency
!   ac0bht - height s-curve parameter
!   ac0blf - leaf partitioning parameter
!   ac0bn1 - crop nitrogen uptake parm (n fraction at emergence)
!   ac0bn2 - crop nitrogen uptake parm (n fraction at 1/2 maturity)
!   ac0bn3 - crop nitrogen uptake parm (n fraction at maturity)
!   ac0bp1 - crop phosphorous uptake parm (n fraction at emergence)
!   ac0bp2 - crop phosphorous uptake parm (n fraction @ 1/2 maturity)
!   ac0bp3 - crop phosphorous uptake parm (n fraction at maturity)
!   ac0brp - rprd partitioning parameter
!   ac0ck -  canopy light extinction coefficient (0.0 < ac0ck < 1.0)
!   ac0clf - leaf partitioning parameter
!   ac0crp - rprd partitioning parameter
!   ac0dlf - leaf partitioning parameter
!   ac0diammax - crop maximum plant diameter (m)
!   ac0drp - rprd partitioning parameter
!   ac0fd1 - xy coordinate for 1st pt on frost damage curve
!   ac0fd2 - xy coordinate for 2nd pt on frost damage curve
!   ac0growdepth - depth of growing point at time of planting (m)
!   ac0hue - relative heat unit for emergence (fraction)
!   ac0idc - crop type:annual,perennial,etc
!   ac0nam - crop name
!   ac0rg - crop seeding location in relation to ridge: 0 = plant in
!           furrow; 1 = plant on ridge
!   ac0shoot - mass from root storage required for each regrowth shoot
!              (mg/shoot) seed shoots are smaller and adjusted for
!              available seed mass
!   ac0ssa - stem area to mass coefficient a, result is m^2 per plant
!   ac0ssb - stem area to mass coefficient b, argument is kg per plant
!   ac0sla - specific leaf area (cm^2/g)
!   ac0storeinit - db input, crop storage root mass initialzation
!                  (mg/plant)
!   ac0transf - currently not used. db input flag: 0=crop is planted
!               using stored biomass of seed or vegatative propagants;
!               1=crop is planted as a transplant with roots, stems and
!               leaves present.
!   acbaf - biomass adjustment factor
!   acbaflg - flag for biomass adjustment action: 0=normal crop growth;
!             1=find biomass adjustment factor for target yield; 2=use
!             given biomass adjustment factor.
!   accovfact - residue cover factor (m^2/kg)
!   acdayam - number of days since crop matured
!   acdayap - number of days of growth completed since crop planted
!   acdayspring - day of year in which a winter annual released stored
!                 growth
!   acddsthrsh - decomposition days required for first stem fall
!   acdkrate - array of decomposition rate parameters
!     acdkrate(1) - standing residue mass decomposition rate (d<1) (g/g/day)
!     acdkrate(2) - flat residue mass decomposition rate (d<1) (g/g/day)
!     acdkrate(3) - buried residue mass decomposition rate (d<1) (g/g/day)
!     acdkrate(4) - root residue mass decomposition rate (d<1) (g/g/day)
!     acdkrate(5) - stem residue number decline rate (d<1) (#/m^2/day)? (fall rate)
!   acdmaxshoot - maximum number of shoots possible from each plant
!   acdpop - crop seeding density (#/m^2)
!   acdstm - number of crop stems per unit area (#/m^2)
!   acehu0 - heat unit index leaf senescence starts
!   acfcancov - fraction of soil surface covered by crop canopy (m^2/m^2)
!   acffcv - crop biomass cover - flat  (m^2/m^2)
!   acfleaf2stor - fraction of assimilate partitioned to leaf that is
!                  diverted to root store
!   acfleafstem - crop leaf to stem mass ratio for shoots
!   acfliveleaf - fraction of standing plant leaf which is living
!                 (transpiring)
!   acfscv - crop biomass cover - standing  (m^2/m^2)
!   acfshoot - crop ratio of shoot diameter to length
!   acfstem2stor - fraction of assimilate partitioned to stem that is
!                  diverted to root store
!   acfstor2stor - fraction of assimilate partitioned to standing storage
!                  (reproductive) that is diverted to root store
!   acftcv - crop biomass cover - total  (m^2/m^2)
!   acgrainf - internally computed grain fraction of reproductive mass
!   acgrf - fraction of reproductive biomass that is grain (mg/mg)
!   achyfg - flag indicating the part of plant to apply the "grain
!            fraction", grf to when removing that plant part for yield:
!            0 = above ground storage (seeds, reproductive)
!            1 = growth stage factor (see growth.for) applied to above
!                ground storage (seeds, reproductive)
!            2 = all above ground biomass (forage)
!            3 = leaf mass (tobacco)
!            4 = stem mass (sugarcane)
!            5 = below ground storage mass (potatoes, peanuts)
!   acleafareatrend - direction in which leaf area is trending. saves
!                     trend even if leaf area is static for long periods.
!   acm - total crop mass (stand + flat+ root) (kg/m^2)
!   acmbgstem - crop stem mass below soil surface (kg/m^2)
!   acmbgstemz - crop stem mass below soil surface by layer (kg/m^2)
!   acmf - flat crop mass (flatstem + flatleaf + flatstore) (kg/m^2)
!          flag to crop distributes stem, leaf and storeabove elements
!          between standing and flat
!   acmflatleaf - crop flat leaf mass (kg/m^2)
!   acmflatstem - crop flat stem mass (kg/m^2)
!   acmflatstore - crop flat storage mass (kg/m^2)
!   acmrootfiber - crop root fibrous mass (kg/m^2)
!   acmrootfiberz - crop root fibrous mass by soil layer (kg/m^2)
!   acmrootstore - crop root storage mass (kg/m^2)
!   acmrootstorez - crop root storage mass by soil layer (kg/m^2
!   acmrt - total crop root mass (rootfiber + rootstore) (kg/m^2)
!   acmrtz - crop root mass by soil layer (kg/m^2)
!   acmshoot - mass of shoot growing from root storage biomass (kg/m^2)
!   acmst - standing crop mass (standstem + standleaf + standstore) (kg/m^2)
!   acmstandleaf - crop standing leaf mass (kg/m^2)
!   acmstandstem - crop standing stem mass (kg/m^2)
!   acmstandstore - crop standing storage mass (kg/m^2)
!   acmtotshoot - total mass of shoot growing from root storage biomass
!                 (kg/m^2) in the period from beginning to completion of
!                 emergence heat units
!   acrbc - crop residue burial class (it exists in crop so it can be
!           carried into residue):
!           1 - fragile-very small (soybeans) residue
!           2 - moderately tough-short (wheat) residue
!           3 - non fragile-med (corn) residue
!           4 - woody-large residue
!           5 - gravel-rock
!   acrcd - effective biomass silhouette area (sai+lai) (m^2/m^2)
!           (combination of leaf area and stem area indices)
!   acresevapa - coefficient a in relation ea/ep = exp(resevap *
!                (flat mass kg/m^2)**resevapb)
!   acresevapb - coefficient b in relation ea/ep = exp(resevap *
!                (flat mass kg/m^2)**resevapb)
!   acresid_int - residue intercept (kg/m^2)
!                 harvest_residue = acyld_coef(kg/kg) * yield + acresid_int
!                 (kg/m^2)
!   acrlai - crop leaf area index (m^2/m^2)
!   acrlaz - crop leaf area index by height (1/m)
!   acrsai - crop stem area index (m^2/m^2)
!   acrsaz - crop stem area index by height (1/m)
!   actchillucum - accumulated chilling units (days)
!   actdtm - days from planting to maturity for summer crops, or the days
!            from start of spring growth to maturity for winter and
!            perennial crops.
!   acthu_shoot_beg - heat unit total for beginning of shoot grow from
!                     root storage period
!   acthu_shoot_end - heat unit total for end of shoot grow from root
!                     storage period
!   acthucum - crop accumulated heat units
!   acthudf - heat units or days to maturity flag
!   acthum - accumulated heat units from planting to maturity, or from
!            start of growth to maturity for perennial crops
!   actmin - minimum temperature for plant growth (deg c)
!   actopt - optimal temperature for plant growth (deg c)
!   actrthucum - accumulated root growth heat units (degree-days)
!   actverndel - thermal delay coefficient pre-vernalization
!   actwarmdays - number of consecutive days that the temperature has
!                 been above the minimum growth temperature
!   acxrow - crop row spacing (m)
!   acxstm - crop stem diameter (m)
!   acxstmrep - a representative diameter so that
!               acdstm*acxstmrep*aczht=acrsai
!   acyld_coef - yield coefficient (kg/kg)
!                harvest_residue = acyld_coef(kg/kg) * yield + acresid_int
!                (kg/m^2)
!   acyraf - yield to biomass ratio adjustment factor
!   aczgrowpt - depth in the soil of the growing point (m)
!   aczht - crop height (m)
!   aczloc_regrow - location of regrowth point (+ on stem, 0 or negative
!                   from crown at or below surface) (m)
!   aczmrt - maximum crop root depth (m)
!   aczmxc - maximum potential plant height (m)
!   aczrtd - crop root depth (m)
!   aczshoot - length of actively growing shoot from root biomass (m)
!   ad0ck - residue light extinction coeffficient (fraction)
!   ad0nam - residue name
!   ad0sla - residue specific leaf area
!   addstm - number of residue stems per unit area (#/m^2)
!   adgrainf - internally computed grain fraction of reproductive mass
!   adhyfg - flag indicating the part of plant to apply the "grain
!            fraction", grf, to when removing that plant part for yield:
!            0 - grf applied to above ground storage (seeds, reproductive)
!            1 - grf times growth stage factor (see growth.for) applied
!                to above ground storage (seeds, reproductive)
!            2 - grf applied to all aboveground biomass (forage)
!            3 - grf applied to leaf mass (tobacco)
!            4 - grf applied to stem mass (sugarcane)
!            5 - grf applied to below ground storage mass (potatoes,
!                peanuts)
!   admbgleafz - buried leaf mass by layer (kg/m^2)
!   admbgrootfiberz - buried fibrous root mass by layer (kg/m^2)
!   admbgrootstorez - buried storage root mass by layer (kg/m^2)
!   admbgstemz - buried stem mass by layer (kg/m^2)
!   admbgstorez - buried (from above ground) storage mass by layer (kg/m^2)
!   admbgz - buried residue mass by soil layer (kg/m^2)
!   admflatleaf - flat leaf mass (kg/m^2)
!   admflatrootfiber - flat fibrous root mass (kg/m^2)
!   admflatrootstore - flat storage root mass (kg/m^2)
!   admflatstem - flat stem mass (kg/m^2)
!   admflatstore - flat storage mass (kg/m^2)
!   admstandleaf - standing leaf mass (kg/m^2)
!   admstandstem - standing stem mass (kg/m^2)
!   admstandstore - standing storage mass (kg/m^2)
!   adrbc - residue burial class
!   adresevapa - coefficient a in relation
!                ea/ep = exp(resevap * (flat mass kg/m^2)**resevapb)
!   adresevapb - coefficient b in relation
!                ea/ep = exp(resevap * (flat mass kg/m^2)**resevapb)
!   adxstm - residue stem diameter (m)
!   adxstmrep - a representative diameter so that
!               addstm*adxstmrep*adzht=adrsai
!   adzht - residue height (m)
!   ahfwsf - crop growth water stress factor (unitless)
!   ahtsmn - minimum daily soil temperature (deg c)
!   ahtsmx - maximum daily soil temperature (deg c)
!   ahzeta - actual evapotranspiration (mm/day)
!   ahzpta - actual plant transpiration (mm/day)
!   ahzptp - potential plant transpiration (mm/day)
!   am0cdb - flag to print crop variables before and after call to crop
!   am0cgf - flag to run crop growth if between plant and harvest
!   am0cif - flag to run initialization of crop submodel at each crop
!            planting
!   am0cropupfl - flag to determine that the crop state has been changed
!               external to crop and that the crop update process must
!               run to synchronize dependent variable values with state
!               values
!   as0ph - soil layer ph
!   asargo - ridge orientation (degrees)
!   asdblk - soil layer bulk density for each subregion (mg/m^3)
!   asfcce - soil layer calcium carbonate equivalent (dec %)
!   asfcec - soil layer cation exchange capacity (cmol/kg) (meq/100g)
!   asfcla - soil layer clay content (mg/mg)
!   asfom - soil layer organic matter content (mg/mg)
!   asfsmb - sum of bases (cmol/kg)
!   asftan - soil layer available nitrogen (kg/ha)
!   asftap - soil layer available phosphorus (kg/ha
!   asmno3 - amount of n03 nitrogen applied as fertilizer (kg/ha)
!   asxrgs - ridge spacing (mm)
!   aszlyd - depth to bottom of each soil layer for each subregion (mm)
!   aszlyt - soil layer thicknesses for each subregion (mm)
!   aszrgh - ridge height (mm)
!   atdstm - number of crop stems per unit area (#/m^2). it is computed
!            by taking the tillering factor times the plant population
!            density.
!   atgrainf - internally computed grain fraction of reproductive mass
!   atmbgleafz - crop buried leaf mass by layer (kg/m^2)
!   atmbgrootfiberz - crop root fibrous mass by layer (kg/m^2)
!   atmbgrootstorez - crop root storage mass by layer (kg/m^2).
!                     (tubers (potatoes, carrots), extended leaf (onion),
!                     seeds (peanuts))
!   atmbgstemz - crop buried stem mass by layer (kg/m^2)
!   atmbgstorez - crop buried storage mass by layer (kg/m^2)
!   atmflatleaf - crop flat leaf mass (kg/m^2)
!   atmflatrootfiber - crop flat root fibrous mass (kg/m^2)
!   atmflatrootstore - crop flat root storage mass (kg/m^2).
!                      (tubers (potatoes, carrots), extended leaf (onion), seeds
!                      (peanuts))
!   atmflatstem - crop flat stem mass (kg/m^2)
!   atmflatstore - crop flat storage mass (kg/m^2)
!   atmstandleaf - crop standing leaf mass (kg/m^2)
!   atmstandstem - crop standing stem mass (kg/m^2)
!   atmstandstore - crop standing storage mass (kg/m^2). (head with seed, or
!                   vegetative head (cabbage, pineapple))
!   atxstmrep - a representative diameter so that
!               acdstm*acxstmrep*aczht=acrsai
!   atzht - crop height (m)
!   aweirr - daily global radiation (mj/m^2)
!   awtdmn - minimum daily air temperature (deg c)
!   awtdmx - maximum daily air temperature (deg c)
!   awzdpt - daily precipitation (mm)
!   covfact - coeficient for converting mass to cover (m^2/kg)
!   cumddf - cummlative decomp days for surface res. by pool (days)
!   cumddg - cumm. decomp days below ground res. by pool and layer (days)
!   cumdds - cumulative decomp days for standing res. by pool (days)
!   ddsthrsh - threshhold number of decomp. days before stems begin to
!              fall
!   dkrate - decomposition rate for each age pool and location (d < 1)
!            (g/g/day)
!   growth_stress - flag setting which turns on water or temperature
!                 stress (or both)
!                 growth_stress = 0  ! no stress values applied
!                 growth_stress = 1  ! turn on water stress
!                 growth_stress = 2  ! turn on temperature stress
!                 growth_stress = 3  ! turn on both
!    debe because it is now being read in it is commented out in command.inc
!   nslay - number of soil layers being used for each subregion
!   prevbgstemz - crop stem mass below soil surface by layer (kg/m^2)
!   prevchillucum - accumulated chilling units (days)
!   prevdayap - number of days of growth completed since crop planted
!   prevflatleaf - crop flat leaf mass (kg/m^2)
!   prevflatstem - crop flat stem mass (kg/m^2)
!   prevflatstore - crop flat storage mass (kg/m^2)
!   prevgrainf - internally computed grain fraction of reproductive mass
!   prevht - crop height (m)
!   prevhucum - crop accumulated heat units
!   prevliveleaf - fraction of standing plant leaf which is
!                  living (transpiring)
!   prevmshoot - mass of shoot growing from root storage biomass (kg/m^2)
!   prevmtotshoot - total mass of shoot growing from root storage biomass
!                   (kg/m^2). in the period from beginning to completion
!                   of emegence heat units
!   prevrootfiberz - crop root fibrous mass by soil layer (kg/m^2)
!   prevrootstorez - crop root storage mass by soil layer (kg/m^2)
!                    (tubers (potatoes, carrots), extended leaf (onion),
!                    seeds (peanuts))
!   prevrtd - crop root depth (m)
!   prevrthucum - crop accumulated heat units with no
!                 vernalization/photoperiod delay
!   prevstandleaf - crop standing leaf mass (kg/m^2)
!   prevstandstem - crop standing stem mass (kg/m^2)
!   prevstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage,
!                    pineapple))
!   prevstm - number of crop stems per unit area (#/m^2). it is computed
!             by taking the tillering factor times the plant population
!             density.
!   prevzshoot - length of actively growing shoot from root biomass (m)

! call timer(timcrop,timstart)  ! contains required variables for biodrag()
!
! note that crop "may" really require (admbgz + admrtz) in place of admbgz
! because crop wants to know the amount of biomass in each soil layer
! for nutrient cycling.  however, since the nutrient cycling is supposed
! to be disabled, we won't worry about it right now.  lew - 04/23/99
!
! check for a valid growing crop
if ((ac0shoot(sr)<=0.0).or.(acdpop(sr)<=0.0)) bio%growth%am0cgf = .false.
!
!     only continue if crop is growing
if (bio%growth%am0cgf) then
 
  if (ctrl%sim%am0cdb==1) call cdbug(sr,sppdat%nslay,ctrl,clidat,sppdat,bio )
!  print*, 'in callcrop just before call to crop seedsw = ', seedsw
  call crop(ctrl,clidat,bio, sppdat%nslay,sppdat%aszlyt(1,sr),sppdat%aszlyd(1,sr),sppdat%asdblk(1),asfcce(1,sr),      &
          & asfom(1,sr),asfcec(1,sr),asfsmb(1,sr),sppdat%asfcla(1,sr),as0ph(1,sr),     &
          & asftan(1,sr),asftap(1,sr),asmno3(sr),ac0bn1(sr),ac0bn2(sr),         &
          & ac0bn3(sr),ac0bp1(sr),ac0bp2(sr),ac0bp3(sr),ac0ck(sr),acgrf(sr),    &
          & acehu0(sr),aczmxc(sr),ac0nam(sr),ac0idc(sr),acxrow(sr),actdtm(sr),  &
          & aczmrt(sr),actmin(sr),actopt(sr),ac0fd1(1,sr),ac0fd2(1,sr),         &
          & ac0fd1(2,sr),ac0fd2(2,sr),ac0bceff(sr),admbgz(1,1,sr),ac0alf(sr),   &
          & ac0blf(sr),ac0clf(sr),ac0dlf(sr),ac0arp(sr),ac0brp(sr),ac0crp(sr),  &
          & ac0drp(sr),ac0aht(sr),ac0bht(sr),ac0sla(sr),ac0hue(sr),             &
          & actverndel(sr),sppdat%ahtsmn,sppdat%ahtsmn&
          & ,ahzpta,ahzeta,ahzptp,ahfwsf(sr),bio%growth%am0cif,bio%growth%am0cgf,acthudf(sr),         &
          & acbaflg(sr),acbaf(sr),acyraf(sr),achyfg(sr),acthum(sr),acdpop(sr),  &
          & acdmaxshoot(sr),ac0transf(sr),ac0storeinit(sr),acfshoot(sr),        &
          & ac0growdepth(sr),acfleafstem(sr),ac0shoot(sr),ac0diammax(sr),       &
          & ac0ssa(sr),ac0ssb(sr),acfleaf2stor(sr),acfstem2stor(sr),            &
          & acfstor2stor(sr),acyld_coef(sr),acresid_int(sr),acxstm(sr),         &
          & acmstandstem(sr),acmstandleaf(sr),acmstandstore(sr),acmflatstem(sr),&
          & acmflatleaf(sr),acmflatstore(sr),acmshoot(sr),acmtotshoot(sr),      &
          & acmbgstemz(1,sr),acmrootstorez(1,sr),acmrootfiberz(1,sr),aczht(sr), &
          & aczshoot(sr),acdstm(sr),aczrtd(sr),acdayap(sr),acdayam(sr),         &
          & acthucum(sr),actrthucum(sr),acgrainf(sr),aczgrowpt(sr),             &
          & acfliveleaf(sr),acleafareatrend(sr),actwarmdays(sr),actchillucum(sr)&
          & ,acthu_shoot_beg(sr),acthu_shoot_end(sr),acxstmrep(sr),             &
          & prevstandstem(sr),prevstandleaf(sr),prevstandstore(sr),             &
          & prevflatstem(sr),prevflatleaf(sr),prevflatstore(sr),prevmshoot(sr), &
          & prevmtotshoot(sr),prevbgstemz(1,sr),prevrootstorez(1,sr),           &
          & prevrootfiberz(1,sr),prevht(sr),prevzshoot(sr),prevstm(sr),         &
          & prevrtd(sr),prevdayap(sr),prevhucum(sr),prevrthucum(sr),            &
          & prevgrainf(sr),prevchillucum(sr),prevliveleaf(sr),daysim,           &
          & acdayspring(sr),aczloc_regrow(sr),bio%mass%standstem,bio%mass%standleaf,&
          & bio%mass%standstore,bio%mass%flatstem,bio%mass%flatleaf,bio%mass%flatstore, &
          & bio%mass%stemz(1:1),bio%geometry%zht,bio%geometry%dstm,bio%geometry%xstmrep,bio%geometry%grainf,   &
          & aepa,aifs,antes,antss,blstrs,boots,browns,callgdd,canht,canopyflg,  &
          & cliname,cots,cropname,dayhtinc,dents,doughs,drs,dummy1,dummy2,ears, &
          & ecanht,egdd,emrgflg,ems,endlgs,epods,ergdd,eseeds,first7,fps,fullbs,&
          & gddtbg,germgdd,germs,ggdd,gmethod,gpds,growth_stress,halfbs,heads,  &
          & hrs,icli,ies,ies2,infls,joints,lf12s,lf1s,lf2s,lf3s,lf4s,lf8s,mats, &
          & maxht,mffls,milks,mpods,mseeds,opens,pchron,pd,phenolflg,pm,py,     &
          & seedsw,silks,soilwat,srs,tbase,tis,toptlo,toptup,tsints,tss,tupper, &
          & wfpslo,wfpsup,yelows,co2x,co2y,co2atmos)
 
!debe added growth_stress to pass to crop because growth_stress is now being
!read in. debe added toptlo, topup and tbase, canopyflg.
!debe added dayhtinc to be able to pass the daily increase in height to growth
! for the ht_dia_sai subroutine in place of the weps/upgm variable dht when
! canopyflg = 1.
!debe added growth stage array variables and phenolflg to be passed to crop from main.
 
  if (ctrl%sim%am0cdb==1) call cdbug(sr,sppdat%nslay,ctrl, clidat,sppdat,bio)
end if
 
      ! check for abandoned stems in crop regrowth
if ((bio%mass%standstem+bio%mass%standleaf+bio%mass%standstore+bio%mass%flatstem        &
  & +bio%mass%flatleaf+bio%mass%flatstore)>0.0) then
          ! zero out residue pools which crop is not transferring
  bio%mass%flatrootstore = 0.0
  bio%mass%flatrootfiber = 0.0
  do lay = 1,sppdat%nslay
     bio%mass%leafz(lay) = 0.0
     bio%mass%storez(lay) = 0.0
     bio%mass%rootstorez(lay) = 0.0
     bio%mass%rootfiberz(lay) = 0.0
  end do
  call trans(bio%mass%standstem,bio%mass%standleaf,bio%mass%standstore,bio%mass%flatstem&
           & ,bio%mass%flatleaf,bio%mass%flatstore,bio%mass%flatrootstore,            &
           & bio%mass%flatrootfiber,bio%mass%stemz,bio%mass%leafz,            &
           & bio%mass%storez,bio%mass%rootstorez,bio%mass%rootfiberz,     &
           & bio%geometry%zht,bio%geometry%dstm,bio%geometry%xstmrep,bio%geometry%grainf,admstandstem(1,sr),&
           & admstandleaf(1,sr),admstandstore(1,sr),admflatstem(1,sr),          &
           & admflatleaf(1,sr),admflatstore(1,sr),admflatrootstore(1,sr),       &
           & admflatrootfiber(1,sr),admbgstemz(1,1,sr),admbgleafz(1,1,sr),      &
           & admbgstorez(1,1,sr),admbgrootstorez(1,1,sr),admbgrootfiberz(1,1,sr)&
           & ,adzht(1,sr),addstm(1,sr),adxstmrep(1,sr),adgrainf(1,sr),ac0nam(sr)&
           & ,acxstm(sr),acrbc(sr),ac0sla(sr),ac0ck(sr),acdkrate(1,sr),         &
           & accovfact(sr),acddsthrsh(sr),achyfg(sr),acresevapa(sr),            &
           & acresevapb(sr),ad0nam(1,sr),adxstm(1,sr),adrbc(1,sr),ad0sla(1,sr), &
           & ad0ck(1,sr),dkrate(1,1,sr),covfact(1,sr),ddsthrsh(1,sr),           &
           & adhyfg(1,sr),adresevapa(1,sr),adresevapb(1,sr),cumdds(1,sr),       &
           & cumddf(1,sr),cumddg(1,1,sr),sppdat%nslay)
end if
 
! update all derived globals for crop global variables
call cropupdate(acmstandstem(sr),acmstandleaf(sr),acmstandstore(sr),            &
              & acmflatstem(sr),acmflatleaf(sr),acmflatstore(sr),acmshoot(sr),  &
              & acmbgstemz(1,sr),acmrootstorez(1,sr),acmrootfiberz(1,sr),       &
              & aczht(sr),acdstm(sr),aczrtd(sr),acthucum(sr),aczgrowpt(sr),     &
              & acmbgstem(sr),acmrootstore(sr),acmrootfiber(sr),acxstmrep(sr),  &
              & acm(sr),acmst(sr),acmf(sr),acmrt(sr),acmrtz(1,sr),acrcd(sr),    &
              & sppdat%aszrgh(sr),sppdat%asxrgs(sr),sppdat%asargo(sr),acrsai(sr),acrlai(sr),         &
              & acrsaz(1,sr),acrlaz(1,sr),acffcv(sr),acfscv(sr),acftcv(sr),     &
              & acfcancov(sr),ac0rg(sr),acxrow(sr),sppdat%nslay,ac0ssa(sr),        &
              & ac0ssb(sr),ac0sla(sr),accovfact(sr),ac0ck(sr),acxstm(sr),       &
              & acdpop(sr))
 
!
! call timer(timcrop,timstop)
!
end subroutine callcrop

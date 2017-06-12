subroutine crop(ctrl,clidat,bnslay,bszlyt,bszlyd,bsdblk,bsfcce,bsfom,bsfcec,bsfsmb,bsfcla,  &
              & bs0ph,bsftan,bsftap,bsmno3,bc0bn1,bc0bn2,bc0bn3,bc0bp1,bc0bp2,  &
              & bc0bp3,bc0ck,bcgrf,bcehu0,bczmxc,bc0nam,bc0idc,bcxrow,bctdtm,   &
              & bczmrt,bctmin,bctopt,bc0fd1,bc0fd2,cc0fd1,cc0fd2,bc0bceff,bdmb, &
              & bc0alf,bc0blf,bc0clf,bc0dlf,bc0arp,bc0brp,bc0crp,bc0drp,bc0aht, &
              & bc0bht,bc0sla,bc0hue,bctverndel,    &
              & bhtsmx,bhtsmn,bhzpta,bhzeta,bhzptp,bhfwsf,bm0cif,bm0cgf,bcthudf,&
              & bcbaflg,bcbaf,bcyraf,bchyfg,bcthum,bcdpop,bcdmaxshoot,bc0transf,&
              & bc0storeinit,bcfshoot,bc0growdepth,bcfleafstem,bc0shoot,        &
              & bc0diammax,bc0ssa,bc0ssb,bcfleaf2stor,bcfstem2stor,bcfstor2stor,&
              & bcyld_coef,bcresid_int,bcxstm,bcmstandstem,bcmstandleaf,        &
              & bcmstandstore,bcmflatstem,bcmflatleaf,bcmflatstore,bcmshoot,    &
              & bcmtotshoot,bcmbgstemz,bcmrootstorez,bcmrootfiberz,bczht,       &
              & bczshoot,bcdstm,bczrtd,bcdayap,bcdayam,bcthucum,bctrthucum,     &
              & bcgrainf,bczgrowpt,bcfliveleaf,bcleafareatrend,bctwarmdays,     &
              & bctchillucum,bcthu_shoot_beg,bcthu_shoot_end,bcxstmrep,         &
              & bprevstandstem,bprevstandleaf,bprevstandstore,bprevflatstem,    &
              & bprevflatleaf,bprevflatstore,bprevmshoot,bprevmtotshoot,        &
              & bprevbgstemz,bprevrootstorez,bprevrootfiberz,bprevht,           &
              & bprevzshoot,bprevstm,bprevrtd,bprevdayap,bprevhucum,            &
              & bprevrthucum,bprevgrainf,bprevchillucum,bprevliveleaf,daysim,   &
              & bcdayspring,bczloc_regrow,btmstandstem,btmstandleaf,            &
              & btmstandstore,btmflatstem,btmflatleaf,btmflatstore,btmbgstemz,  &
              & btzht,btdstm,btxstmrep,btgrainf,aepa,aifs,antes,antss,blstrs,   &
              & boots,browns,callgdd,canht,canopyflg,cliname,cots,cropname,     &
              & dayhtinc,dents,doughs,drs,dummy1,dummy2,ears,ecanht,egdd,       &
              & emrgflg,ems,endlgs,epods,ergdd,eseeds,first7,fps,fullbs,gddtbg, &
              & germgdd,germs,ggdd,gmethod,gpds,growth_stress,halfbs,heads,hrs, &
              & icli,ies,ies2,infls,joints,lf12s,lf1s,lf2s,lf3s,lf4s,lf8s,mats, &
              & maxht,mffls,milks,mpods,mseeds,opens,pchron,pd,phenolflg,pm,py, &
              & seedsw,silks,soilwat,srs,tbase,tis,toptlo,toptup,tsints,tss,    &
              & tupper,wfpslo,wfpsup,yelows,co2x,co2y,co2atmos)
    use upgm_simdata, only : upgm_ctrls, controls
!
!debe dummy1 is not currently used here or passed on but left it here in
! case it is used in the future. ! verns is also not currently used.
!debe added growth_stress because it is now being read in from a file.
!debe added variables for dry beans in call to phenol.
!debe added temperature variables toptlo, topup, tbase.
!debe added canopyflg.
!debe added ecanht so that it can be read in instead of set in the code for each crop.
!debe  - growth stage variables are now declared and initialized in main via cropinit.
! phenolflg is read in from upgm_crop.dat. these are now passed into crop from main via callcrop.
!
! ***** emergence *****
!
!debe added the following emergence arguments so that they can be passed to
! shoot_grow. these include seedsw, cropname, soilwat, wfpslo, wfpsup,
! germgdd, ergdd, gddday, gddtbg.  ac0nam is passed into cropname in main to
! be passed to emerge.
!debe added the emergence flag (emrgflg)to determine whether to call the
! emerge subroutine or not.
!debe added icli to pass to emerge to enable printing the weather file name
! in emerge.out. also, added pd,pm,py to print planting date in emerge.out.
! later changed to using cliname to print the weather file name.  it is read
! in from the header in the climate file.
!debe changed seedsw from integer to real to allow moving a half soil moisture level.
! later changed it back because array subscripts must be integers or constants.
!debe added two new arrays of 6 elements to be used in emerge.for to
! enable adding values for germgdd and ergdd for two soil moisture
! levels that are half steps between dry and medium and medium and optimum.
!debe added tempsw to be passed to cinit to be initialized and then used in emerge.
!
! ***** phenology *****
!
!debe added aepa and first7 because they are initialized in cropinit and
! need to be passed to crop. other phenology variables include: dummy2,
! pchron.
!
! ***** gddcalc *****
!
!debe added the variable for the maximum upper temperature (tupper) value
!and tbase, toptlo, topup values.
! read in from upgm_crop.dat in main. it will be passed from crop to
! gddcalc.
!debe added callgdd which is initialized in cropinit and used in crop.
!debe added gmethod passed into crop which is read in from upgm_crop.dat
! in main.
!
! ***** canopyht *****
!
!debe added maxht, read in from upgm_crop in main and passed to canopyht.
! add canht so the final canopy height can be passed back to main and
! printed in phenol.out.
!debe added canopyflg to determine which method of calculating plant height
! will be used. if 0, use weps/upgm method; if 1, use phenologymms method.
!debe added dayhtinc to be able to pass the daily increase in height to growth
! for the ht_dia_sai subroutine in place of the weps variable dht when
! canopyflg = 1.
!
! ***** co2 *****
!debe added co2x, co2y for the co2 arrays and co2atmos for the atmospheric co2 level.
! Also, added co2eff which will be the effect of the read-in atmospheric co2 
! level on the plant, i.e., the adjustment factor. Nedded to have it initialized before calculating 
! this value in Growth.f90
!
    use constants, only : mgtokg, civilrise
    use climate, only : climate_data

implicit none
!
include 'file.fi'
include 'p1werm.inc'
include 'm1flag.inc'
include 'cenvr.inc'
include 'cparm.inc'
include 'chumus.inc'
include 'cfert.inc'
!
! PARAMETER definitions
!
    type(controls) :: ctrl
    type(climate_data) :: clidat
real,parameter :: chilluv = 50.0,shoot_delay = 7.0,verndelmax = 0.04,           &
                & dev_floor = 0.01,spring_trig = 0.29
!
! Dummy arguments
!
real :: aepa,bc0aht,bc0alf,bc0arp,bc0bceff,bc0bht,bc0blf,bc0bn1,bc0bn2,bc0bn3,  &
      & bc0bp1,bc0bp2,bc0bp3,bc0brp,bc0ck,bc0clf,bc0crp,bc0diammax,bc0dlf,      &
      & bc0drp,bc0fd1,bc0fd2,bc0growdepth,bc0hue,bc0shoot,bc0sla,bc0ssa,bc0ssb, &
      & bc0storeinit,bcbaf,bcdmaxshoot,bcdpop,bcdstm,bcehu0,bcfleaf2stor,       &
      & bcfleafstem,bcfliveleaf,bcfshoot,bcfstem2stor,bcfstor2stor,bcgrainf,    &
      & bcgrf,bcleafareatrend,bcmflatleaf,bcmflatstem,bcmflatstore,bcmshoot,    &
      & bcmstandleaf,bcmstandstem,bcmstandstore,bcmtotshoot,bcresid_int,        &
      & bctchillucum,bcthucum,bcthum,bcthu_shoot_beg,bcthu_shoot_end,bctmin,    &
      & bctopt,bctrthucum,bctverndel,bcxrow,bcxstm,bcxstmrep,bcyld_coef,bcyraf, &
      & bczgrowpt,bczht,bczloc_regrow,bczmrt,bczmxc,bczrtd,bczshoot,bhfwsf,     &
      & bhzeta,bhzpta,bhzptp,bprevchillucum,bprevflatleaf,bprevflatstem
integer :: bc0idc,bc0transf,bcbaflg,bcdayam,bcdayap,bcdayspring,bchyfg,bctdtm,  &
         & bcthudf,bctwarmdays,bnslay,bprevdayap,canopyflg,daysim,emrgflg,      &
         & first7,gmethod,growth_stress,icli,pd,phenolflg,pm,py,seedsw
character(80) :: bc0nam,cropname
logical :: bm0cgf,bm0cif,callgdd
real :: bprevflatstore,bprevgrainf,bprevht,bprevhucum,bprevliveleaf,bprevmshoot,&
      & bprevmtotshoot,bprevrtd,bprevrthucum,bprevstandleaf,bprevstandstem,     &
      & bprevstandstore,bprevstm,bprevzshoot,bsmno3,btdstm,btgrainf,btmflatleaf,&
      & btmflatstem,btmflatstore,btmstandleaf,btmstandstem,btmstandstore,       &
      & btxstmrep,btzht,canht,cc0fd1,cc0fd2,        &
      & dayhtinc,ecanht,gddtbg,maxht,pchron,tbase,toptlo,toptup,tupper,         &
      & ln,co2atmos,co2eff 
character(80) :: cliname
integer,dimension(4) :: aifs,antes,antss,blstrs,boots,browns,cots,dents,doughs, &
                      & drs,ears,ems,endlgs,epods,eseeds,fps,fullbs,germs,gpds, &
                      & halfbs,heads,hrs,ies,ies2,infls,joints,lf12s,lf1s,lf2s, &
                      & lf3s,lf4s,lf8s,mats,mffls,milks,mpods,mseeds,opens,     &
                      & silks,srs,tis,tsints,tss,yelows
real,dimension(*) :: bcmbgstemz,bcmrootfiberz,bcmrootstorez,bdmb,bhtsmn,bhtsmx, &
                   & bprevbgstemz,bprevrootfiberz,bprevrootstorez,bs0ph,bsdblk, &
                   & bsfcce,bsfcec,bsfcla,bsfom,bsfsmb,bsftan,bsftap,bszlyd,    &
                   & bszlyt,btmbgstemz
character(5),dimension(30) :: dummy1
real,dimension(30) :: dummy2
real,dimension(6) :: egdd,ggdd
real,dimension(4) :: ergdd,germgdd,wfpslo,wfpsup
character(80),dimension(4) :: soilwat
real,dimension(400,2) :: lnarray
real,dimension(100,2),save :: lnpout
real,dimension(10) :: co2x,co2y
!
! Local variables
!
real,save :: canhty,elong,gdda,gddday,gdde,gdds,gddv,hu_delay,photo_delay,      & 
           & pot_leaf_mass,pot_stems,root_store_rel,todayln,trend,vern_delay,   &
           & yestln
character(80) :: cname
integer,save :: daa,dae,dap,dav,daynum,dd,lay,lncntr,mm,pdate,rowcntr,tempsw,   &
              & verns,yr,yy
integer :: dayear
real :: daylen,huc1
integer,dimension(20),save :: ddae,ddap,ddav
real,dimension(20),save :: dgdde,dgdds,dgddv
logical,save :: endphenol,jan1
real,dimension(15,5),save :: gddwsf

!
!debe added canhty so it can be initialized in cinit
!
!print*,'just after declarations in crop, seedsw  ', seedsw
!print*,'just after declarations in crop, silks(1) = ', silks(1)
!
 
! ***** upgm/weps variables *****
!
!debe added growth_stress because it is now being read in from a file.
!
!     + + + argument declarations + + +
!
! ***** newly added variables *****
! note: some variables are used in more than one subroutine
!
! ******** seedling emergence ********
!debe added variables that need to be passed to emerge after initialization.
 
!***** phenology *****
!
! phenol and leaf number variables
! debe added gddwsf array to be passed to cinit for initialization.
!note: leaf number variables are not used yet because determining growth
! stages by the number of leaves is not currently implemented.
!debe changed dimensions of dummy 1 and 2 arrays for stressed and
! non-stressed values
!
!***** phenology maturity *****
! debe added phenolflg to determine whether the upgm/weps method of determining maturity will
! be used or whether the phenologymms method will be used. if phenolflg = 0, the upgm/weps way will
! be used and if phenolflg = 1, the phenologymms way will be used. later this variable will be read in in main and initialized
! and passed to crop. for testing, phenolflg will be a local variable only.
 
! ***** gdd calculation *****
!
!debe added callgdd as a flag to either call the new gddcalc subroutine
! or use the code within crop.
! gcm 013009 added gmethod and three methods of calculating gddday
!
! ***** vernalization *****
!
!debe added jan1 variable for vernalization to test if
! jan1 has been passed for winter crops
!
!***** other variables *****
!
!
! ***** canopy height *****
!
!debe added canopyflg, dayhtinc, ecanht.
!
!***** CO2 variables *****
! debe added co2atmos, co2eff, co2x, co2y
!
 
! + + + local variables + + +
!
!debe added verns variable to allow for accumulation of gdd after
! vernalization. this will be used to trigger tillering in crops that
! tiller. this will be done in the appropriate phenol_cropname.
!debe 020309 added local variables tavgtemp, tmaxtemp, tmintemp to
!  calculate gdd by method 2 within crop.
! the rest of the local variables are upgm/weps variables.
!
!dayear, daylen and huc1 are functions
 
!debe added to save variables for next day's use.
!
!
!     the following subroutine arguments are not used: bc0transf, bhtsmx,
!     bhzeta, bhzpta, bhzptp, bm0cgf, bprevmtotshoot, bwzdpt   jcaii  8/08/08
!
!     + + + purpose + + +
!     this is the main program for implementing the crop growth
!     calculations in the various subroutines. for any questions refer
!     to amare retta at the usda wind erosion research laboratory,
!     university, manhattan ks 66506.
 
!     + + + keywords + + +
!     wind erosion crop model
 
!     the following subroutine arguments are not used: bc0transf, bhtsmx,
!     bhzeta, bhzpta, bhzptp, bm0cgf, bprevmtotshoot, bwzdpt   jcaii  8/08/08
 
!     + + + argument definitions + + +
!     bc0aht (ac0aht)- height s-curve parameter
!     bc0alf (ac0alf)- leaf partitioning parameter
!     bc0arp - rprd partitioning parameter
!     bc0bceff - biomass conversion efficiency (kg/ha/mj). currently
!                not used.
!     bc0bht - height s-curve parameter
!     bc0blf - leaf partitioning parameter.
!     bc0bn1 - normal fraction of n in crop biomass at emergence.
!              currently not used.
!     bc0bn2 - normal fraction of n in crop biomass at midseason
!     bc0bn3 - normal fraction of n in crop biomass at maturity
!     bc0bp1 - normal fraction of p in crop biomass at emergence
!     bc0bp2 - normal fraction of p in crop biomass at midseason
!     bc0bp3 - normal fraction of p in crop biomass at maturity
!     bc0brp - rprd partitioning parameter
!     bc0ck  - light extinction coefficient (fraction)
!     bc0clf - leaf partitioning parameter
!     bc0crp - rprd partitioning parameter
!     bc0diammax - crop maximum plant diameter (m)
!     bc0dlf - leaf partitioning parameter
!     bc0drp - rprd partitioning parameter
!     bc0fd1 - minimum temperature below zero (c)
!     bc0fd2 - minimum temperature below zero (c)
!     bc0growdepth - depth of growing point at time of planting (m)
!     bc0hue - relative heat unit for emergence (fraction)
!     bc0idc - crop type:annual,perennial,etc
!     bc0nam - crop name
!     bc0shoot - mass from root storage required for each shoot (mg/shoot)
!     bc0sla - specific leaf area (cm^2/g)
!     bc0ssa - stem area to mass coefficient a, result is m^2 per plant
!     bc0ssb - stem area to mass coefficient b, argument is kg per plant
!     bc0storeinit - db input, crop storage root mass initialzation
!                    (mg/plant)
!     bc0transf - currently not used. db input flag:
!                 0 = crop is planted using stored biomass of seed or
!                     vegatative propagants
!                 1 = crop is planted as a transplant with roots, stems
!                     and leaves present
!     bcbaf (acbaf) - biomass adjustment factor
!     bcbaflg (acbaflg) - flag for biomass adjustment action
!         0     o normal crop growth
!         1     o find biomass adjustment factor for target yield
!         2     o use given biomass adjustment factor
!     bcdayam - number of days since crop matured
!     bcdayap - number of days of growth completed since crop planted
!     bcdayspring - day of year in which a winter annual releases stored
!                   growth
!     bcdmaxshoot - maximum number of shoots possible from each plant
!     bcdpop - crop seeding density (#/m^2)
!     bcdstm - number of crop stems per unit area (#/m^2)
!     bcehu0 - relative gdd at start of senescence
!     bcfleaf2stor - fraction of assimilate partitioned to leaf that is
!                    diverted to root store
!     bcfleafstem - crop leaf to stem mass ratio for shoots
!     bcfliveleaf - fraction of standing plant leaf which is living
!                   (transpiring)
!     bcfshoot - crop ratio of shoot diameter to length
!     bcfstem2stor - fraction of assimilate partitioned to stem that is
!                    diverted to root store
!     bcfstor2stor - fraction of assimilate partitioned to standing
!                    storage (reproductive)that is diverted to root store
!     bcgrainf - internally computed reproductive grain fraction
!     bcgrf - fraction of reproductive biomass that is yield
!     bchyfg - flag indicating the part of plant to apply the "grain
!              fraction",
!              grf, to when removing that plant part for yield
!         0     grf applied to above ground storage (seeds, reproductive)
!         1     grf times growth stage factor (see growth.for) applied to
!               above ground storage (seeds, reproductive)
!         2     grf applied to all aboveground biomass (forage)
!         3     grf applied to leaf mass (tobacco)
!         4     grf applied to stem mass (sugarcane)
!         5     grf applied to below ground storage mass (potatoes,
!               peanuts)
!     bcleafareatrend - direction in which leaf area is trending. saves
!                       trend even if leaf area is static for long
!                       periods.
!     bcmbgstemz - crop stem mass below soil surface by soil layer
!                  (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstem  - crop flat stem mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)
!     bcmrootfiberz - crop root fibrous mass by soil layer (kg/m^2)
!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                     (tubers (potatoes, carrots), extended leaf (onion),
!                     seeds (peanuts))
!     bcmshoot - crop shoot mass grown from root storage (kg/m^2)
!                this is a "breakout" mass and does not represent a
!                unique pool since this mass is distributed into below
!                ground stem and standing stem as each increment of the
!                shoot is added
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                     (head with seed, or vegetative head (cabbage,
!                     pineapple))
!     bcmtotshoot - total mass of shoot growing from root storage
!                   biomass (kg/m^2) in the period from beginning to
!                   completion of emegence heat units
!     bcresid_int - residue intercept (kg/m^2)
!                   harvest_residue = bcyld_coef(kg/kg)
!                   * yield + bcresid_int (kg/m^2)
!     bctchillucum - accumulated chilling units (days)
!     bctdtm - days to maturity (same as dtm)
!     bcthu_shoot_beg - heat unit index (fraction) for beginning of
!                       shoot grow from root storage period
!     bcthu_shoot_end - heat unit index (fraction) for end of shoot
!                       grow from root storage period
!     bcthucum - crop accumulated heat units
!     bcthudf (acthudf) - heat units or days to maturity flag
!                         0 = days to maturity and average conditions
!                             used to find heat units
!                         1 = heat units specified used directly
!     bcthum - potential heat units for crop maturity (deg. c)
!     bctmin - base temperature (deg. c)
!     bctopt - optimum temperature (deg. c)
!     bctrthucum - accumulated root growth heat units (degree-days)
!     bctverndel - thermal delay coefficient pre-vernalization
!     bctwarmdays - number of consecutive days that the daily average
!                   temperature has been above the minimum growth
!                   temperature
!     bcxrow (acxrow) - crop row spacing (m)
!     bcxstm - crop stem diameter (m)
!     bcxstmrep - a representative diameter so that
!                 acdstm*acxstmrep*aczht=acrsai
!     bcyld_coef - yield coefficient (kg/kg)
!                  harvest_residue = bcyld_coef(kg/kg)
!                  * yield + bcresid_int (kg/m^2)
!     bcyraf (acyraf) - yield to biomass ratio adjustment factor
!     bczgrowpt - depth in the soil of the gowing point (m)
!     bczht - crop height (m)
!     bczloc_regrow - location of regrowth point (+ on stem, 0 or
!                     negative from crown at or below surface) (m)
!     bczmrt - maximum root depth
!     bczmxc - maximum potential plant height (m)
!     bczrtd  - crop root depth (m)
!     bczshoot - length of actively growing shoot from root biomass (m)
!     bdmb - residue amount by soil layer
!     bhfwsf - water stress factor ratio (0-1).  this is read in daily.
!     bhtsmn - daily minimum soil temperature by layer (deg.c)
!     bhtsmx - daily maximum soil temperature by layer (deg.c). currently
!              not used.
!     bhzeta (ahzeta) - actual evapotranspiration (mm/day). currently
!              not used.
!     bhzpta (ahzpta) - actual plant transpiration (mm/day). currently
!              not used.
!     bhzptp (ahzptp) - potential plant transpiration (mm/day). currently
!              not used.
!     bm0cgf (am0cgf) - flag to run crop growth if between plant and
!                       harvest. currently not used.
!     bm0cif - flag to initialize crop at start of planting
!     bnslay - number of soil layers
!     bprevbgstemz (prevbgstemz) - crop stem mass below soil surface by
!                    layer (kg/m^2)
!     bprevchillucum (prevchillucum) - accumulated chilling units (days)
!     bprevdayap - number of days of growth completed since crop planted
!     bprevflatleaf (prevflatleaf)  - crop flat leaf mass (kg/m^2)
!     bprevflatstem (prevflatstem)  - crop flat stem mass (kg/m^2)
!     bprevflatstore (prevflatstore) - crop flat storage mass (kg/m^2)
!     bprevgrainf (prevgrainf) - internally computed grain fraction of
!                                reproductive mass
!     bprevht (prevht) - crop height (m)
!     bprevhucum (prevhucum) - crop accumulated heat units
!     bprevliveleaf (prevliveleaf) - fraction of standing plant leaf
!                                    which is living (transpiring)
!     bprevmshoot (prevmshoot) - mass of shoot growing from root storage
!                                biomass (kg/m^2)
!     bprevmtotshoot (prevmtotshoot) - total mass of shoot growing from
!                                      root storage biomass (kg/m^2) in
!                                      the period from beginning to
!                                      completion of emergence heat units.
!                                      currently not used.
!     bprevrootfiberz (prevrootfiberz) - crop root fibrous mass by soil
!                                        layer (kg/m^2)
!     bprevrootstorez (prevrootstorez) - crop root storage mass by soil
!                                        layer (kg/m^2) (tubers (potatoes,
!                                        carrots), extended leaf (onion),
!                                        seeds (peanuts))
!     bprevrtd (prevrtd) - crop root depth (m)
!     bprevrthucum (prevrthucum) - crop accumulated heat units with no
!                                  vernalization/photoperiod delay
!     bprevstandleaf (prevstandleaf) - crop standing leaf mass (kg/m^2)
!     bprevstandstem (prevstandstem) - crop standing stem mass (kg/m^2)
!     bprevstandstore (prevstandstore) - crop standing storage mass
!                                        (kg/m^2)(head with seed, or
!                                        vegetativehead (cabbage,
!                                        pineapple))
!     bprevstm (prevstm) - number of crop stems per unit area (#/m^2). it
!                          is computed by taking the tillering factor
!                          times the plant population density.
!     bprevzshoot (prevzshoot) - length of actively growing shoot from
!                                root biomass (m)
!     bs0ph  - soil ph
!     bsdblk - bulk density of a layer (g/cm^3=t/m^3)
!     bsfcce - calcium carbonate (%)
!     bsfcec - cation exchange capacity (cmol/kg)
!     bsfcla - % clay
!     bsfom - percent organic matter
!     bsfsmb - sum of bases (cmol/kg)
!     bsftan - total available n in a layer from all sources (kg/ha)
!     bsftap - total available p in a layer from all sources (kg/ha)
!     bsmno3 - amount of applied n (t/ha)
!     bszlyd - depth from top of soil to botom of layer, m
!     bszlyt (aszlyt) - soil layer thicknesses for each subregion (mm)
!     btdstm - number of crop stems per unit area (#/m^2). it is computed
!              by taking the tillering factor times the plant population
!              density.
!     btgrainf - internally computed grain fraction of reproductive mass
!     btmbgstemz - crop buried stem mass by layer (kg/m^2)
!     btmflatleaf - crop flat leaf mass (kg/m^2)
!     btmflatstem - crop flat stem mass (kg/m^2)
!     btmflatstore - crop flat storage mass (kg/m^2)
!     btmstandleaf - crop standing leaf mass (kg/m^2)
!     btmstandstem - crop standing stem mass (kg/m^2)
!     btmstandstore - crop standing storage mass (kg/m^2)
!                     (head with seed, or vegetative head (cabbage,
!                     pineapple))
!     btxstmrep - a representative diameter so that
!                 acdstm*acxstmrep*aczht=acrsai
!     btzht - crop height (m)
!     bweirr (aweirr) - daily global radiation (mj/m^2)
!     bwtdmn - daily minimum air temperature (deg.c)
!     bwtdmx - daily maximum air temperature (deg.c)
!     bwzdpt (awzdpt) - daily precipitation (mm). currently not used.
!     cc0fd1 - fraction of biomass lost each day due to frost
!     cc0fd2 - fraction of biomass lost each day due to frost
!     daysim - day of the simulation
!     growth_stress - flag setting which turns on water or temperature
!                   stress (or both)
!                   growth_stress = 0  ! no stress values applied
!                   growth_stress = 1  ! turn on water stress
!                   growth_stress = 2  ! turn on temperature stress
!                   growth_stress = 3  ! turn on both
!     debe: because it is now being read in, it is commented out in command.inc
!     + + + local variable definitions + + +
!     dd - the current day
!     hu_delay - combined reduction in heat unit accummulation
!     lay - this is a counter
!     mm - the current month
!     photo_delay - reduction in heat unit accumulation based on
!                   photoperiod
!     pot_leaf_mass - potential leaf mass which could be released for
!                     regrowth.
!     pot_stems - potential number of stems which could be released for
!                 regrowth
!     root_store_rel - root storage which could be released for regrowth
!     trend - test computation for trend direction of living leaf area
!     vern_delay - reduction in heat unit accumulation based on
!                  vernalization
!     yy - the current year
 
!     + + + local parameter definitions + + +
!     chilluv - total of chill units require for vernalization (deg c)
!     shoot_delay - number of days minimum temperature must be above base
!                   crop growth temperature for shoot growth to occur
!     verndelmax - maximum value of vernalization delay parameter
!                  (see actverndel definition in include file)
!     dev_floor - minimum development rate fraction allowed (
!                 1-full rate, 0-no development)
!     max_photo_per - photo period where maximum development rate occurs
!                     (hours)
!     spring_trig - heat units ratio to spring allowing release of
!                   winter annual crown storage
 
!     + + + common block variables definitions + + +
!     a_fr - parameter in the frost damage s-curve
!     am0cfl - flag to print crop submodel output
!     ap - total available p in a layer from all sources (kg/ha)
!     b_fr - parameter in the frost damage s-curve
!     civilrise - parameter. solar altitude angle defined as civil
!                 twilight
!     hrlt - day length on day i (h)
!     huirt - heat unit index used to drive root growth (no delays)
!     hrlty - day length on day (i-1)
!     hui - heat unit index (ratio of acthucum to acthum)
!     huirty - heat unit index for root expansion (ratio of actrthucum to
!              acthum) on day (i-1)
!     huiy - heat unit index (ratio of acthucum to acthum) on day (i-1)
!     jd - today's julian date calander
!     mgtokg - parameter (mgtokg = 0.000001); to convert milligrams to
!              kilograms, multiply by 0.000001
!     rsd - current amount of residue (t/ha) in a layer
!     wn - organic n concentration of humus (g/t)
!     wno3 - total available n in a layer from all sources (kg/ha)
!     wp - organic p concentration of humus (g/t)
!     xlat - latitude of a location (deg.)
 
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
!     canhty - yesterday's canopy height value.
!     canopyflg - a flag to indicate if the weps/upgm method to calculate
!                 plant height will be used. value will then be 0. if using
!                 the phenologymms method, the value will be 1.
!     cliname - the name of the location for the climate data
!     cname - cropname passed to phenol.
!     co2atmos - the atmospheric level of CO2.
!     co2eff - the effect on the plant of the read-in atmospherice co2 level.
!              The adjustment factor 
!     co2x - the CO2 levels in ppm. The x axis on the relationship curve.
!     co2y - the relative effect at different levels of CO2, i.e. co2x.
!     cots - cotyledonary and unifoliolate leaves are visible in dry
!            beans. this array includes daynum, year, month and day
!            of when this stage was reached.
!     cropname - name of the crop
!     daa - days after anthesis
!     dae - days after emergence
!     dap - days after planting
!     dav - days after vernalization
!     dayhtinc - the increase in plant height for today.
!     daynum - day number of the year
!     ddae - array holding the dae value for each growth stage
!     ddap - array holding the dap value for each growth stage
!     ddav - array holding the dav value for each growth stage
!     dents - the dent growth stage in corn. this array includes daynum,
!             year, month and day of when this stage was reached.
!     dgdde - array holding the gdde value for each growth stage
!     dgdds - array holding the gdds value for each growth stage
!     dgddv - array holding the gddv value for each growth stage
!     doughs - the dough growth stage in corn. this array includes daynum,
!              year, month and day of when this stage was reached.
!     drs - double ridge growth stage for hay millet, proso millet,
!           spring barley, spring wheat, winter barley and winter wheat.
!           this array includes daynum, year, month and day of when this
!           stage was reached.
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
!     elong - total elongation of the emerging seedling based on the
!             day's gdd (mm)
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
!     gdda - sum of growing degree days since anthesis
!     gddday - the number of gdd with 0°C base temperature for that day.
!     gdde - sum of growing degree days since emergence
!     gdds - sum of growing degree days since seeding
!     gddtbg - used to accumulate gdd for seeds planted in dust after a
!              rainfall event has moved the soil moisture condition to
!              dry.  the seed can begin accumulating gdd's germination.
!     gddv - sum of growing degree days since vernalization
!     gddwsf - an array to hold the gn and gs gdd values plus the high
!              and low water stress factor values.  these are used in
!              calculating the slope of the line for each growth stage
!              and this is then used to calculate the adjusted gdd value
!              for the current growth stage.
!              column one contains the gn values and is y2.
!              column two contains the gs values and is y1.
!              column three contains wsfhi (high water stress) and is x1.
!              column four contains wsflo (low water stress) and is x2.
!              column five contains the adjgdd value for the stage.
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
!     icli - a flag to determine which type of weather file to read.  a
!            value of 1 indicates that climate data should be read from
!            the cligen weather file.  a value of 0 indicates that a
!            historical climate file will be used.
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
!     lnarray - an array to hold the leaf number calculated for each day
!     lncntr - counter for the leafno subroutine
!     lnpout - an array used in writing out daynum and the number of
!              leaves on that day. the values are written each time a new
!              leaf has appeared, i.e. when the integer version of the
!              real leaf number has incremented.
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
!     pdate - day of year planting can occur
!     pm - planting month
!     py - planting year.  currently, not the calendar year.
!     rowcntr - a counter for the rows in an array
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
!     tempsw - a new variable to designate the array subscripts for the new 6 element
!              arrays: egdd, ggdd used in emerge.
!     tis - start of tillering growth stage for corn, hay millet, proso
!           millet, sorghum, spring barley, spring wheat, winter barley
!           and winter wheat. this array includes daynum, year, month and
!           day of when this stage was reached.
!     todayln - the value of the current day's leaf number.
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
!     verns - sum of gdd after vernalization. currently not used.
!     wfpslo - an array holding the low values for each soil moisture
!              condition.
!     wfpsup - an array holding the high values for each soil moisture
!              condition.
!     yelows - back of the sunflower head is a light yellow. this array
!              includes daynum, year, month and day of when this stage
!              was reached.
!     yestln - the value of yesterday's leaf number
!     yr - year
 
!     + + + new local variable definitions + + +
!     phenolflg - a flag that determines if the upgm/weps method of determining maturity
!                 will be used (phenolflg =0) or the phenologymms method will be used (phenolflg = 1).
!     tavgtemp - the variable that holds the average of tmaxtemp and
!                tmintemp.
!     tmaxtemp - the variable that is set equal to bctmin if the day's
!                maximum air temperature is less than the base
!                temperature. also, it is set equal to tupper if the day's
!                maximum air temperature is greater than the
!                upper/maximum temperature.
!     tmintemp - the variable that is set equal to bctmin if the day's
!                minimum air temperature is less than the base temperature.
!                also, it is set equal to tupper if the day's minimum air
!                temperature is greater than the upper/maximum temperature.
 
!     + + + global common blocks + + +
 
!     + + + common blocks + + +
!     include 'cgrow.inc'
!     include 'csoil.inc'
 
!     + + + subroutines called + + +
!     caldatw
!     canopyht
!     chillu
!     cinit
!     huc1 - this is a function
!     gddcalc
!     growth
!     leafno
!     npcy - this is not called
!     phenol
!     scrv1
!     shoot_grow
!     shootnum
!
!     + + + function declarations + + +
 
!     + + + end of specifications + + +
 
!print*,'in crop before doing stuff, seedsw = ', seedsw, 'daysim = ', daysim
!debe cname is not set equal to cropname anywhere. added that so that the
!correct cropname will get passed to phenol.for.
 
                                                                                 ! debe added phenolflg
cname = cropname   ! debe added for phenol subroutine call
 
! debe added for testing. later phenolflg will be read in in main and
! initialized and passed on to crop.
!phenolflg = 1 !this is now passed from main on through to crop and is global
!
!     day of year
call caldat(ctrl%sim%julday, dd,mm,yy)
jd = dayear(dd,mm,yy)
!
!debe set daynum = jd here at the top of the subroutine
daynum = jd
!
do lay = 1,bnslay
  bsfcce(lay) = bsfcce(lay)*100.
  bsfom(lay) = bsfom(lay)*100.
  bsfcla(lay) = bsfcla(lay)*100.
 
  wn(lay) = 0.0
  wp(lay) = 0.0
  wno3(lay) = bsftan(lay)
  ap(lay) = bsftap(lay)
!    residue is now passed from main and converted here from kg/m^2 to
!    t/ha residue was previously estimated in subroutine sdst
!    the validity of this needs to be checked since type of residue (rsd)
!    needed is not clear in crop    - jt  07/21/94
!      i think this (rsd) is being used in the nutrient cycling.
!      thus, it probably should be the sum of admbgz and admrtz
!      (all pools) for each layer.  lew 4/23/99
  rsd(lay) = bdmb(lay)*10.0
end do
 
!     initialize growth and nutrient variables when crop is planted
!     bm0cif is flag to initialize crop at start of planting
 
if (bm0cif) then
  call cinit(ctrl, clidat,bnslay,bszlyt,bszlyd,bsdblk,bsfcce,bsfcec,bsfsmb,bsfom,bsfcla,     &
           & bs0ph,bc0bn1,bc0bn2,bc0bn3,bc0bp1,bc0bp2,bc0bp3,bsmno3,bc0fd1,     &
           & bc0fd2,bctopt,bctmin,cc0fd1,cc0fd2,bc0sla,bc0idc,dd,mm,yy,bcthudf, &
           & bctdtm,bcthum,bc0hue,bcdmaxshoot,bc0shoot,bc0growdepth,            &
           & bc0storeinit,bcmstandstem,bcmstandleaf,bcmstandstore,bcmflatstem,  &
           & bcmflatleaf,bcmflatstore,bcmshoot,bcmtotshoot,bcmbgstemz,          &
           & bcmrootstorez,bcmrootfiberz,bczht,bczshoot,bcdstm,bczrtd,bcdayap,  &
           & bcdayam,bcthucum,bctrthucum,bcgrainf,bczgrowpt,bcfliveleaf,        &
           & bcleafareatrend,bctwarmdays,bctchillucum,bcthu_shoot_beg,          &
           & bcthu_shoot_end,bcdpop,bcdayspring,canht,canhty,daa,dae,dap,dav,   &
           & ddae,ddap,ddav,dgdde,dgdds,dgddv,elong,endphenol,gddday,gdda,gdde, &
           & gdds,gddv,gddwsf,jan1,lnarray,lncntr,lnpout,pdate,rowcntr,   &
           & seedsw,tempsw,todayln,verns,yestln,yr,ln,co2eff) 
 
!debe 091208 added the following to be initialized in cinit: ddap, elong,
! gddday, yr, leaf number variables, verns, gddwsf to be initialized. later
! added canht for initialization for canopy height.
! added tempsw to be initialized in cinit and used in emerge. passed seedsw to
! cinit to initialize tempsw with the value in seedsw. 5122011
 
          ! set previous values to initial values
  bprevstandstem = bcmstandstem
  bprevstandleaf = bcmstandleaf
  bprevstandstore = bcmstandstore
  bprevflatstem = bcmflatstem
  bprevflatleaf = bcmflatleaf
  bprevflatstore = bcmflatstore
  bprevmshoot = bcmshoot
  do lay = 1,bnslay
     bprevbgstemz(lay) = bcmbgstemz(lay)
     bprevrootstorez(lay) = bcmrootstorez(lay)
     bprevrootfiberz(lay) = bcmrootfiberz(lay)
  end do
  bprevht = bczht
  bprevzshoot = bczshoot
  bprevstm = bcdstm
  bprevrtd = bczrtd
  bprevdayap = bcdayap
  bprevhucum = bcthucum
  bprevrthucum = bctrthucum
  bprevgrainf = bcgrainf
  bprevchillucum = bctchillucum
  bprevliveleaf = bcfliveleaf
 
  if (am0cfl>=1) then
          ! put double blank lines in daily files to create growth blocks
     write (luocrop,*)           ! crop.out
     write (luocrop,*)           ! crop.out
     write (luoshoot,*)          ! shoot.out
     write (luoshoot,*)          ! shoot.out
     write (luocanopyht,*)       ! canopyht.out debe added phenol and emerge.out lines
     write (luocanopyht,*)       ! canopyht.out
     write (luoemerge,*)         ! emerge.out
     write (luoemerge,*)         ! emerge.out
  end if
 
  bm0cif = .false.          !turn off after initialization is complete
else
          ! calculate day length
  hrlty = hrlt
  hrlt = daylen(xlat,jd,civilrise)
 
          ! set trend direction for living leaf area from external forces
  trend = (bcfliveleaf*bcmstandleaf) - (bprevliveleaf*bprevstandleaf)
  if ((trend/=0.0).and.(bcthucum/bcthum>bc0hue)) bcleafareatrend = trend
                          ! trend non-zero and heat units past emergence
end if
!
!if ((daynum.ge.258).and.(daynum.le.365)) then
!    print*, 'in crop before call to phenolmms, daynum = ', daynum
!end if
 
!debe added call to phenolmms. call it after call to cinit to insure that
!variables are initialized.
call phenolmms(aepa,aifs,antes,antss,bc0growdepth,bctmin,bhfwsf,blstrs,boots,   &
             & browns,clidat%awtdmn,clidat%awtdmx,canht,cliname,cname,cots,cropname,daa,dae,  &
             & dap,dav,dayhtinc,daynum,dd,ddae,ddap,ddav,dents,dgdde,dgdds,     &
             & dgddv,doughs,drs,dummy2,ears,ecanht,emrgflg,ems,endlgs,endphenol,&
             & epods,eseeds,first7,fps,fullbs,gdda,gddday,gdde,gdds,gddv,gddwsf,&
             & gmethod,gpds,halfbs,heads,hrs,ies,ies2,infls,jan1,joints,lf1s,   &
             & lf12s,lf2s,lf3s,lf4s,lf8s,lnarray,lncntr,lnpout,mats,maxht,mffls,&
             & milks,mm,mpods,mseeds,opens,pchron,pdate,rowcntr,seedsw,   &
             & silks,soilwat,srs,tbase,tis,todayln,toptlo,toptup,tsints,tss,    &
             & tupper,yelows,yestln,yy,ln)                  
                   
 
!debe added dayhtinc to be able to pass the daily increase in height to growth
! for the ht_dia_sai subroutine in place of the weps/upgm variable dht when
! canopyflg = 1.
 
!debe moved setting of jan1 to true out of phenolmms so that it can be
! passed into phenolmms the day after vernalization has occurred so that
! dav (days after vernalization) will be = 1 on the first day after
! vernalization.
if ((mm==1).and.(dd==1).and.(yy==2)) jan1 = .true.
 
      ! check for consecutive "warm" days based on daily
      ! average temperature
if (0.5*(clidat%awtdmx+clidat%awtdmn)>bctmin) then
          ! this is a warm day
  bctwarmdays = bctwarmdays + 1
else
          ! reduce warm day total, but do not zero, for proper fall
          ! regrow of perennials
  bctwarmdays = bctwarmdays/2
end if
 
      ! accumulate chill units
call chillu(bctchillucum,clidat%awtdmx,clidat%awtdmn)
 
      ! zero out temp pool variables used in testing for residue
      ! from regrowth in callcrop
btmstandstem = 0.0
btmstandleaf = 0.0
btmstandstore = 0.0
btmflatstem = 0.0
btmflatleaf = 0.0
btmflatstore = 0.0
 
      ! check crop type for shoot growth action
if ((bcfleaf2stor>0.0).or.(bcfstem2stor>0.0).or.(bcfstor2stor>0.0)) then
  if ((bc0idc==2).or.(bc0idc==5)) then
          ! check winter annuals for completion of vernalization,
          ! warming and spring day length
     if ((bczgrowpt>0.0).and.(bctchillucum>=chilluv).and.                       &
       & (bctwarmdays>=shoot_delay*bctverndel/verndelmax).and.(huiy>spring_trig)&
       & ) then
              ! vernalized and ready to grow in spring
        bcthu_shoot_beg = bcthucum/bcthum
        bcthu_shoot_end = bcthucum/bcthum + bc0hue
 
        call shootnum(bnslay,bc0idc,bcdpop,bc0shoot,bcdmaxshoot,bcmtotshoot,    &
                    & bcmrootstorez,bcdstm)
              ! eliminate diversion of biomass to crown storage
        bcfleaf2stor = 0.0
        bcfstem2stor = 0.0
        bcfstor2stor = 0.0
              ! set day of year on which transition took place
        bcdayspring = jd
 
     end if
  else if (bc0idc/=7) then
          ! bi-annuals and perennials with tuber dormancy don't need
          ! either of these checks. doing nothing here prevents
          ! resprouting after defoliation
          ! check summer annuals and perennials for removal of all
          ! (most) leaf mass
     if (bcleafareatrend<0.0) then     ! last change in leaf area was a
                                       ! reduction
                                       ! ! 0.42 * 2 = 0.84
        if (bcfliveleaf*bcmstandleaf<0.84*bc0storeinit*bcdpop*mgtokg*bcfleafstem&
          & /(bcfleafstem+1.0)) then            ! below minimum living leaf mass
                                                !(which is twice seed leaf mass)
           if (bctwarmdays>=shoot_delay) then   ! enough warm days to start
                                                ! regrowth
              if (bcthucum/bcthum>=bc0hue) then   ! heat units past emergence
                                                  ! ! not yet mature
                 if ((bcthucum<bcthum).or.((bc0idc==3).or.(bc0idc==6))) then
                                                                ! perennial
               ! find out how much root store could be released for regrowth
                    call shootnum(bnslay,bc0idc,bcdpop,bc0shoot,bcdmaxshoot,    &
                                & root_store_rel,bcmrootstorez,pot_stems)
               ! find the potential leaf mass to be achieved with regrowth
                    if (bczloc_regrow>0.0) then
                       pot_leaf_mass = bcmstandleaf +                           &
                                     & 0.42*min(root_store_rel,bcmtotshoot)     &
                                     & *bcfleafstem/(bcfleafstem+1.0)
                    else
                       pot_leaf_mass = 0.42*root_store_rel*bcfleafstem/         &
                                     & (bcfleafstem+1.0)
                    end if
               ! is present living leaf mass less than leaf mass from
               ! storage regrowth
                    if ((bcfliveleaf*bcmstandleaf)<pot_leaf_mass) then
                  ! regrow possible from shoot for perennials, annuals.
                  ! reset growth clock
                       bcthucum = 0.0
                       bcthu_shoot_beg = 0.0
                       bcthu_shoot_end = bc0hue
                  ! reset shoot grow configuration
                       if (bczloc_regrow>0.0) then
                      ! regrows from stem, stem does not become residue
                      ! note, flat leaves are dead leaves, no storage in
                      ! shoot.
                          bcmshoot = bcmstandstem + bcmflatstem + bcmstandleaf
                          do lay = 1,bnslay
                             bcmshoot = bcmshoot + bcmbgstemz(lay)
                          end do
                          bcmtotshoot = min(root_store_rel,bcmtotshoot)
                       else
                      ! regrows from crown, stem becomes residue
                          btmstandstem = bcmstandstem
                          btmstandleaf = bcmstandleaf
                          btmstandstore = bcmstandstore
                          btmflatstem = bcmflatstem
                          btmflatleaf = bcmflatleaf
                          btmflatstore = bcmflatstore
                          do lay = 1,bnslay
                             btmbgstemz(lay) = bcmbgstemz(lay)
                          end do
                          btgrainf = bcgrainf
                          btzht = bczht
                          btdstm = bcdstm
                          btxstmrep = bcxstmrep
                      ! reset crop values to indicate new growth cycle
                          bcmshoot = 0.0
                          bcmstandstem = 0.0
                          bcmstandleaf = 0.0
                          bcmstandstore = 0.0
                          bcmflatstem = 0.0
                          bcmflatleaf = 0.0
                          bcmflatstore = 0.0
                          do lay = 1,bnslay
                             bcmbgstemz(lay) = 0.0
                          end do
                          bcgrainf = 0.0
                          bczht = 0.0
                          bcmtotshoot = root_store_rel
                          bcdstm = pot_stems
                       end if
                    end if
                 end if
              end if
           end if
        end if
     end if
  end if
end if
 
      ! calculate growing degree days
      ! set default heat unit delay value
hu_delay = 1.0
if (bcthum<=0.0) then
          ! always keep this invalid plant in first stage growth
  huiy = 0.0
  hui = 0.0
else
          ! previous day heat unit index
  huiy = bcthucum/bcthum
  huirty = bctrthucum/bcthum
          ! check for growth completion
  if (huiy<1.0) then
              ! accumulate additional for today
              ! check for emergence status
     if ((huiy>=bc0hue).and.(huiy<spring_trig)) then
                  ! emergence completed, account for vernalization and
                  ! photo period by delaying development rate until chill
                  ! units completed and spring trigger reached
 
!vern_delay = 1.0 minus the thermal delay coefficient pre-vernalization
! multiplied by the value of the total number of chill units needed for
! vernalization (parameter set to 50) minus the accumulated chilling
! units in days.
        vern_delay = 1.0 - bctverndel*(chilluv-bctchillucum)
        photo_delay = 1.0
!hu_delay: combined reduction in heat unit accummulation
        hu_delay = max(dev_floor,min(vern_delay,photo_delay))
     end if
              ! do not accumulate heat units if daily minimum is below
              ! freezing.
!if( bwtdmn .gt. 0.0 ) then accumulate heat units using set heat unit
! delay
     bcthucum = bcthucum + huc1(clidat%awtdmx,clidat%awtdmn,bctopt,bctmin)*hu_delay
!              end if
              ! root depth growth heat units
     bctrthucum = bctrthucum + huc1(clidat%awtdmx,clidat%awtdmn,bctopt,bctmin)
              ! do not cap this for annuals, to allow it to continue
              ! root mass partition is reduced to lower levels after the
              ! first full year. out of range is capped in the function
              ! in growth.for
              ! bctrthucum = min(bctrthucum, bcthum)
              ! calculate heat unit index
     hui = bcthucum/bcthum
     huirt = bctrthucum/bcthum
 
  end if
 
end if
 
!de and gm changed this line of code for 'if(huiy.lt.1.0) then'
! to incorporate the phenologymms way of determining maturity.
!de changed the if statement to include the decision of which method will
! be used to determine maturity using phenolflg. if phenolflg is 1 then the phenologymms
! way of reaching maturity is to be used and that occurs when mats(1) has a value not equal
! to 999. if phenolflg = 0, then the weps way will be used. when huiy is greater than or
! equal to 1.0, maturity has been reached. this allows the possibility of using either
! approach to determine maturity.
 
if (((phenolflg==1).and.(mats(1)==999)).or.((phenolflg==0).and.(huiy<1.0))) then
 
          ! crop growth not yet complete
          ! increment day after planting counter since growth happens
         ! same day
   !                           on the day of planting.
 ! if (daynum.gt.pdate) bcdayap = bcdayap + 1!debe added to prevent bcdayap incrementing. also, doesn't work for crops going over winter, e.g. winter wheat
  bcdayap = bcdayap + 1   ! changed back to this way because of comment above that
                        ! growth happens on planting day.
 
          ! seedling, transplant initialization and winter annual shoot
          ! growth calculations using root reserves
              ! daily shoot growth
!
! try #1, change bcthu_shoot_end in debugger on daynum 264 and 265
!   (daysim = 7&8) from 0.05 to 0.06 so shoot_grow would be called and
!   d_shoot_mass became negative in shoot_grow (~line 242) and emerge
!   never got called:
!  if ((huiy .lt. 0.06).and.(hui.gt.bcthu_shoot_beg))
 
! try #2, change bcthu_shoot_end with following code on daynum 264
!   and 265 (daysim = 7&8)from 0.05 to 0.06 so shoot_grow would be called
!      if ((daysim .eq. 7) .or. (daysim .eq. 8)) then
!          bcthu_shoot_end = 0.06
!      else
!          bcthu_shoot_end = 0.05
!      endif
 
! try #3, if using phenologymms emergence code (emrgflg=1), emergence
! has not occurred yet (ems(1)=999), and hui.gt.bcthu_shoot_beg) call
! shoot_grow:
  if ((emrgflg==1).and.(ems(1)==999).and.(hui>bcthu_shoot_beg)) then
!      if ((daysim .eq. 7) .or. (daysim .eq. 8)) then
!          bcthu_shoot_end = 0.06
!      else
!          bcthu_shoot_end = 0.05
!      endif
 
!debe replaced bcthucum (upgm/weps) with gdds (phenologymms) in the call
! to shoot_grow.
!
! the following "fix" will also need to reset bcthu_shoot_end back to
! original value once emergence occurs. to avoid the problem where huiy
! = bcthu_shoot_end in the call to shoot_grow, add 0.00001 to huiy below:
     if (huiy>=bcthu_shoot_end) bcthu_shoot_end = huiy + 0.00001      !should 'then' be here?
     call shoot_grow(ctrl,clidat,bnslay,bszlyd,bcdpop,bczmxc,bczmrt,bcfleafstem,bcfshoot,   &
                   & bc0ssa,bc0ssb,bc0diammax,hui,huiy,bcthu_shoot_beg,         &
                   & bcthu_shoot_end,bcmstandstem,bcmstandleaf,bcmstandstore,   &
                   & bcmflatstem,bcmflatleaf,bcmflatstore,bcmshoot,bcmtotshoot, &
                   & bcmbgstemz,bcmrootstorez,bcmrootfiberz,bczht,bczshoot,     &
                   & bcdstm,bczrtd,bczgrowpt,bcfliveleaf,bc0nam,bchyfg,         &
                   & bcyld_coef,bcresid_int,bcgrf,daysim,dap,gdds,bc0growdepth, &
                   & seedsw,cropname,soilwat,wfpslo,wfpsup,germgdd,ergdd,gddtbg,&
                   & ddap,dgdds,elong,ems,germs,gddday,yy,emrgflg,icli,pd,pm,py,&
                   & yr,cliname,egdd,ggdd,tempsw)                          !used dap in place of bcdayap
  else if ((huiy<bcthu_shoot_end).and.(hui>bcthu_shoot_beg)) then
     call shoot_grow(ctrl,clidat, bnslay,bszlyd,bcdpop,bczmxc,bczmrt,bcfleafstem,bcfshoot,   &
                   & bc0ssa,bc0ssb,bc0diammax,hui,huiy,bcthu_shoot_beg,         &
                   & bcthu_shoot_end,bcmstandstem,bcmstandleaf,bcmstandstore,   &
                   & bcmflatstem,bcmflatleaf,bcmflatstore,bcmshoot,bcmtotshoot, &
                   & bcmbgstemz,bcmrootstorez,bcmrootfiberz,bczht,bczshoot,     &
                   & bcdstm,bczrtd,bczgrowpt,bcfliveleaf,bc0nam,bchyfg,         &
                   & bcyld_coef,bcresid_int,bcgrf,daysim,dap,gdds,bc0growdepth, &
                   & seedsw,cropname,soilwat,wfpslo,wfpsup,germgdd,ergdd,gddtbg,&
                   & ddap,dgdds,elong,ems,germs,gddday,yy,emrgflg,icli,pd,pm,py,&
                   & yr,cliname,egdd,ggdd,tempsw)        !used dap in place of this bcdayap
  end if
        ! ends the if emrgflg = 1 if block
 
!debe added the new arrays egdd, ggdd to the calls above to shoot_grow to be
! passed on to emerge where they are used. added tempsw as the array index of these arrays.
 
  if ((huiy<bcthu_shoot_end).and.(hui>=bcthu_shoot_end)) then
              ! shoot growth completed on this day
              ! move growing point to regrowth depth after shoot growth
              ! complete
     bczgrowpt = (-bczloc_regrow)
                  ! single blank line to separate shoot growth periods
     if (am0cfl>=1) write (luoshoot,*)
                                        ! shoot.out
 
  end if
 
          ! temporary location
  call scrv1(bc0fd1,cc0fd1,bc0fd2,cc0fd2,a_fr,b_fr)   ! frost damage
  !   print*, 'in crop before call to growth, daysim = ', daysim
 
!          ! calculate plant growth state variables
  call growth(ctrl, bnslay,bszlyd,bc0ck,bcgrf,bcehu0,bczmxc,bc0idc,bc0nam,a_fr,b_fr,  &
            & bcxrow,bc0diammax,bczmrt,bctmin,bctopt,bc0bceff,bc0alf,bc0blf,    &
            & bc0clf,bc0dlf,bc0arp,bc0brp,bc0crp,bc0drp,bc0aht,bc0bht,bc0ssa,   &
            & bc0ssb,bc0sla,bcxstm,bhtsmn,clidat%awtdmx,clidat%awtdmn,clidat%aweirr,bhfwsf,hui,huiy, &
            & huirt,huirty,hu_delay,bcthu_shoot_end,bcbaflg,bcbaf,bcyraf,bchyfg,&
            & bcfleaf2stor,bcfstem2stor,bcfstor2stor,bcyld_coef,bcresid_int,    &
            & bcmstandstem,bcmstandleaf,bcmstandstore,bcmflatstem,bcmflatleaf,  &
            & bcmflatstore,bcmrootstorez,bcmrootfiberz,bcmbgstemz,bczht,bcdstm, &
            & bczrtd,bcfliveleaf,bcdayap,bcgrainf,bcdpop,dayhtinc,daysim,gddday,&
            & growth_stress,canht,canhty,canopyflg,antss,phenolflg,boots,heads, &
            & joints,mats,ln,co2x,co2y,co2atmos,co2eff,ts)
  
! debe added joints, heads and mats to send to growth. 
 
!debe090408 added gddday to print out gdd.
! a. later added variables for canopyht to be passed to growth to canopyht subroutine.
! b. later decided to call canopyht right from crop which was done below after the call to
! phenol.
! c. even later most all calls to subroutines from phenologymms were put into a new
! subroutine called phenolmms which is called above here in crop.
 
!debe added growth_stress because it is now read in.
!debe added canopyflg and canht to be passed to growth so that canht calculated
! from the phenologymms canopy height subroutine can be used in place of dht in growth.
!debe added dayhtinc to be able to pass the daily increase in height to growth
! for the ht_dia_sai subroutine in place of the weps/upgm variable dht when
! canopyflg = 1.
!debe added antss to the growth subroutine to limit calculating canopy height after anthesis.
!debe added canhty (yesterday's canopy height) so that it will be initialized the first time
! it is passed to growth and so it can be saved here and be available tomorrow to compare
! with the 'today' value tomorrow.
!debe added passing antss, phenolflg and boots to growth.
 
          ! set trend direction for living leaf area
  trend = (bcfliveleaf*bcmstandleaf) - (bprevliveleaf*bprevstandleaf)
  if ((trend/=0.0).and.(bcthucum/bcthum>bc0hue)) bcleafareatrend = trend
          ! trend non-zero and heat units past emergence
          ! set saved values of crop state variables for comparison next
          ! time
  bprevstandstem = bcmstandstem
  bprevstandleaf = bcmstandleaf
  bprevstandstore = bcmstandstore
  bprevflatstem = bcmflatstem
  bprevflatleaf = bcmflatleaf
  bprevflatstore = bcmflatstore
  bprevmshoot = bcmshoot
  do lay = 1,bnslay
     bprevbgstemz(lay) = bcmbgstemz(lay)
     bprevrootstorez(lay) = bcmrootstorez(lay)
     bprevrootfiberz(lay) = bcmrootfiberz(lay)
  end do
  bprevht = bczht
  bprevzshoot = bczshoot
  bprevstm = bcdstm
  bprevrtd = bczrtd
  bprevdayap = bcdayap
  bprevhucum = bcthucum
  bprevrthucum = bctrthucum
  bprevgrainf = bcgrainf
  bprevchillucum = bctchillucum
  bprevliveleaf = bcfliveleaf
else
          ! heat units completed, crop leaf mass is non transpiring
  bcfliveleaf = 0.0
 
! check for mature perennial that may re-sprout before fall
          ! (alfalfa, grasses)
  if ((bc0idc==3).or.(bc0idc==6)) then
              ! check for growing weather and regrowth ready state
                  ! transfer all mature biomass to residue pool
                  ! find number of stems to regrow
                  ! reset heat units to start shoot regrowth
  end if
 
          ! accumulate days after maturity
  bcdayam = bcdayam + 1
 
end if
!
! debe 091008 changed the following phenol variables to the upgm variable
! name already being used:
! phenology name = upgm name
! cname          = cropname
! year           = yy
 
do lay = 1,bnslay
  bsfcce(lay) = bsfcce(lay)/100.
  bsfom(lay) = bsfom(lay)/100.
  bsfcla(lay) = bsfcla(lay)/100.
end do
!
end subroutine crop

subroutine growth(bnslay,bszlyd,bc0ck,bcgrf,bcehu0,bczmxc,bc0idc,bc0nam,a_fr,   &
                & b_fr,bcxrow,bc0diammax,bczmrt,bctmin,bctopt,cc0be,bc0alf,     &
                & bc0blf,bc0clf,bc0dlf,bc0arp,bc0brp,bc0crp,bc0drp,bc0aht,      &
                & bc0bht,bc0ssa,bc0ssb,bc0sla,bcxstm,bhtsmn,bwtdmx,bwtdmn,      &
                & bweirr,bhfwsf,hui,huiy,huirt,huirty,hu_delay,bcthu_shoot_end, &
                & bcbaflg,bcbaf,bcyraf,bchyfg,bcfleaf2stor,bcfstem2stor,        &
                & bcfstor2stor,bcyld_coef,bcresid_int,bcmstandstem,bcmstandleaf,&
                & bcmstandstore,bcmflatstem,bcmflatleaf,bcmflatstore,           &
                & bcmrootstorez,bcmrootfiberz,bcmbgstemz,bczht,bcdstm,bczrtd,   &
                & bcfliveleaf,bcdayap,bcgrainf,bcdpop,dayhtinc,daysim,gddday,   &
                & growth_stress,canht,canhty,canopyflg,antss,phenolflg,boots,   &
                & heads,joints,mats,ln,co2x,co2y,co2atmos,co2eff,ts)      
    
!debe gddday added to print out gdd.
!debe added growth_stress because it is now read in.
!debe added canopyflg to be used to determine which method of calculating canopy
!height will be used. 0=weps/upgm method and 1=phenologymms.
!debe added dayhtinc to be able to pass the daily increase in height to growth
! for the ht_dia_sai subroutine in place of the weps/upgm variable dht when
! canopyflg = 1.
!debe added antss to the growth subroutine to limit calculating canopy height after anthesis.
!debe added canhty to store yesterday's canopy height value.
!debe added phenolflg and boots to begin incorporating phenologymms tie in to upgm.
!debe added joints, heads and mats to help with debugging to specific growth stages.
!debe added co2x, co2y, co2atmos, co2eff for use in affecting plant growth by co2.
!    
implicit none
!
include 'file.fi'
include 'p1werm.inc'
include 'm1flag.inc'
include 'precision.inc'
include 'p1unconv.inc'
include 'p1const.inc'
include 'command.inc'
include 'clai.inc'
!DE added
include 'cgrow.inc'
!
! Dummy arguments
!
real :: a_fr,bc0aht,bc0alf,bc0arp,bc0bht,bc0blf,bc0brp,bc0ck,bc0clf,bc0crp,     &
      & bc0diammax,bc0dlf,bc0drp,bc0sla,bc0ssa,bc0ssb,bcbaf,bcdpop,bcdstm,      &
      & bcehu0,bcfleaf2stor,bcfliveleaf,bcfstem2stor,bcfstor2stor,bcgrainf,     &
      & bcgrf,bcmflatleaf,bcmflatstem,bcmflatstore,bcmstandleaf,bcmstandstem,   &
      & bcmstandstore,bcresid_int,bcthu_shoot_end,bctmin,bctopt,bcxrow,bcxstm,  &
      & bcyld_coef,bcyraf,bczht,bczmrt,bczmxc,bczrtd,bhfwsf,bweirr,bwtdmn,      &
      & bwtdmx,b_fr,canht,canhty,cc0be,dayhtinc,gddday,hui,huirt,huirty,huiy,   &
      & hu_delay,todayln,ln,co2atmos,co2eff,ts
integer :: bc0idc,bcbaflg,bcdayap,bchyfg,bnslay,canopyflg,daysim,growth_stress, &
         & phenolflg
integer,dimension(4) :: antss,boots,heads,joints,mats
real,dimension(*) :: bcmbgstemz,bcmrootfiberz,bcmrootstorez,bhtsmn,bszlyd
character(80) :: bc0nam
real,dimension(10) :: co2x,co2y
!
! Local variables
!
real :: apar,arg_exp,bhfwsf_adj,ddm_rem,dht,dlfwt,drfwt,drpwt,drswt,dstwt,      &
      & eff_lai,ff,ffa,ffr,ffw,frst,gif,huf,hufy,hui0f,hux,lost_mass,par,pdht,  &
      & pdiam,pdrd,p_lf,p_lf_rp,p_rp,p_rw,p_st,stem_propor,strsdayhtinc,        &
      & temp_sai,temp_stmrep,wcg,wffiber,wfstore,wmaxd,xw,temp_fiber,           &
      & temp_stem,temp_store

!      trad_lai,pddm, DE moved these out of the real declaration above.

!these are in the common block cgrow.inc, that is now included so DE moved 
!these out of the real declaration above.:
    !clfarea,clfwt,ddm,parea,pcht,pchty,prd,prdy,strs,

integer :: day,doy,i,irfiber,irstore,mo,yr,j,k
integer :: dayear
real :: temps
real,dimension(mnsz) :: wfl,za
!
!debe 061009 removed variables that were to be used for canopyht because canopyht
!  is now called from crop: antss(4), canht, cropname, dummy2, ems, gdde,
!  ies, joints, lf4s, maxht. later canopyht was moved and is called from the
!  new subroutine phenolmms. later added back joints, as well as mats for ease in
!  debugging to a specific growth stage.
!2/20/15 canopyht is called from PhenolMMS.
!debe added growth_stress because it is now read in.
!debe added antss to control the end of calculating canopy height when canopyflg = 1
! and anthesis stage has been passed. plant height should not increase after anthesis.
!debe added a local variable strsdayhtinc to hold the dayhtinc value after stress has been applied.
! debe added canhty to holds yesterday's canopy height to print in canopyht.out.
 
!     the following subroutine arguments are not used: bc0idc, bcbaflg,
!     bcthu_shoot_end, bcyraf   jcaii  8/08/08
!
!     author : amare retta
!     + + + purpose + + +
!     this subroutine calculates plant height, biomass partitioning,
!     rootmass distribution, rooting depth.
 
!     + + + keywords + + +
!     biomass
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     bc0aht (ac0aht) - height s-curve parameter
!     bc0alf (ac0alf) - leaf partitioning parameter
!     bc0arp - rprd partitioning parameter
!     bc0bht - height s-curve parameter
!     bc0blf - leaf partitioning parameter
!     bc0brp - rprd partitioning parameter
!     bc0ck  - extinction coeffficient (fraction)
!     bc0clf - leaf partitioning parameter
!     bc0crp - rprd partitioning parameter
!     bc0diammax - crop maximum plant diameter (m)
!     bc0dlf - leaf partitioning parameter
!     bc0drp - rprd partitioning parameter
!     bc0idc - crop type:annual,perennial,etc
!     bc0nam - crop name
!     bc0sla - specific leaf area (cm^2/g)
!     bc0ssa - biomass to stem area conversion coefficient a
!     bc0ssb - biomass to stem area conversion coefficient b
!     bcbaf  - biomass adjustment factor
!     bcbaflg - flag for biomass adjustment action
!         0     o normal crop growth
!         1     o find biomass adjustment factor for target yield
!         2     o use given biomass adjustment factor
!     bcdayap - number of days of growth completed since crop planted
!     bcdpop - number of plants per unit area (#/m^2)
!            - note: bcdstm/bcdpop gives the number of stems per plant
!     bcdstm - number of plant stems per unit area (#/m^2)
!            - note: bcdstm/bcdpop gives the number of stems per plant
!     bcehu0 - relative gdd at start of senescence
!     bcfleaf2stor - fraction of assimilate partitioned to leaf that is
!                    diverted to root store
!     bcfliveleaf - fraction of standing plant leaf which is living
!                   (transpiring)
!     bcfstem2stor - fraction of assimilate partitioned to stem that is
!                    diverted to root store
!     bcfstor2stor - fraction of assimilate partitioned to standing
!                    storage(reproductive) that is diverted to root store
!     bcgrainf - internally computed grain fraction of reproductive mass
!     bcgrf  - fraction of reproductive biomass that is yield
!     bchyfg - flag indicating the part of plant to apply the "grain
!              fraction", grf, to when removing that plant part for yield
!         0     grf applied to above ground storage (seeds, reproductive)
!         1     grf times growth stage factor (see growth.for) applied to
!               above ground storage (seeds, reproductive)
!         2     grf applied to all aboveground biomass (forage)
!         3     grf applied to leaf mass (tobacco)
!         4     grf applied to stem mass (sugarcane)
!         5     grf applied to below ground storage mass (potatoes,
!                   peanuts)
!     bcmbgstemz  - crop buried stem mass by layer (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstem  - crop flat stem mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)
!     bcmrootfiberz - crop root fibrous mass by soil layer (kg/m^2)
!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                     (tubers (potatoes, carrots), extended leaf (onion),
!                     seeds (peanuts))
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                     (head with seed, or vegetative head (cabbage,
!                     pineapple))
!     bcresid_int - residue intercept (kg/m^2)
!       harvest_residue = bcyld_coef(kg/kg) * yield + bcresid_int (kg/m^2)
!     bctmin - base temperature (deg. c)
!     bctopt - optimum temperature (deg. c)
!     bcxrow - crop row spacing (m)
!     bcxstm - mature crop stem diameter (m)
!     bcyld_coef - yield coefficient (kg/kg)
!       harvest_residue = bcyld_coef(kg/kg) * yield + bcresid_int (kg/m^2)
!     bcyraf - yield to biomass ratio adjustment factor
!     bczht  - crop height (m)
!     bczmrt - maximum root depth
!     bczmxc - maximum potential plant height (m)
!     bczrtd - root depth (m)
!     bhfwsf - water stress factor (ratio)
!     bhtsmn - daily minimum soil temperature (deg c)
!     bnslay - number of soil layers
!     bszlyd - depth from top of soil to botom of layer, m
!     bweirr - daily global radiation (mj/m^2)
!     bwtdmn - daily minimum air temperature (c)
!     bwtdmx - daily maximum air temperature (deg c)
!     cc0be - biomass conversion efficiency (kg/ha)/(mj/m^2)
!     hu_delay - fraction of heat units accummulated
!                based on incomplete vernalization and day length
!     ln - leaf number
 
!     + + + local variable definitions + + +
!     bhfwsf_adj - water stress factor adjusted by biomass adjustment
!                  factor
!     day - day of month
!     ddm_rem - increment in dry matter excluding fibrous roots(kg/m^2)
!     dead_mass - mass of living tissue that died today. currently use of
!                 this variable is commented out.
!     dht - daily height increment (m)
!     dlfwt - increment in leaf dry weight (kg/m^2)
!     doy - day of year
!     drfwt - increment in fibrous root weight (kg/m^2)
!     drpwt - increment in reproductive mass (kg/m^2)
!     drswt - biomass diverted from partitioning to root storage
!     dstwt - increment in dry weight of stem (kg/m^2)
!     ff - senescence factor (ratio)
!     ffa - leaf senescence factor (ratio)
!     ffr - fibrous root weight reduction factor (ratio)
!     ffw - leaf weight reduction factor (ratio)
!     frst - frost damage factor
!     gif  - grain index accounting for development of chaff before grain
!            fill
!     huf - heat unit factor for driving root depth, plant height
!           development
!     hufy - value of huf on day (i-1)
!     hui0f - relative gdd at start of scenescence
!     hux - relative gdd offset to start at scenescence
!     i - array index used in loops
!     irfiber - index of deepest soil layer for fibrous roots
!     irstore - index of deepest soil layer for storage roots
!     lost_mass - biomass that decayed (disappeared)
!     mo - month of year
!     p_lf - leaf partitioning ratio
!     p_lf_rp - sum of leaf and reproductive partitioning fractions
!     p_rp - reproductive partitioning ratio
!     p_rw - fibrous root partitioning ratio
!     p_st - stem partitioning ratio
!     par - photosynthetically active radiation (mj/m2)
!     pddm - increment in potential dry matter (kg)
!     pdht - increment in potential height (m)
!     pdiam - reach of growing plant (m)
!     pdrd - potential increment in root length (m)
!     stem_propor - fraction of stem mass increase allocated to standing
!                   stems (remainder goes flat)
!     strsdayhtinc - today's increase in height after the strs variable has been applied.
!     temp_fiber - a temporary variable that holds the sum of the crop
!                  root fibrous mass by soil layer (bcmrootfiberz)
!                  (debe definition)
!     temp_sai - a temporary variable for crop stem area index
!               (debe definition)
!     temp_stem - a temporary variable for the sum of the crop buried
!                 stem mass by layer (bcmbgstemz)  (debe definition)
!     temp_stmrep - a temporary variable used in calculating a stem
!                   representative diameter (debe definition)
!     temp_store - a temporary variable to hold the sum of the crop root
!                  storage mass by soil layer (bcmrootstorez)
!                  (debe definition)
!     trad_lai - leaf area index based on whole field area (traditional)
!     wcg - root mass distribution function exponent (see reference at
!           equation)
!     wffiber - total of weight fractions for fibrous roots
!               (normalization)
!     wfl(mnsz) - weight fraction by layer (distribute root mass into the
!                 soil layers)
!     wfstore - total of weight fractions for storage roots
!               (normalization)
!     wmaxd - root mass distribution function depth limit parameter
!     xw - absolute value of minimum temperature
!     yr - year
 
!     + + + common block variables definitions + + +
!     a_fr - parameter in the frost damage s-curve
!     am0cfl - flag to print crop submodel output
!     apar - intercepted photosynthetically active radiation (mj/m2)
!     arg_exp - argument calculated for exponential function
!               (to test for validity)
!     b_fr - parameter in the frost damage s-curve
!     clfarea - leaf area (m^2/plant)
!     clfwt - leaf dry weight (kg/plant)
!     cook_yield - flag setting which uses input from crop record to
!                  guarantee a fixed yield/redsidue ratio at harvest
!     daysim - day of the simulation
!     ddm - stress modified increment in dry matter (kg/m^2)
!     eff_lai - single plant effective leaf area index (based on maximum
!               single plant coverage area)
!     growth_stress - flag setting which turns on water or temperature
!                   stress (or both)
!                   growth_stress = 0  ! no stress values applied
!                   growth_stress = 1  ! turn on water stress
!                   growth_stress = 2  ! turn on temperature stress
!                   growth_stress = 3  ! turn on both
!      debe because it is now being read in, it is commented out in command.inc
!     hatom2 - parameter - hectare to square meters. to convert
!                          hectares to square meters, multiply by 10,000.
!     hui - heat unit index (ratio of acthucum to acthum)
!     huirt - heat unit index for root expansion (ratio of actrthucum to
!             acthum)
!     huirty - heat unit index for root expansion (ratio of actrthucum to
!              acthum) on day (i-1).
!     huiy - heat unit index (ratio of acthucum to acthum) on day (i-1)
!     max_arg_exp - maximum value allowed for argument to exponential
!                   function without overflowing.
!     max_real    - maximum real number allowed.
!     mmtom  - unit conversion constant (m/mm).
!     mnsz -
!     parea - aerial extent occupied by plant leaf (m^2/plant)
!     pcht - potential plant height for today
!     pchty - potential plant height from previous day
!     prd - potential root depth today
!     prdy - potential root depth from previous day
!     strs - stress factor. Includes water temperature stresses only.
!     ts - temperature stress factor
!     water_stress_max - cap water stress at some maximum value
!                        (note maximum stress occurs at 0.0 and minimum
!                        stress at 1.0).
!                        water_stress_max = x.xx  !specified stress limit
!     winter_ann_root - select root growth option for winter annuals
!                       winter_ann_root = 0  ! root depth grows at same
!                       rate as height.
!                       winter_ann_root = 1  ! root depth grows with fall
!                       heat units.
!     za(mnsz) - soil layer representative depth
 
!     + + +  newly added arguments definitions + + +
!     antss - start of anthesis growth stage for hay millet, proso millet,
!             sorghum (first bloom), spring barley, spring wheat,
!             sunflower, winter barley and winter wheat. also, dry beans
!             and corn. this array includes daynum, year, month and day
!             of when this stage was reached.
!     boots - start of booting/flag leaf complete growth stage. this array includes
!             daynum, year, month and day of when this stage was reached.
!     canht - holds the final canopy height of the crop calculated with
!             the phenologymms way. it comes in as cm and gets converted to m.
!     canhty - holds yesterday's canopy height value. used in printing to canopyht.out.
!              this would be the stressed value from yesterday.
!     canopyflg - a flag to indicate if the weps/upgm method to calculate
!                 plant height will be used. value will then be 0. if using
!                 the phenologymms method, the value will be 1.
!     co2atmos - the atmospheric level of CO2.
!     co2eff - the effect of the atmospheric co2 level. Used to affect biomass.
!              The adjustment factor.
!     co2x - the CO2 levels in ppm. The x axis on the relationship curve.
!     co2y - the relative effect at different levels of CO2, i.e. co2x.
!     dayhtinc - the increase in plant height for today.
!     gddday - the number of gdd with 0°C base temperature for that day
!     heads - the start of heading stage of growth. this array includes daynum,
!             year, month and day of when this stage was reached.
!     joints - start of jointing stage. this array includes daynum, year, month and day
!              of when this stage was reached.
!     mats - start of maturity stage. this array includes daynum, year, month and day
!            of when this stage was reached.
 
!     + + + currently unused variables + + +
!     stem_area_index - stem silhoutte area per unit ground area (m^2/m^2)
!                       this variable is not currently used.
!     temp_ht - temporary height variable (m). this variables is not
!               currently used.
 
      ! used with plant population adjustment
!     ppx
!     ppveg
!     pprpd
 
!     + + + common blocks + + +
 
!     + + + functions called + + +
 
!     + + + subroutines called + + +
!     caldatw
!     nuse       !disabled
!     najn       !disabled
!     najna      !disabled
!     nuts       !disabled
!     waters     !disabled
 
!     + + + end of specifications + + +


call caldatw(day,mo,yr)
              !function
          !  and weight fraction by layer used to distribute root mass
          !  into the soil layers
doy = dayear(day,mo,yr)
 
!     reduce green leaf mass in freezing weather
if (bhtsmn(1)<-2.0) then
!          xw=abs(bwtdmn)
!         use daily minimum soil temperature of first layer to account for
!         snow cover effects
  xw = abs(bhtsmn(1))
          ! this was obviously to prevent excessive leaf loss
          ! frst=sqrt((1.-xw/(xw+exp(a_fr-b_fr*xw)))+0.000001)
          ! frst=sqrt(frst)
          ! tested to match the values input in the database
  frst = xw/(xw+exp(a_fr-b_fr*xw))
  frst = min(1.0,max(0.0,frst))
 
          ! eliminate these in favor of dead to live ratio
          ! reduce green leaf area due to frost damage (10/1/99)
          ! dead_mass = bcmstandleaf * bcfliveleaf * frst
          ! bcmstandleaf = bcmstandleaf - dead_mass
          ! bcmflatleaf = bcmflatleaf + dead_mass
 
          ! reduce green leaf area due to frost damage (9/22/2003)
  bcfliveleaf = bcfliveleaf*(1.0-frst)
 
          ! these are set here to show up on the output as initialized
  p_rw = 0.0
  p_lf = 0.0
  p_st = 0.0
  p_rp = 0.0
else
  frst = 0.0
end if
 
!debe added to set break points for different winter wheat developmental stages:
!if(doy .eq. joints(1)) then
! print *, 'eff_lai = ', eff_lai, 'pddm - ', pddm, 'parea = ', parea
!elseif (doy .eq. boots(1)) then
!  print *, 'eff_lai = ', eff_lai, 'pddm - ', pddm, 'parea = ', parea
!elseif (doy .eq. heads(1)) then
!    print *, 'eff_lai = ', eff_lai, 'pddm - ', pddm, 'parea = ', parea
!elseif (doy .eq. antss(1)) then
!    print *, 'eff_lai = ', eff_lai, 'pddm - ', pddm, 'parea = ', parea
!elseif (doy .eq. mats(1)) then
!    print *, 'eff_lai = ', eff_lai, 'pddm - ', pddm, 'parea = ', parea
!endif
!
 
      !!!!! start single plant calculations !!!!!
      ! calculate single plant effective lai (standing living leaves only)
clfwt = bcmstandleaf/bcdpop             ! kg/m^2 / plants/m^2 = kg/plant
clfarea = clfwt*bc0sla*bcfliveleaf      ! kg/plant * m^2/kg = m^2/plant
 
      ! limiting plant area to a feasible plant area results in a
      ! leaf area index covering the "plant's area"
      ! 1/(#/m^2) = m^2/plant. plant diameter now used to limit leaf
      ! coverage to present plant diameter.
      ! find present plant diameter (proportional to diam/height ratio)
      !pdiam = min( 2.0*bczht * max(1.0, bc0diammax/bczmxc), bc0diammax )
      ! this expression above may not give correct effect since it is
      ! difficult to correctly model plant area expansion without additional
      ! plant parameters and process description. presently using leaf area
      ! over total plant maximum area before trying this effect. reducing
      ! effective plant area can only reduce early season growth.
pdiam = bc0diammax
      ! account for row spacing effects
if (bcxrow>0.0) then
          ! use row spacing and plants maximum reach
  parea = min(bcxrow,pdiam)*min(1.0/(bcdpop*bcxrow),pdiam)
else
          ! this is broadcast, so use uniform spacing
  parea = min(pi*pdiam*pdiam/4.0,1.0/bcdpop)
end if
 
      ! check for valid plant area
if (parea>0.0) then
  eff_lai = clfarea/parea
else
  eff_lai = 1.0
end if
 
      !traditional lai calculation for reporting puposes
trad_lai = clfarea*bcdpop
 
!     start biomass calculations
!     bweirr is total shortwave radiation and a factor of .5 is assumed
!     to get to the photosynthetically active radiation
par = 0.5*bweirr                        ! mj/m^2                ! c-4
 
!     calculate intercepted par, which is the good stuff less what hits
!     the ground
apar = par*(1.-exp(-bc0ck*eff_lai))                             ! c-4
 
!     calculate potential biomass conversion (kg/plant/day) using
!     biomass conversion efficiency at ambient co2 levels
! units: ((m^2)/plant)*(kg/ha)/(mj/m^2) * (mj/m^2)
!           / 10000 m^2/ha = kg/plant

!
    
!co2eff = CO2 effect on the plant growth rate. The adjustment factor.
j=0
k=10
    do 100 j=2,k
       if(co2atmos .GT. co2x(j)) go to 100
       go to 200
100 continue   
    j = k 
200 co2eff = (co2atmos-co2x(j-1))*(co2y(j)-co2y(j-1))/(co2x(j)-co2x(j-1))   &
              & +co2y(j-1) 

!print *, 'co2eff = ', co2eff

pddm = parea*cc0be*apar/hatom2                                  ! c-4
!print *, 'pddm before co2 = ', pddm

!affect dry matter by co2
pddm = pddm * co2eff
!print *, 'pddm AFTER co2 = ', pddm 

!     biomass adjustment factor applied
      ! apply to both biomass converstion efficiency and water stress
      ! factor, see below
pddm = pddm*bcbaf
 
      ! these were attempts at compensating for low yield as a result of
      ! water stress. (ie. this is the cause of unrealistically low yield)
      ! these methods had many side effects and were abandoned
      ! if( bcbaf .gt. 1.0 ) then
          ! first attempt. reduces water stress in the middle stress region
          !  = bhfwsf ** (1.0/(bcbaf*bcbaf))
          ! second attempt. reduces extreme water stress (zero values).
          !  = min( 1.0, max( bhfwsf, bcbaf-1.0 ) )
      ! else
          !  = bhfwsf
      ! end if
bhfwsf_adj = max(water_stress_max,bhfwsf)
      !bhfwsf_adj = 1 !no water stress
 
!     begin stress factor section
 
!     calculate n & p demand and supply
!      call nuse
!     calculate n & p uptake with increase in supply if necessary
!      call najn
!      call najna
!     calculate n stress
!      call nuts (un1,un2,sn)
!      call nuts (sun,un2,sn)
!     calculate p stress
!      call nuts (up1,up2,sp)
 
!     calculate temperature stress
ts = temps(bwtdmx,bwtdmn,bctopt,bctmin)
 
      ! select application of stress functions based on command line flag
if (growth_stress==0) then
  strs = 1.0
else if (growth_stress==1) then
  strs = bhfwsf_adj
else if (growth_stress==2) then
  strs = ts
else if (growth_stress==3) then
  strs = min(ts,bhfwsf_adj)
end if
 
      ! until shoot breaks surface, no solar driven growth
      ! call it lack of light stress
if (bczht<=0.0) strs = 0.0
 
      ! left here to show some past incantations of stress factors
!      strs=min(sn,sp,ts,bhfwsf)
!      if (hui.lt.0.25) strs=strs**2
!      if (hui.gt.huilx) strs=sqrt(strs)
 
 
      ! apply stress factor to generated biomass
ddm = pddm*strs
!     end stress factor section
 
      ! convert from mass per plant to mass per square meter
      ! + kg/plant * plant/m^2 = kg/m^2
ddm = ddm*bcdpop
pddm = pddm*bcdpop !de added to convert pddm mass per plant to mass per sq. m
 
      !!!!! end single plant calculations !!!!!
 
      ! find partitioning between fibrous roots and all other biomass
      ! root partition done using root heat unit index, which is not
      ! reset when a harvest removes all the leaves. this index also
      ! is not delayed in prevernalization winter annuals. made to
      ! parallel winter annual rooting depth flag as well.
if (winter_ann_root==0) then
  p_rw = (.4-.2*hui)                                                    ! c-5
else
  p_rw = max(0.05,(.4-.2*huirt))                                        ! c-5
end if
drfwt = ddm*p_rw
ddm_rem = ddm - drfwt
 
!     find partitioning factors of the remaining biomass
!      (not fibrous root)
!     calculate leaf partitioning.
      !debe try adding phenologymms growth stage variable and phenolflg to affect p_lf
arg_exp = -(hui-bc0clf)/bc0dlf
if (arg_exp>=max_arg_exp) then
    p_lf = bc0alf + bc0blf/max_real
else
    p_lf = bc0alf + bc0blf/(1.+exp(-(hui-bc0clf)/bc0dlf))
end if
p_lf = max(0.0,min(1.0,p_lf))
!     calculate reproductive partitioning based on partioning curve
arg_exp = -(hui-bc0crp)/bc0drp
if (arg_exp>=max_arg_exp) then
  p_rp = bc0arp + bc0brp/max_real
else
  p_rp = bc0arp + bc0brp/(1.+exp(-(hui-bc0crp)/bc0drp))
end if
p_rp = max(0.0,min(1.0,p_rp))
 
      ! normalize leaf and reproductive fractions so sum never greater
      ! than 1.0
p_lf_rp = p_lf + p_rp
if (p_lf_rp>1.0) then
  p_lf = p_lf/p_lf_rp
  p_rp = p_rp/p_lf_rp
          ! set stem partitioning parameter.
  p_st = 0.0
else
          ! set stem partitioning parameter.
  p_st = 1.0 - p_lf_rp
end if
 
      ! calculate assimate mass increments (kg/m^2)
dlfwt = ddm_rem*p_lf
dstwt = ddm_rem*p_st
drpwt = ddm_rem*p_rp
 
      ! use ratios to divert biomass to root storage
drswt = dlfwt*bcfleaf2stor + dstwt*bcfstem2stor + drpwt*bcfstor2stor
dlfwt = dlfwt*(1.0-bcfleaf2stor)
dstwt = dstwt*(1.0-bcfstem2stor)
drpwt = drpwt*(1.0-bcfstor2stor)
 
      ! senescence is done on a whole plant mass basis not incremental
      ! mass this starts scencescence before the entered heat unit index
      ! for the start of scencscence. for most leaf partitioning
      ! functions the coefficients draw a curve that approaches 1
      ! around -0.5 but the value at zero, raised to fractional powers
      ! still very small
hui0f = bcehu0 - bcehu0*.1
if (hui>=hui0f) then
  hux = hui - bcehu0
  ff = 1./(1.+exp(-(hux-bc0clf/2.)/bc0dlf))
  ffa = ff**0.125
  ffw = ff**0.0625
  ffr = 0.98
          ! loss from weathering of leaf mass
  lost_mass = bcmstandleaf*(1.0-ffw)
          ! adjust for senescence (done here, not below, so consistent
          !  with lost mass amount)
  bcmstandleaf = bcmstandleaf*ffw
          ! change in living mass fraction due scenescence
          ! and accounting for weathering mass loss of dead leaf
  bcfliveleaf = ffa*bcfliveleaf/(1.0+bcfliveleaf*(ffw-1.0))
else
          ! set a value to be written out
  ffa = 1.0
  ffw = 1.0
  ffr = 1.0
  lost_mass = 0.0
end if
 
      ! yield residue relationship adjustment
 
if ((cook_yield==1).and.(bcyld_coef>1.0).and.(bcresid_int>=0.0).and.            &
  & ((bchyfg==0).or.(bchyfg==1).or.(bchyfg==5)))                                &
  & call cookyield(bchyfg,bnslay,dlfwt,dstwt,drpwt,drswt,bcmstandstem,          &
  & bcmstandleaf,bcmstandstore,bcmflatstem,bcmflatleaf,bcmflatstore,            &
  & bcmrootstorez,lost_mass,bcyld_coef,bcresid_int,bcgrf)
 
!     *****plant height*****
!     added method (different from epic) of calculating plant height
!     huf - heat unit factor for driving root depth, plant height
!           development
!     hufy - value of huf on day (i-1)
!     pht - cumulated potential height
!     pdht - daily potential height
!     pcht - potential plant height for today
!     pchty - potential plant height from previous day
!     bczmxc - maximum potential plant height (m)
!     aczht(am0csr) - cumulated actual height
!     adht - daily actual height
!     dht - daily height increment (m)
!     bc0aht, bc0bht are height-scurve parameters (formerly lai parameters)
 
          ! previous day
hufy = .01 + 1./(1.+exp((huiy-bc0aht)/bc0bht))
          ! today
huf = .01 + 1./(1.+exp((hui-bc0aht)/bc0bht))
 
pchty = min(bczmxc,bczmxc*hufy)
pcht = min(bczmxc,bczmxc*huf)
pdht = pcht - pchty
 
!debe consolidate code so lines are not repeated. if statements include only
! the call to ht_dia_sai and setting the height.
 
          ! calculate stress adjusted height
dht = pdht*strs
  ! print*, 'dht =', dht, 'pdht =', pdht, 'strs =', strs
      ! add mass increment to accumulated biomass (kg/m^2)
      ! all leaf mass added to living leaf in standing pool
if (dlfwt>0.0) then
              ! recalculate fraction of leaf which is living
  bcfliveleaf = (bcfliveleaf*bcmstandleaf+dlfwt)/(bcmstandleaf+dlfwt)
              ! next add in the additional mass
  bcmstandleaf = bcmstandleaf + dlfwt
end if
 
          ! divide between standing and flat stem and storage in proportion
          ! to maximum height and maximum radius ratio
stem_propor = min(1.0,2.0*bczmxc/bc0diammax)
bcmstandstem = bcmstandstem + dstwt*stem_propor
bcmflatstem = bcmflatstem + dstwt*(1.0-stem_propor)
 
          ! for all but below ground place rp portion in standing storage
bcmstandstore = bcmstandstore + drpwt*stem_propor
bcmflatstore = bcmflatstore + drpwt*(1.0-stem_propor)
 
          ! check for consistency of height, diameter and stem area index.
          ! adjust rate of height increase to keep diameter inside a range.
if (canopyflg==0) then
  call ht_dia_sai(bcdpop,bcmstandstem,bc0ssa,bc0ssb,bcdstm,bcxstm,bczmxc,bczht, &
                & dht,temp_stmrep,temp_sai)
 
          ! increment plant height
  bczht = bczht + dht !cummulated height plus today's height which has been stressed.
  strsdayhtinc = 0
!  print *, 'bczht =', bczht, 'dht =', dht, 'strs =', strs
else if ((canopyflg==1).and.(antss(1)==999)) then         !use canht from phenologymms method
 
!debe added the following strs detemininations as above so that stress can be applied within the
! canopy height method from phenologymms and not be related to the weps calculated day
! of emergence. otherwise, if upgm shows emergence and canht has a value greater than 0.0 but
! weps has not accomplished emergence yet and bczht is still 0.0, then the strs value is set
! to 0.0. when strs is multiplied by the dayhtinc from the upgm method then the day's height
! increase is set back to 0.0. on the first day of emergence, canht comes in as 0.0 and if
! dayhtinc has been set back to 0.0 there would be no increase in height on the first day of
! emergence if weps has not also arrived at emergence.
 
          ! select application of stress functions based on command line flag
  if (growth_stress==0) then
     strs = 1.0
  else if (growth_stress==1) then
     strs = bhfwsf_adj
  else if (growth_stress==2) then
     strs = ts
  else if (growth_stress==3) then
     strs = min(ts,bhfwsf_adj)
  end if
 
      ! until shoot breaks surface, no solar driven growth
      ! call it lack of light stress
  if (canht<=0.0) strs = 0.0
 
!subtract today's height increase from total canopy height. the stress factor
!(strs) will be applied  to today's height increase. it will then be added back to
!canht to get the total canopy height for today with stress applied.
  canht = canht - dayhtinc
 
 ! apply stress to dayhtinc from canopyht giving strsdayhtinc
  strsdayhtinc = dayhtinc*strs
  !print*,'strs =', strs, 'dayhtinc =', dayhtinc,'strsdayhtinc =', strsdayhtinc
  !debe - i believe strsdayhtinc is comparable to dht and is what should be passed to ht_dia_sai.
 
          ! check for consistency of height, diameter and stem area index.
          ! adjust rate of height increase to keep diameter inside a range.
  call ht_dia_sai(bcdpop,bcmstandstem,bc0ssa,bc0ssb,bcdstm,bcxstm,bczmxc,bczht, &
                & strsdayhtinc,temp_stmrep,temp_sai) !dayhtinc
 
 
  !add back in today's height increase to which stress has been applied.
  canht = canht + strsdayhtinc
!    print*, 'canht =', canht
  !don't let canopy height shrink.
  if (canht<canhty) canht = canhty
 
          ! increment plant height and store in weps bczht to be used in other areas of upgm.
          !if emergence has not occurred, i.e. canht = 0.0, don't let bzcht be set equal to 0.0
   ! bczht = bczht + (canht/100) this gives incredibly tall plants!
  if (canht>0.0) bczht = canht/100
 
  canhty = canht !set yesterday's canopy height value to today's value for use tomorrow.
else
    temp_sai = 0.
    temp_stmrep = 0.
    strsdayhtinc = 0
end if
 
 
      ! root mass distributed by layer below after root depth set
 
!     calculate rooting depth (eq. 2.203) and check that it is not deeper
!     than the maximum potential depth, and the depth of the root zone.
!     this change from the epic method is undocumented!! it says that
!     root depth starts at 10cm and increases from there at the rate
!     determined by huf. the 10 cm assumption was prevously removed from
!     elsewhere in the code and is subsequently removed here. the initial
!     depth is now set in crop record seeding depth, and  the function
!     just increases it. this is now based on a no delay heat unit
!     accumulation to allow rapid root depth development by winter
!     annuals.
if (winter_ann_root==0) then
  prdy = min(bczmrt,bczmrt*hufy+0.1)
  prd = min(bczmrt,bczmrt*huf+0.1)
else
  prdy = bczmrt*(.01+1.0/(1.0+exp((huirty-bc0aht)/bc0bht)))
  prd = bczmrt*(.01+1.0/(1.0+exp((huirt-bc0aht)/bc0bht)))
end if
pdrd = max(0.0,prd-prdy)
bczrtd = min(bczmrt,bczrtd+pdrd)
bczrtd = min(bszlyd(bnslay)*mmtom,bczrtd)
 
      ! determine bottom layer # where there are roots
      ! and calculate root distribution function
      ! the root distribution functions were taken from
      ! agron. monog. 31, equ. 26 on page 99. wcg should be a crop
      ! parameter. (impact is probably small
      ! since this is only affecting mass distribution, not water uptake)
      ! wcg = 1.0 for sunflowers (deep uniform root distribution)
      ! wcg = 2.0 for corn and soybeans
      ! wcg = 3.0 for sorghum (alot of roots close to the surface)
      ! wmaxd could also be a parameter but there is insufficient info
      ! to indicate how the values would vary the shape of the
      ! distribution. the article indicates that it must be greater than
      ! maximum root depth.
wcg = 2.0
wmaxd = max(3.0,bczmrt)
do i = 1,bnslay
  if (i==1) then
              ! calculate depth to the middle of a layer
     za(i) = (bszlyd(i)/2.0)*mmtom
              ! calculate root distribution function
     if (za(i)<wmaxd) then
        wfl(i) = (1.0-za(i)/wmaxd)**wcg
     else
        wfl(i) = 0.0
     end if
     wfstore = wfl(i)
     irstore = i
     wffiber = wfl(i)
     irfiber = i
  else
              ! calculate depth to the middle of a layer
     za(i) = (bszlyd(i-1)+(bszlyd(i)-bszlyd(i-1))/2.0)*mmtom
              ! calculate root distribution function
     if (za(i)<wmaxd) then
        wfl(i) = (1.0-za(i)/wmaxd)**wcg
     else
        wfl(i) = 0.0
     end if
     if (bczrtd/3.0>za(i)) then
        wfstore = wfstore + wfl(i)
        irstore = i
     end if
              ! check if reached bottom of root zone
     if (bczrtd>za(i)) then
        wffiber = wffiber + wfl(i)
        irfiber = i
     end if
  end if
end do
 
      ! distribute root weight into each layer
do i = 1,irfiber
  if (i<=irstore) bcmrootstorez(i) = bcmrootstorez(i) + (drswt*wfl(i)/wfstore)
  bcmrootfiberz(i) = bcmrootfiberz(i) + (drfwt*wfl(i)/wffiber)
          ! root senescence : 02/16/2000 (a. retta)
  bcmrootfiberz(i) = bcmrootfiberz(i)*ffr
end do
 
      ! this factor prorates the grain reproductive fraction (grf) defined
      ! in the database for crop type 1, grains. compensates for the
      ! development of chaff before grain filling, ie., grain is not
      ! uniformly a fixed fraction of reproductive mass during the entire
      ! reproductive development stage.
gif = 1./(1.0+exp(-(hui-0.64)/.05))
if (bchyfg==1) then
  bcgrainf = bcgrf*gif
else
  bcgrainf = bcgrf
end if
 
!     the following write statements are for 'crop.out'
!     am0cfl is flag to print crop submodel output
if (am0cfl>=1) then
!          ! temporary sum for output
  temp_store = 0.0
 
  temp_fiber = 0.0
  temp_stem = 0.0
  do i = 1,bnslay
     temp_store = temp_store + bcmrootstorez(i)
     temp_fiber = temp_fiber + bcmrootfiberz(i)
     temp_stem = temp_stem + bcmbgstemz(i)
  end do
  
  write (luocrop,1000) daysim,doy,yr,bcdayap,hui,bcmstandstem,bcmstandleaf,     &
                     & bcmstandstore,bcmflatstem,bcmflatleaf,bcmflatstore,      &
                     & temp_store,temp_fiber,temp_stem,                     &
                     & bcmstandleaf + bcmflatleaf,bcmstandstem + bcmflatstem +  &
                     & temp_stem,bczht,bcdstm,trad_lai,eff_lai,bczrtd,bcgrainf, &
                     & ts,bhfwsf,frst,ffa,ffw,par,apar,pddm,p_rw,p_st,p_lf,p_rp,&
                     & stem_propor,pdiam,parea,pdiam/bc0diammax,parea*bcdpop,   &
                     & hu_delay,temp_sai,temp_stmrep,bc0nam,gddday,ln
  
 !DE moves thes out of the write to luocrop above. They are not used here now:
  !& temp_store,temp_fiber,temp_stem,                         &
 
 !bcmstandstem + bcmflatstem + temp_stem, &
  !used acmshoot in place of temp_stem in the write statement
                       
  end if 

write (luocanopyht,1100) daysim,doy,day,'/',mo,'/',yr,canopyflg,dht,strs,bczht, &
                       & dayhtinc,strsdayhtinc,canhty,canht,antss(1)
!
 1000 format (1x,i5,1x,i3,1x,i4,1x,i4,1x,f6.3,12(1x,f7.4),1x,f7.2,3(1x,f7.4),   &
            & 8(1x,f6.3),1x,e12.3,10(1x,f6.3),2(1x,f8.5),' ',a20,f8.3,3x,f5.2)
 
 1100 format (2x,i3,2x,i3,2x,i2,a1,i2,a1,i2,5x,i1,5x,f6.4,2x,f6.4,2x,f6.4,2x,   &
            & f6.4,5x,f6.4,4x,f12.8,4x,f12.8,5x,i3)
 
!
end subroutine growth

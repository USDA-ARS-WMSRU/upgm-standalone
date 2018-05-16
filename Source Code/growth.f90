    subroutine growth(ctrl,clidat, bio,soils,a_fr,b_fr,hu_delay,daysim,gddday,canhty,ln,co2eff,partcoefleaf,partcoefstem,partcoefrepro,useupgmpart)
    !debe gddday added to print out gdd.
    !debe added bio%upgm%growth_stress because it is now read in.
    !debe added bio%upgm%canopyflg to be used to determine which method of calculating canopy
    !height will be used. 0=weps/upgm method and 1=phenologymms.
    !debe added bio%upgm%dayhtinc to be able to pass the daily increase in height to growth
    ! for the ht_dia_sai subroutine in place of the weps/upgm variable dht when
    ! bio%upgm%canopyflg = 1.
    !debe added bio%upgm%antss to the growth subroutine to limit calculating canopy height after anthesis.
    !debe added canhty to store yesterday's canopy height value.
    !debe added bio%upgm%phenolflg and bio%upgm%boots to begin incorporating phenologymms tie in to upgm.
    !debe added bio%upgm%joints, bio%upgm%heads and bio%upgm%mats to help with debugging to specific growth stages.
    !debe added bio%upgm%co2x, bio%upgm%co2y, clidat%co2atmos, co2eff for use in affecting plant growth by co2.
    !
    use constants, only : hatom2, mmtom, max_arg_exp, max_real, pi, mnsz
    use upgm_simdata, only : controls
    use datetime, only : dayear, caldat
    use climate, only : climate_data
    use soil, only : soildata
    use biomaterial
    implicit none
    !DE added
    !
    ! Dummy arguments
    !
    type(controls), intent(inout) :: ctrl
    type(biomatter), intent(inout) :: bio
    type(climate_data), intent(inout) :: clidat
    type(soildata),     intent(inout) :: soils
    real :: a_fr,b_fr,canhty,gddday,hu_delay,todayln,ln,co2eff,partcoefleaf,partcoefstem,partcoefrepro
    logical :: useupgmpart
    integer :: daysim
    !
    ! Local variables
    !
    real :: apar,arg_exp,bhfwsf_adj,ddm_rem,dht,dlfwt,drfwt,drpwt,drswt,dstwt,      &
        & eff_lai,ff,ffa,ffr,ffw,frst,gif,huf,hufy,hui0f,hux,lost_mass,par,pdht,  &
        & pdiam,pdrd,p_lf,p_lf_rp,p_rp,p_rw,p_st,stem_propor,strsdayhtinc,        &
        & temp_sai,temp_stmrep,wcg,wffiber,wfstore,wmaxd,xw,temp_fiber,           &
        & temp_stem,temp_store, uself,usest,userp
    !      trad_lai,pddm, DE moved these out of the real declaration above.

    !these are in the common block cgrow.inc, that is now included so DE moved
    !these out of the real declaration above.:
    !clfarea,clfwt,ddm,parea,pcht,pchty,prd,prdy,strs,

    integer :: day,doy,i,irfiber,irstore,mo,yr,j,k
    real :: temps
    real,dimension(mnsz) :: wfl,za
    real :: clfarea
    real :: clfwt
    real :: parea
    real :: pddm
    real :: ddm
    real :: pcht
    real :: pchty
    real :: prd
    real :: prdy
    real :: strs
    !
    !debe 061009 removed variables that were to be used for canopyht because canopyht
    !  is now called from crop: bio%upgm%antss(4), bio%upgm%canht, cropname, dummy2, ems, gdde,
    !  ies, bio%upgm%joints, lf4s, maxht. later canopyht was moved and is called from the
    !  new subroutine phenolmms. later added back bio%upgm%joints, as well as bio%upgm%mats for ease in
    !  debugging to a specific growth stage.
    !2/20/15 canopyht is called from PhenolMMS.
    !debe added bio%upgm%growth_stress because it is now read in.
    !debe added bio%upgm%antss to control the end of calculating canopy height when bio%upgm%canopyflg = 1
    ! and anthesis stage has been passed. plant height should not increase after anthesis.
    !debe added a local variable strsdayhtinc to hold the bio%upgm%dayhtinc value after stress has been applied.
    ! debe added canhty to holds yesterday's canopy height to print in canopyht.out.

    !     the following subroutine arguments are not used: bio%database%idc, bio%database%baflg,
    !     bio%growth%thu_shoot_end, bio%database%yraf   jcaii  8/08/08
    !
    !     author : amare retta
    !     + + + purpose + + +
    !     this subroutine calculates plant height, biomass partitioning,
    !     rootmass distribution, rooting depth.

    !     + + + keywords + + +
    !     biomass

    !     + + + argument declarations + + +

    !     + + + argument definitions + + +
    !     bio%database%aht (ac0aht) - height s-curve parameter
    !     bio%database%alf (ac0alf) - leaf partitioning parameter
    !     bio%database%arp - rprd partitioning parameter
    !     bio%database%bht - height s-curve parameter
    !     bio%database%blf - leaf partitioning parameter
    !     bio%database%brp - rprd partitioning parameter
    !     bio%database%ck  - extinction coeffficient (fraction)
    !     bio%database%clf - leaf partitioning parameter
    !     bio%database%crp - rprd partitioning parameter
    !     bio%database%diammax - crop maximum plant diameter (m)
    !     bio%database%dlf - leaf partitioning parameter
    !     bio%database%drp - rprd partitioning parameter
    !     bio%database%idc - crop type:annual,perennial,etc
    !     bio%bname - crop name
    !     bio%database%sla - specific leaf area (cm^2/g)
    !     bio%database%ssa - biomass to stem area conversion coefficient a
    !     bio%database%ssb - biomass to stem area conversion coefficient b
    !     bio%database%baf  - biomass adjustment factor
    !     bio%database%baflg - flag for biomass adjustment action
    !         0     o normal crop growth
    !         1     o find biomass adjustment factor for target yield
    !         2     o use given biomass adjustment factor
    !     bio%growth%dayap - number of days of growth completed since crop planted
    !     bio%geometry%dpop - number of plants per unit area (#/m^2)
    !            - note: bio%geometry%dstm/bio%geometry%dpop gives the number of stems per plant
    !     bio%geometry%dstm - number of plant stems per unit area (#/m^2)
    !            - note: bio%geometry%dstm/bio%geometry%dpop gives the number of stems per plant
    !     bio%database%ehu0 - relative gdd at start of senescence
    !     bio%database%fleaf2stor - fraction of assimilate partitioned to leaf that is
    !                    diverted to root store
    !     bio%growth%fliveleaf - fraction of standing plant leaf which is living
    !                   (transpiring)
    !     bio%database%fstem2stor - fraction of assimilate partitioned to stem that is
    !                    diverted to root store
    !     bio%database%fstor2stor - fraction of assimilate partitioned to standing
    !                    storage(reproductive) that is diverted to root store
    !     bio%geometry%grainf - internally computed grain fraction of reproductive mass
    !     bio%database%grf  - fraction of reproductive biomass that is yield
    !     bio%geometry%hyfg - flag indicating the part of plant to apply the "grain
    !              fraction", grf, to when removing that plant part for yield
    !         0     grf applied to above ground storage (seeds, reproductive)
    !         1     grf times growth stage factor (see growth.for) applied to
    !               above ground storage (seeds, reproductive)
    !         2     grf applied to all aboveground biomass (forage)
    !         3     grf applied to leaf mass (tobacco)
    !         4     grf applied to stem mass (sugarcane)
    !         5     grf applied to below ground storage mass (potatoes,
    !                   peanuts)
    !     bio%mass%stemz  - crop buried stem mass by layer (kg/m^2)
    !     bio%mass%flatleaf  - crop flat leaf mass (kg/m^2)
    !     bio%mass%flatstem  - crop flat stem mass (kg/m^2)
    !     bio%mass%flatstore - crop flat storage mass (kg/m^2)
    !     bio%mass%rootfiberz - crop root fibrous mass by soil layer (kg/m^2)
    !     bio%mass%rootstorez - crop root storage mass by soil layer (kg/m^2)
    !                     (tubers (potatoes, carrots), extended leaf (onion),
    !                     seeds (peanuts))
    !     bio%mass%standleaf - crop standing leaf mass (kg/m^2)
    !     bio%mass%standstem - crop standing stem mass (kg/m^2)
    !     bio%mass%standstore - crop standing storage mass (kg/m^2)
    !                     (head with seed, or vegetative head (cabbage,
    !                     pineapple))
    !     bio%database%resid_int - residue intercept (kg/m^2)
    !       harvest_residue = bio%database%yld_coef(kg/kg) * yield + bio%database%resid_int (kg/m^2)
    !     bio%database%tmin - base temperature (deg. c)
    !     bio%database%topt - optimum temperature (deg. c)
    !     bio%geometry%xrow - crop row spacing (m)
    !     bio%database%xstm - mature crop stem diameter (m)
    !     bio%database%yld_coef - yield coefficient (kg/kg)
    !       harvest_residue = bio%database%yld_coef(kg/kg) * yield + bio%database%resid_int (kg/m^2)
    !     bio%database%yraf - yield to biomass ratio adjustment factor
    !     bio%geometry%zht  - crop height (m)
    !     bio%database%zmrt, - maximum root depth
    !     bio%database%zmxc - maximum potential plant height (m)
    !     bio%geometry%zrtd - root depth (m)
    !     ctrl%cropstress%ahfwsf - water stress factor (ratio)
    !     soils%spp%ahtsmn - daily minimum soil temperature (deg c)
    !     soils%spp%nslay - number of soil layers
    !     soils%spp%aszlyd - depth from top of soil to botom of layer, m
    !     clidat%aweirr - daily global radiation (mj/m^2)
    !     clidat%awtdmn - daily minimum air temperature (c)
    !     clidat%awtdmx - daily maximum air temperature (deg c)
    !     bio%database%bceff - biomass conversion efficiency (kg/ha)/(mj/m^2)
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
    !                  root fibrous mass by soil layer (bio%mass%rootfiberz)
    !                  (debe definition)
    !     temp_sai - a temporary variable for crop stem area index
    !               (debe definition)
    !     temp_stem - a temporary variable for the sum of the crop buried
    !                 stem mass by layer (bio%mass%stemz)  (debe definition)
    !     temp_stmrep - a temporary variable used in calculating a stem
    !                   representative diameter (debe definition)
    !     temp_store - a temporary variable to hold the sum of the crop root
    !                  storage mass by soil layer (bio%mass%rootstorez)
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
    !     bio%upgm%growth_stress - flag setting which turns on water or temperature
    !                   stress (or both)
    !                   bio%upgm%growth_stress = 0  ! no stress values applied
    !                   bio%upgm%growth_stress = 1  ! turn on water stress
    !                   bio%upgm%growth_stress = 2  ! turn on temperature stress
    !                   bio%upgm%growth_stress = 3  ! turn on both
    !      debe because it is now being read in, it is commented out in command.inc
    !     hatom2 - parameter - hectare to square meters. to convert
    !                          hectares to square meters, multiply by 10,000.
    !     clidat%hui - heat unit index (ratio of acthucum to acthum)
    !     clidat%huirt - heat unit index for root expansion (ratio of actrthucum to
    !             acthum)
    !     clidat%huirty - heat unit index for root expansion (ratio of actrthucum to
    !              acthum) on day (i-1).
    !     clidat%huiy - heat unit index (ratio of acthucum to acthum) on day (i-1)
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
    !     clidat%ts - temperature stress factor
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
    !     bio%upgm%antss - start of anthesis growth stage for hay millet, proso millet,
    !             sorghum (first bloom), soybean (beginning bloom) spring barley, spring wheat,
    !             sunflower, winter barley and winter wheat. also, dry beans
    !             and corn. this array includes daynum, year, month and day
    !             of when this stage was reached.
    !     bio%upgm%boots - start of booting/flag leaf complete growth stage. this array includes
    !             daynum, year, month and day of when this stage was reached.
    !     bio%upgm%canht - holds the final canopy height of the crop calculated with
    !             the phenologymms way. it comes in as cm and gets converted to m.
    !     canhty - holds yesterday's canopy height value. used in printing to canopyht.out.
    !              this would be the stressed value from yesterday.
    !     bio%upgm%canopyflg - a flag to indicate if the weps/upgm method to calculate
    !                 plant height will be used. value will then be 0. if using
    !                 the phenologymms method, the value will be 1.
    !     clidat%co2atmos - the atmospheric level of CO2.
    !     co2eff - the effect of the atmospheric co2 level. Used to affect biomass.
    !              The adjustment factor.
    !     bio%upgm%co2x - the CO2 levels in ppm. The x axis on the relationship curve.
    !     bio%upgm%co2y - the relative effect at different levels of CO2, i.e. bio%upgm%co2x.
    !     bio%upgm%dayhtinc - the increase in plant height for today.
    !     gddday - the number of gdd with 0°C base temperature for that day
    !     bio%upgm%heads - the start of heading stage of growth. this array includes daynum,
    !             year, month and day of when this stage was reached.
    !     bio%upgm%joints - start of jointing stage. this array includes daynum, year, month and day
    !              of when this stage was reached.
    !     bio%upgm%mats - start of maturity stage. this array includes daynum, year, month and day
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


    call caldat(ctrl%sim%juldate, day,mo,yr)
    !function
    !  and weight fraction by layer used to distribute root mass
    !  into the soil layers
    doy = dayear(day,mo,yr)

    !     reduce green leaf mass in freezing weather
    if (soils%spp%ahtsmn(1)<-2.0) then
        !          xw=abs(clidat%awtdmn)
        !         use daily minimum soil temperature of first layer to account for
        !         snow cover effects
        xw = abs(soils%spp%ahtsmn(1))
        ! this was obviously to prevent excessive leaf loss
        ! frst=sqrt((1.-xw/(xw+exp(a_fr-b_fr*xw)))+0.000001)
        ! frst=sqrt(frst)
        ! tested to match the values input in the database
        frst = xw/(xw+exp(a_fr-b_fr*xw))
        frst = min(1.0,max(0.0,frst))

        ! eliminate these in favor of dead to live ratio
        ! reduce green leaf area due to frost damage (10/1/99)
        ! dead_mass = bio%mass%standleaf * bio%growth%fliveleaf * frst
        ! bio%mass%standleaf = bio%mass%standleaf - dead_mass
        ! bio%mass%flatleaf = bio%mass%flatleaf + dead_mass

        ! reduce green leaf area due to frost damage (9/22/2003)
        bio%growth%fliveleaf = bio%growth%fliveleaf*(1.0-frst)

        ! these are set here to show up on the output as initialized
        p_rw = 0.0
        p_lf = 0.0
        p_st = 0.0
        p_rp = 0.0
    else
        frst = 0.0
    end if

    !debe added to set break points for different winter wheat developmental stages:
    !if(doy .eq. bio%upgm%joints(1)) then
    ! print *, 'eff_lai = ', eff_lai, 'pddm - ', pddm, 'parea = ', parea
    !elseif (doy .eq. bio%upgm%boots(1)) then
    !  print *, 'eff_lai = ', eff_lai, 'pddm - ', pddm, 'parea = ', parea
    !elseif (doy .eq. bio%upgm%heads(1)) then
    !    print *, 'eff_lai = ', eff_lai, 'pddm - ', pddm, 'parea = ', parea
    !elseif (doy .eq. bio%upgm%antss(1)) then
    !    print *, 'eff_lai = ', eff_lai, 'pddm - ', pddm, 'parea = ', parea
    !elseif (doy .eq. bio%upgm%mats(1)) then
    !    print *, 'eff_lai = ', eff_lai, 'pddm - ', pddm, 'parea = ', parea
    !endif
    !

    !!!!! start single plant calculations !!!!!
    ! calculate single plant effective lai (standing living leaves only)
    clfwt = bio%mass%standleaf/bio%geometry%dpop             ! kg/m^2 / plants/m^2 = kg/plant
    clfarea = clfwt*bio%database%sla*bio%growth%fliveleaf      ! kg/plant * m^2/kg = m^2/plant

    ! limiting plant area to a feasible plant area results in a
    ! leaf area index covering the "plant's area"
    ! 1/(#/m^2) = m^2/plant. plant diameter now used to limit leaf
    ! coverage to present plant diameter.
    ! find present plant diameter (proportional to diam/height ratio)
    !pdiam = min( 2.0*bio%geometry%zht * max(1.0, bio%database%diammax/bio%database%zmxc), bio%database%diammax )
    ! this expression above may not give correct effect since it is
    ! difficult to correctly model plant area expansion without additional
    ! plant parameters and process description. presently using leaf area
    ! over total plant maximum area before trying this effect. reducing
    ! effective plant area can only reduce early season growth.
    pdiam = bio%database%diammax
    ! account for row spacing effects
    if (bio%geometry%xrow>0.0) then
        ! use row spacing and plants maximum reach
        parea = min(bio%geometry%xrow,pdiam)*min(1.0/(bio%geometry%dpop*bio%geometry%xrow),pdiam)
    else
        ! this is broadcast, so use uniform spacing
        parea = min(pi*pdiam*pdiam/4.0,1.0/bio%geometry%dpop)
    end if

    ! check for valid plant area
    if (parea>0.0) then
        eff_lai = clfarea/parea
    else
        eff_lai = 1.0
    end if

    !traditional lai calculation for reporting puposes
    bio%upgm%trad_lai = clfarea*bio%geometry%dpop

    !     start biomass calculations
    !     clidat%aweirr is total shortwave radiation and a factor of .5 is assumed
    !     to get to the photosynthetically active radiation
    par = 0.5*clidat%aweirr                        ! mj/m^2                ! c-4

    !     calculate intercepted par, which is the good stuff less what hits
    !     the ground
    apar = par*(1.-exp(-bio%database%ck*eff_lai))                             ! c-4

    !     calculate potential biomass conversion (kg/plant/day) using
    !     biomass conversion efficiency at ambient co2 levels
    ! units: ((m^2)/plant)*(kg/ha)/(mj/m^2) * (mj/m^2)
    !           / 10000 m^2/ha = kg/plant

    !

    !co2eff = CO2 effect on the plant growth rate. The adjustment factor.
    j=0
    k=10
    do 100 j=2,k
        if(clidat%co2atmos .GT. bio%upgm%co2x(j)) go to 100
        go to 200
100 continue
    j = k
200 co2eff = (clidat%co2atmos-bio%upgm%co2x(j-1))*(bio%upgm%co2y(j)-bio%upgm%co2y(j-1))/(bio%upgm%co2x(j)-bio%upgm%co2x(j-1))   &
        & +bio%upgm%co2y(j-1)

    !print *, 'co2eff = ', co2eff

    pddm = parea*bio%database%bceff*apar/hatom2                                  ! c-4
    !print *, 'pddm before co2 = ', pddm

    !affect dry matter by co2
    pddm = pddm * co2eff
    !print *, 'pddm AFTER co2 = ', pddm

    !     biomass adjustment factor applied
    ! apply to both biomass converstion efficiency and water stress
    ! factor, see below
    pddm = pddm*bio%database%baf

    ! these were attempts at compensating for low yield as a result of
    ! water stress. (ie. this is the cause of unrealistically low yield)
    ! these methods had many side effects and were abandoned
    ! if( bio%database%baf .gt. 1.0 ) then
    ! first attempt. reduces water stress in the middle stress region
    !  = ctrl%cropstress%ahfwsf ** (1.0/(bio%database%baf*bio%database%baf))
    ! second attempt. reduces extreme water stress (zero values).
    !  = min( 1.0, max( ctrl%cropstress%ahfwsf, bio%database%baf-1.0 ) )
    ! else
    !  = ctrl%cropstress%ahfwsf
    ! end if
    bhfwsf_adj = max(ctrl%sim%water_stress_max,ctrl%cropstress%ahfwsf)
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
    clidat%ts = temps(clidat%awtdmx,clidat%awtdmn,bio%database%topt,bio%database%tmin)

    ! select application of stress functions based on command line flag
    if (bio%upgm%growth_stress==0) then
        strs = 1.0
    else if (bio%upgm%growth_stress==1) then
        strs = bhfwsf_adj
    else if (bio%upgm%growth_stress==2) then
        strs = clidat%ts
    else if (bio%upgm%growth_stress==3) then
        strs = min(clidat%ts,bhfwsf_adj)
    end if

    ! until shoot breaks surface, no solar driven growth
    ! call it lack of light stress
    if (bio%geometry%zht<=0.0) strs = 0.0

    ! left here to show some past incantations of stress factors
    !      strs=min(sn,sp,clidat%ts,ctrl%cropstress%ahfwsf)
    !      if (clidat%hui.lt.0.25) strs=strs**2
    !      if (clidat%hui.gt.huilx) strs=sqrt(strs)


    ! apply stress factor to generated biomass
    ddm = pddm*strs
    !     end stress factor section

    ! convert from mass per plant to mass per square meter
    ! + kg/plant * plant/m^2 = kg/m^2
    ddm = ddm*bio%geometry%dpop
    pddm = pddm*bio%geometry%dpop !de added to convert pddm mass per plant to mass per sq. m

    !!!!! end single plant calculations !!!!!

    ! find partitioning between fibrous roots and all other biomass
    ! root partition done using root heat unit index, which is not
    ! reset when a harvest removes all the leaves. this index also
    ! is not delayed in prevernalization winter annuals. made to
    ! parallel winter annual rooting depth flag as well.
    if (ctrl%sim%winter_ann_root==0) then
        p_rw = (.4-.2*clidat%hui)                                                    ! c-5
    else
        p_rw = max(0.05,(.4-.2*clidat%huirt))                                        ! c-5
    end if
    drfwt = ddm*p_rw
    ddm_rem = ddm - drfwt

    !     find partitioning factors of the remaining biomass
    !      (not fibrous root)
    !     calculate leaf partitioning.
    !debe try adding phenologymms growth stage variable and bio%upgm%phenolflg to affect p_lf
    arg_exp = -(clidat%hui-bio%database%clf)/bio%database%dlf
    if (arg_exp>=max_arg_exp) then
        p_lf = bio%database%alf + bio%database%blf/max_real
    else
        p_lf = bio%database%alf + bio%database%blf/(1.+exp(-(clidat%hui-bio%database%clf)/bio%database%dlf))
    end if
    p_lf = max(0.0,min(1.0,p_lf))
    !     calculate reproductive partitioning based on partioning curve
    arg_exp = -(clidat%hui-bio%database%crp)/bio%database%drp
    if (arg_exp>=max_arg_exp) then
        p_rp = bio%database%arp + bio%database%brp/max_real
    else
        p_rp = bio%database%arp + bio%database%brp/(1.+exp(-(clidat%hui-bio%database%crp)/bio%database%drp))
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

! use new partitioning method using partitioning coefficients for leaves, stems and reproductive parts
    if(useupgmpart == .true.) then
        uself = partcoefleaf
        usest = partcoefstem
        userp = partcoefrepro
    else
        uself = p_lf
        usest = p_st
        userp = p_rp
    end if

    ! calculate assimate mass increments (kg/m^2)
    dlfwt = ddm_rem*p_lf
    dstwt = ddm_rem*p_st
    drpwt = ddm_rem*p_rp

    ! use ratios to divert biomass to root storage
    drswt = dlfwt*bio%database%fleaf2stor + dstwt*bio%database%fstem2stor + drpwt*bio%database%fstor2stor
    dlfwt = dlfwt*(1.0-bio%database%fleaf2stor)
    dstwt = dstwt*(1.0-bio%database%fstem2stor)
    drpwt = drpwt*(1.0-bio%database%fstor2stor)

    ! senescence is done on a whole plant mass basis not incremental
    ! mass this starts scencescence before the entered heat unit index
    ! for the start of scencscence. for most leaf partitioning
    ! functions the coefficients draw a curve that approaches 1
    ! around -0.5 but the value at zero, raised to fractional powers
    ! still very small
    hui0f = bio%database%ehu0 - bio%database%ehu0*.1
    if (clidat%hui>=hui0f) then
        hux = clidat%hui - bio%database%ehu0
        ff = 1./(1.+exp(-(hux-bio%database%clf/2.)/bio%database%dlf))
        ffa = ff**0.125
        ffw = ff**0.0625
        ffr = 0.98
        ! loss from weathering of leaf mass
        lost_mass = bio%mass%standleaf*(1.0-ffw)
        ! adjust for senescence (done here, not below, so consistent
        !  with lost mass amount)
        bio%mass%standleaf = bio%mass%standleaf*ffw
        ! change in living mass fraction due scenescence
        ! and accounting for weathering mass loss of dead leaf
        bio%growth%fliveleaf = ffa*bio%growth%fliveleaf/(1.0+bio%growth%fliveleaf*(ffw-1.0))
    else
        ! set a value to be written out
        ffa = 1.0
        ffw = 1.0
        ffr = 1.0
        lost_mass = 0.0
    end if

    ! yield residue relationship adjustment

    if ((ctrl%sim%cook_yield==1).and.(bio%database%yld_coef>1.0).and.(bio%database%resid_int>=0.0).and.            &
        & ((bio%geometry%hyfg==0).or.(bio%geometry%hyfg==1).or.(bio%geometry%hyfg==5)))                                &
        & call cookyield(bio%geometry%hyfg,soils%spp%nslay,dlfwt,dstwt,drpwt,drswt,bio%mass%standstem,          &
        & bio%mass%standleaf,bio%mass%standstore,bio%mass%flatstem,bio%mass%flatleaf,bio%mass%flatstore,            &
        & bio%mass%rootstorez,lost_mass,bio%database%yld_coef,bio%database%resid_int,bio%database%grf)

    !     *****plant height*****
    !     added method (different from epic) of calculating plant height
    !     huf - heat unit factor for driving root depth, plant height
    !           development
    !     hufy - value of huf on day (i-1)
    !     pht - cumulated potential height
    !     pdht - daily potential height
    !     pcht - potential plant height for today
    !     pchty - potential plant height from previous day
    !     bio%database%zmxc - maximum potential plant height (m)
    !     aczht(am0csr) - cumulated actual height
    !     adht - daily actual height
    !     dht - daily height increment (m)
    !     bio%database%aht, bio%database%bht are height-scurve parameters (formerly lai parameters)

    ! previous day
    hufy = .01 + 1./(1.+exp((clidat%huiy-bio%database%aht)/bio%database%bht))
    ! today
    huf = .01 + 1./(1.+exp((clidat%hui-bio%database%aht)/bio%database%bht))

    pchty = min(bio%database%zmxc,bio%database%zmxc*hufy)
    pcht = min(bio%database%zmxc,bio%database%zmxc*huf)
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
        bio%growth%fliveleaf = (bio%growth%fliveleaf*bio%mass%standleaf+dlfwt)/(bio%mass%standleaf+dlfwt)
        ! next add in the additional mass
        bio%mass%standleaf = bio%mass%standleaf + dlfwt
    end if

    ! divide between standing and flat stem and storage in proportion
    ! to maximum height and maximum radius ratio
    stem_propor = min(1.0,2.0*bio%database%zmxc/bio%database%diammax)
    bio%mass%standstem = bio%mass%standstem + dstwt*stem_propor
    bio%mass%flatstem = bio%mass%flatstem + dstwt*(1.0-stem_propor)

    ! for all but below ground place rp portion in standing storage
    bio%mass%standstore = bio%mass%standstore + drpwt*stem_propor
    bio%mass%flatstore = bio%mass%flatstore + drpwt*(1.0-stem_propor)

    ! check for consistency of height, diameter and stem area index.
    ! adjust rate of height increase to keep diameter inside a range.
    if (bio%upgm%canopyflg==0) then
        call ht_dia_sai(bio%geometry%dpop,bio%mass%standstem,bio%database%ssa,bio%database%ssb,bio%geometry%dstm,bio%database%xstm,bio%database%zmxc,bio%geometry%zht, &
            & dht,temp_stmrep,temp_sai)

        ! increment plant height
        bio%geometry%zht = bio%geometry%zht + dht !cummulated height plus today's height which has been stressed.
        strsdayhtinc = 0
        !  print *, 'bio%geometry%zht =', bio%geometry%zht, 'dht =', dht, 'strs =', strs
    else if ((bio%upgm%canopyflg==1).and.(bio%upgm%antss(1)==999)) then         !use bio%upgm%canht from phenologymms method

        !debe added the following strs detemininations as above so that stress can be applied within the
        ! canopy height method from phenologymms and not be related to the weps calculated day
        ! of emergence. otherwise, if upgm shows emergence and bio%upgm%canht has a value greater than 0.0 but
        ! weps has not accomplished emergence yet and bio%geometry%zht is still 0.0, then the strs value is set
        ! to 0.0. when strs is multiplied by the bio%upgm%dayhtinc from the upgm method then the day's height
        ! increase is set back to 0.0. on the first day of emergence, bio%upgm%canht comes in as 0.0 and if
        ! bio%upgm%dayhtinc has been set back to 0.0 there would be no increase in height on the first day of
        ! emergence if weps has not also arrived at emergence.

        ! select application of stress functions based on command line flag
        if (bio%upgm%growth_stress==0) then
            strs = 1.0
        else if (bio%upgm%growth_stress==1) then
            strs = bhfwsf_adj
        else if (bio%upgm%growth_stress==2) then
            strs = clidat%ts
        else if (bio%upgm%growth_stress==3) then
            strs = min(clidat%ts,bhfwsf_adj)
        end if

        ! until shoot breaks surface, no solar driven growth
        ! call it lack of light stress
        if (bio%upgm%canht<=0.0) strs = 0.0

        !subtract today's height increase from total canopy height. the stress factor
        !(strs) will be applied  to today's height increase. it will then be added back to
        !bio%upgm%canht to get the total canopy height for today with stress applied.
        bio%upgm%canht = bio%upgm%canht - bio%upgm%dayhtinc

        ! apply stress to bio%upgm%dayhtinc from canopyht giving strsdayhtinc
        strsdayhtinc = bio%upgm%dayhtinc*strs
        !print*,'strs =', strs, 'bio%upgm%dayhtinc =', bio%upgm%dayhtinc,'strsdayhtinc =', strsdayhtinc
        !debe - i believe strsdayhtinc is comparable to dht and is what should be passed to ht_dia_sai.

        ! check for consistency of height, diameter and stem area index.
        ! adjust rate of height increase to keep diameter inside a range.
        call ht_dia_sai(bio%geometry%dpop,bio%mass%standstem,bio%database%ssa,bio%database%ssb,bio%geometry%dstm,bio%database%xstm,bio%database%zmxc,bio%geometry%zht, &
            & strsdayhtinc,temp_stmrep,temp_sai) !bio%upgm%dayhtinc


        !add back in today's height increase to which stress has been applied.
        bio%upgm%canht = bio%upgm%canht + strsdayhtinc
        !    print*, 'bio%upgm%canht =', bio%upgm%canht
        !don't let canopy height shrink.
        if (bio%upgm%canht<canhty) bio%upgm%canht = canhty

        ! increment plant height and store in weps bio%geometry%zht to be used in other areas of upgm.
        !if emergence has not occurred, i.e. bio%upgm%canht = 0.0, don't let bzcht be set equal to 0.0
        ! bio%geometry%zht = bio%geometry%zht + (bio%upgm%canht/100) this gives incredibly tall plants!
        if (bio%upgm%canht>0.0) bio%geometry%zht = bio%upgm%canht/100

        canhty = bio%upgm%canht !set yesterday's canopy height value to today's value for use tomorrow.
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
    if (ctrl%sim%winter_ann_root==0) then
        prdy = min(bio%database%zmrt,bio%database%zmrt*hufy+0.1)
        prd = min(bio%database%zmrt,bio%database%zmrt*huf+0.1)
    else
        prdy = bio%database%zmrt*(.01+1.0/(1.0+exp((clidat%huirty-bio%database%aht)/bio%database%bht)))
        prd = bio%database%zmrt*(.01+1.0/(1.0+exp((clidat%huirt-bio%database%aht)/bio%database%bht)))
    end if
    pdrd = max(0.0,prd-prdy)
    bio%geometry%zrtd = min(bio%database%zmrt,bio%geometry%zrtd+pdrd)
    bio%geometry%zrtd = min(soils%spp%aszlyd(soils%spp%nslay)*mmtom,bio%geometry%zrtd)

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
    wmaxd = max(3.0,bio%database%zmrt)
    do i = 1,soils%spp%nslay
        if (i==1) then
            ! calculate depth to the middle of a layer
            za(i) = (soils%spp%aszlyd(i)/2.0)*mmtom
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
            za(i) = (soils%spp%aszlyd(i-1)+(soils%spp%aszlyd(i)-soils%spp%aszlyd(i-1))/2.0)*mmtom
            ! calculate root distribution function
            if (za(i)<wmaxd) then
                wfl(i) = (1.0-za(i)/wmaxd)**wcg
            else
                wfl(i) = 0.0
            end if
            if (bio%geometry%zrtd/3.0>za(i)) then
                wfstore = wfstore + wfl(i)
                irstore = i
            end if
            ! check if reached bottom of root zone
            if (bio%geometry%zrtd>za(i)) then
                wffiber = wffiber + wfl(i)
                irfiber = i
            end if
        end if
    end do

    ! distribute root weight into each layer
    do i = 1,irfiber
        if (i<=irstore) bio%mass%rootstorez(i) = bio%mass%rootstorez(i) + (drswt*wfl(i)/wfstore)
        bio%mass%rootfiberz(i) = bio%mass%rootfiberz(i) + (drfwt*wfl(i)/wffiber)
        ! root senescence : 02/16/2000 (a. retta)
        bio%mass%rootfiberz(i) = bio%mass%rootfiberz(i)*ffr
    end do

    ! this factor prorates the grain reproductive fraction (grf) defined
    ! in the database for crop type 1, grains. compensates for the
    ! development of chaff before grain filling, ie., grain is not
    ! uniformly a fixed fraction of reproductive mass during the entire
    ! reproductive development stage.
    gif = 1./(1.0+exp(-(clidat%hui-0.64)/.05))
    if (bio%geometry%hyfg==1) then
        bio%geometry%grainf = bio%database%grf*gif
    else
        bio%geometry%grainf = bio%database%grf
    end if

    !     the following write statements are for 'crop.out'
    !     am0cfl is flag to print crop submodel output
    if (bio%growth%am0cfl>=1) then
        !          ! temporary sum for output
        temp_store = 0.0

        temp_fiber = 0.0
        temp_stem = 0.0
        do i = 1,soils%spp%nslay
            temp_store = temp_store + bio%mass%rootstorez(i)
            temp_fiber = temp_fiber + bio%mass%rootfiberz(i)
            temp_stem = temp_stem + bio%mass%stemz(i)
        end do

        write (ctrl%handles%luocrop,1000) daysim,doy,yr,bio%growth%dayap,clidat%hui,bio%mass%standstem,bio%mass%standleaf,     &
            & bio%mass%standstore,bio%mass%flatstem,bio%mass%flatleaf,bio%mass%flatstore,      &
            & temp_store,temp_fiber,temp_stem,                     &
            & bio%mass%standleaf + bio%mass%flatleaf,bio%mass%standstem + bio%mass%flatstem +  &
            & temp_stem,bio%geometry%zht,bio%geometry%dstm,bio%upgm%trad_lai,eff_lai,bio%geometry%zrtd,bio%geometry%grainf, &
            & clidat%ts,ctrl%cropstress%ahfwsf,frst,ffa,ffw,par,apar,pddm,p_rw,p_st,p_lf,p_rp,&
            & stem_propor,pdiam,parea,pdiam/bio%database%diammax,parea*bio%geometry%dpop,   &
            & hu_delay,temp_sai,temp_stmrep,bio%bname,gddday,ln

        !DE moved these out of the write to luocrop above. They are not used here now:
        !& temp_store,temp_fiber,temp_stem,                         &

        !bio%mass%standstem + bio%mass%flatstem + temp_stem, &
        !used acmshoot in place of temp_stem in the write statement

    end if

    write (ctrl%handles%luocanopyht,1100) daysim,doy,day,'/',mo,'/',yr,bio%upgm%canopyflg,dht,strs,bio%geometry%zht, &
        & bio%upgm%dayhtinc,strsdayhtinc,canhty,bio%upgm%canht,bio%upgm%antss(1)
    !
1000 format (1x,i5,1x,i3,1x,i4,1x,i4,1x,f6.3,12(1x,f7.4),1x,f7.2,3(1x,f7.4),   &
        & 8(1x,f6.3),1x,e12.3,10(1x,f6.3),2(1x,f8.5),' ',a20,f8.3,3x,f5.2)

1100 format (2x,i3,2x,i3,2x,i2,a1,i2,a1,i2,5x,i1,5x,f6.4,2x,f6.4,2x,f6.4,2x,   &
        & f6.4,5x,f6.4,4x,f12.8,4x,f12.8,5x,i3)

    !
    end subroutine growth

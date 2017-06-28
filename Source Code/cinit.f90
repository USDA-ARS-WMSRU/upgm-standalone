subroutine cinit(ctrl,clidat,bio,bnslay,bszlyt,bszlyd,bsdblk,bsfcce,bsfcec,bsfsmb,bsfom,bsfcla, &
               & bs0ph,bc0bn1,bc0bn2,bc0bn3,bc0bp1,bc0bp2,bc0bp3,bsmno3,bc0fd1, &
               & bc0fd2,bctopt,bctmin,cc0fd1,cc0fd2,bc0sla,bc0idc,dd,mm,yy,     &
               & bcthudf,bctdtm,bcthum,bc0hue,bcdmaxshoot,bc0shoot,bc0growdepth,&
               & bc0storeinit,bcmstandstem,bcmstandleaf,bcmstandstore,          &
               & bcmflatstem,bcmflatleaf,bcmflatstore,bcmshoot,bcmtotshoot,     &
               & bcmbgstemz,bcmrootstorez,bcmrootfiberz,bczht,bczshoot,bcdstm,  &
               & bczrtd,bcdayap,bcdayam,bcthucum,bctrthucum,bcgrainf,bczgrowpt, &
               & bcfliveleaf,bcleafareatrend,bctwarmdays,bctchillucum,          &
               & bcthu_shoot_beg,bcthu_shoot_end,bcdpop,bcdayspring,canht,      &
               & canhty,daa,dae,dap,dav,ddae,ddap,ddav,dgdde,dgdds,dgddv,elong, &
               & endphenol,gddday,gdda,gdde,gdds,gddv,gddwsf,jan1,lnarray,      &
               & lncntr,lnpout,pdate,rowcntr,seedsw,tempsw,todayln,verns, &
               & yestln,yr,ln,co2eff)
!
!debe added to the subroutine arguments coming into cinit as follows:
! ***** emergence *****
! variables: dap, ddap, dgdds, elong, ems, gdds, germs, gddday, seedsw, tempsw
!
! ***** phenology *****
! variables: aifs, antes, antss, blstrs, boots, browns, daa, dae, dav, ddae,
!  ddav, dents, dgdde, dgddv, doughs, drs, ears, endlgs, endphenol, fps, fullbs, gdda,
!  gdde, gddv, gddwsf, gpds, halfbs, heads, hrs, ies, ies2, infls, joints, lf12s,
!  lf4s, lf8s, lncntr, lnpout, mats, milks, nolvs, opens, pdate, silks, srs,
!  tis, tsints, tss, yelows
!  for dry beans: cots, lf1s, lf2s, lf3s, mffls, epods, mpods, eseeds, mseeds
!
! ***** vernalization *****
! variables: jan1, verns
!
! ***** leaf number *****
! variables:ln,lnarray,lncntr,lnpout,rowcntr,todayln,yestln
    
! ***** canopy height *****
! variables: canht, canhty, maxht
!
!***** CO2 variables *****
! debe added co2eff
!    
! note: some variables are used in more than one subroutine.
!
    use constants, only : mgtokg, mmtom, civilrise, mnsz
    use upgm_simdata, only : upgm_ctrls, controls
    use climate, only : climate_data
    use biomaterial
implicit none
!
include 'file.fi'
include 'csoil.inc'
include 'chumus.inc'
include 'cfert.inc'
include 'cgrow.inc'
!
! Dummy arguments
!
    type(controls) :: ctrl
    type(climate_data) :: clidat
    type(biomatter) :: bio
real :: bc0bn1,bc0bn2,bc0bn3,bc0bp1,bc0bp2,bc0bp3,bc0fd1,bc0fd2,bc0growdepth,   &
      & bc0hue,bc0shoot,bc0sla,bc0storeinit,bcdmaxshoot,bcdpop,bcdstm,          &
      & bcfliveleaf,bcgrainf,bcleafareatrend,bcmflatleaf,bcmflatstem,           &
      & bcmflatstore,bcmshoot,bcmstandleaf,bcmstandstem,bcmstandstore,          &
      & bcmtotshoot,bctchillucum,bcthucum,bcthum,bcthu_shoot_beg,               &
      & bcthu_shoot_end,bctmin,bctopt,bctrthucum,bczgrowpt,bczht,bczrtd,        &
      & bczshoot,bsmno3,canht,canhty,cc0fd1,cc0fd2,elong,gdda,gddday,gdde,gdds, &
      & gddv,todayln,yestln,ln,co2eff, bctwarmdays
integer :: bc0idc,bcdayam,bcdayap,bcdayspring,bctdtm,bcthudf,bnslay,&
         & daa,dae,dap,dav,dd,lncntr,mm,pdate,rowcntr,seedsw,tempsw,verns,yr,yy
logical :: endphenol,jan1
real,dimension(*) :: bcmbgstemz,bcmrootfiberz,bcmrootstorez,bs0ph,bsdblk,bsfcce,&
                   & bsfcec,bsfcla,bsfom,bsfsmb,bszlyd,bszlyt
integer,dimension(20) :: ddae,ddap,ddav
real,dimension(20) :: dgdde,dgdds,dgddv
real,dimension(15,5) :: gddwsf
real,dimension(400,2) :: lnarray
real,dimension(100,2) :: lnpout
!
! Local variables
!
real :: alog
real :: bphu,bsa,dg,dg1,ephu,heat_unit,jreal,max_air,min_air,sphu,wt1,xz,yp1,ypn
real,dimension(365,3) :: d1
real,dimension(730,3) :: d2
integer :: dayear
real :: daylen,huc1
integer :: dxx,hdate,i,j,m,n,sdmn,sdmx
real,dimension(14) :: dy_mon,mn_air_temp,mn_air_temp2,mx_air_temp,mx_air_temp2
integer :: int
!
! subroutine arguments
!
!
! local variables
!
!
!***** newly added variables *****
!debe added declarations for emergence, phenol, leaf number, anthesis, and
! vernalization variables.
!
! local variables for phenol.
!debe added endphenol to end the simulation once the end of growth
! is reached.
!
!     the following subroutine arguments are not used: bc0bn1, bc0bn2, bc0bn3,
!     bc0bp1, bc0bp2, bc0bp3, bc0idc, bc0sla    jcaii  8/8/08
!
!     author : amare retta
!     + + + purpose + + +
!     this subroutine initializes parameters for a crop every time it is planted.
 
!     + + + keywords + + +
!     initialization
!
!     + + + argument definitions + + +
!     bc0bn1 - currently not used. normal fraction of n in crop biomass
!              at emergence.
!     bc0bn2 - currently not used. normal fraction of n in crop biomass
!              at midseason
!     bc0bn3 - currently not used. normal fraction of n in crop biomass
!              at maturity
!     bc0bp1 - currently not used. normal fraction of p in crop biomass
!              at emergence
!     bc0bp2 - currently not used. normal fraction of p in crop biomass
!              at midseason
!     bc0bp3 - currently not used. normal fraction of p in crop biomass
!              at maturity
!     bc0growdepth - db input, initial depth of growing point (m)
!     bc0hue - relative heat unit for emergence (fraction)
!     bc0shoot - mass from root storage required for each regrowth shoot
!                (mg/shoot). seed shoots are smaller and adjusted for
!                available seed mass
!     bc0sla - currently not used. specific leaf area (cm^2/g)
!     bc0storeinit - db input, crop storage root mass initialzation (mg/plant)
!     bcdayam - number of days since crop matured
!     bcdayap - number of days of growth completed since crop planted
!     bcdayspring - day of year in which a winter annual releases stored
!                   growth
!     bcdmaxshoot - maximum number of shoots possible from each plant
!     bcdpop - number of plants per unit area (#/m^2)
!            - note: bcdstm/bcdpop gives the number of stems per plant
!     bcdstm - number of crop stems per unit area (#/m^2)
!     bcfliveleaf - fraction of standing plant leaf which is living
!                   (transpiring)
!     bcgrainf - internally computed reproductive grain fraction
!     bcleafareatrend - direction in which leaf area is trending.
!                       saves trend even if leaf area is static for long
!                       periods.
!     bcmbgstemz - crop stem mass below soil surface by soil layer (kg/m^2)
!     bcmflatleaf - crop flat leaf mass (kg/m^2)
!     bcmflatstem - crop flat stem mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)
!     bcmrootfiberz - crop root fibrous mass by soil layer (kg/m^2)
!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                     (tubers (potatoes, carrots), extended leaf (onion),
!                     seeds (peanuts))
!     bcmshoot - crop shoot mass grown from root storage (kg/m^2)
!                this is a "breakout" mass and does not represent a
!                unique pool since this mass is destributed into below
!                ground stem and standing stem as each increment of the
!                shoot is added
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                     (head with seed, or vegetative head (cabbage,
!                     pineapple))
!     bcmtotshoot - total mass of shoot growing from root storage biomass
!                   (kg/m^2) in the period from beginning to completion
!                   of emegence heat units
!     bctchillucum - accumulated chilling units (deg c day)
!     bctdtm - db input days to maturity
!     bcthu_shoot_beg - heat unit total for beginning of shoot grow from
!                       root storage period
!     bcthu_shoot_end - heat unit total for end of shoot grow from root
!                       storage period
!     bcthucum - crop accumulated heat units
!     bcthudf - flag 0-grow in days to maturity, 1-grow in heat units
!     bcthum - db input heat units to maturity
!     bctopt - optimum temperature (deg. c)
!     bctrthucum - accumulated root growth heat units (degree-days)
!     bctwarmdays - number of consecutive days that the temperature
!                   has been above the minimum growth temperature
!     bczgrowpt - depth in the soil of the gowing point (m)
!     bczht - crop height (m)
!     bczrtd - crop root depth (m)
!     bczshoot - length of actively growing shoot from root biomass (m)
!     bn4 - currently not used
!     bnslay - number of soil layers
!     bp4 - currently not used
!     bs0ph - soil ph
!     bsdblk - bulk density of a layer (g/cm^3=t/m^3)
!     bsfcce - calcium carbonate (%)
!     bsfcec - cation exchange capacity (cmol/kg)
!     bsfcla - % clay
!     bsfom - percent organic matter
!     bsfsmb - sum of bases (cmol/kg)
!     bsmno3 - fertilizer applied as no3
!     bszlyd - depth from top of soil to botom of layer, m
!     bszlyt - soil layer thicknesses for each subregion (mm)
!     dd - day
!     mm - month
!     yy - year
 
!     + + + local variable definitions + + +
!     alog -
!     bc0fd1 - minimum temperature below zero (c) (1st x-coordinate in
!              the frost damage s-curve)
!     bc0fd2 - minimum temperature below zero (c) (2nd x-coordinate in
!              the frost damage s-curve)
!     bc0idc - crop type:annual,perennial,etc
!     bctmin - base temperature (deg. c)
!     bphu - sphu at planting date
!     bsa - base saturation (%?)
!     cc0fd1 - fraction of biomass lost each day due to frost
!              (1st y-coordinate in the frost damge s-curve)
!     cc0fd2 - fraction of biomass lost each day due to frost
!              (2nd y-coordinate in the frost damge s-curve)
!     ch - currently not used. interim variable in calculations of day
!          length
!     ck - currently not used. extinction coeffficient (fraction)
!     co2 - currently not used. co2 concentration in the atmosphere (ppm)
!     cpnm - currently not used. crop name
!     d1 - day in the first year in the array containing heat unit values
!     d2 - day in the second year in the array containing heat unit values
!     dayear - a function which given a date in dd/mm/yyyy format, will
!              return the number of days from the first of that year.
!     daylen - a function which calculates the daylength (hours) for any
!              simulation site based on the global position of the site,
!              and day of the year.
!     dg - soil layer thickness (mm)
!     dg1 - previous value of dg
!     dlax1 - currently not used. fraction of grwing season
!             (1st x-coordinate in lai s-curve)
!     dlax2 - currently not used. fraction of grwing season
!             (2nd x-coordinate in lai s-curve)
!     dlay1 - currently not used. fraction of maximum lai
!             (1st y-coordinate in the lai s-curve)
!     dlay2 - currently not used. fraction of maximum lai
!             (2nd y-coordinate in the lai s-curve)
!     dtm - days from planting to maturity
!     dxx -
!     dy_mon -
!     ephu - sphu at harvest date
!     frs1 - same as bc0fd1 (value needed in cfrost.for:11/20/96)
!     frs2 - same as bc0fd2 (value needed in cfrost.for:11/20/96)
!     h - currently not used? interim variable in calculations of day
!         length
!     hdate - day of year harvest occurs
!     heat_unit - daily heat units
!     hi - currently not used. harvest index of a crop
!     hix1 - currently not used. ratio of actual to potential et
!            (1st x-coordinate in the water stress - harvest index
!            s-curve)
!     hix2 - currently not used. ratio of actual to potential et
!            (2nd x-coordinate in the water stress - harvest index
!            s-curve)
!     hiy1 - currently not used. fraction of reduction in harvest index
!            (1st x-coordinate in the water stress - harvest index
!            s-curve)
!     hiy2 - currently not used. fraction of reduction in harvest index
!            (2nd x-coordinate in the water stress - harvest index
!            s-curve)
!     hlmn - currently not used. minimum daylength for a site (hr)
!     hlmx - currently not used. maximum daylength for a site (hr)
!     hmx - currently not used. maximum potential plant height (m)
!     huc1 - a function which calculates single day heat units for given
!            temperatures
!     hui0 - currently not used. heat unit index when leaf senescence
!            starts.
!     irint - currently not used. flag for printing end-of-season values : 10/6/99
!     jreal -
!     max_air - dayly maximum air temperature (cubic spline)
!     min_air - dayly minimum air temperature (cubic spline)
!     mn_air_temp -
!     mn_air_temp2 -
!     mx_air_temp -
!     mx_air_temp2 -
!     n - used in the spline subroutine
!     nc - currently not used. crop number
!     phu - potential heat units for crop maturity (deg. c)
!     rbmd - currently not used. biomass-energy ratio decline factor
!     rdmx - currently not used. maximum root depth
!     rlad - currently not used. crop parameter that governs leaf area
!            index decline rate
!     s11x1 - currently not used. soil labile p concentration (ppm)
!             (1st x-coordinate in the p uptake reduction s-curve)
!     s11x2 - currently not used. soil labile p concentration (ppm)
!             (2nd x-coordinate in the p uptake reduction s-curve)
!     s11y1 - currently not used. p uptake restriction factor
!             (1st y-coordinate in the p uptake reduction s-curve)
!     s11y2 - currently not used. p uptake restriction factor
!             (2nd y-coordinate in the p uptake reduction s-curve)
!     s8x1 - currently not used. scaled ratio of actual to potential n or p
!            (1st x-coordinate in the n stress factor s-curve)
!     s8x2 - currently not used. scaled ratio of actual to potential n or p
!            (2nd x-coordinate in the n stress factor s-curve)
!     s8y1 - currently not used. n or p stress factor (1st y-coordinate in
!            the n or p stress s-curve)
!     s8y2 - currently not used. n or p stress factor (2nd y-coordinate in
!            the n or p stress s-curve)
!     sdmn - day of the year when daylength is minimum
!     sdmx - day of the year when daylength is maximum
!     sphu - running sum of heat units
!     tdmag -
!     topt - currently not used. optimum temperature (deg. c)
!     wsyf - currently not used. minimum crop harvest index under drought
!     wt - wt of soil
!     wt1 - convert n, p, etc. conc. from g/t to kg/t
!     x1 - currently not used. temporary bulk density value
!     xx - currently not used. depth of above layer
!     xz - temporary variable for humus in a soil layer
!     yp1 -
!     ypn -
 
!     + + + common block variables definitions + + +
!     a_co - parameter in the biomass conversion efficiency s-curve
!     a_fr - parameter in the frost damage s-curve
!     a_s11 - parameter in p uptake eqn. (corresponding to scrp(11,1)
!             in epic)
!     a_s8 - parameter in n or p stress eqn. (corresponds to scrp(8,1)
!            in epic)
!     am0cfl - flag to print crop submodel output
!     amalat - latitude of simulation site
!     ap - total available p in a layer from all sources
!     awtmnav - average monthly minimum air temperature (deg c) obtained
!               from the cligen run file
!     awtmxav - average monthly maximum air temperature (deg c) obtained
!               from the cligen run file
!     b_co - parameter in the biomass conversion efficiency s-curve
!     b_fr - parameter in the frost damage s-curve
!     b_s11 - parameter in p uptake eqn. (corresponding to scrp(11,2)
!            in epic)
!     b_s8 - parameter in n or p stress eqn. (corresponds to scrp(8,2)
!            in epic)
!     bk - flow coefficient between active and stable p pools (1/d)
!     ceta - actual cummulated evapotranspiration
!     civilrise - parameter. solar altitude angle defined as civil
!                 twilight
!     cta - actual cummulated transpiration
!     ctp - potential cummulated transpiration
!     daye - days after emergence
!     dmag - stress adjusted cumulated aboveground biomass (t/ha).
!            use is commented out.
!     ehu - heat units required from planting seedling emergence
!     fon - amount of n in residue
!     fop - amount of p in residue
!     hrlt - day length on day i
!     hrlty - day length on day (i-1)
!     hum - amount of humus
!     ids - soil weathering code
!     mgtokg - parameter (mgtokg = 0.000001); to convert milligrams to
!              kilograms, multiply by 0.000001
!     mmtom - unit conversion constant (m/mm). parameter.
!     op - amount of stable mineral p pool
!     prcp - cummulated precipitation
!     psp - fraction of fertilizer p that is labile
!     pmn - amount of active mineral p pool
!     rsd - current amount of residue in a layer
!     rtn - active pool fraction
!     slai -
!     slaix - maximum actual leaf area index attained
!     ssaix - maximum actual stem area index
!     tap - total labile p
!     tfon - total amount of n from fresh organic matter
!     tfop - total amount of p from fresh organic matter
!     tmp - total active mineral p from all layers
!     tno3 - total no3_n
!     top - total stable mineral p from all layers
!     tp - total p from all humus
!     trsd - sum of residue of all layers
!     twmn - total organic n from active humus pool
!     twn - total organic n from humus
!     wmn - active humus n pool
!     wn - organic n concentration of humus
!     wno3 - total available n in a layer from all sources
!     wp - organic p concentration of humus
!     yc - period of cultivation before simualtion starts
 
!     + + +  newly added arguments definitions + + +
!     canht -  canopy height of the crop for the current day.
!     canhty - yesterday's canopy height value.
!     co2eff - the effect on the plant of the read-in atmospherice co2 level. 
!              The adjustment factor.
!     daa - days after anthesis
!     dae - days after emergence
!     dap - days after planting
!     dav - days after vernalization
!     ddae - array holding the dae value for each growth stage
!     ddap - array holding the dap value for each growth stage
!     ddav - array holding the dav value for each growth stage
!     dgdde - array holding the gdde value for each growth stage
!     dgdds - array holding the gdds value for each growth stage
!     dgddv - array holding the gddv value for each growth stage
!     elong - total elongation of the emerging seedling based on the
!             day's gdd (mm)
!     endphenol - a flag to indicate if this subroutine should be called
!                 again the next day.
!     gdda - sum of growing degree days since anthesis
!     gddday - the number of gdd with 0°C base temperature for that day.
!     gdde - sum of growing degree days since emergence
!     gdds - sum of growing degree days since seeding
!     gddv - sum of growing degree days since vernalization
!     gddwsf - an array to hold the gn and gs gdd values plus the high and
!              low water stress factor values.  these are used in
!              calculating the slope of the line for each growth stage
!              and this is then used to calculate the adjusted gdd value
!              for the current growth stage.
!              column one contains the gn values and is y2.
!              column two contains the gs values and is y1.
!              column three contains wsfhi (high water stress) and is x1.
!              column four contains wsflo (low water stress) and is x2.
!              column five contains the adjgdd value for the stage.
!     jan1 - a flag to test if january 1 has occurred.  if it has passed, then
!            the winter annual crop is assumed to have completed
!            vernalization.
!     lnarray - an array to hold the leaf number calculated for each day
!     lncntr - counter for the leafno subroutine
!     lnpout - an array used in writing out daynum and the number of
!              leaves on that day. the values are written each time a new
!              leaf has appeared, i.e. when the integer version of the
!              real leaf number has incremented.
!     pdate - day of year planting can occur
!     rowcntr - a counter for the rows in an array
!     seedsw - soil water content at seed depth.  it is read in as
!              optimum, medium, dry or planted in dust and converted
!              to an integer.	 1 = optimum, 2 = medium, 3 = dry and
!              4 = planted in dust
!     todayln - the value of the current day's leaf number.
!     verns - sum of gdd after vernalization. currently not used.
!     yestln - the value of yesterday's leaf number
!     yr - year
 
!     + + + new local variable definitions + + +
!     d - day. used in filling the growth stage arrays
!     dn - day number of the year. used in filling the growth stage
!          arrays
!     mo - month. used in filling the growth stage arrays
!     y  - year. used in filling the growth stage arrays

!     initialize variables needed for season heat unit computation: added on
!     3/16/1998 (a. retta)
data dy_mon/ - 15,15,45,74,105,135,166,196,227,258,288,319,349,380/
                ! added so a local variable would be set correctly - lew
!     transfer average monthly temperatures from the global array to a local.
!     for the southern hemisphere, monthly average temperatures should start
!     in july.1?

do i = 1,12
  mx_air_temp(i+1) = clidat%awtmxav(i)
  mn_air_temp(i+1) = clidat%awtmnav(i)
end do
mx_air_temp(1) = mx_air_temp(13)
mx_air_temp(14) = mx_air_temp(2)
mn_air_temp(1) = mn_air_temp(13)
mn_air_temp(14) = mn_air_temp(2)
 
!     added algorithm to compute yield adjustment factor; 10/18/99
!     tdmag=15.0
!     if (dmag.le.0.)yaf=1.0
!     if (dmag.gt.0.)yaf=log(tdmag/0.00001)/log(dmag/0.00001)
!     write (*,*)'yaf, dmag, tdmag  ' ,yaf,dmag,tdmag
 
      ! determine number of shoots (for seeds, bc0shoot should be much
      ! greater than bc0storeinit resulting in one shoot with a mass
      ! reduced below bc0shoot
      ! units are mg/plant * plant/m^2 / mg/shoot = shoots/m^2
bcdstm = bc0storeinit*bcdpop/bc0shoot
if (bcdstm<bcdpop) then
          ! adjust count to reflect limit
  bcdstm = bcdpop
          ! not enough mass to make a full shoot
          ! adjust shoot mass to reflect storage mass for one shoot per plant
          ! units are mg/plant * kg/mg * plant/m^2 = kg/m^2
  bcmtotshoot = bc0storeinit*mgtokg*bcdpop
else if (bcdstm>bcdmaxshoot*bcdpop) then
          ! adjust count to reflect limit
  bcdstm = bcdmaxshoot*bcdpop
          ! more mass than maximum number of shoots
          ! adjust total shoot mass to reflect maximum number of shoots
          ! units are shoots/m^2 * mg/shoot * kg/mg = kg/m^2
  bcmtotshoot = bcdstm*bc0shoot*mgtokg
else
          ! mass and shoot number correspond
          ! units are mg/plant * kg/mg * plant/m^2 = kg/m^2
  bcmtotshoot = bc0storeinit*mgtokg*bcdpop
end if
 
      ! all types initialized with no stem, leaves or roots, just root storage mass
      ! transplants start with a very short time to "sprout"
bio%mass%standleaf = 0.0
bio%mass%standstem = 0.0
bio%mass%standstore = 0.0
bcmflatstem = 0.0
bcmflatleaf = 0.0
bcmflatstore = 0.0
bcmshoot = 0.0
 
do i = 1,bnslay
  bcmbgstemz(i) = 0.0
  bcmrootfiberz(i) = 0.0
end do
 
bczht = 0.0
bczshoot = 0.0
 
bcdayap = 0
bcdayam = 0
bcthucum = 0.0
bctrthucum = 0.0
bcgrainf = 0.0
bczgrowpt = bc0growdepth
bcfliveleaf = 1.0
bcleafareatrend = 0.0
 
      ! zero out day of year that spring growth is released
bcdayspring = 0
 
      ! root depth
bczrtd = bc0growdepth
 
      ! initialize the root storage mass into a single layer
if ((bszlyd(1)*mmtom>bczrtd)) then
          ! mg/plant * #/m^2 * 1kg/1.0e6mg = kg/m^2
  bcmrootstorez(1) = bc0storeinit*bcdpop*mgtokg
!          write(*,*) "cinit: stor lay ", 1, bczrtd, bcmrootstorez(1)
else
  bcmrootstorez(1) = 0.0
end if
do i = 2,bnslay
	!Rmarquez 2.14.17 -> changed logic for these conditions to check for
	!					top of layer <= planting depth
	!					and
	!					bottom of layer > planting depth
  if (((bszlyd(i-1)*mmtom<=bczrtd).and.(bszlyd(i)*mmtom>bczrtd))) then
              ! mg/plant * #/m^2 * 1kg/1.0e6mg = kg/m^2
     bcmrootstorez(i) = bc0storeinit*bcdpop*mgtokg
!              write(*,*) "cinit: stor lay ", i, bczrtd, bcmrootstorez(i)
  else
     bcmrootstorez(i) = 0.0
  end if
end do
 
bctwarmdays = 0.0
bctchillucum = 0.0
      ! set initial emergence (shoot growth) values
bcthu_shoot_beg = 0.0
bcthu_shoot_end = bc0hue
 
 
!     set variable in local include file
clidat%xlat = ctrl%sim%amalat
 
!     minimum and maximum daylength for a location
if (ctrl%sim%amalat>0.) then
  sdmn = 354
  sdmx = 173
else
  sdmn = 173
  sdmx = 354
end if
!
!     hlmn, hlmx are set but not used so commented out   jcaii  8/08/08
!
!     hlmn = daylen(amalat, sdmn, civilrise)
!     hlmx = daylen(amalat, sdmx, civilrise)
 
!     planting day of year
call caldat(ctrl%sim%juldate, dd,mm,yy)
pdate = dayear(dd,mm,yy)
!     initial daylength calculations
clidat%hrlt = daylen(ctrl%sim%amalat,pdate,civilrise)
clidat%hrlty = daylen(ctrl%sim%amalat,pdate-1,civilrise)
 
!     start calculation of seasonal heat unit requirement
sphu = 0.
ephu = 0.
bphu = 0.
n = 14
yp1 = 1.0E31          ! signals spline to use natural bound (2nd deriv = 0)
ypn = 1.0E31          ! signals spline to use natural bound (2nd deriv = 0)
 
      ! call cubic spline interpolation routines for air temperature
call spline(dy_mon,mx_air_temp,n,yp1,ypn,mx_air_temp2)
call spline(dy_mon,mn_air_temp,n,yp1,ypn,mn_air_temp2)
do i = 1,365
  jreal = i
          ! calculate daily temps. and heat units
  call splint(dy_mon,mx_air_temp,mx_air_temp2,n,jreal,max_air)
  call splint(dy_mon,mn_air_temp,mn_air_temp2,n,jreal,min_air)
  heat_unit = huc1(max_air,min_air,bctopt,bctmin)
  d1(i,1) = i
  d1(i,2) = heat_unit
  d2(i,1) = i
  d2(i,2) = heat_unit
end do
!     duplicate the first year into the second year
do j = 1,365
  m = j + 365
  d2(m,1) = m
  d2(m,2) = d1(j,2)
end do
!     running sum of heat units
do j = 1,730
  sphu = sphu + d2(j,2)
  d2(j,3) = sphu
!          if (am0cfl .gt. 0) then
!              print for debugging
!              write(60,*) d2(j,1),d2(j,2),d2(j,3)
!          end if
end do
sphu = 0.
 
!     find dtm or phu depending on heat unit flag=1
do j = 1,730
  if (d2(j,1)==pdate) bphu = d2(j,3)
end do
if (bcthudf==1) then
         ! use heat unit calculations to find dtm
  clidat%phu = bcthum
  do j = 1,730
     if (d2(j,3)<=bphu+clidat%phu) dtm = d2(j,1) - pdate
  end do
  hdate = pdate + dtm !hdate = harvest date
else
         ! calculate average seasonal heat units
  dtm = bctdtm
  hdate = pdate + dtm
  if (hdate>d2(730,1)) then
            ! this crop grows longer than one year
     ephu = d2(730,3)
     clidat%phu = ephu - bphu
            ! cap this at two years
     dxx = min(730,hdate-int(d2(730,1)))
     clidat%ehu = d2(dxx,3)
     clidat%phu = clidat%phu + clidat%ehu
  else
     do j = 1,730
        if (d2(j,1)==hdate) ephu = d2(j,3)
     end do
     clidat%phu = ephu - bphu
  end if
end if
 
      ! print out heat average heat unit and days to maturity
if (bio%growth%am0cfl>0) write (luoinpt,1100) pdate,hdate,bcthudf,dtm,bctdtm,clidat%phu,bcthum
 
      ! after printing the value, set the global parameter for maximum
      ! heat units to the new calculated value (this database value is
      ! read from management file every time crop is planted, so changing
      ! it here does not corrupt it)
bcthum = clidat%phu
 
!     calculate s-curve parameters
!call scrv1(bc0fd1,cc0fd1,bc0fd2,cc0fd2,a_fr,b_fr)         ! frost damage
!      call scrv1(s11x1,s11y1,s11x2,s11y2,a_s11,b_s11)    ! p uptake stress parameters
!      call scrv1(s8x1,s8y1,s8x2,s8y2,a_s8,b_s8)          ! n and p availability stress
!      if (am0cfl .gt. 0) write (60,2110)a_co,b_co,a_fr,b_fr
 
!     calculate bc0bn1,bc0bn2,bc0bn3,bn4,bc0bp1,bc0bp2,bc0bp3,bp4
!      call nconc (bc0bn1,bc0bn2,bc0bn3,bn4)
!      call nconc (bc0bp1,bc0bp2,bc0bp3,bp4)
!      write (33,2115) bc0bn1,bc0bn2,bc0bn3,bn4,bc0bp1,bc0bp2,bc0bp3,bp4
 
!     this section estimates soil test data and calculates initial
!     values of no3, labile p, fresh organic matter, humus, etc in each layer.
 
!      write (33,2990)
!      write (33,2992) (bszlyd(i),bsdblk(i),bsfcla(i),wp(i),bs0ph(i),    &
!     &bsfsmb(i),bsfom(i),bsfcce(i),bsfcec(i),wno3(i),ap(i),rsd(i),      &
!     &psp(i),wn(i),i=1,10)
trsd = 0.0
tfon = 0.0
tfop = 0.0
tmp = 0.0
top = 0.0
twn = 0.0
twmn = 0.0
tp = 0.0
tap = 0.0
tno3 = 0.0
do i = 1,bnslay
!          x1=bsdblk(i)
          ! dg=soil layer thickness in mm; xx=bszlyd(i-1); wt(i)=wt of soil(t/ha-mm)
          ! (convert to t/ha ??) wt1=convert n, p, etc. conc. from g/t to kg/t
          ! bsfcla(i)=100.-san(i)-sil(i)
          ! note:  bszlyd is in "mm" and not "m" - 5/14/99 - lew
          ! since "dg" is just the layer thickness in "mm", we have set it
          ! to the now included weps global layer thickness variable here.
          ! dg=1000.*(bszlyd(i)-xx)
  dg = bszlyt(i)
  wt(i) = bsdblk(i)*dg*10.
  wt1 = wt(i)/1000.
          ! estimate initial values of: rsd(residue:t/ha);ap(labile p conc.:g/t);
          ! wno3 (no3 conc.:g/t). dg1=previous value of dg.
          ! call sdst (rsd,dg,dg1,i)
  call sdst(ap,dg,dg1,i)
  call sdst(wno3,dg,dg1,i)
  trsd = trsd + rsd(i)
  dg1 = dg
          ! calculate ratio (rtn) of active(wmn) to total(wn) n pools associated
          ! with humus. yc=years of cultivation.
          ! yc was set to 150. as suggested by a. retta - jt 1/10/94
  yc = 150.
  if (i==1) rtn(1) = 0.4*exp(-0.0277*yc) + 0.1
  if (i>1) rtn(i) = rtn(1)*.4
 
          ! estimate bsfcec, bsa
          ! there are some very questionable things done here - jt  1/10/94
          ! xx=bszlyd(i)
  if (bsfcec(i)>0.) then
     if (bsfcce(i)>0.) bsfsmb(i) = bsfcec(i)
     if (bsfsmb(i)>bsfcec(i)) bsfsmb(i) = bsfcec(i)
     bsa = bsfsmb(i)*100./(bsfcec(i)+1.E-20)
     bsfcec(i) = bs0ph(i)
     bsfsmb(i) = bsfcec(i)
  end if
 
          ! calculate amounts of n & p (kg/ha) from fresh organic matter(rsd),
          ! assuming that n content of residue is 0.8%; fon & fop are in kg/ha.
  fon(i) = rsd(i)*8.
  fop(i) = rsd(i)*1.1
  tfon = tfon + fon(i)
  tfop = tfop + fop(i)
 
          ! initial organic(humus) n & p concentrations (g/t) in the soil
  if (wn(i)==0.) wn(i) = 1000.*bsfom(i)
  if (wp(i)==0.) wp(i) = 0.125*wn(i)
 
          ! estimate psp(p sorption ratio), which is the fraction of fertilizer p that
          ! remains in the labile form after incubation for different soil conditions.
          ! the weathering status code ids is inputted.
          ! ids=1:esitmate psp for calcareous soils without weathering information
          ! ids=2:estimate psp for noncalcareous slightly weathered soils
          ! ids=3:estimate psp for noncalcareous moderately weathered soils
          ! ids=4:estimate psp for noncalcareous highly weathered soils
          ! ids=5: input value of psp
          ! finally estimate the flow coefficient bk between the active and stable
          ! p pools (1/d)
  ids = 1
  if (ids==1) then
     psp(i) = 0.5
     if (bsfcce(i)>0.) psp(i) = 0.58 - 0.0061*bsfcce(i)
  end if
  if (ids==2) psp(i) = 0.02 + 0.0104*ap(i)
  if (ids==3) psp(i) = 0.0054*bsa + 0.116*bs0ph(i) - 0.73
  if (ids==4) psp(i) = 0.46 - 0.0916*alog(bsfcla(i))
  if (psp(i)<0.05) psp(i) = 0.05
  if (psp(i)>0.75) psp(i) = 0.75
  bk(i) = 0.0076
  if (bsfcce(i)>0.) bk(i) = exp(-1.77*psp(i)-7.05)
 
          ! calculate initial amount of active(pmn) and stable(op) mineral p pools
          ! ap=initial amount of labile p(g/t);wt1=conversion factor to kg/ha
  pmn(i) = ap(i)*(1.-psp(i))/psp(i)*wt1
  tmp = tmp + pmn(i)
  op(i) = 4.*pmn(i)
  top = top + op(i)
 
          ! calculate amount of active(readily mineralizable) humus n pool(wmn)
          ! and total(active+stable) humus n pool(wn)
  wmn(i) = rtn(i)*wn(i)
  wn(i) = wn(i) - wmn(i)
  wn(i) = wn(i)*wt1
  twn = twn + wn(i)
  wmn(i) = wmn(i)*wt1
  twmn = twmn + wmn(i)
 
          ! convert total(active+stable) humus p pool to kg/ha
  wp(i) = wp(i)*wt1
  tp = tp + wp(i)
 
          ! convert initial no3 & labile p in the soil to kg/ha
  ap(i) = ap(i)*wt1
  tap = tap + ap(i)
  wno3(i) = wno3(i)*wt1
  tno3 = tno3 + wno3(i)
 
          ! calculate amount of humus in a layer (t/ha)
          ! moved from original location
  xz = bsfom(i)*.0172
  hum(i) = xz*wt(i)
end do
 
      ! add applied fertilizer to the top layer
wno3(1) = wno3(1) + bsmno3
tno3 = tno3 + bsmno3
 
!debe added initialization of emergence variables
yr = 0
elong = 0.0
gddday = 0.0
 
do i = 1,20
  ddap(i) = 999
  dgdds(i) = 999.9
end do
 
!! debe added initialization for phenol variables
!dn = 999       !day number of the year (daynum)
!y = 0000       !year
!mo = 0          !month
!d = 0          !day
!
!! initialize arrays:
!!
!!  growth stages arrays	(daynum, year, mo, day)
!do i = 1,4
!  if (i.eq.1) then
!     aifs(i) = dn
!     antes(i) = dn
!     antss(i) = dn
!     blstrs(i) = dn
!     browns(i) = dn
!     boots(i) = dn
!     cots(i) = dn
!     dents(i) = dn
!     doughs(i) = dn
!     drs(i) = dn
!     ears(i) = dn
!     ems(i) = dn
!     endlgs(i) = dn
!     epods(i) = dn
!     eseeds(i) = dn
!     fps(i) = dn
!     fullbs(i) = dn
!     germs(i) = dn
!     gpds(i) = dn
!     halfbs(i) = dn
!     heads(i) = dn
!     hrs(i) = dn
!     ies(i) = dn
!     ies2(i) = dn
!     infls(i) = dn
!     joints(i) = dn
!     lf1s(i) = dn
!     lf12s(i) = dn
!     lf2s(i) = dn
!     lf3s(i) = dn
!     lf4s(i) = dn
!     lf8s(i) = dn
!     mats(i) = dn
!     mffls(i) = dn
!     milks(i) = dn
!     mpods(i) = dn
!     mseeds(i) = dn
!     opens(i) = dn
!     silks(i) = dn
!     srs(i) = dn
!     tis(i) = dn
!     tsints(i) = dn
!     tss(i) = dn
!     yelows(i) = dn
!  else if (i.eq.2) then
!     aifs(i) = y
!     antes(i) = y
!     antss(i) = y
!     blstrs(i) = y
!     browns(i) = y
!     boots(i) = y
!     cots(i) = y
!     dents(i) = y
!     doughs(i) = y
!     drs(i) = y
!     ears(i) = y
!     ems(i) = y
!     endlgs(i) = y
!     epods(i) = y
!     eseeds(i) = y
!     fps(i) = y
!     fullbs(i) = y
!     germs(i) = y
!     gpds(i) = y
!     halfbs(i) = y
!     heads(i) = y
!     hrs(i) = y
!     ies(i) = y
!     ies2(i) = y
!     infls(i) = y
!     joints(i) = y
!     lf1s(i) = y
!     lf12s(i) = y
!     lf2s(i) = y
!     lf3s(i) = y
!     lf4s(i) = y
!     lf8s(i) = y
!     mats(i) = y
!     mffls(i) = y
!     milks(i) = y
!     mpods(i) = y
!     mseeds(i) = y
!     opens(i) = y
!     silks(i) = y
!     srs(i) = y
!     tis(i) = y
!     tsints(i) = y
!     tss(i) = y
!     yelows(i) = y
!  else if (i.eq.3) then
!     aifs(i) = mo
!     antes(i) = mo
!     antss(i) = mo
!     blstrs(i) = mo
!     browns(i) = mo
!     boots(i) = mo
!     cots(i) = mo
!     dents(i) = mo
!     doughs(i) = mo
!     drs(i) = mo
!     ears(i) = mo
!     ems(i) = mo
!     endlgs(i) = mo
!     epods(i) = mo
!     eseeds(i) = mo
!     fps(i) = mo
!     fullbs(i) = mo
!     germs(i) = mo
!     gpds(i) = mo
!     halfbs(i) = mo
!     heads(i) = mo
!     hrs(i) = mo
!     ies(i) = mo
!     ies2(i) = mo
!     infls(i) = mo
!     joints(i) = mo
!     lf1s(i) = mo
!     lf12s(i) = mo
!     lf2s(i) = mo
!     lf3s(i) = mo
!     lf4s(i) = mo
!     lf8s(i) = mo
!     mats(i) = mo
!     milks(i) = mo
!     mffls(i) = mo
!     mpods(i) = mo
!     mseeds(i) = mo
!     opens(i) = mo
!     silks(i) = mo
!     srs(i) = mo
!     tis(i) = mo
!     tsints(i) = mo
!     tss(i) = mo
!     yelows(i) = mo
!  else if (i.eq.4) then
!     aifs(i) = d
!     antes(i) = d
!     antss(i) = d
!     blstrs(i) = d
!     browns(i) = d
!     boots(i) = d
!     cots(i) = d
!     dents(i) = d
!     doughs(i) = d
!     drs(i) = d
!     ears(i) = d
!     ems(i) = d
!     endlgs(i) = d
!     epods(i) = d
!     eseeds(i) = d
!     fps(i) = d
!     fullbs(i) = d
!     germs(i) = d
!     gpds(i) = d
!     halfbs(i) = d
!     heads(i) = d
!     hrs(i) = d
!     ies(i) = d
!     ies2(i) = d
!     infls(i) = d
!     joints(i) = d
!     lf1s(i) = d
!     lf12s(i) = d
!     lf2s(i) = d
!     lf3s(i) = d
!     lf4s(i) = d
!     lf8s(i) = d
!     mats(i) = d
!     milks(i) = d
!     mffls(i) = d
!     mpods(i) = d
!     mseeds(i) = d
!     opens(i) = d
!     silks(i) = d
!     srs(i) = d
!     tis(i) = d
!     tsints(i) = d
!     tss(i) = d
!     yelows(i) = d
!  end if
!end do
!
!
! arrays of days after emergence, or vernalization for growth stages
do i = 1,20
  ddae(i) = 999
  ddav(i) = 999
! arrays of growing degree-days from emergence, or vernalizaton for
!    growth stages
  dgdde(i) = 999.9
  dgddv(i) = 999.9
end do
 
!  growing degree-days from anthesis, emergence, vernalization, seeding.
gdda = 0.
gdde = 0.
gddv = 0.
gdds = 0.
!
!  days after
!dae = 0
dae = -1
dav = 0
daa = 0
dap = 0
 
 !  arrays for leaf number output table
 
do i = 1,400
  do j = 1,2
     lnarray(i,j) = 0.0
  end do
end do
 
do i = 1,100
  do j = 1,2
     lnpout(i,j) = 0.0
  end do
end do
!
! other variables
   !leaves
lncntr = 0
rowcntr = 1
todayln = 0.0
yestln = 0.0
ln = 0.
   !phenology
endphenol = .false. !debe added.
   !vernalization
jan1 = .false.      !debe added to be able to determine if jan 1 has occurred.
verns = 999         !debe added.
   !canopy height
canht = 0.0         !debe added both for canopy height
canhty = 0.0
   !emergence
!tempsw = seedsw     !debe added to initialize the array index for ggdd and egdd arrays used in emerge 5122011
 
!try the following to give tempsw a value that will work with the six element arrays in emerge
if (seedsw==1) then
  tempsw = 1
else if (seedsw==2) then
  tempsw = 3
else if (seedsw==3) then
  tempsw = 5
else if (seedsw==4) then
  tempsw = 6
end if
 
 
! debe 010809 added initilization of gddwsf array
do i = 1,15
  do j = 1,5
     gddwsf(i,j) = 0.0
  end do
end do

!debe added the initialization of co2eff variable.
co2eff = 1.0

!
!     + + + subroutines called + + +
!     scrv1
!     sdst
!     nconc
 
!     + + + input formats + + +
! print crop input data
 
 
!     + + + output formats + + +
 1000 format (5x,' a_co2=',f6.3,' b_co2=',f6.3,' a_frst=',f6.3,' b_frst=',f6.3)
 1100 format (i5,i7,i9,i11,i10,2x,2F10.1)
!
! write (*,*) 'past cinit slai=',slai
!
end subroutine cinit

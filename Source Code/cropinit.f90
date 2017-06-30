subroutine cropinit(ctrl,soils,bio, biotot)
 

    use constants, only : mnsub,mnsz, mncz,mndk
    use biomaterial, only : biomatter, biototal
    use soil, only : soildata
    use upgm_simdata, only : controls
implicit none
!
! Subroutine arguments
!
    type(controls) :: ctrl
    type(biomatter) :: bio
    type(soildata) :: soils
    type(biototal) :: biotot
!
! Local variables
!
integer :: d,dn,i,idx,mo,newrow,row,y,k
!
! Start Initialization
!
bio%mass%standstem = 0.0
bio%mass%standleaf = 0.0
bio%mass%standstore = 0.0
bio%mass%flatstem = 0.0
bio%mass%flatleaf = 0.0
bio%mass%flatstore = 0.0
 
do idx = 1, size(bio%mass%rootstorez)
  bio%mass%rootstorez(idx) = 0.0
  bio%mass%rootfiberz(idx) = 0.0
  bio%mass%stemz(idx) = 0.0
end do
 
bio%geometry%zht = 0.0
bio%geometry%dstm = 0.0
bio%geometry%zrtd = 0.0
bio%growth%dayap = 0
bio%growth%thucum = 0.0
bio%growth%trthucum = 0.0
bio%geometry%grainf = 0.0
bio%deriv%mbgrootstore = 0.0
bio%deriv%mbgrootfiber = 0.0
biotot%xstmrep = 0.0
bio%growth%fliveleaf = 0.0
 
bio%deriv%m = 0.0
bio%deriv%mst = 0.0
bio%deriv%mf = 0.0
bio%deriv%mrt = 0.0
 
do idx = 1,size(bio%deriv%mrtz)
  bio%deriv%mrtz(idx) = 0.0
end do
 
bio%deriv%rsai = 0.0
bio%deriv%rlai = 0.0
 
do idx = 1,size(bio%deriv%rsaz)
  bio%deriv%rsaz(idx) = 0.0
  bio%deriv%rlaz(idx) = 0.0
end do
 
bio%deriv%ffcv = 0.0
bio%deriv%fscv = 0.0
bio%deriv%ftcv = 0.0
 
bio%database%xstm = 0.0
bio%database%rbc = 1
bio%database%covfact = 0.0
bio%database%ck = 0.0
 
      ! initialize some derived globals for crop global variables
bio%deriv%fcancov = 0.0
bio%deriv%rcd = 0.0
 
!     initialize crop yield reporting parameters in case harvest call before planting
bio%bname = ''
bio%database%ynmu = ''
bio%database%ycon = 1.0
bio%database%ywct = 0.0
 
!     initialize crop type id to 0 indicating no crop type is growing
bio%database%idc = 0
bio%database%sla= 0.0
bio%geometry%dpop = 0.0
 
 
!     initialize row placement to be on the ridge
bio%geometry%rg = 1
 
      ! initialize decomp parameters since they are used before a crop is growing
do idx = 1,mndk
  bio%database%dkrate(idx) = 0.0
end do
bio%database%ddsthrsh = 0.0
 
!      ! temporary crop
!tempbio%mass%standstem = 0.0
!tempbio%mass%standleaf = 0.0
!tempbio%mass%standstore = 0.0
!tempbio%mass%flatstem = 0.0
!tempbio%mass%flatleaf = 0.0
!tempbio%mass%flatstore = 0.0
!tempbio%mass%flatrootstore = 0.0
!tempbio%mass%flatrootfiber = 0.0
! 
!do idx = 1, soils%spp%nslay
!  tempbio%mass%stemz(idx) = 0.0
!  tempbio%mass%leafz(idx) = 0.0
!  tempbio%mass%storez(idx) = 0.0
!  tempbio%mass%rootstorez(idx) = 0.0
!  tempbio%mass%rootfiberz(idx) = 0.0
!end do
! 
!tempbio%geometry%zht = 0.0
!tempbio%geometry%dstm = 0.0
!tempbio%geometry%xstmrep = 0.0
!tempbio%geometry%zrtd = 0.0
!tempbio%geometry%grainf = 0.0
! 
      ! values that need initialization for cdbug calls (before initial crop entry)
bio%database%tdtm = 0
 
!debe initialize emergence and phenology variables.
row = 4
newrow = 6
! emergence variables
bio%upgm%emrgflg = 0
bio%upgm%seedsw = 0
bio%upgm%gddtbg = 0.0
bio%upgm%ergdd = 0.0
bio%upgm%germgdd = 0.0
bio%upgm%wfpslo = 0.0
bio%upgm%wfpsup = 0.0
bio%upgm%soilwat = ''
bio%upgm%egdd = 0.0
bio%upgm%ggdd = 0.0
! phenology variables
bio%upgm%pchron = 0.0
bio%upgm%phenolflg = 0
!

bio%upgm%dummy1 = ''
bio%upgm%dummy2 = 0.0

!
bio%upgm%first7 = 0
bio%upgm%aepa = 0.0
!
! gddcalc variables
bio%upgm%callgdd = .true.
 
!
!canopy height
bio%upgm%ecanht = 0.0
bio%upgm%maxht = 0.0
bio%upgm%canopyflg = 0
bio%upgm%dayhtinc = 0.0
 
!growth_stress
bio%upgm%growth_stress = 0
!
! initialize arrays:
 
! debe added initialization for phenol variables
dn = 999       !day number of the year (daynum)
y = 0000       !year
mo = 0          !month
d = 0          !day
 
!  growth stages arrays	(daynum, year, mo, day)
do i = 1,4
  if (i==1) then
     bio%upgm%aifs(i) = dn
     bio%upgm%antes(i) = dn
     bio%upgm%antss(i) = dn
     bio%upgm%blstrs(i) = dn
     bio%upgm%browns(i) = dn
     bio%upgm%boots(i) = dn
     bio%upgm%cots(i) = dn
     bio%upgm%dents(i) = dn
     bio%upgm%doughs(i) = dn
     bio%upgm%drs(i) = dn
     bio%upgm%ears(i) = dn
     bio%upgm%ems(i) = dn
     bio%upgm%endlgs(i) = dn
     bio%upgm%epods(i) = dn
     bio%upgm%eseeds(i) = dn
     bio%upgm%fps(i) = dn
     bio%upgm%fullbs(i) = dn
     bio%upgm%germs(i) = dn
     bio%upgm%gpds(i) = dn
     bio%upgm%halfbs(i) = dn
     bio%upgm%heads(i) = dn
     bio%upgm%hrs(i) = dn
     bio%upgm%ies(i) = dn
     bio%upgm%ies2(i) = dn
     bio%upgm%infls(i) = dn
     bio%upgm%joints(i) = dn
     bio%upgm%lf1s(i) = dn
     bio%upgm%lf12s(i) = dn
     bio%upgm%lf2s(i) = dn
     bio%upgm%lf3s(i) = dn
     bio%upgm%lf4s(i) = dn
     bio%upgm%lf8s(i) = dn
     bio%upgm%mats(i) = dn
     bio%upgm%mffls(i) = dn
     bio%upgm%milks(i) = dn
     bio%upgm%mpods(i) = dn
     bio%upgm%mseeds(i) = dn
     bio%upgm%opens(i) = dn
     bio%upgm%silks(i) = dn
     bio%upgm%srs(i) = dn
     bio%upgm%tis(i) = dn
     bio%upgm%tsints(i) = dn
     bio%upgm%tss(i) = dn
     bio%upgm%yelows(i) = dn
  else if (i==2) then
     bio%upgm%aifs(i) = y
     bio%upgm%antes(i) = y
     bio%upgm%antss(i) = y
     bio%upgm%blstrs(i) = y
     bio%upgm%browns(i) = y
     bio%upgm%boots(i) = y
     bio%upgm%cots(i) = y
     bio%upgm%dents(i) = y
     bio%upgm%doughs(i) = y
     bio%upgm%drs(i) = y
     bio%upgm%ears(i) = y
     bio%upgm%ems(i) = y
     bio%upgm%endlgs(i) = y
     bio%upgm%epods(i) = y
     bio%upgm%eseeds(i) = y
     bio%upgm%fps(i) = y
     bio%upgm%fullbs(i) = y
     bio%upgm%germs(i) = y
     bio%upgm%gpds(i) = y
     bio%upgm%halfbs(i) = y
     bio%upgm%heads(i) = y
     bio%upgm%hrs(i) = y
     bio%upgm%ies(i) = y
     bio%upgm%ies2(i) = y
     bio%upgm%infls(i) = y
     bio%upgm%joints(i) = y
     bio%upgm%lf1s(i) = y
     bio%upgm%lf12s(i) = y
     bio%upgm%lf2s(i) = y
     bio%upgm%lf3s(i) = y
     bio%upgm%lf4s(i) = y
     bio%upgm%lf8s(i) = y
     bio%upgm%mats(i) = y
     bio%upgm%mffls(i) = y
     bio%upgm%milks(i) = y
     bio%upgm%mpods(i) = y
     bio%upgm%mseeds(i) = y
     bio%upgm%opens(i) = y
     bio%upgm%silks(i) = y
     bio%upgm%srs(i) = y
     bio%upgm%tis(i) = y
     bio%upgm%tsints(i) = y
     bio%upgm%tss(i) = y
     bio%upgm%yelows(i) = y
  else if (i==3) then
     bio%upgm%aifs(i) = mo
     bio%upgm%antes(i) = mo
     bio%upgm%antss(i) = mo
     bio%upgm%blstrs(i) = mo
     bio%upgm%browns(i) = mo
     bio%upgm%boots(i) = mo
     bio%upgm%cots(i) = mo
     bio%upgm%dents(i) = mo
     bio%upgm%doughs(i) = mo
     bio%upgm%drs(i) = mo
     bio%upgm%ears(i) = mo
     bio%upgm%ems(i) = mo
     bio%upgm%endlgs(i) = mo
     bio%upgm%epods(i) = mo
     bio%upgm%eseeds(i) = mo
     bio%upgm%fps(i) = mo
     bio%upgm%fullbs(i) = mo
     bio%upgm%germs(i) = mo
     bio%upgm%gpds(i) = mo
     bio%upgm%halfbs(i) = mo
     bio%upgm%heads(i) = mo
     bio%upgm%hrs(i) = mo
     bio%upgm%ies(i) = mo
     bio%upgm%ies2(i) = mo
     bio%upgm%infls(i) = mo
     bio%upgm%joints(i) = mo
     bio%upgm%lf1s(i) = mo
     bio%upgm%lf12s(i) = mo
     bio%upgm%lf2s(i) = mo
     bio%upgm%lf3s(i) = mo
     bio%upgm%lf4s(i) = mo
     bio%upgm%lf8s(i) = mo
     bio%upgm%mats(i) = mo
     bio%upgm%milks(i) = mo
     bio%upgm%mffls(i) = mo
     bio%upgm%mpods(i) = mo
     bio%upgm%mseeds(i) = mo
     bio%upgm%opens(i) = mo
     bio%upgm%silks(i) = mo
     bio%upgm%srs(i) = mo
     bio%upgm%tis(i) = mo
     bio%upgm%tsints(i) = mo
     bio%upgm%tss(i) = mo
     bio%upgm%yelows(i) = mo
  else if (i==4) then
     bio%upgm%aifs(i) = d
     bio%upgm%antes(i) = d
     bio%upgm%antss(i) = d
     bio%upgm%blstrs(i) = d
     bio%upgm%browns(i) = d
     bio%upgm%boots(i) = d
     bio%upgm%cots(i) = d
     bio%upgm%dents(i) = d
     bio%upgm%doughs(i) = d
     bio%upgm%drs(i) = d
     bio%upgm%ears(i) = d
     bio%upgm%ems(i) = d
     bio%upgm%endlgs(i) = d
     bio%upgm%epods(i) = d
     bio%upgm%eseeds(i) = d
     bio%upgm%fps(i) = d
     bio%upgm%fullbs(i) = d
     bio%upgm%germs(i) = d
     bio%upgm%gpds(i) = d
     bio%upgm%halfbs(i) = d
     bio%upgm%heads(i) = d
     bio%upgm%hrs(i) = d
     bio%upgm%ies(i) = d
     bio%upgm%ies2(i) = d
     bio%upgm%infls(i) = d
     bio%upgm%joints(i) = d
     bio%upgm%lf1s(i) = d
     bio%upgm%lf12s(i) = d
     bio%upgm%lf2s(i) = d
     bio%upgm%lf3s(i) = d
     bio%upgm%lf4s(i) = d
     bio%upgm%lf8s(i) = d
     bio%upgm%mats(i) = d
     bio%upgm%milks(i) = d
     bio%upgm%mffls(i) = d
     bio%upgm%mpods(i) = d
     bio%upgm%mseeds(i) = d
     bio%upgm%opens(i) = d
     bio%upgm%silks(i) = d
     bio%upgm%srs(i) = d
     bio%upgm%tis(i) = d
     bio%upgm%tsints(i) = d
     bio%upgm%tss(i) = d
     bio%upgm%yelows(i) = d
  end if
end do

!initialize CO2 arrays and variable
bio%upgm%co2x = 0
bio%upgm%co2y = 0



    end subroutine cropinit

    
        !for adding these variables:
!call cropinit(aifs,antes,antss,blstrs,boots,browns,callgdd,canopyflg,    &
!            & cots,dayhtinc,dents,doughs,drs,dummy1,dummy2,    &
!            & ears,ecanht,egdd,emrgflg,ems,endlgs,epods,ergdd,eseeds,first7,    &
!            & fps,fullbs,gddtbg,germgdd,ggdd,gmethod,gpds,growth_stress,halfbs, &
!            & heads,hrs,ies,ies2,infls,joints,lf1s,lf12s,lf2s,lf3s,lf4s,lf8s,   &
!            & mats,maxht,mffls,milks,mpods,mseeds,opens,pchron,phenolflg,       &
!            & seedsw,silks,soilwat,srs,tbase,tis,toptlo,toptup,tsints,tss,      &
!            & tupper,wfpslo,wfpsup,yelows)
!
 
 
!
!debe added emergence variables: seedsw, ergdd, germgdd, wfpslo, wfpsup,
! soilwat, gddtbg passed from main for initialization. added cliname to print
! out the name of the climate file.
!debe added growth_stress because it is now read in.
!debe added temperature variables, emrgflg, cropname and gmethod.
!debe changed seedsw from integer to real to allow moving half a soil moisture level.
! later changed it back to an integer becasue array subscripts must be integers or constants.
!debe added canopyflg.
!debe added dayhtinc to be able to pass the daily increase in height to growth
! for the ht_dia_sai subroutine in place of the weps/upgm variable dht when
! canopyflg = 1.
!debe added two new arrays of 6 elements to be used in emerge.for to
! enable adding values for germgdd and ergdd for two soil moisture
! levels that are half steps between dry and medium and medium and optimum.
!debe added ecanht so that it can be read in instead of set in the code for each crop.
!debe added all growth stage variables and phenolflg to be initialized here.
!debe added CO2 variables to be initialized here.
! debe declare variables that to be initialized for emergence, phenology
! or canopy height
!
!debe changed dimensions of dummy 1 and 2 arrays for stressed and
! non-stressed values.
!
! local variables to use in initialization of emergence variables
!
!     + + + argument declarations + + +
 
!     + + + parameters and common blocks + + +
!     include 's1layr.inc'
!     include 's1sgeo.inc'
 
!     + + + local variable declarations + + +
 
!   + + + argument definitions + + +
!   isr - this variable holds the subregion index (from definition in cdbug)
 
!   + + + local variable definitions + + +
!   d  - day. used in filling the growth stage arrays
!   dn - day number of the year. used in filling the growth stage arrays
!   i - loop control variable
!   idx - loop control variable
!   mo - month. used in filling the growth stage arrays
!   newrow - counter for filling two 6 element arrays for use in emergence
!   row - loop control variable used in filling the emergence arrays
!   y  - year. used in filling the growth stage arrays
 
!     + + + common block variables definitions + + +
!   abdstm - total number of stems (#/m^2) (live and dead).
!            may be a weighted summation.
!   abffcv - biomass cover - flat  (m^2/m^2)
!   abfscv - biomass cover - standing  (m^2/m^2)
!   ac0ck - canopy light extinction coefficient (0.0 < ac0ck < 1.0)
!   ac0idc - the crop type number (1 = annual, perennial, . . .)
!   ac0nam - crop name
!   ac0rg - crop seeding location in relation to ridge: 0 = plant in
!           furrow; 1 = plant on ridge
!   ac0sla - specific leaf area
!   accovfact - residue cover factor (m^2/kg)
!   acdayap - number of days of growth completed since crop planted
!   acddsthrsh - decomposition days required for first stem fall
!   acdkrate - array of decomposition rate parameters
!     acdkrate(1) - standing residue mass decomposition rate (d<1) (g/g/day)
!     acdkrate(2) - flat residue mass decomposition rate (d<1) (g/g/day)
!     acdkrate(3) - buried residue mass decomposition rate (d<1) (g/g/day)
!     acdkrate(4) - root residue mass decomposition rate (d<1) (g/g/day)
!     acdkrate(5) - stem residue number decline rate (d<1) (#/m^2/day)?
!                   (fall rate)
!   acdpop - crop seeding density (#/m^2)
!   acdstm - number of crop stems per unit area (#/m^2)
!   acfcancov - fraction of soil surface covered by crop canopy (m^2/m^2)
!   acffcv - crop biomass cover - flat  (m^2/m^2)
!   acfliveleaf - fraction of standing plant leaf which is living
!                 (transpiring)
!   acfscv - crop biomass cover - standing  (m^2/m^2)
!   acftcv - crop biomass cover - total  (m^2/m^2)
!            (sum of acffcv and acfscv)
!   acgrainf - internally computed grain fraction of reproductive mass
!   acm - total crop mass (stand + flat+ root) (kg/m^2)
!   acmbgstemz - crop stem mass below soil surface by layer (kg/m^2)
!                 indicates a below ground growing point for shoots
!                 from seeds or roots
!   acmf - flat crop mass (flatstem + flatleaf + flatstore) (kg/m^2)
!              flag to crop distributes stem, leaf and storeabove
!              elements between standing and flat
!   acmflatleaf - crop flat leaf mass (kg/m^2)
!   acmflatstem - crop flat stem mass (kg/m^2)
!   acmflatstore - crop flat storage mass (kg/m^2)
!   acmrootfiber - crop root fibrous mass (kg/m^2)
!   acmrootfiberz - crop root fibrous mass by soil layer (kg/m^2)
!   acmrootstore - crop root storage mass (kg/m^2)
!                  (tubers (potatoes, carrots), extended leaf (onion),
!                  seeds (peanuts))
!   acmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!   acmrt - total crop root mass (rootfiber + rootstore) (kg/m^2)
!   acmrtz - crop root mass by soil layer (kg/m^2)
!   acmst - standing crop mass (standstem + standleaf + standstore) (kg/m^2)
!   acmstandleaf - crop standing leaf mass (kg/m^2)
!   acmstandstem - crop standing stem mass (kg/m^2)
!   acmstandstore - crop standing storage mass (kg/m^2) (head with seed,
!                   or vegetative head (cabbage, pineapple))
!   acrbc - crop residue burial class (it exists in crop so it can be carried into residue)
!           1 - fragile-very small (soybeans) residue
!           2 - moderately tough-short (wheat) residue
!           3 - non fragile-med (corn) residue
!           4 - woody-large residue
!           5 - gravel-rock
!   acrcd - effective biomass silhouette area (sai+lai) (m^2/m^2)
!              (combination of leaf area and stem area indices)
!   acrlai - crop leaf area index (m^2/m^2)
!   acrlaz - crop leaf area index by height (1/m)
!   acrsai - crop stem area index (m^2/m^2)
!   acrsaz - crop stem area index by height (1/m)
!   actdtm - days from planting to maturity for summer crops, or the days
!            from start of spring growth to maturity for winter and
!            perennial crops.
!   acthucum - crop accumulated heat units
!   actrthucum - accumulated root growth heat units (degree-days)
!   acxstm - crop stem diameter (m)
!   acxstmrep - a representative diameter so that
!               acdstm*acxstmrep*aczht=acrsai
!   acycon - conversion factor from kg/m^2 to units named in acynmu (all
!            dry weight)
!   acynmu - string for name of units in which yield of interest will be
!            reported
!   acywct - water content at which yield is to be reported (percent)
!   aczht - crop height (m)
!   aczrtd - crop root depth (m)
!   atdstm - temporary storage of number of crop stems per unit area
!            (#/m^2)
!   atgrainf - temporary storage of internally computed grain fraction
!              of reproductive mass
!   atmbgleafz - temporary storage of crop buried leaf mass by layer(kg/m^2)
!   atmbgrootfiberz - temporary storage of crop root fibrous mass by layer
!                     (kg/m^2)
!   atmbgrootstorez - temporary storage of crop root storage mass by layer
!                     (kg/m^2)
!   atmbgstemz - temporary storage of crop buried stem mass by layer
!                (kg/m^2)
!   atmbgstorez - temporary storage of crop buried storage mass by layer
!                 (kg/m^2)
!   atmflatleaf - temporary storage of crop flat leaf mass (kg/m^2)
!   atmflatrootfiber - temporary storage of crop flat root fibrous mass
!                      (kg/m^2)
!   atmflatrootstore - temporary storage of crop flat root storage mass
!                      (kg/m^2) (tubers (potatoes, carrots), extended
!                      leaf (onion), seeds (peanuts))
!   atmflatstem - temporary storage of crop flat stem mass (kg/m^2)
!   atmflatstore - temporary storage of crop flat storage mass (kg/m^2)
!   atmstandleaf - temporary storage of crop standing leaf mass (kg/m^2)
!   atmstandstem - temporary storage of crop standing stem mass (kg/m^2)
!   atmstandstore - temporary storage of crop standing storage mass (kg/m^2)
!                   (head with seed, or vegetative head (cabbage,
!                   pineapple))
!   atxstmrep - temporary storage of a representative diameter so that
!               acdstm*acxstmrep*aczht=acrsai
!   atzht - temporary storage of crop height (m)
!   atzrtd - temporary storage of crop root depth (m)
!   cprevrotation - rotation count number previously printed in crop
!                   harvest report
!   growth_stress- flag setting which turns on water or temperature
!                  stress (or both)
!                  growth_stress = 0  ! no stress values applied
!                  growth_stress = 1  ! turn on water stress
!                  growth_stress = 2  ! turn on temperature stress
!                  growth_stress = 3  ! turn on both
!    debe because it is now being read in it is commented out in command.inc
!   mncz - maximum number of crop height segments
!   mndk - maximum number of decay coefficients (st,fl,bg,rt,stem no)
!   mnsz - maximum number of soil layers
 
!     + + +  newly added arguments definitions + + +
!   aepa - the parameter for duration of anthesis (i.e., gdd from start
!          to end of anthesis.
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
!   callgdd - a flag to switch between methods for determining gdd.
!             if the flag is set to true then gddcalc subroutine is
!             called. otherwise, the code within crop is used.
!   canopyflg - a flag to determine whether the old upgm/weps method of
!               determining canopy height will be used (canopyflg=0) or the
!               method brought in from phenologymms will be used (canopyflg=1).
!   co2atmos - the atmospheric level of CO2.
!   co2x - the CO2 levels in ppm. The x axis on the relationship curve.
!   co2y - the relative effect at different levels of CO2, i.e. co2x.
!   cots - cotyledonary and unifoliolate leaves are visible in dry
!            beans. this array includes daynum, year, month and day
!            of when this stage was reached.
!   cropname - name of the crop
!   dayhtinc - the increase in plant height for today.
!   dents - the dent growth stage in corn. this array includes daynum,
!             year, month and day of when this stage was reached.
!     doughs - the dough growth stage in corn. this array includes daynum,
!              year, month and day of when this stage was reached.
!     drs - double ridge growth stage for hay millet, proso millet, spring
!           barley, spring wheat, winter barley and winter wheat. this array
!           includes daynum, year, month and day of when this stage was
!           reached.
!   dummy1 - in determining the next phenological growth stage, this
!            holds whether the condition is gn or gs, that is when gdd
!            values are used to advance to the next growth stage is it
!            done under non-stressed or stressed conditions.
!   dummy2 - an array to hold the gdd values, both under stressed
!            and non- stressed conditions,required to reach each growth
!            stage of the current crop.
!     ears - the ear initiation stage in corn. this array includes daynum,
!            year, month and day of when this stage was reached.
!   ecanht - this is the maximum canopy height of the crop in phase 1 of
!            the canoy height growth.  this is usually from emergence to
!            when the plant begins elongating stems but this stage varies
!            among crops. it is an input parameter and is read in from upgm_crop.dat.
!   egdd - a 6 element array that holds the ergdd values plus calculated values
!            for two intermediate soil moisture level values in elements 2 and 4.
!   emrgflg - a flag to determine if the new emerge subroutine should be
!             called (emrgflg=1) or to proceed with the weps/upgm method
!             of achieving emergence (emrgflg=0).
!     ems - day when emergence occurred in all crops. this array includes
!     endlgs - end of leaf growth stage in sorghum. this array includes
!              daynum, year, month and day of when this stage was reached.
!     epods - one pod has reached the maximum length in dry beans (early
!             pod set). this array includes daynum,year, month and day of
!             when this stage was reached.
!     ergdd - an array holding 4 elongation rates in mm per gdd
!             based on each soil moisture description.
!     eseeds - there is one pod with fully developed seeds in dry
!              beans (early seed fill). this array includes daynum, year,
!              month and day of when this stage was reached.
!           daynum, year, month and day of when this event occurred.
!   first7 - used to set the value of aepa the first time phenolww is
!            called.
!     fps - flower primordium initiation growth stage. this array includes
!           daynum, year, month and day of when this stage was reached.
!     fullbs - full bloom growth stage in sorghum. this array includes
!            daynum, year, month and day of when this stage was reached.
!   gddtbg - used to accumulate gdd for seeds planted in dust after a
!            rainfall event has moved the soil moisture condition to
!            dry.  the seed can begin accumulating gdd's germination.
!   germgdd - an array holding 4 germination times in gdd at base 0°c for
!             the soil moisture levels
!   ggdd - a 6 element array that holds the germgdd values plus calculated values for
!           two intermediate soil moisture level values in elements 2 and 4.
!   gmethod - number indicates which method of calculating gdd is used.
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
!     mats - physiological maturity growth stage for corn, dry beans,
!            hay millet, proso millet, sorghum, spring barley, spring
!            wheat, sunflower, winter barley and winter wheat. in dry beans,
!            one pod has changed color/striped. this array includes
!            daynum, year, month and day of when this stage was reached.
!   maxht - this is the maximum canopy height of the crop.  it is an
!           input parameter and is read in from upgm_crop.dat.
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
!     opens - the sunflower inflorescence begins to open. this array includes
!             daynum, year, month and day of when this stage was reached.
!   pchron - phyllochron value which is the number of gdd per leaf.
!     phenolflg - a flag that determines if the upgm/weps method of determining maturity
!                 will be used (phenolflg =0) or the phenologymms method will be used (phenolflg = 1).
!   seedsw - soil water content at seed depth.  it is read in as
!            optimum, medium, dry or planted in dust and converted
!            to an integer.	 1 = optimum, 2 = medium, 3 = dry and
!            4 = planted in dust
!     silks - the silking growth stage in corn. this array includes daynum,
!             year, month and day of when this stage was reached.
!   soilwat - an array holding the swtype for each soil moisture
!             condition
!     srs - single ridge growth stage for hay millet, proso millet, spring
!           barley, spring wheat, winter barley and winter wheat. this array
!           includes daynum, year, month and day of when this stage was
!           reached.
!   tbase - lowest temperature below which no growth occurs (deg.c).
!     tis - start of tillering growth stage for corn, hay millet, proso
!           millet, sorghum, spring barley, spring wheat, winter barley and
!           winter wheat. this array includes daynum, year, month and day of
!           when this stage was reached.
!   toptlo - the lower temperature in the optimum range for plant
!            growth (deg.c).
!   toptup - the upper temperature in the optimum range for plant
!            growth (deg.c).
!     tsints - tassel initiation growth stage in corn. this array includes
!              daynum, year, month and day of when this stage was reached.
!     tss - terminal spikelet growth stage for spring and winter wheat. this
!           array includes daynum, year, month and day of when this stage was
!           reached.
!   tupper - upper/maximum temperature for plant growth (deg.c).
!            no growth with temperatures above this point.
!   wfpslo - an array holding the low values for each soil moisture
!            condition.
!   wfpsup - an array holding the high values for each soil moisture
!            condition.
!     yelows - back of the sunflower head is a light yellow. this array
!              includes daynum, year, month and day of when this stage was
!              reached.
 
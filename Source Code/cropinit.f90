subroutine cropinit(sppdat,bio,isr,aepa,aifs,antes,antss,blstrs,boots,browns,callgdd,      &
                  & canopyflg,cliname,cots,cropname,dayhtinc,dents,doughs,drs,  &
                  & dummy1,dummy2,ears,ecanht,egdd,emrgflg,ems,endlgs,epods,    &
                  & ergdd,eseeds,first7,fps,fullbs,gddtbg,germgdd,germs,ggdd,   &
                  & gmethod,gpds,growth_stress,halfbs,heads,hrs,ies,ies2,infls, &
                  & joints,lf1s,lf12s,lf2s,lf3s,lf4s,lf8s,mats,maxht,mffls,     &
                  & milks,mpods,mseeds,opens,pchron,phenolflg,seedsw,silks,     &
                  & soilwat,srs,tbase,tis,toptlo,toptup,tsints,tss,tupper,      &
                  & wfpslo,wfpsup,yelows,co2atmos,co2x,co2y)
 
    !for adding these variables:
!call cropinit(aifs,antes,antss,blstrs,boots,browns,callgdd,canopyflg,    &
!            & cliname,cots,cropname,dayhtinc,dents,doughs,drs,dummy1,dummy2,    &
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
    use constants, only : mnsub,mnsz, mncz,mndk
    use biomaterial, only : biomatter
    use soil, only : soil_phys_props
implicit none
!
include 'c1info.inc'
include 'c1gen.inc'
include 'c1db1.inc'
include 'c1db2.inc'
include 'c1glob.inc'
!
! Dummy arguments
!
    type(biomatter) :: bio
    type(soil_phys_props) :: sppdat
real :: aepa,dayhtinc,ecanht,gddtbg,maxht,pchron,tbase,toptlo,toptup,tupper,    &
      & co2atmos
logical :: callgdd
integer :: canopyflg,emrgflg,first7,gmethod,growth_stress,isr,phenolflg,seedsw
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
integer :: d,dn,i,idx,mo,newrow,row,y,k
!
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
!   cliname - the name of the location for the climate data
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
 
acmstandstem(isr) = 0.0
acmstandleaf(isr) = 0.0
acmstandstore(isr) = 0.0
acmflatstem(isr) = 0.0
acmflatleaf(isr) = 0.0
acmflatstore(isr) = 0.0
 
do idx = 1,mnsz
  acmrootstorez(idx,isr) = 0.0
  acmrootfiberz(idx,isr) = 0.0
  acmbgstemz(idx,isr) = 0.0
end do
 
aczht(isr) = 0.0
acdstm(isr) = 0.0
aczrtd(isr) = 0.0
acdayap(isr) = 0
acthucum(isr) = 0.0
actrthucum(isr) = 0.0
acgrainf(isr) = 0.0
acmrootstore(isr) = 0.0
acmrootfiber(isr) = 0.0
acxstmrep(isr) = 0.0
acfliveleaf(isr) = 0.0
 
acm(isr) = 0.0
acmst(isr) = 0.0
acmf(isr) = 0.0
acmrt(isr) = 0.0
 
do idx = 1,mnsz
  acmrtz(idx,isr) = 0.0
end do
 
acrsai(isr) = 0.0
acrlai(isr) = 0.0
 
do idx = 1,mncz
  acrsaz(idx,isr) = 0.0
  acrlaz(idx,isr) = 0.0
end do
 
acffcv(isr) = 0.0
acfscv(isr) = 0.0
acftcv(isr) = 0.0
 
acxstm(isr) = 0.0
acrbc(isr) = 1
accovfact(isr) = 0.0
ac0ck(isr) = 0.0
 
      ! initialize some derived globals for crop global variables
acfcancov(isr) = 0.0
acrcd(isr) = 0.0
 
!     initialize crop yield reporting parameters in case harvest call before planting
ac0nam(isr) = ''
acynmu(isr) = ''
acycon(isr) = 1.0
acywct(isr) = 0.0
 
!     initialize crop type id to 0 indicating no crop type is growing
ac0idc(isr) = 0
ac0sla(isr) = 0.0
acdpop(isr) = 0.0
 
 
!     initialize row placement to be on the ridge
ac0rg = 1
 
      ! initialize decomp parameters since they are used before a crop is growing
do idx = 1,mndk
  acdkrate(idx,isr) = 0.0
end do
acddsthrsh(isr) = 0.0
 
      ! temporary crop
bio%mass%standstem = 0.0
bio%mass%standleaf = 0.0
bio%mass%standstore = 0.0
bio%mass%flatstem = 0.0
bio%mass%flatleaf = 0.0
bio%mass%flatstore = 0.0
bio%mass%flatrootstore = 0.0
bio%mass%flatrootfiber = 0.0
 
do idx = 1, sppdat%nslay
  bio%mass%stemz(idx) = 0.0
  bio%mass%leafz(idx) = 0.0
  bio%mass%storez(idx) = 0.0
  bio%mass%rootstorez(idx) = 0.0
  bio%mass%rootfiberz(idx) = 0.0
end do
 
bio%geometry%zht = 0.0
bio%geometry%dstm = 0.0
bio%geometry%xstmrep = 0.0
bio%geometry%zrtd = 0.0
bio%geometry%grainf = 0.0
 
      ! values that need initialization for cdbug calls (before initial crop entry)
actdtm(isr) = 0
 
!debe initialize emergence and phenology variables.
row = 4
newrow = 6
! emergence variables
emrgflg = 0
seedsw = 0
gddtbg = 0.0
do idx = 1,row
  ergdd(idx) = 0.0
  germgdd(idx) = 0.0
  wfpslo(idx) = 0.0
  wfpsup(idx) = 0.0
  soilwat(idx) = ''
end do
do idx = 1,newrow
  egdd(idx) = 0.0
  ggdd(idx) = 0.0
end do
! phenology variables
pchron = 0.0
phenolflg = 0
!
do i = 1,30
  dummy1(i) = ''
  dummy2(i) = 0.0
end do
!
first7 = 0
aepa = 0.0
!
! gddcalc variables
callgdd = .true.
!
!climate file name
cliname = ' '
 
!
!canopy height
ecanht = 0.0
maxht = 0.0
canopyflg = 0
dayhtinc = 0.0
 
!growth_stress
growth_stress = 0
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
     aifs(i) = dn
     antes(i) = dn
     antss(i) = dn
     blstrs(i) = dn
     browns(i) = dn
     boots(i) = dn
     cots(i) = dn
     dents(i) = dn
     doughs(i) = dn
     drs(i) = dn
     ears(i) = dn
     ems(i) = dn
     endlgs(i) = dn
     epods(i) = dn
     eseeds(i) = dn
     fps(i) = dn
     fullbs(i) = dn
     germs(i) = dn
     gpds(i) = dn
     halfbs(i) = dn
     heads(i) = dn
     hrs(i) = dn
     ies(i) = dn
     ies2(i) = dn
     infls(i) = dn
     joints(i) = dn
     lf1s(i) = dn
     lf12s(i) = dn
     lf2s(i) = dn
     lf3s(i) = dn
     lf4s(i) = dn
     lf8s(i) = dn
     mats(i) = dn
     mffls(i) = dn
     milks(i) = dn
     mpods(i) = dn
     mseeds(i) = dn
     opens(i) = dn
     silks(i) = dn
     srs(i) = dn
     tis(i) = dn
     tsints(i) = dn
     tss(i) = dn
     yelows(i) = dn
  else if (i==2) then
     aifs(i) = y
     antes(i) = y
     antss(i) = y
     blstrs(i) = y
     browns(i) = y
     boots(i) = y
     cots(i) = y
     dents(i) = y
     doughs(i) = y
     drs(i) = y
     ears(i) = y
     ems(i) = y
     endlgs(i) = y
     epods(i) = y
     eseeds(i) = y
     fps(i) = y
     fullbs(i) = y
     germs(i) = y
     gpds(i) = y
     halfbs(i) = y
     heads(i) = y
     hrs(i) = y
     ies(i) = y
     ies2(i) = y
     infls(i) = y
     joints(i) = y
     lf1s(i) = y
     lf12s(i) = y
     lf2s(i) = y
     lf3s(i) = y
     lf4s(i) = y
     lf8s(i) = y
     mats(i) = y
     mffls(i) = y
     milks(i) = y
     mpods(i) = y
     mseeds(i) = y
     opens(i) = y
     silks(i) = y
     srs(i) = y
     tis(i) = y
     tsints(i) = y
     tss(i) = y
     yelows(i) = y
  else if (i==3) then
     aifs(i) = mo
     antes(i) = mo
     antss(i) = mo
     blstrs(i) = mo
     browns(i) = mo
     boots(i) = mo
     cots(i) = mo
     dents(i) = mo
     doughs(i) = mo
     drs(i) = mo
     ears(i) = mo
     ems(i) = mo
     endlgs(i) = mo
     epods(i) = mo
     eseeds(i) = mo
     fps(i) = mo
     fullbs(i) = mo
     germs(i) = mo
     gpds(i) = mo
     halfbs(i) = mo
     heads(i) = mo
     hrs(i) = mo
     ies(i) = mo
     ies2(i) = mo
     infls(i) = mo
     joints(i) = mo
     lf1s(i) = mo
     lf12s(i) = mo
     lf2s(i) = mo
     lf3s(i) = mo
     lf4s(i) = mo
     lf8s(i) = mo
     mats(i) = mo
     milks(i) = mo
     mffls(i) = mo
     mpods(i) = mo
     mseeds(i) = mo
     opens(i) = mo
     silks(i) = mo
     srs(i) = mo
     tis(i) = mo
     tsints(i) = mo
     tss(i) = mo
     yelows(i) = mo
  else if (i==4) then
     aifs(i) = d
     antes(i) = d
     antss(i) = d
     blstrs(i) = d
     browns(i) = d
     boots(i) = d
     cots(i) = d
     dents(i) = d
     doughs(i) = d
     drs(i) = d
     ears(i) = d
     ems(i) = d
     endlgs(i) = d
     epods(i) = d
     eseeds(i) = d
     fps(i) = d
     fullbs(i) = d
     germs(i) = d
     gpds(i) = d
     halfbs(i) = d
     heads(i) = d
     hrs(i) = d
     ies(i) = d
     ies2(i) = d
     infls(i) = d
     joints(i) = d
     lf1s(i) = d
     lf12s(i) = d
     lf2s(i) = d
     lf3s(i) = d
     lf4s(i) = d
     lf8s(i) = d
     mats(i) = d
     milks(i) = d
     mffls(i) = d
     mpods(i) = d
     mseeds(i) = d
     opens(i) = d
     silks(i) = d
     srs(i) = d
     tis(i) = d
     tsints(i) = d
     tss(i) = d
     yelows(i) = d
  end if
end do

!initialize CO2 arrays and variable
co2x = 0
co2y = 0
co2atmos = 0.0
!do k = 1,10
!  co2x(k) = 0.0
!  co2y(k) = 0.0
!end do


end subroutine cropinit

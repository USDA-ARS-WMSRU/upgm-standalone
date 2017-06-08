subroutine shoot_grow(bnslay,bszlyd,bcdpop,bczmxc,bczmrt,bcfleafstem,bcfshoot,  &
                    & bc0ssa,bc0ssb,bc0diammax,hui,huiy,bcthu_shoot_beg,        &
                    & bcthu_shoot_end,bcmstandstem,bcmstandleaf,bcmstandstore,  &
                    & bcmflatstem,bcmflatleaf,bcmflatstore,bcmshoot,bcmtotshoot,&
                    & bcmbgstemz,bcmrootstorez,bcmrootfiberz,bczht,bczshoot,    &
                    & bcdstm,bczrtd,bczgrowpt,bcfliveleaf,bc0nam,bchyfg,        &
                    & bcyld_coef,bcresid_int,bcgrf,daysim,dap,gdds,bc0growdepth,&
                    & seedsw,cropname,soilwat,wfpslo,wfpsup,germgdd,ergdd,      &
                    & gddtbg,ddap,dgdds,elong,ems,germs,gddday,yy,emrgflg,icli, &
                    & pd,pm,py,yr,cliname,egdd,ggdd,tempsw)
 
! ***** emergence *****
!
!debe added the following emergence arguments so that they can be passed
! to emerge. these include seedsw, cropname, soilwat, wfpslo, wfpsup,
! germgdd, ergdd, gddday, gddtbg, gdds, dap, ddap, dgdds, elong, ems,
! germs, yy. ac0nam is passed into cropname in main to be passed to
! emerge. bc0growdepth (weps/upgm) variable is passed into
! emerge where it is used in setting pdepth (planting depth).
!debe added the emergence flag (emrgflg)to determine whether to call the
! emerge subroutine or not.
!debe added icli to pass to emerge to enable printing the weather file
! name in emerge.out. also, added pd,pm,py to print planting date in
! emerge.out. debe used gdds and dap (phenologymms variables) in place
! of bcthucum and bcdayap (weps/upgm variables).
!debe changed seedsw from integer to real to allow moving half a soil moisture level.
! later changed back to an integer because array subscripts must be integers or constants.
!debe flag to call emerge subroutine - 0 = don't call emerge and
! 1 = call emerge
!debe added seedbed to write it to the output file when implementing
! emergence the weps/upgm way.
!debe added two new arrays of 6 elements to be used in emerge.for to
! enable adding values for germgdd and ergdd for two soil moisture
! levels that are half steps between dry and medium and medium and optimum.
! later added tempsw, the array index for these new arrays. it needed to be
! initialized in cinit and passed through. 5122011
    use constants, only : mgtokg, mmtom
 
implicit none
!
include 'file.fi'
include 'm1flag.inc'
include 'command.inc'
!
! PARAMETER definitions
!
real,parameter :: shoot_exp = 2.0,be_stor = 0.7,rootf = 0.4
!
! Dummy arguments
!
real :: bc0diammax,bc0growdepth,bc0ssa,bc0ssb,bcdpop,bcdstm,bcfleafstem,        &
      & bcfliveleaf,bcfshoot,bcgrf,bcmflatleaf,bcmflatstem,bcmflatstore,        &
      & bcmshoot,bcmstandleaf,bcmstandstem,bcmstandstore,bcmtotshoot,           &
      & bcresid_int,bcthu_shoot_beg,bcthu_shoot_end,bcyld_coef,bczgrowpt,bczht, &
      & bczmrt,bczmxc,bczrtd,bczshoot,elong,gddday,gdds,gddtbg,hui,huiy
character(80) :: bc0nam,cropname
integer :: bchyfg,bnslay,dap,daysim,emrgflg,icli,pd,pm,py,seedsw,tempsw,yr,yy
character(80) :: cliname
real,dimension(*) :: bcmbgstemz,bcmrootfiberz,bcmrootstorez
real, dimension(bnslay) :: bszlyd
integer,dimension(20) :: ddap
real,dimension(20) :: dgdds
real,dimension(6) :: egdd,ggdd
integer,dimension(4) :: ems,germs
real,dimension(4) :: ergdd,germgdd,wfpslo,wfpsup
character(80),dimension(4) :: soilwat
!
! Local variables
!
real :: ag_stem,bg_stem,diff_mass,dlfwt,drpwt,drswt,dstwt,d_leaf_mass,          &
      & d_root_mass,d_shoot_mass,d_stem_mass,d_s_root_mass,end_root_mass,       &
      & end_shoot_len,end_shoot_mass,end_stem_area,end_stem_mass,fexp_hui,      &
      & fexp_huiy,flat_stem,f_root_sum,lost_mass,red_mass_rat,shoot_hui,        &
      & shoot_huiy,stand_stem,stem_propor,s_root_sum,tot_mass_req,yesterday_len
integer :: day,doy,lay,mo
integer :: dayear
real :: frac_lay
character(80) :: seedbed
!
!     + + + purpose + + +
 
!     + + + keywords + + +
!     shoot growth
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     bc0diammax - crop maximum plant diameter (m)
!     bc0growdepth - depth of growing point at time of planting (m)
!     bc0nam - crop name
!     bc0ssa - stem area to mass coefficient a, result is m^2 per plant
!     bc0ssb - stem area to mass coefficient b, argument is kg per plant
!     bcdayap - number of days of growth completed since crop planted
!     bcdpop - number of plants per unit area (#/m^2)
!            - note: bcdstm/bcdpop gives the number of stems per plant
!     bcdstm - number of crop stems per unit area (#/m^2)
!     bcfleafstem - crop leaf to stem mass ratio for shoots
!     bcfliveleaf - fraction of standing plant leaf which is living
!                   (transpiring)
!     bcfshoot - crop ratio of shoot diameter to length
!     bcgrf  - fraction of reproductive biomass that is yield
!     bchyfg - flag indicating the part of plant to apply the "grain
!              fraction", grf, to when removing that plant part for yield
!         0    grf applied to above ground storage (seeds, reproductive)
!         1    grf times growth stage factor (see growth.for) applied to
!              above ground storage (seeds, reproductive)
!         2    grf applied to all aboveground biomass (forage)
!         3    grf applied to leaf mass (tobacco)
!         4    grf applied to stem mass (sugarcane)
!         5    grf applied to below ground storage mass (potatoes,
!              peanuts)
!     bcmbgstemz - crop stem mass below soil surface by layer (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstem  - crop flat stem mass (kg/m^2)
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
!                    (head with seed, or vegetative head
!                    (cabbage, pineapple))
!     bcmtotshoot - total mass of shoot growing from root storage biomass
!                   (kg/m^2)in the period from beginning to completion of
!                   emergence heat units
!     bcresid_int - residue intercept (kg/m^2)
!                   harvest_residue = bcyld_coef(kg/kg) *
!                   yield + bcresid_int (kg/m^2)
!     bcthu_shoot_beg - heat unit index (fraction) for beginning of shoot
!                       grow from root storage period
!     bcthu_shoot_end - heat unit index (fraction) for end of shoot grow
!                       from root storage period
!     bcyld_coef - yield coefficient (kg/kg)
!                  harvest_residue = bcyld_coef(kg/kg) *
!                  yield + bcresid_int (kg/m^2)
!     bczgrowpt - depth in the soil of the growing point (m)
!     bczht  - crop height (m)
!     bczmrt - maximum root depth
!     bczmxc - maximum potential plant height (m)
!     bczrtd - root depth (m)
!     bczshoot - length of actively growing shoot from root biomass (m)
!     bnslay - number of soil layers
!     bszlyd - depth from top of soil to bottom of layer, m
!     daysim - day of the simulation
!     hui - heat unit index for today
!     huiy - heat unit index for yesterday
 
!     + + + local variables + + +
 
!     + + + local variable definitions + + +
!     ag_stem - above ground stem mass (mg/shoot)
!     bg_stem - below ground stem mass (mg/shoot)
!     d_leaf_mass - mass increment added to leaf for the present day
!                   (mg/shoot)
!     d_root_mass - mass increment added to roots for the present day
!                   (mg/shoot)
!     d_s_root_mass - mass increment removed from storage roots for the
!                     present day (mg/shoot)
!     d_shoot_mass - mass increment added to shoot for the present day
!                    (mg/shoot)
!     d_stem_mass - mass increment added to stem for the present day
!                   (mg/shoot)
!     day - day of month
!     diff_mass - mass difference for adjustment
!     dlfwt - increment in leaf dry weight (kg/m^2)
!     doy - day of year
!     drpwt - increment in reproductive mass (kg/m^2)
!     drswt - biomass diverted from partitioning to root storage
!     dstwt - increment in dry weight of stem (kg/m^2)
!     end_root_mass - total root mass at end of shoot growth period
!                     (mg/shoot)
!     end_shoot_len - total shoot length at end of shoot growth period (m)
!     end_shoot_mass - total shoot mass at end of shoot growth period
!                      (mg/shoot)
!     end_stem_area - total stem area at end of shoot growth period
!                     (m^2/shoot)
!     end_stem_mass - total stem mass at end of shoot growth period
!                     (mg/shoot)
!     f_root_sum - fibrous root mass sum (total in all layers (kg/m^2)
!     fexp_hui - exponential function evaluated at todays shoot heat unit
!                index
!     fexp_huiy - exponential function evaluated at yesterdays shoot heat
!                 unit index
!     flat_stem - flat stem mass (mg/shoot)
!     lay - index into soil layers for looping
!     lost_mass - passed into cook yield, is simply set to zero
!     mo - month of year
!     red_mass_rat - ratio of reduced mass available for stem growth to
!                    expected mass available
!     s_root_sum - storage root mass sum (total in all layers) (kg/m^2)
!     shoot_hui - today's fraction of heat unit shoot growth index
!                 accumulation
!     shoot_huiy - previous day fraction of heat unit shoot growth index
!                  accumulation
!     stand_stem - standing stem mass (mg/shoot)
!     stem_propor - ratio of standing stems mass to flat stem mass
!     tot_mass_req - mass required from root mass for one shoot (mg/shoot)
!     yesterday_len - length of shoot yesterday (m)
!     yr - year
 
!     + + + local parameters + + +
 
!     + + + local parameter definitions + + +
!     be_stor - conversion efficiency of biomass from storage to growth
!     rootf - fraction of biomass allocated to roots when growing from
!             seed
!     shoot_exp - exponent for shape of exponential function
!                 small numbers  go toward straight line
!                 large numbers delay development to end of period
 
!     + + + common blocks + + +
 
!     + + + common block variable definitions + + +
!     am0cfl - flag to print crop output
!     cook_yield - flag setting which uses input from crop record to
!                  guarantee a fixed yield/redsidue ratio at harvest
!     mgtokg - to convert milligrams to kilograms, multiply by 0.000001.
!              parameter (mgtokg = 0.000001)
!     mmtom - unit conversion constant (m/mm). parameter (mmtom  = 0.001)
 
!     + + + newly added arguments + + +
 
!     + + +  newly added arguments definitions + + +
!     cliname - the name of the location for the climate file. used to
!               write out the name of the climate file location in
!               emerge.out.
!     cropname - name of the current crop.
!     dap - days after planting.
!     ddap - array holding the number of days after planting for up
!            to 20 different stages.
!     dgdds - array holding the number of gdd after seeding for up
!             to 20 different stages.
!     egdd - a 6 element array that holds the ergdd values plus calculated values
!            for two intermediate soil moisture level values in elements 2 and 4.
!     elong - total elongation of the emerging seedling based on the
!             day's gdd (mm)
!     emrgflg - a flag to determine if the new emerge subroutine should
!               be called (emrgflg=1) or to proceed with the weps/upgm
!               method of achieving emergence (emrgflg=0).
!     ems - simulated day that emergence began.
!     ergdd - an array holding 4 elongation rates in mm per gdd
!             based on each soil moisture description.
!     gddday - the number of gdd with 0°C base temperature for that day.
!     gdds - cumulative growing degree-days since planting, not including
!            days with severely dry soil.  accumulation ends when
!            emergence is complete; ec day.
!     gddtbg - used to accumulate gdd to begin germination.
!     germgdd - an array holding 4 germination times in gdd at base 0°c
!               for the soil moisture levels.
!     germs - simulated day that germination occurs.
!     ggdd - a 6 element array that holds the germgdd values plus calculated values for
!           two intermediate soil moisture level values in elements 2 and 4.
!     icli - a flag to determine which type of weather file to read.  a
!            value of 1 indicates that climate data should be read from
!            the cligen weather file.  a value of 0 indicates that a
!            historical climate file will be used.
!     pd - planting day.
!     pm - planting month.
!     py - planting year.  currently, not the calendar year.
!     seedbed - description of the soil moisture condition. used to
!               convert the character to an integer.
!     seedsw - soil water content at seed depth.  it is read in as
!              optimum, medium, dry or planted in dust and converted
!              to an integer.	 1 = optimum, 2 = medium, 3 = dry and
!              4 = planted in dust.
!     soilwat - an array holding the swtype for each soil moisture
!       condition.
!     tempsw - a new variable to designate the array subscripts for the new 6 element
!              arrays: egdd, ggdd
!     wfpslo - an array holding the low values for each soil moisture
!       condition.
!     wfpsup - an array holding the high values for each soil moisture
!       condition.
!     yy - year from cinit subroutine.
 
!     + + + functions called + + +
!     dayear
!     frac_lay
 
!     + + + end of specifications + + +
 
call caldatw(day,mo,yr)
doy = dayear(day,mo,yr)
 
   ! fraction of shoot growth from stored reserves (today and yesterday)
shoot_hui = min(1.0,(hui-bcthu_shoot_beg)/(bcthu_shoot_end-bcthu_shoot_beg))
shoot_huiy = max(0.0,(huiy-bcthu_shoot_beg)/(bcthu_shoot_end-bcthu_shoot_beg))
 
      ! total shoot mass is grown at an exponential rate
fexp_hui = (exp(shoot_exp*shoot_hui)-1.0)/(exp(shoot_exp)-1)
fexp_huiy = (exp(shoot_exp*shoot_huiy)-1.0)/(exp(shoot_exp)-1)
 
      ! sum present storage and fibrous root mass (kg/m^2)
s_root_sum = 0.0
f_root_sum = 0.0
do lay = 1,bnslay
  s_root_sum = s_root_sum + bcmrootstorez(lay)
  f_root_sum = f_root_sum + bcmrootfiberz(lay)
end do
 
      ! calculate storage mass required to grow a single shoot
      ! units: kg/m^2 / ( shoots/m^2 * kg/mg ) = mg/shoot
tot_mass_req = bcmtotshoot/(bcdstm*mgtokg)
 
      ! divide ending mass between shoot and root
if (f_root_sum<=bcmshoot) then   ! this works as long as rootf <= 0.5
          !roots develop along with shoot from same mass
  end_shoot_mass = tot_mass_req*be_stor*(1.0-rootf)
  end_root_mass = tot_mass_req*be_stor*rootf
else
          !roots remain static, while shoot uses all mass from storage
  end_shoot_mass = tot_mass_req*be_stor
  end_root_mass = 0.0
end if
 
      ! this days incremental shoot mass for a single shoot (mg/shoot)
d_shoot_mass = end_shoot_mass*(fexp_hui-fexp_huiy)
d_root_mass = end_root_mass*(fexp_hui-fexp_huiy)
 
      ! this days mass removed from the storage root (mg/shoot)
d_s_root_mass = (d_shoot_mass+d_root_mass)/be_stor
 
      ! check that sufficient storage root mass is available
      ! units: mg/shoot = kg/m^2 / (kg/mg * shoot/m^2)
diff_mass = d_s_root_mass - s_root_sum/(bcdstm*mgtokg)
if (diff_mass>0.0) then
          ! reduce removal to match available storage
  red_mass_rat = d_s_root_mass/(diff_mass+d_s_root_mass)
          ! adjust root increment to match
  d_root_mass = d_root_mass*red_mass_rat
          ! adjust shoot increment to match
  d_shoot_mass = d_shoot_mass*red_mass_rat
          ! adjust removal amount to match exactly
  d_s_root_mass = d_s_root_mass*red_mass_rat
end if
 
      ! if no additional mass, no need to go further
if (d_shoot_mass<=0.0) return
!! +++++++++++++ return from here if zero +++++++++++++++++
 
      ! find stem mass when shoot completely developed
      ! (mg tot/shoot) / ((kg leaf/kg stem)+1) = mg stem/shoot
end_stem_mass = end_shoot_mass/(bcfleafstem+1.0)
 
      ! length of shoot when completely developed, use the mass of stem
      ! per plant (mg stem/shoot)*(kg/mg)*(#stem/m^2)/(#plants/m^2) = kg
      ! stem/plant inserted into stem area index equation to get stem area
      ! in m^2 per plant and then conversted back to m^2 per stem
end_stem_area = bc0ssa*(end_stem_mass*mgtokg*bcdstm/bcdpop)                     &
              & **bc0ssb*bcdpop/bcdstm
      ! use silhouette area and stem diameter to length ratio to find length
      ! since silhouette area = length * diameter
      ! *** the square root is included since straight ratios do not really
      ! fit, but grossly underestimate the shoot length. this is possibly
      ! due to the difference between mature stem density vs. new growth
      ! with new stems being much higher in water content ***
      ! note: diameter to length ratio is when shoot has fully grown from
      ! root reserves during it's extension, it is assumed to grow at full
      ! diameter
end_shoot_len = sqrt(end_stem_area/bcfshoot)
 
      ! screen shoot emergence parameters for validity
if (end_shoot_len<=bczgrowpt) write (unit=6,fmt='(1x,3(a),f7.4,a,f7.4,a)')      &
                                    & 'warning: ',bc0nam(1:len_trim(bc0nam)),   &
                                    &' growth halted. shoot extension: ',       &
                                   & end_shoot_len,' depth in soil: ',bczgrowpt,&
                                    &' meters.'
 
      ! today and yesterday shoot length and stem and leaf mass increments
      ! length increase scaled by mass increase
      ! stem and leaf mass allocated proportionally (prevents premature
      ! emergence)
bczshoot = end_shoot_len*((bcmshoot/(mgtokg*bcdstm))+d_shoot_mass)              &
         & /end_shoot_mass
yesterday_len = end_shoot_len*(bcmshoot/(mgtokg*bcdstm))/end_shoot_mass
d_stem_mass = d_shoot_mass/(bcfleafstem+1.0)
d_leaf_mass = d_shoot_mass*bcfleafstem/(bcfleafstem+1.0)
 
!debe added the flag 'emrgflg' to conduct emergence the (weps/upgm) way
! when the value is 0 or to call the emerge subroutine when the value is 1.

!debe 090808 added the 'seedbed' variable to print out the soil moisture
! condition.
!!!seedbed = soilwat(seedsw)
!de added to prevent soilwat getting an array index of 0
!Nathan moved before writing to guarantee initialized when emrgflg == 0
if (seedsw .NE. 0) then
    seedbed = soilwat(seedsw)
else 
    seedbed = ""    
end if 
 
      ! divide above ground and below ground mass
if (bczshoot<=bczgrowpt) then
          ! all shoot growth for today below ground
  ag_stem = 0.0
  bg_stem = d_stem_mass
 
else if (yesterday_len>=bczgrowpt) then
          ! all shoot growth for today above ground
  ag_stem = d_stem_mass
  bg_stem = 0.0
else
          ! shoot breaks ground surface today
  ag_stem = d_stem_mass*(bczshoot-bczgrowpt)/(bczshoot-yesterday_len)
  bg_stem = d_stem_mass*(bczgrowpt-yesterday_len)/(bczshoot-yesterday_len)
 
!debe moved following code here as suggested from manhattan meetings:
!!debe added the following to print out 'day of emergence' when conducting
!! emergence the (weps/upgm) way.
!!debe moved this line here to allow incrementing of stem masses above.
  if (emrgflg==0) then         !(weps/upgm) way
    !  if (shoot_hui.eq.1.0) then
     ems(1) = doy
     ems(2) = yy
     call date1(ems)
     ddap(1) = dap
     dgdds(1) = gdds
     print *,'ems = ',ems
 
!      write (luoemerge,1000) cropname,doy,dap,pd,pm,py,bc0growdepth,ems(1),ems(4),  &
!            & ems(3),ems(2),seedbed,emrgflg,gddday,gddtbg,cliname,                  &
!            &'in shoot_grow'
  end if
!
!!debe added the variable 'cliname' to write out the location of the
!! climate file which is passed in from crop which received it ultimately
!! from test_crop_climint where it was read in from the climate file
!! header information.
!
!! de changed writing bcdayap to dap because in crop dap is passed to
!! shoot_grow in place of bcdayap.
!end of moved section
  if (emrgflg==0) write (luoemerge,1000) cropname,doy,dap,pd,pm,py,bc0growdepth,&
                       & ems(1),ems(4),ems(3),ems(2),seedbed,emrgflg,gddday,    &
                       & gddtbg,cliname,'in shoot_grow'
 
end if

 
!moved following section above as suggested from manhattan meetings:
!!debe added the following to print out 'day of emergence' when conducting
!! emergence the (weps/upgm) way.
!!debe moved this line here to allow incrementing of stem masses above.
!if (emrgflg.eq.0) then     !(weps/upgm) way
!  if (shoot_hui.eq.1.0) then
!     ems(1) = doy
!     ems(2) = yy
!     call date1(ems)
!     ddap(1) = dap
!     dgdds(1) = gdds
!     print *,'ems = ',ems
!  end if
!
!!debe added the variable 'cliname' to write out the location of the
!! climate file which is passed in from crop which received it ultimately
!! from test_crop_climint where it was read in from the climate file
!! header information.
!
!! de changed writing bcdayap to dap because in crop dap is passed to
!! shoot_grow in place of bcdayap.
!  write (luoemerge,1000) cropname,doy,dap,pd,pm,py,bc0growdepth,ems(1),ems(4),  &
!                       & ems(3),ems(2),seedbed,emrgflg,gddday,gddtbg,cliname
!
!debe call emerge subroutine to handle emergence when emrgflg = 1
! variables read in from weps variables into emerge subroutine variables:
!     doy into daynum
!     bc0growdepth into pdepth
! end of stuff moved above
 
!else if (emrgflg.eq.1) then  !elseif should be if now that above section
!                               is moved higher
if (emrgflg==1) then
      ! not used: wfpslo,wfpsup
  !debe added this to prevent emerge being called after emergence has occurred
  if (ems(1)==999) call emerge(cliname,cropname,dap,doy,ddap,dgdds,egdd,elong,  &
                             & emrgflg,ems,ergdd,gddday,gddtbg,germgdd,germs,   &
                             & ggdd,pd,bc0growdepth,pm,py,seedsw,soilwat,tempsw,&
                             & yy)
end if
!end if
 
      !convert to from mg/shoot to kg/m^2
dlfwt = d_leaf_mass*mgtokg*bcdstm
dstwt = ag_stem*mgtokg*bcdstm
drpwt = 0.0
drswt = 0.0
lost_mass = 0.0
 
      ! yield residue relationship adjustment
      ! since this is in shoot_grow, do not allow this with bchyfg=5
      ! since it is illogical to store yield into the storage root while
      ! at the same time using the storage root to grow the shoot
 
if ((cook_yield==1).and.(bcyld_coef>1.0).and.(bcresid_int>=0.0).and.            &
  & ((bchyfg==0).or.(bchyfg==1))) call cookyield(bchyfg,bnslay,dlfwt,dstwt,     &
  & drpwt,drswt,bcmstandstem,bcmstandleaf,bcmstandstore,bcmflatstem,bcmflatleaf,&
  & bcmflatstore,bcmrootstorez,lost_mass,bcyld_coef,bcresid_int,bcgrf)
 
      ! divide above ground stem between standing and flat
stem_propor = min(1.0,bczmxc/bc0diammax)
stand_stem = dstwt*stem_propor
flat_stem = dstwt*(1.0-stem_propor)
 
      ! distribute mass into mass pools
      ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
      ! shoot mass pool (breakout pool, not true accumulator)
bcmshoot = bcmshoot + d_shoot_mass*mgtokg*bcdstm
 
      ! reproductive mass is added to above ground pools
bcmstandstore = bcmstandstore + drpwt*stem_propor
bcmflatstore = bcmflatstore + drpwt*(1.0-stem_propor)
 
      ! leaf mass is added even if below ground
      ! leaf has very low mass (small effect) and some light interaction
      ! does occur as emergence apporaches (if problem can be changed
      ! easily) added leaf mass adjusts live leaf fraction, otherwise no
      ! change
if ((bcmstandleaf+dlfwt)>0.0) bcfliveleaf = (bcfliveleaf*bcmstandleaf+dlfwt)    &
  & /(bcmstandleaf+dlfwt)
bcmstandleaf = bcmstandleaf + dlfwt
 
      ! above ground stems
bcmstandstem = bcmstandstem + stand_stem
bcmflatstem = bcmflatstem + flat_stem
 
      ! below ground stems
do lay = 1,bnslay
  if (lay==1) then
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
     bcmbgstemz(lay) = bcmbgstemz(lay)                                          &
                     & + bg_stem*mgtokg*bcdstm*frac_lay(bczgrowpt-bczshoot,     &
                     & bczgrowpt-yesterday_len,0.0,bszlyd(lay)*mmtom)
  else
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
     bcmbgstemz(lay) = bcmbgstemz(lay)                                          &
                     & + bg_stem*mgtokg*bcdstm*frac_lay(bczgrowpt-bczshoot,     &
                     & bczgrowpt-yesterday_len,bszlyd(lay-1)*mmtom,bszlyd(lay)  &
                     & *mmtom)
  end if
end do
 
      ! check plant height, the case of regrowth from stem
      ! do not allow reaching max height in single day
      ! use stem proportion to account for flat stems
bczht = min(0.5*(bczmxc+bczht),                                                 &
      & max(bczht,max(0.0,(bczshoot-bczgrowpt)*stem_propor)))
 
      ! check root depth
bczrtd = max(bczrtd,(bczgrowpt+bczshoot))
 
      ! add to fibrous root mass, remove from storage root mass
do lay = 1,bnslay
  if (lay==1) then
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
     bcmrootfiberz(lay) = bcmrootfiberz(lay)                                    &
                        & + d_root_mass*mgtokg*bcdstm*frac_lay(bczgrowpt,bczrtd,&
                        & 0.0,bszlyd(lay)*mmtom)
  else
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
     bcmrootfiberz(lay) = bcmrootfiberz(lay)                                    &
                        & + d_root_mass*mgtokg*bcdstm*frac_lay(bczgrowpt,bczrtd,&
                        & bszlyd(lay-1)*mmtom,bszlyd(lay)*mmtom)
  end if
          ! check for sufficient storage in layer to meet demand
  if ((bcmrootstorez(lay)>0.0).and.(d_s_root_mass>0.0)) then
              ! demand and storage to meet it
              ! units: mg/shoot * kg/mg * shoots/m^2 = kg/m^2
     bcmrootstorez(lay) = bcmrootstorez(lay) - d_s_root_mass*mgtokg*bcdstm
     if (bcmrootstorez(lay)<0.0) then
                  ! not enough mass in this layer to meet need. carry over
                  ! to next layer in d_s_root_mass
        d_s_root_mass = -bcmrootstorez(lay)/(mgtokg*bcdstm)
        bcmrootstorez(lay) = 0.0
     else
                  ! no more mass needed
        d_s_root_mass = 0.0
     end if
  end if
end do
 
!     the following write statements are for 'shoot.out'
!     am0cfl is flag to print crop submodel output
! debe took bcdayap out of the list to write out. bcdayap was not passed
! into shoot_grow and so when trying to print bcdayap it was not
! initialized and it held garbage. put in dap instead.
if (am0cfl>=1) write (luoshoot,1100) daysim,doy,yr,dap,shoot_hui,s_root_sum,    &
                                   & f_root_sum,tot_mass_req,end_shoot_mass,    &
                                   & end_root_mass,d_root_mass,d_shoot_mass,    &
                                   & d_s_root_mass,end_stem_mass,end_stem_area, &
                                   & end_shoot_len,bczshoot,bcmshoot,bcdstm,    &
                                   & bc0nam,gddday
!debe added gddday to output daily gdd values.
      ! check if shoot sucessfully reached above ground
if ((d_s_root_mass>0.0).and.(bczht<=0.0)) then
  write (0,*) 'shoot_grow: not enough root storage to grow shoot'
  call exit(1)
end if
 
 1000 format (1x,a15,2x,i3,5x,i3,6x,i2,2x,i2,3x,i1,3x,f5.3,4x,i3,2x,i2,2x,i2,1x,&
            & i2,2x,a15,7x,i1,3x,f8.2,1x,f8.2,5x,a40) !4x,f8.2,5x,a40) debe changed the 4x to 1x
 
 1100 format (1x,i5,2x,i3,1x,i4,1x,i3,1x,f6.3,4(1x,f8.4),4(1x,f8.4),4(1x,f8.4), &
            & (1x,f8.4),(1x,f8.3),1x,a20,1x,f7.3)
            !debe 090308 added formatting for gddday output
!
end subroutine shoot_grow 
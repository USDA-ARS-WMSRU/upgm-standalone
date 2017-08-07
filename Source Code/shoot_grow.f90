subroutine shoot_grow(ctrl, clidat, bio, soils,daysim,dap,gdds,ddap,dgdds,elong,gddday,yy,yr,tempsw)
 
! ***** emergence *****
!
!debe added the following emergence arguments so that they can be passed
! to emerge. these include bio%upgm%seedsw, bio%bname, bio%upgm%soilwat, bio%upgm%wfpslo, bio%upgm%wfpsup,
! bio%upgm%germgdd, bio%upgm%ergdd, gddday, bio%upgm%gddtbg, gdds, dap, ddap, dgdds, elong, bio%upgm%ems,
! bio%upgm%germs, yy. ac0nam is passed into bio%bname in main to be passed to
! emerge. bio%database%growdepth (weps/upgm) variable is passed into
! emerge where it is used in setting pdepth (planting depth).
!debe added the emergence flag (bio%upgm%emrgflg)to determine whether to call the
! emerge subroutine or not.
!debe added ctrl%sim%icli to pass to emerge to enable printing the weather file
! name in emerge.out. also, added ctrl%mngt%pd,ctrl%mngt%pm,ctrl%mngt%py to print planting date in
! emerge.out. debe used gdds and dap (phenologymms variables) in place
! of bcthucum and bcdayap (weps/upgm variables).
!debe changed bio%upgm%seedsw from integer to real to allow moving half a soil moisture level.
! later changed back to an integer because array subscripts must be integers or constants.
!debe flag to call emerge subroutine - 0 = don't call emerge and
! 1 = call emerge
!debe added seedbed to write it to the output file when implementing
! emergence the weps/upgm way.
!debe added two new arrays of 6 elements to be used in emerge.for to
! enable adding values for bio%upgm%germgdd and bio%upgm%ergdd for two soil moisture
! levels that are half steps between dry and medium and medium and optimum.
! later added tempsw, the array index for these new arrays. it needed to be
! initialized in cinit and passed through. 5122011
    use constants, only : mgtokg, mmtom
    use upgm_simdata, only : controls
    use climate, only: climate_data
    use datetime, only : dayear, caldat
    use soil, only : soildata
    use biomaterial
implicit none
!
! PARAMETER definitions
!
real,parameter :: shoot_exp = 2.0,be_stor = 0.7,rootf = 0.4
!
! Dummy arguments
!
    type(controls),      intent(inout) :: ctrl
    type(climate_data),  intent(inout) :: clidat
    type(biomatter),     intent(inout) :: bio
    type(soildata),      intent(inout) :: soils
real :: elong,gddday,gdds
integer :: dap,daysim,tempsw,yr,yy

integer,dimension(20) :: ddap
real,dimension(20) :: dgdds
!
! Local variables
!
real :: ag_stem,bg_stem,diff_mass,dlfwt,drpwt,drswt,dstwt,d_leaf_mass,          &
      & d_root_mass,d_shoot_mass,d_stem_mass,d_s_root_mass,end_root_mass,       &
      & end_shoot_len,end_shoot_mass,end_stem_area,end_stem_mass,fexp_hui,      &
      & fexp_huiy,flat_stem,f_root_sum,lost_mass,red_mass_rat,shoot_hui,        &
      & shoot_huiy,stand_stem,stem_propor,s_root_sum,tot_mass_req,yesterday_len
integer :: day,doy,lay,mo
real :: frac_lay
character(80) :: seedbed
!
!     + + + purpose + + +
 
!     + + + keywords + + +
!     shoot growth
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     bio%database%diammax - crop maximum plant diameter (m)
!     bio%database%growdepth - depth of growing point at time of planting (m)
!     bio%bname - crop name
!     bio%database%ssa - stem area to mass coefficient a, result is m^2 per plant
!     bio%database%ssb - stem area to mass coefficient b, argument is kg per plant
!     bcdayap - number of days of growth completed since crop planted
!     bio%geometry%dpop - number of plants per unit area (#/m^2)
!            - note: bio%geometry%dstm/bio%geometry%dpop gives the number of stems per plant
!     bio%geometry%dstm - number of crop stems per unit area (#/m^2)
!     bio%database%fleafstem - crop leaf to stem mass ratio for shoots
!     bio%growth%fliveleaf - fraction of standing plant leaf which is living
!                   (transpiring)
!     bio%database%fshoot - crop ratio of shoot diameter to length
!     bio%database%grf  - fraction of reproductive biomass that is yield
!     bio%geometry%hyfg - flag indicating the part of plant to apply the "grain
!              fraction", grf, to when removing that plant part for yield
!         0    grf applied to above ground storage (seeds, reproductive)
!         1    grf times growth stage factor (see growth.for) applied to
!              above ground storage (seeds, reproductive)
!         2    grf applied to all aboveground biomass (forage)
!         3    grf applied to leaf mass (tobacco)
!         4    grf applied to stem mass (sugarcane)
!         5    grf applied to below ground storage mass (potatoes,
!              peanuts)
!     bio%mass%stemz - crop stem mass below soil surface by layer (kg/m^2)
!     bio%mass%flatleaf  - crop flat leaf mass (kg/m^2)
!     bio%mass%flatstem  - crop flat stem mass (kg/m^2)
!     bio%mass%flatstore - crop flat storage mass (kg/m^2)
!     bio%mass%rootfiberz - crop root fibrous mass by soil layer (kg/m^2)
!     bio%mass%rootstorez - crop root storage mass by soil layer (kg/m^2)
!                     (tubers (potatoes, carrots), extended leaf (onion),
!                     seeds (peanuts))
!     bio%growth%mshoot - crop shoot mass grown from root storage (kg/m^2)
!                this is a "breakout" mass and does not represent a
!                unique pool since this mass is destributed into below
!                ground stem and standing stem as each increment of the
!                shoot is added
!     bio%mass%standleaf - crop standing leaf mass (kg/m^2)
!     bio%mass%standstem - crop standing stem mass (kg/m^2)
!     bio%mass%standstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head
!                    (cabbage, pineapple))
!     bio%growth%mtotshoot - total mass of shoot growing from root storage biomass
!                   (kg/m^2)in the period from beginning to completion of
!                   emergence heat units
!     bio%database%resid_int - residue intercept (kg/m^2)
!                   harvest_residue = bio%database%yld_coef(kg/kg) *
!                   yield + bio%database%resid_int (kg/m^2)
!     bio%growth%thu_shoot_beg - heat unit index (fraction) for beginning of shoot
!                       grow from root storage period
!     bio%growth%thu_shoot_end - heat unit index (fraction) for end of shoot grow
!                       from root storage period
!     bio%database%yld_coef - yield coefficient (kg/kg)
!                  harvest_residue = bio%database%yld_coef(kg/kg) *
!                  yield + bio%database%resid_int (kg/m^2)
!     bio%growth%zgrowpt - depth in the soil of the growing point (m)
!     bio%geometry%zht  - crop height (m)
!     bio%database%zmrt - maximum root depth
!     bio%database%zmxc - maximum potential plant height (m)
!     bio%geometry%zrtd - root depth (m)
!     bio%geometry%zshoot - length of actively growing shoot from root biomass (m)
!     soils%spp%nslay - number of soil layers
!     soils%spp%aszlyd - depth from top of soil to bottom of layer, m
!     daysim - day of the simulation
!     clidat%hui - heat unit index for today
!     clidat%huiy - heat unit index for yesterday
 
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
!     ctrl%sim%cliname - the name of the location for the climate file. used to
!               write out the name of the climate file location in
!               emerge.out.
!     bio%bname - name of the current crop.
!     dap - days after planting.
!     ddap - array holding the number of days after planting for up
!            to 20 different stages.
!     dgdds - array holding the number of gdd after seeding for up
!             to 20 different stages.
!     bio%upgm%egdd - a 6 element array that holds the bio%upgm%ergdd values plus calculated values
!            for two intermediate soil moisture level values in elements 2 and 4.
!     elong - total elongation of the emerging seedling based on the
!             day's gdd (mm)
!     bio%upgm%emrgflg - a flag to determine if the new emerge subroutine should
!               be called (bio%upgm%emrgflg=1) or to proceed with the weps/upgm
!               method of achieving emergence (bio%upgm%emrgflg=0).
!     bio%upgm%ems - simulated day that emergence began.
!     bio%upgm%ergdd - an array holding 4 elongation rates in mm per gdd
!             based on each soil moisture description.
!     gddday - the number of gdd with 0°C base temperature for that day.
!     gdds - cumulative growing degree-days since planting, not including
!            days with severely dry soil.  accumulation ends when
!            emergence is complete; ec day.
!     bio%upgm%gddtbg - used to accumulate gdd to begin germination.
!     bio%upgm%germgdd - an array holding 4 germination times in gdd at base 0°c
!               for the soil moisture levels.
!     bio%upgm%germs - simulated day that germination occurs.
!     bio%upgm%ggdd - a 6 element array that holds the bio%upgm%germgdd values plus calculated values for
!           two intermediate soil moisture level values in elements 2 and 4.
!     ctrl%sim%icli - a flag to determine which type of weather file to read.  a
!            value of 1 indicates that climate data should be read from
!            the cligen weather file.  a value of 0 indicates that a
!            historical climate file will be used.
!     ctrl%mngt%pd - planting day.
!     ctrl%mngt%pm - planting month.
!     ctrl%mngt%py - planting year.  currently, not the calendar year.
!     seedbed - description of the soil moisture condition. used to
!               convert the character to an integer.
!     bio%upgm%seedsw - soil water content at seed depth.  it is read in as
!              optimum, medium, dry or planted in dust and converted
!              to an integer.	 1 = optimum, 2 = medium, 3 = dry and
!              4 = planted in dust.
!     bio%upgm%soilwat - an array holding the swtype for each soil moisture
!       condition.
!     tempsw - a new variable to designate the array subscripts for the new 6 element
!              arrays: bio%upgm%egdd, bio%upgm%ggdd
!     bio%upgm%wfpslo - an array holding the low values for each soil moisture
!       condition.
!     bio%upgm%wfpsup - an array holding the high values for each soil moisture
!       condition.
!     yy - year from cinit subroutine.
 
!     + + + functions called + + +
!     dayear
!     frac_lay
 
!     + + + end of specifications + + +
 
call caldat(ctrl%sim%juldate, day,mo,yr)
doy = dayear(day,mo,yr)
 
   ! fraction of shoot growth from stored reserves (today and yesterday)
shoot_hui = min(1.0,(clidat%hui-bio%growth%thu_shoot_beg)/(bio%growth%thu_shoot_end-bio%growth%thu_shoot_beg))
shoot_huiy = max(0.0,(clidat%huiy-bio%growth%thu_shoot_beg)/(bio%growth%thu_shoot_end-bio%growth%thu_shoot_beg))
 
      ! total shoot mass is grown at an exponential rate
fexp_hui = (exp(shoot_exp*shoot_hui)-1.0)/(exp(shoot_exp)-1)
fexp_huiy = (exp(shoot_exp*shoot_huiy)-1.0)/(exp(shoot_exp)-1)
 
      ! sum present storage and fibrous root mass (kg/m^2)
s_root_sum = 0.0
f_root_sum = 0.0
do lay = 1,soils%spp%nslay
  s_root_sum = s_root_sum + bio%mass%rootstorez(lay)
  f_root_sum = f_root_sum + bio%mass%rootfiberz(lay)
end do
 
      ! calculate storage mass required to grow a single shoot
      ! units: kg/m^2 / ( shoots/m^2 * kg/mg ) = mg/shoot
tot_mass_req = bio%growth%mtotshoot/(bio%geometry%dstm*mgtokg)
 
      ! divide ending mass between shoot and root
if (f_root_sum<=bio%growth%mshoot) then   ! this works as long as rootf <= 0.5
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
diff_mass = d_s_root_mass - s_root_sum/(bio%geometry%dstm*mgtokg)
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
end_stem_mass = end_shoot_mass/(bio%database%fleafstem+1.0)
 
      ! length of shoot when completely developed, use the mass of stem
      ! per plant (mg stem/shoot)*(kg/mg)*(#stem/m^2)/(#plants/m^2) = kg
      ! stem/plant inserted into stem area index equation to get stem area
      ! in m^2 per plant and then conversted back to m^2 per stem
end_stem_area = bio%database%ssa*(end_stem_mass*mgtokg*bio%geometry%dstm/bio%geometry%dpop)                     &
              & **bio%database%ssb*bio%geometry%dpop/bio%geometry%dstm
      ! use silhouette area and stem diameter to length ratio to find length
      ! since silhouette area = length * diameter
      ! *** the square root is included since straight ratios do not really
      ! fit, but grossly underestimate the shoot length. this is possibly
      ! due to the difference between mature stem density vs. new growth
      ! with new stems being much higher in water content ***
      ! note: diameter to length ratio is when shoot has fully grown from
      ! root reserves during it's extension, it is assumed to grow at full
      ! diameter
end_shoot_len = sqrt(end_stem_area/bio%database%fshoot)
 
      ! screen shoot emergence parameters for validity
if (end_shoot_len<=bio%growth%zgrowpt) write (unit=6,fmt='(1x,3(a),f7.4,a,f7.4,a)')      &
                                    & 'warning: ',bio%bname(1:len_trim(bio%bname)),   &
                                    &' growth halted. shoot extension: ',       &
                                   & end_shoot_len,' depth in soil: ',bio%growth%zgrowpt,&
                                    &' meters.'
 
      ! today and yesterday shoot length and stem and leaf mass increments
      ! length increase scaled by mass increase
      ! stem and leaf mass allocated proportionally (prevents premature
      ! emergence)
bio%geometry%zshoot = end_shoot_len*((bio%growth%mshoot/(mgtokg*bio%geometry%dstm))+d_shoot_mass)              &
         & /end_shoot_mass
yesterday_len = end_shoot_len*(bio%growth%mshoot/(mgtokg*bio%geometry%dstm))/end_shoot_mass
d_stem_mass = d_shoot_mass/(bio%database%fleafstem+1.0)
d_leaf_mass = d_shoot_mass*bio%database%fleafstem/(bio%database%fleafstem+1.0)
 
!debe added the flag 'bio%upgm%emrgflg' to conduct emergence the (weps/upgm) way
! when the value is 0 or to call the emerge subroutine when the value is 1.

!debe 090808 added the 'seedbed' variable to print out the soil moisture
! condition.
!!!seedbed = bio%upgm%soilwat(bio%upgm%seedsw)
!de added to prevent bio%upgm%soilwat getting an array index of 0
!Nathan moved before writing to guarantee initialized when bio%upgm%emrgflg == 0
if (bio%upgm%seedsw .NE. 0) then
    seedbed = bio%upgm%soilwat(bio%upgm%seedsw)
else 
    seedbed = ""    
end if 
 
      ! divide above ground and below ground mass
if (bio%geometry%zshoot<=bio%growth%zgrowpt) then
          ! all shoot growth for today below ground
  ag_stem = 0.0
  bg_stem = d_stem_mass
 
else if (yesterday_len>=bio%growth%zgrowpt) then
          ! all shoot growth for today above ground
  ag_stem = d_stem_mass
  bg_stem = 0.0
else
          ! shoot breaks ground surface today
  ag_stem = d_stem_mass*(bio%geometry%zshoot-bio%growth%zgrowpt)/(bio%geometry%zshoot-yesterday_len)
  bg_stem = d_stem_mass*(bio%growth%zgrowpt-yesterday_len)/(bio%geometry%zshoot-yesterday_len)
 
!debe moved following code here as suggested from manhattan meetings:
!!debe added the following to print out 'day of emergence' when conducting
!! emergence the (weps/upgm) way.
!!debe moved this line here to allow incrementing of stem masses above.
  if (bio%upgm%emrgflg==0) then         !(weps/upgm) way
    !  if (shoot_hui.eq.1.0) then
     bio%upgm%ems(1) = doy
     bio%upgm%ems(2) = yy
     call date1(bio%upgm%ems)
     ddap(1) = dap
     dgdds(1) = gdds
     print *,'bio%upgm%ems = ',bio%upgm%ems
 
!      write (luoemerge,1000) bio%bname,doy,dap,ctrl%mngt%pd,ctrl%mngt%pm,ctrl%mngt%py,bio%database%growdepth,bio%upgm%ems(1),bio%upgm%ems(4),  &
!            & bio%upgm%ems(3),bio%upgm%ems(2),seedbed,bio%upgm%emrgflg,gddday,bio%upgm%gddtbg,ctrl%sim%cliname,                  &
!            &'in shoot_grow'
  end if
!
!!debe added the variable 'ctrl%sim%cliname' to write out the location of the
!! climate file which is passed in from crop which received it ultimately
!! from test_crop_climint where it was read in from the climate file
!! header information.
!
!! de changed writing bcdayap to dap because in crop dap is passed to
!! shoot_grow in place of bcdayap.
!end of moved section
  if (bio%upgm%emrgflg==0) write (ctrl%handles%luoemerge,1000) bio%bname,doy,dap,ctrl%mngt%pd,ctrl%mngt%pm,ctrl%mngt%py,bio%database%growdepth,&
                       & bio%upgm%ems(1),bio%upgm%ems(4),bio%upgm%ems(3),bio%upgm%ems(2),seedbed,bio%upgm%emrgflg,gddday,    &
                       & bio%upgm%gddtbg,ctrl%sim%cliname,'in shoot_grow'
 
end if

 
!moved following section above as suggested from manhattan meetings:
!!debe added the following to print out 'day of emergence' when conducting
!! emergence the (weps/upgm) way.
!!debe moved this line here to allow incrementing of stem masses above.
!if (bio%upgm%emrgflg.eq.0) then     !(weps/upgm) way
!  if (shoot_hui.eq.1.0) then
!     bio%upgm%ems(1) = doy
!     bio%upgm%ems(2) = yy
!     call date1(bio%upgm%ems)
!     ddap(1) = dap
!     dgdds(1) = gdds
!     print *,'bio%upgm%ems = ',bio%upgm%ems
!  end if
!
!!debe added the variable 'ctrl%sim%cliname' to write out the location of the
!! climate file which is passed in from crop which received it ultimately
!! from test_crop_climint where it was read in from the climate file
!! header information.
!
!! de changed writing bcdayap to dap because in crop dap is passed to
!! shoot_grow in place of bcdayap.
!  write (luoemerge,1000) bio%bname,doy,dap,ctrl%mngt%pd,ctrl%mngt%pm,ctrl%mngt%py,bio%database%growdepth,bio%upgm%ems(1),bio%upgm%ems(4),  &
!                       & bio%upgm%ems(3),bio%upgm%ems(2),seedbed,bio%upgm%emrgflg,gddday,bio%upgm%gddtbg,ctrl%sim%cliname
!
!debe call emerge subroutine to handle emergence when bio%upgm%emrgflg = 1
! variables read in from weps variables into emerge subroutine variables:
!     doy into daynum
!     bio%database%growdepth into pdepth
! end of stuff moved above
 
!else if (bio%upgm%emrgflg.eq.1) then  !elseif should be if now that above section
!                               is moved higher
if (bio%upgm%emrgflg==1) then
      ! not used: bio%upgm%wfpslo,bio%upgm%wfpsup
  !debe added this to prevent emerge being called after emergence has occurred
  if (bio%upgm%ems(1)==999) call emerge(ctrl, clidat, ctrl%sim%cliname,bio%bname,dap,doy,ddap,dgdds,bio%upgm%egdd,elong,  &
                             & bio%upgm%emrgflg,bio%upgm%ems,bio%upgm%ergdd,gddday,bio%upgm%gddtbg,bio%upgm%germgdd,bio%upgm%germs,   &
                             & bio%upgm%ggdd,ctrl%mngt%pd,bio%database%growdepth,ctrl%mngt%pm,ctrl%mngt%py,bio%upgm%seedsw,bio%upgm%soilwat,tempsw,&
                             & yy)
end if
!end if
 
      !convert to from mg/shoot to kg/m^2
dlfwt = d_leaf_mass*mgtokg*bio%geometry%dstm
dstwt = ag_stem*mgtokg*bio%geometry%dstm
drpwt = 0.0
drswt = 0.0
lost_mass = 0.0
 
      ! yield residue relationship adjustment
      ! since this is in shoot_grow, do not allow this with bio%geometry%hyfg=5
      ! since it is illogical to store yield into the storage root while
      ! at the same time using the storage root to grow the shoot
 
if ((ctrl%sim%cook_yield==1).and.(bio%database%yld_coef>1.0).and.(bio%database%resid_int>=0.0).and.            &
  & ((bio%geometry%hyfg==0).or.(bio%geometry%hyfg==1))) call cookyield(bio%geometry%hyfg,soils%spp%nslay,dlfwt,dstwt,     &
  & drpwt,drswt,bio%mass%standstem,bio%mass%standleaf,bio%mass%standstore,bio%mass%flatstem,bio%mass%flatleaf,&
  & bio%mass%flatstore,bio%mass%rootstorez,lost_mass,bio%database%yld_coef,bio%database%resid_int,bio%database%grf)
 
      ! divide above ground stem between standing and flat
stem_propor = min(1.0,bio%database%zmxc/bio%database%diammax)
stand_stem = dstwt*stem_propor
flat_stem = dstwt*(1.0-stem_propor)
 
      ! distribute mass into mass pools
      ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
      ! shoot mass pool (breakout pool, not true accumulator)
bio%growth%mshoot = bio%growth%mshoot + d_shoot_mass*mgtokg*bio%geometry%dstm
 
      ! reproductive mass is added to above ground pools
bio%mass%standstore = bio%mass%standstore + drpwt*stem_propor
bio%mass%flatstore = bio%mass%flatstore + drpwt*(1.0-stem_propor)
 
      ! leaf mass is added even if below ground
      ! leaf has very low mass (small effect) and some light interaction
      ! does occur as emergence apporaches (if problem can be changed
      ! easily) added leaf mass adjusts live leaf fraction, otherwise no
      ! change
if ((bio%mass%standleaf+dlfwt)>0.0) bio%growth%fliveleaf = (bio%growth%fliveleaf*bio%mass%standleaf+dlfwt)    &
  & /(bio%mass%standleaf+dlfwt)
bio%mass%standleaf = bio%mass%standleaf + dlfwt
 
      ! above ground stems
bio%mass%standstem = bio%mass%standstem + stand_stem
bio%mass%flatstem = bio%mass%flatstem + flat_stem
 
      ! below ground stems
do lay = 1,soils%spp%nslay
  if (lay==1) then
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
     bio%mass%stemz(lay) = bio%mass%stemz(lay)                                          &
                     & + bg_stem*mgtokg*bio%geometry%dstm*frac_lay(bio%growth%zgrowpt-bio%geometry%zshoot,     &
                     & bio%growth%zgrowpt-yesterday_len,0.0,soils%spp%aszlyd(lay)*mmtom)
  else
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
     bio%mass%stemz(lay) = bio%mass%stemz(lay)                                          &
                     & + bg_stem*mgtokg*bio%geometry%dstm*frac_lay(bio%growth%zgrowpt-bio%geometry%zshoot,     &
                     & bio%growth%zgrowpt-yesterday_len,soils%spp%aszlyd(lay-1)*mmtom,soils%spp%aszlyd(lay)  &
                     & *mmtom)
  end if
end do
 
      ! check plant height, the case of regrowth from stem
      ! do not allow reaching max height in single day
      ! use stem proportion to account for flat stems
bio%geometry%zht = min(0.5*(bio%database%zmxc+bio%geometry%zht),                                                 &
      & max(bio%geometry%zht,max(0.0,(bio%geometry%zshoot-bio%growth%zgrowpt)*stem_propor)))
 
      ! check root depth
bio%geometry%zrtd = max(bio%geometry%zrtd,(bio%growth%zgrowpt+bio%geometry%zshoot))
 
      ! add to fibrous root mass, remove from storage root mass
do lay = 1,soils%spp%nslay
  if (lay==1) then
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
     bio%mass%rootfiberz(lay) = bio%mass%rootfiberz(lay)                                    &
                        & + d_root_mass*mgtokg*bio%geometry%dstm*frac_lay(bio%growth%zgrowpt,bio%geometry%zrtd,&
                        & 0.0,soils%spp%aszlyd(lay)*mmtom)
  else
              ! units: mg stem/shoot * kg/mg * shoots/m^2 = kg/m^2
     bio%mass%rootfiberz(lay) = bio%mass%rootfiberz(lay)                                    &
                        & + d_root_mass*mgtokg*bio%geometry%dstm*frac_lay(bio%growth%zgrowpt,bio%geometry%zrtd,&
                        & soils%spp%aszlyd(lay-1)*mmtom,soils%spp%aszlyd(lay)*mmtom)
  end if
          ! check for sufficient storage in layer to meet demand
  if ((bio%mass%rootstorez(lay)>0.0).and.(d_s_root_mass>0.0)) then
              ! demand and storage to meet it
              ! units: mg/shoot * kg/mg * shoots/m^2 = kg/m^2
     bio%mass%rootstorez(lay) = bio%mass%rootstorez(lay) - d_s_root_mass*mgtokg*bio%geometry%dstm
     if (bio%mass%rootstorez(lay)<0.0) then
                  ! not enough mass in this layer to meet need. carry over
                  ! to next layer in d_s_root_mass
        d_s_root_mass = -bio%mass%rootstorez(lay)/(mgtokg*bio%geometry%dstm)
        bio%mass%rootstorez(lay) = 0.0
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
if (bio%growth%am0cfl>=1) write (ctrl%handles%luoshoot,1100) daysim,doy,yr,dap,shoot_hui,s_root_sum,    &
                                   & f_root_sum,tot_mass_req,end_shoot_mass,    &
                                   & end_root_mass,d_root_mass,d_shoot_mass,    &
                                   & d_s_root_mass,end_stem_mass,end_stem_area, &
                                   & end_shoot_len,bio%geometry%zshoot,bio%growth%mshoot,bio%geometry%dstm,    &
                                   & bio%bname,gddday
!debe added gddday to output daily gdd values.
      ! check if shoot sucessfully reached above ground
if ((d_s_root_mass>0.0).and.(bio%geometry%zht<=0.0)) then
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
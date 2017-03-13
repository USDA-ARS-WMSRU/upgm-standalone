SUBROUTINE remove(sel_position,sel_pool,bflg,stemf,leaff,storef,rootstoref,     &
                & rootfiberf,bcmstandstem,bcmstandleaf,bcmstandstore,           &
                & bcmflatstem,bcmflatleaf,bcmflatstore,bcmrootstorez,           &
                & bcmrootfiberz,bcmbgstemz,bczht,bcdstm,bcgrainf,bchyfg,        &
                & btmstandstem,btmstandleaf,btmstandstore,btmflatstem,          &
                & btmflatleaf,btmflatstore,btmflatrootstore,btmflatrootfiber,   &
                & btmbgstemz,btmbgleafz,btmbgstorez,btmbgrootstorez,            &
                & btmbgrootfiberz,btzht,btdstm,btgrainf,bdmstandstem,           &
                & bdmstandleaf,bdmstandstore,bdmflatstem,bdmflatleaf,           &
                & bdmflatstore,bdmflatrootstore,bdmflatrootfiber,bdmbgstemz,    &
                & bdmbgleafz,bdmbgstorez,bdmbgrootstorez,bdmbgrootfiberz,bdzht, &
                & bddstm,bdgrainf,bdhyfg,nslay,tot_mass_rem,sel_mass_left)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
!
! Subroutine arguments
!
REAL :: bcdstm,bcgrainf,bcmflatleaf,bcmflatstem,bcmflatstore,bcmstandleaf,      &
      & bcmstandstem,bcmstandstore,bczht,btdstm,btgrainf,btmflatleaf,           &
      & btmflatrootfiber,btmflatrootstore,btmflatstem,btmflatstore,btmstandleaf,&
      & btmstandstem,btmstandstore,btzht,leaff,rootfiberf,rootstoref,           &
      & sel_mass_left,stemf,storef,tot_mass_rem
INTEGER :: bchyfg,bflg,nslay,sel_pool,sel_position
REAL,DIMENSION(mnsz) :: bcmbgstemz,bcmrootfiberz,bcmrootstorez,btmbgleafz,      &
                      & btmbgrootfiberz,btmbgrootstorez,btmbgstemz,btmbgstorez
REAL,DIMENSION(mnbpls) :: bddstm,bdgrainf,bdmflatleaf,bdmflatrootfiber,         &
                        & bdmflatrootstore,bdmflatstem,bdmflatstore,            &
                        & bdmstandleaf,bdmstandstem,bdmstandstore,bdzht
INTEGER,DIMENSION(mnbpls) :: bdhyfg
REAL,DIMENSION(mnsz,mnbpls) :: bdmbgleafz,bdmbgrootfiberz,bdmbgrootstorez,      &
                             & bdmbgstemz,bdmbgstorez
!
! Local variables
!
INTEGER :: idx,idy,tflg
REAL :: pool_temp1,pool_temp2,start_leaf,start_stem,start_store
REAL,DIMENSION(mnsz) :: pool_temp1z,pool_temp2z,pool_temp3z,start_rootfiber,    &
                      & start_rootstore
!
!     the following subroutine arguments are not used: bczht, bdzht,
!     btzht  jcaii  8/08/08
!
!     + + + purpose + + +
!     this subroutine performs the biomass manipulation of removing
!     biomass. the amount of each component removed is determined by
!     the fraction passed into this subroutine for each component.
!     pools are changed in the order: crop, temporary, residue and
!     locations in the order: stand with roots, flat, below ground.
!     consideration is given that if root mass or stem mass is removed,
!     then the leaves and storage portion must become flat in the same
!     pool. removal of stem mass also results in a reduction in stem
!     count. in order to avoid double accounting, proportions of standing
!     are tracked, but the final adjustment and movement is not done
!     until all removals are completed in that pool.
 
!     possible future enhancements
!     a)  bioflg - selects which age pools will be processed.  probably the
!     same definition as other biomass manipulation process effects use.
!     b)  xxlocflg - selects the individual mass component pools that are
!     being effected (material being removed in this case).  there would
!     likely need to be more than one of these flags, possibly one for each
!     "age" pool.  example settings could be:
!
!     crlocflg (st,yld,flt,bg,rt)	decomp1locflg (st,flt,bg,rt)
!
!     bit             val                    bit              val
!     x    st+yld+flt  0                      x    st+flt      0
!     0    yld*fract   1                      0    -           1
!     1    st*fract    2                      1    st*fract    2
!     2    fl*fract    4                      2    fl*fract    4
!     3    bg*fract    8                      3    bg*fract    8
!     4    rt*fract    16                     4    rt*fract    16
!     5    st*cutht    32                     5    st*cutht    32
 
!     + + + keywords + + +
!     remove, biomass manipulation
 
!     + + + common blocks + + +
 
!     + + + argument declarations + + +

!     + + + argument definitions + + +
!     sel_position - position to which percentages will be applied
!                0 - don't apply to anything
!                1 - apply to standing (and attached roots)
!                2 - apply to flat
!                3 - apply to standing (and attached roots) and flat
!                4 - apply to buried
!                5 - apply to standing (and attached roots) and buried
!                6 - apply to flat and buried
!                7 - apply to standing (and attached roots), flat and buried
!                this corresponds to the bit pattern:
!                msb(buried, flat, standing)lsb
 
!     sel_pool - pool to which percentages will be applied
!            0 - don't apply to anything
!            1 - apply to crop pool
!            2 - apply to temporary pool
!            3 - apply to crop and temporary pools
!            4 - apply to residue pools
!            5 - apply to crop and residue pools
!            6 - apply to temporary and residue pools
!            7 - apply to crop, temporary and residue pools
!                this corresponds to the bit pattern:
!                msb(residue, temporary, crop)lsb
 
!     bflg      - flag indicating what to manipulate
!       0 - all standing material is manipulated (both crop and residue)
!       1 - crop
!       2 - 1'st residue pool
!       4 - 2'nd residue pool
!       ....
!       2**n - nth residue pool
 
!     storef   - fraction of storage (reproductive components) removed (kg/kg)
!     leaff    - fraction of plant leaves removed (kg/kg)
!     stemf    - fraction of plant stems removed (kg/kg)
!     rootstoref - fraction of plant storage root removed (kg/kg)
!     rootfiberf - fraction of plant fibrous root removed (kg/kg)
 
!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
 
!     bcmflatstem  - crop flat stem mass (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)
 
!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     bcmrootfiberz - crop root fibrous mass by soil layer (kg/m^2)
 
!     bcmbgstemz  - crop buried stem mass by layer (kg/m^2)
 
!     bczht  - crop height (m)
!     bcdstm - number of crop stems per unit area (#/m^2)
!            - it is computed by taking the tillering factor
!              times the plant population density.
!     bcgrainf - internally computed grain fraction of reproductive mass
 
!     btmstandstem - crop standing stem mass (kg/m^2)
!     btmstandleaf - crop standing leaf mass (kg/m^2)
!     btmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
 
!     btmflatstem  - crop flat stem mass (kg/m^2)
!     btmflatleaf  - crop flat leaf mass (kg/m^2)
!     btmflatstore - crop flat storage mass (kg/m^2)
 
!     btmflatrootstore - crop flat root storage mass (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     btmflatrootfiber - crop flat root fibrous mass (kg/m^2)
 
!     btmbgflatstemz  - crop buried stem mass by layer (kg/m^2)
!     btmbgflatleafz  - crop buried leaf mass by layer (kg/m^2)
!     btmbgflatstorez - crop buried storage mass by layer (kg/m^2)
 
!     btmbgrootstorez - crop root storage mass by layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     btmbgrootfiberz - crop root fibrous mass by layer (kg/m^2)
 
!     btzht  - crop height (m)
!     btdstm - number of crop stems per unit area (#/m^2)
!            - it is computed by taking the tillering factor
!              times the plant population density.
!     btgrainf - internally computed grain fraction of reproductive mass
 
!     bdmstandstem  - standing stem mass (kg/m^2)
!     bdmstandleaf  - standing leaf mass (kg/m^2)
!     bdmstandstore - standing storage mass (kg/m^2)
 
!     bdmflatstem  - flat stem mass (kg/m^2)
!     bdmflatleaf  - flat leaf mass (kg/m^2)
!     bdmflatstore - flat storage mass (kg/m^2)
 
!     bdmflatrootstore - flat storage root mass (kg/m^2)
!     bdmflatrootfiber - flat fibrous root mass (kg/m^2)
 
!     bdmbgstemz  - buried stem mass by layer (kg/m^2)
!     bdmbgleafz  - buried leaf mass by layer (kg/m^2)
!     bdmbgstorez - buried (from above ground) storage mass by layer (kg/m^2)
 
!     bdmbgrootstorez - buried storage root mass by layer (kg/m^2)
!     bdmbgrootfiberz - buried fibrous root mass by layer (kg/m^2)
 
!     bdzht  - residue height (m)
!     bddstm - number of residue stems per unit area (#/m^2)
!     bdgrainf - internally computed grain fraction of reproductive mass
!     bdhyfg - flag indicating the part of plant to apply the "grain fraction",
!              grf, to when removing that plant part for yield
!         0     grf applied to above ground storage (seeds, reproductive)
!         1     grf times growth stage factor (see growth.for) applied to above ground storage (seeds, reproductive)
!         2     grf applied to all aboveground biomass (forage)
!         3     grf applied to leaf mass (tobacco)
!         4     grf applied to stem mass (sugarcane)
!         5     grf applied to below ground storage mass (potatoes, peanuts)
 
!     nlay      - number of layer from which below ground biomass is removed
!     tot_mass_rem - mass of material removed by this harvest operation (kg/m^2)
!     sel_mass_left - mass of material left in pools from which mass is removed
!                     by this harvest operation (kg/m^2)
 
!     + + + accessed common block variable definitions + + +
!     mnbpls        - max number of decomposition pools (currently=3)
!     mnsz          - max number of soil layers
 
!     + + + parameters + + +
 
!     + + + local variables + + +
 
 
!     + + + local variable definitions + + +
!     pool_temp1 - used to substitute for non existent pools in crop,
!                  where there are no flatrootstore and flatrootfiber pools
!     pool_temp2 -  see pool_temp1
!     idx       - loop variable for soil layers
!     idy       - loop variable for decomp pools
!     tflg      - temporary flag to carry bioflag value if changes to all pools
 
!     + + + end specifications + + +
 
      !set tflg bits correctly for "all" pools if bflg=0
IF (bflg.EQ.0) THEN
  tflg = 1                          ! crop pool
  DO idy = 1,mnbpls
     tflg = tflg + 2**idy           ! decomp pools
  END DO
ELSE
  tflg = bflg
END IF
 
tot_mass_rem = 0.0
sel_mass_left = 0.0
 
!     assign crop grain fraction and representaive stem diameter values
!     to temporary pool since material may be transferred to the temporary
!     pool without a specific kill operation
btgrainf = bcgrainf
 
pool_temp1 = 0.0
pool_temp2 = 0.0
DO idx = 1,nslay
  pool_temp1z(idx) = 0.0
  pool_temp2z(idx) = 0.0
  pool_temp3z(idx) = 0.0
END DO
 
      ! crop pool
IF (btest(sel_pool,0)) THEN
          ! standing and rooted biomass
          ! set starting values
  start_store = bcmstandstore
  start_leaf = bcmstandleaf
  start_stem = bcmstandstem
  DO idx = 1,nslay
     start_rootstore(idx) = bcmrootstorez(idx)
     start_rootfiber(idx) = bcmrootfiberz(idx)
  END DO
  IF (btest(sel_position,0)) CALL rem_stand_pool(stemf,leaff,storef,rootstoref, &
    & rootfiberf,bcmstandstem,bcmstandleaf,bcmstandstore,bcmrootstorez,         &
    & bcmrootfiberz,nslay,bchyfg,bcgrainf,bcdstm,tot_mass_rem,sel_mass_left)
          ! flat biomass
  IF (btest(sel_position,1)) CALL rem_flat_pool(stemf,leaff,storef,rootstoref,  &
    & rootfiberf,bcmflatstem,bcmflatleaf,bcmflatstore,pool_temp1,pool_temp2,    &
    & bchyfg,bcgrainf,tot_mass_rem,sel_mass_left)
          ! buried biomass
              ! standing not done so root removal done here
  IF (btest(sel_position,2).AND..NOT.btest(sel_position,0))                     &
    & CALL rem_bg_pool(stemf,leaff,storef,rootstoref,rootfiberf,bcmbgstemz,     &
    & pool_temp2z,pool_temp3z,bcmrootstorez,bcmrootfiberz,nslay,bchyfg,bcgrainf,&
    & tot_mass_rem,sel_mass_left)
          ! adjust standing pools if supporting stems or roots removed
  CALL adj_stand_pool(start_stem,start_leaf,start_store,start_rootstore,        &
                    & start_rootfiber,bcmstandstem,bcmstandleaf,bcmstandstore,  &
                    & bcmrootstorez,bcmrootfiberz,bcmflatstem,bcmflatleaf,      &
                    & bcmflatstore,bcdstm,nslay)
END IF
      ! temporary pool
IF (btest(sel_pool,1)) THEN
          ! standing and rooted biomass
          ! set starting values
  start_store = btmstandstore
  start_leaf = btmstandleaf
  start_stem = btmstandstem
  DO idx = 1,nslay
     start_rootstore(idx) = btmbgrootstorez(idx)
     start_rootfiber(idx) = btmbgrootfiberz(idx)
  END DO
  IF (btest(sel_position,0)) CALL rem_stand_pool(stemf,leaff,storef,rootstoref, &
    & rootfiberf,btmstandstem,btmstandleaf,btmstandstore,btmbgrootstorez,       &
    & btmbgrootfiberz,nslay,bchyfg,btgrainf,btdstm,tot_mass_rem,sel_mass_left)
          ! flat biomass
  IF (btest(sel_position,1)) CALL rem_flat_pool(stemf,leaff,storef,rootstoref,  &
    & rootfiberf,btmflatstem,btmflatleaf,btmflatstore,btmflatrootstore,         &
    & btmflatrootfiber,bchyfg,btgrainf,tot_mass_rem,sel_mass_left)
          ! buried biomass
  IF (btest(sel_position,2)) THEN
     IF (btest(sel_position,0)) THEN
                  ! root removal already done in standing
        CALL rem_bg_pool(stemf,leaff,storef,rootstoref,rootfiberf,btmbgstemz,   &
                       & btmbgleafz,btmbgstorez,pool_temp1z,pool_temp2z,nslay,  &
                       & bchyfg,btgrainf,tot_mass_rem,sel_mass_left)
     ELSE
                  ! standing not done so do root removal here
        CALL rem_bg_pool(stemf,leaff,storef,rootstoref,rootfiberf,btmbgstemz,   &
                       & btmbgleafz,btmbgstorez,btmbgrootstorez,btmbgrootfiberz,&
                       & nslay,bchyfg,btgrainf,tot_mass_rem,sel_mass_left)
     END IF
  END IF
          ! adjust standing pools if supporting stems or roots removed
  CALL adj_stand_pool(start_stem,start_leaf,start_store,start_rootstore,        &
                    & start_rootfiber,btmstandstem,btmstandleaf,btmstandstore,  &
                    & btmbgrootstorez,btmbgrootfiberz,btmflatstem,btmflatleaf,  &
                    & btmflatstore,btdstm,nslay)
END IF
      ! residue pools
IF (btest(sel_pool,2)) THEN
  DO idy = 1,mnbpls
     IF (btest(tflg,idy)) THEN
          ! standing and rooted biomass
          ! set starting values
        start_store = bdmstandstore(idy)
        start_leaf = bdmstandleaf(idy)
        start_stem = bdmstandstem(idy)
        DO idx = 1,nslay
           start_rootstore(idx) = bdmbgrootstorez(idx,idy)
           start_rootfiber(idx) = bdmbgrootfiberz(idx,idy)
        END DO
        IF (btest(sel_position,0)) CALL rem_stand_pool(stemf,leaff,storef,      &
          & rootstoref,rootfiberf,bdmstandstem(idy),bdmstandleaf(idy),          &
          & bdmstandstore(idy),bdmbgrootstorez(1,idy),bdmbgrootfiberz(1,idy),   &
          & nslay,bdhyfg(idy),bdgrainf(idy),bddstm(idy),tot_mass_rem,           &
          & sel_mass_left)
          ! flat biomass
        IF (btest(sel_position,1)) CALL rem_flat_pool(stemf,leaff,storef,       &
          & rootstoref,rootfiberf,bdmflatstem(idy),bdmflatleaf(idy),            &
          & bdmflatstore(idy),bdmflatrootstore(idy),bdmflatrootfiber(idy),      &
          & bdhyfg(idy),bdgrainf(idy),tot_mass_rem,sel_mass_left)
          ! buried biomass
        IF (btest(sel_position,2)) THEN
           IF (btest(sel_position,0)) THEN
              ! root removal already done in standing
              CALL rem_bg_pool(stemf,leaff,storef,rootstoref,rootfiberf,        &
                             & bdmbgstemz(1,idy),bdmbgleafz(1,idy),             &
                             & bdmbgstorez(1,idy),pool_temp1z,pool_temp2z,nslay,&
                             & bdhyfg(idy),bdgrainf(idy),tot_mass_rem,          &
                             & sel_mass_left)
           ELSE
              ! standing not done so do root removal here
              CALL rem_bg_pool(stemf,leaff,storef,rootstoref,rootfiberf,        &
                             & bdmbgstemz(1,idy),bdmbgleafz(1,idy),             &
                             & bdmbgstorez(1,idy),bdmbgrootstorez(1,idy),       &
                             & bdmbgrootfiberz(1,idy),nslay,bdhyfg(idy),        &
                             & bdgrainf(idy),tot_mass_rem,sel_mass_left)
           END IF
        END IF
          ! adjust standing pools if supporting stems or roots removed
        CALL adj_stand_pool(start_stem,start_leaf,start_store,start_rootstore,  &
                          & start_rootfiber,bdmstandstem(idy),bdmstandleaf(idy),&
                          & bdmstandstore(idy),bdmbgrootstorez(1,idy),          &
                          & bdmbgrootfiberz(1,idy),bdmflatstem(idy),            &
                          & bdmflatleaf(idy),bdmflatstore(idy),bddstm(idy),     &
                          & nslay)
     END IF
  END DO
END IF
 
      ! check that complete crop failure shows remaining biomass
IF (tot_mass_rem+sel_mass_left.LE.0.0) sel_mass_left = bcmstandstem +           &
  & bcmstandleaf + bcmstandstore + btmstandstem + btmstandleaf + btmstandstore +&
  & btmflatstem + btmflatleaf + btmflatstore
! 
END SUBROUTINE remove
!
SUBROUTINE rem_stand_pool(stemf,leaff,storef,rootstoref,rootfiberf,pool_stem,   &
                        & pool_leaf,pool_store,pool_rootstore,pool_rootfiber,   &
                        & nslay,pool_hyfg,pool_grainf,pool_dstm,tot_mass_rem,   &
                        & sel_mass_left)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
!
! Subroutine arguments
!
REAL :: leaff,pool_dstm,pool_grainf,pool_leaf,pool_stem,pool_store,rootfiberf,  &
      & rootstoref,sel_mass_left,stemf,storef,tot_mass_rem
INTEGER :: nslay,pool_hyfg
REAL,DIMENSION(mnsz) :: pool_rootfiber,pool_rootstore
!
! Local variables
!
INTEGER :: idx,pool_flag
REAL :: rem_frac
!
!     + + + common blocks + + +
! 
pool_flag = 0
! 
rem_frac = storef
IF (pool_hyfg.LE.2) rem_frac = rem_frac*pool_grainf
CALL rem_pool(pool_store,rem_frac,pool_flag,tot_mass_rem)
 
rem_frac = leaff
IF (pool_hyfg.EQ.3) rem_frac = rem_frac*pool_grainf
CALL rem_pool(pool_leaf,rem_frac,pool_flag,tot_mass_rem)
 
rem_frac = stemf
IF (pool_hyfg.EQ.4) rem_frac = rem_frac*pool_grainf
CALL rem_pool(pool_stem,rem_frac,pool_flag,tot_mass_rem)
      ! also reduce stem count
pool_dstm = pool_dstm*(1.0-rem_frac)
 
rem_frac = rootstoref
IF (pool_hyfg.EQ.5) rem_frac = rem_frac*pool_grainf
DO idx = 1,nslay
  CALL rem_pool(pool_rootstore(idx),rem_frac,pool_flag,tot_mass_rem)
END DO
 
rem_frac = rootfiberf
DO idx = 1,nslay
  CALL rem_pool(pool_rootfiber(idx),rem_frac,pool_flag,tot_mass_rem)
END DO
 
      ! all but fibrous root included in harvest index
IF (pool_flag.EQ.1) THEN
  sel_mass_left = sel_mass_left + pool_store + pool_leaf + pool_stem
  DO idx = 1,nslay
     sel_mass_left = sel_mass_left + pool_rootstore(idx)
  END DO
END IF
! 
END SUBROUTINE rem_stand_pool
!
SUBROUTINE rem_flat_pool(stemf,leaff,storef,rootstoref,rootfiberf,pool_stem,    &
                       & pool_leaf,pool_store,pool_rootstore,pool_rootfiber,    &
                       & pool_hyfg,pool_grainf,tot_mass_rem,sel_mass_left)
!
IMPLICIT NONE
!
! Subroutine arguments
!
REAL :: leaff,pool_grainf,pool_leaf,pool_rootfiber,pool_rootstore,pool_stem,    &
      & pool_store,rootfiberf,rootstoref,sel_mass_left,stemf,storef,tot_mass_rem
INTEGER :: pool_hyfg
!
! Local variables
!
INTEGER :: pool_flag
REAL :: rem_frac
!
!     integer idx, pool_flag
! 
pool_flag = 0
! 
rem_frac = storef
IF (pool_hyfg.LE.2) rem_frac = rem_frac*pool_grainf
CALL rem_pool(pool_store,rem_frac,pool_flag,tot_mass_rem)
 
rem_frac = leaff
IF (pool_hyfg.EQ.3) rem_frac = rem_frac*pool_grainf
CALL rem_pool(pool_leaf,rem_frac,pool_flag,tot_mass_rem)
 
rem_frac = stemf
IF (pool_hyfg.EQ.4) rem_frac = rem_frac*pool_grainf
CALL rem_pool(pool_stem,rem_frac,pool_flag,tot_mass_rem)
 
rem_frac = rootstoref
IF (pool_hyfg.EQ.5) rem_frac = rem_frac*pool_grainf
CALL rem_pool(pool_rootstore,rem_frac,pool_flag,tot_mass_rem)
 
rem_frac = rootfiberf
CALL rem_pool(pool_rootfiber,rem_frac,pool_flag,tot_mass_rem)
 
      ! all but fibrous root included in harvest index
IF (pool_flag.EQ.1) sel_mass_left = sel_mass_left + pool_store + pool_leaf +    &
                                  & pool_stem + pool_rootstore
 
END SUBROUTINE rem_flat_pool
!
SUBROUTINE rem_bg_pool(stemf,leaff,storef,rootstoref,rootfiberf,pool_stem,      &
                     & pool_leaf,pool_store,pool_rootstore,pool_rootfiber,nslay,&
                     & pool_hyfg,pool_grainf,tot_mass_rem,sel_mass_left)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
!
! Subroutine arguments
!
REAL :: leaff,pool_grainf,rootfiberf,rootstoref,sel_mass_left,stemf,storef,     &
      & tot_mass_rem
INTEGER :: nslay,pool_hyfg
REAL,DIMENSION(mnsz) :: pool_leaf,pool_rootfiber,pool_rootstore,pool_stem,      &
                      & pool_store
!
! Local variables
!
INTEGER :: idx,pool_flag
REAL :: rem_frac
!
!     + + + common blocks + + +
!
pool_flag = 0
! 
rem_frac = storef
IF (pool_hyfg.LE.2) rem_frac = rem_frac*pool_grainf
DO idx = 1,nslay
  CALL rem_pool(pool_store(idx),rem_frac,pool_flag,tot_mass_rem)
END DO
 
rem_frac = leaff
IF (pool_hyfg.EQ.3) rem_frac = rem_frac*pool_grainf
DO idx = 1,nslay
  CALL rem_pool(pool_leaf(idx),rem_frac,pool_flag,tot_mass_rem)
END DO
 
rem_frac = stemf
IF (pool_hyfg.EQ.4) rem_frac = rem_frac*pool_grainf
DO idx = 1,nslay
  CALL rem_pool(pool_stem(idx),rem_frac,pool_flag,tot_mass_rem)
END DO
 
rem_frac = rootstoref
IF (pool_hyfg.EQ.5) rem_frac = rem_frac*pool_grainf
DO idx = 1,nslay
  CALL rem_pool(pool_rootstore(idx),rem_frac,pool_flag,tot_mass_rem)
END DO
 
rem_frac = rootfiberf
DO idx = 1,nslay
  CALL rem_pool(pool_rootfiber(idx),rem_frac,pool_flag,tot_mass_rem)
END DO
 
      ! all but fibrous root included in harvest index
IF (pool_flag.EQ.1) THEN
  DO idx = 1,nslay
     sel_mass_left = sel_mass_left + pool_store(idx)
     sel_mass_left = sel_mass_left + pool_leaf(idx)
     sel_mass_left = sel_mass_left + pool_stem(idx)
     sel_mass_left = sel_mass_left + pool_rootstore(idx)
  END DO
END IF
 
END SUBROUTINE rem_bg_pool
!
SUBROUTINE rem_pool(pool_mass,pool_frac,pool_flag,tot_mass_rem)
!
IMPLICIT NONE
!
! Subroutine arguments
!
INTEGER :: pool_flag
REAL :: pool_frac,pool_mass,tot_mass_rem
!
! Local variables
!
REAL :: mass_rem
!
mass_rem = pool_mass*pool_frac
IF (mass_rem.GT.0.0) THEN
  pool_flag = 1
  pool_mass = pool_mass - mass_rem
  tot_mass_rem = tot_mass_rem + mass_rem
END IF
! 
END SUBROUTINE rem_pool
!
SUBROUTINE adj_stand_pool(start_standstem,start_standleaf,start_standstore,     &
                        & start_rootstore,start_rootfiber,pool_standstem,       &
                        & pool_standleaf,pool_standstore,pool_rootstore,        &
                        & pool_rootfiber,pool_flatstem,pool_flatleaf,           &
                        & pool_flatstore,pool_dstm,nslay)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
!
! Subroutine arguments
!
INTEGER :: nslay
REAL :: pool_dstm,pool_flatleaf,pool_flatstem,pool_flatstore,pool_standleaf,    &
      & pool_standstem,pool_standstore,start_standleaf,start_standstem,         &
      & start_standstore
REAL,DIMENSION(mnsz) :: pool_rootfiber,pool_rootstore,start_rootfiber,          &
                      & start_rootstore
!
! Local variables
!
INTEGER :: idx
REAL :: mov_mass,rat_leaf,rat_root,rat_rootfiber,rat_rootstore,rat_stem,        &
      & rat_store
!
!     + + + purpose + + +
!     this subroutine checks to see if a greater proportion of roots
!    (storage and fiber) have been removed than stems, and if so turns
!     the now unsupoorted stems into flat biomass. the same check is
!     then done for stems supoorting leaves and storage biomass.
 
!     + + + common blocks + + +
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     start_standstem - before biomass removal, crop standing stem mass (kg/m^2)
!     start_standleaf - before biomass removal, crop standing leaf mass (kg/m^2)
!     start_standstore - before biomass removal, crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
!     start_rootstorez - before biomass removal, crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     start_rootfiberz - before biomass removal, crop root fibrous mass by soil layer (kg/m^2)
 
!     pool_flatstem  - pool flat stem mass (kg/m^2)
!     pool_flatleaf  - pool flat leaf mass (kg/m^2)
!     pool_flatstore - pool flat storage mass (kg/m^2)
 
!     pool_rootstore - pool flat root storage mass (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     pool_rootfiber - pool flat root fibrous mass (kg/m^2)
 
!     pool_flatstem  - pool flat stem mass (kg/m^2)
!     pool_flatleaf  - pool flat leaf mass (kg/m^2)
!     pool_flatstore - pool flat storage mass (kg/m^2)
 
!     pool_dstm - number of crop stems per unit area (#/m^2)
!               - it is computed by taking the tillering factor
!                 times the plant population density.
 
!     nslay - number of soil layers used
 
!     + + + local variables + + +
 
!     rat_root - the fraction of material remaining after removal
 
      ! adjust store, leaf and stem for rootstore or stem removal
IF (start_standstore.GT.0.0) THEN
  rat_store = pool_standstore/start_standstore
ELSE
  rat_store = 1.0
END IF
IF (start_standleaf.GT.0.0) THEN
  rat_leaf = pool_standleaf/start_standleaf
ELSE
  rat_leaf = 1.0
END IF
IF (start_standstem.GT.0.0) THEN
  rat_stem = pool_standstem/start_standstem
ELSE
  rat_stem = 1.0
END IF
rat_rootstore = 1.0
DO idx = 1,nslay
  IF (start_rootstore(idx).GT.0.0)                                              &
    & rat_rootstore = min(rat_rootstore,pool_rootstore(idx)/start_rootstore(idx)&
    & )
END DO
rat_rootfiber = 1.0
DO idx = 1,nslay
  IF (start_rootfiber(idx).GT.0.0)                                              &
    & rat_rootfiber = min(rat_rootfiber,pool_rootfiber(idx)/start_rootfiber(idx)&
    & )
END DO
      ! check if supporting roots removed
rat_root = min(rat_rootstore,rat_rootfiber)
IF (rat_root.LT.rat_stem) THEN
          ! reduce stem count proportionally as well
  pool_dstm = pool_dstm*(rat_root/rat_stem)
          ! move standing mass
  mov_mass = pool_standstem*(1.0-(rat_root/rat_stem))
  pool_flatstem = pool_flatstem + mov_mass
  pool_standstem = pool_standstem - mov_mass
  rat_stem = rat_root
END IF
      ! check if supporting stems removed
IF (rat_stem.LT.rat_leaf) THEN
  mov_mass = pool_standleaf*(1.0-(rat_stem/rat_leaf))
  pool_flatleaf = pool_flatleaf + mov_mass
  pool_standleaf = pool_standleaf - mov_mass
END IF
IF (rat_stem.LT.rat_store) THEN
  mov_mass = pool_standstore*(1.0-(rat_stem/rat_store))
  pool_flatstore = pool_flatstore + mov_mass
  pool_standstore = pool_standstore - mov_mass
END IF
! 
END SUBROUTINE adj_stand_pool

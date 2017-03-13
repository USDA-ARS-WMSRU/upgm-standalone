SUBROUTINE cut(cutflg,cutht,grainf,cropf,standf,bcmstandstem,bcmstandleaf,      &
             & bcmstandstore,bcmflatstem,bcmflatleaf,bcmflatstore,bczht,        &
             & bcgrainf,bchyfg,btmstandstem,btmstandleaf,btmstandstore,         &
             & btmflatstem,btmflatleaf,btmflatstore,btzht,btgrainf,bdmstandstem,&
             & bdmstandleaf,bdmstandstore,bdmflatstem,bdmflatleaf,bdmflatstore, &
             & bdzht,bdgrainf,bdhyfg,tot_mass_rem,sel_mass_left)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
INCLUDE 'p1unconv.inc'
!
! Subroutine arguments
!
REAL :: bcgrainf,bcmflatleaf,bcmflatstem,bcmflatstore,bcmstandleaf,bcmstandstem,&
      & bcmstandstore,bczht,btgrainf,btmflatleaf,btmflatstem,btmflatstore,      &
      & btmstandleaf,btmstandstem,btmstandstore,btzht,cropf,cutht,grainf,       &
      & sel_mass_left,standf,tot_mass_rem
INTEGER :: bchyfg,cutflg
REAL,DIMENSION(mnbpls) :: bdgrainf,bdmflatleaf,bdmflatstem,bdmflatstore,        &
                        & bdmstandleaf,bdmstandstem,bdmstandstore,bdzht
INTEGER,DIMENSION(mnbpls) :: bdhyfg
!
! Local variables
!
INTEGER :: idy
!
!     + + + purpose + + +
!     process # 32 called from doproc.for
 
!     this subroutine performs the biomass manipulation of cutting
!     biomass. any biomass that is cut is considered killed and moved
!     to the temporary pool to become residue. the component (either
!     crop or a biomass pool) removed is determined by flag which is
!     set before the call to this subroutine.
 
!     0  - cut height is measured from ground up
!     1  - cut height is measured from plant top down
!     2  - cut height is fraction of plant height from top down
!          ie 0.7 means 70% of plant is cut off
 
!     note that biomass for any of these pools that are cut is
!     either transferred to the coresponding flat pool or removed
!     depending on the three removal fraction values input
 
!     + + + keywords + + +
!     cut, transfer, biomass manipulation
 
 
!     + + + argument declarations + + +

!     + + + argument definitions + + +
!     cutflg    - cut height definition flag
!     cutht     - above ground height standing crop and/or
!                 residue is cut to (mm) or fraction
 
!     grainf    - fraction of cut grain mass removed from field
!     cropf     - fraction of cut growing crop mass removed from field
!                 (stems, leaves and any part of grain not removed above)
!     standf    - fraction of cut standing residue removed from field
!                 (stems, leaves and any part of grain not removed above)
 
!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
 
!     bcmflatstem  - crop flat stem mass (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)
 
!     bczht  - crop height (m)
!     bcgrainf - internally computed grain fraction of reproductive mass
!     bchyfg - flag indicating the part of plant to apply the "grain fraction",
!              grf, to when removing that plant part for yield
!         0     grf applied to above ground storage (seeds, reproductive)
!         1     grf times growth stage factor (see growth.for) applied to above ground storage (seeds, reproductive)
!         2     grf applied to all aboveground biomass (forage)
!         3     grf applied to leaf mass (tobacco)
!         4     grf applied to stem mass (sugarcane)
!         5     grf applied to below ground storage mass (potatoes, peanuts)
 
!     btmstandstem - temporary crop standing stem mass (kg/m^2)
!     btmstandleaf - temporary crop standing leaf mass (kg/m^2)
!     btmstandstore - temporarycrop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
 
!     btmflatstem  - temporary crop flat stem mass (kg/m^2)
!     btmflatleaf  - temporary crop flat leaf mass (kg/m^2)
!     btmflatstore - temporary crop flat storage mass (kg/m^2)
 
!     btzht  - temporary crop height (m)
!     btgrainf - internally computed grain fraction of reproductive mass
!     note: harvestable yield flag for crop pool used also for temporary pool
 
!     bdmstandstem  - standing stem mass (kg/m^2)
!     bdmstandleaf  - standing leaf mass (kg/m^2)
!     bdmstandstore - standing storage mass (kg/m^2)
 
!     bdmflatstem  - flat stem mass (kg/m^2)
!     bdmflatleaf  - flat leaf mass (kg/m^2)
!     bdmflatstore - flat storage mass (kg/m^2)
 
!     bdzht  - residue height (m)
!     bdgrainf - internally computed grain fraction of reproductive mass
!     bdhyfg - flag indicating the part of plant to apply the "grain fraction",
!              grf, to when removing that plant part for yield
!         0     grf applied to above ground storage (seeds, reproductive)
!         1     grf times growth stage factor (see growth.for) applied to above ground storage (seeds, reproductive)
!         2     grf applied to all aboveground biomass (forage)
!         3     grf applied to leaf mass (tobacco)
!         4     grf applied to stem mass (sugarcane)
!         5     grf applied to below ground storage mass (potatoes, peanuts)
 
!     tot_mass_rem - total of all mass removed from the field
!     sel_mass_left - mass of material left in pools from which mass is removed
!                      by this harvest operation (kg/m^2)
 
!     + + + accessed common block variable definitions + + +
!     mnbpls        - max number of decomposition pools (currently=3)
 
!     + + + parameters + + +
 
!     + + + local variables + + +
 
!     + + + local variable definitions + + +
!     idy      - loop variable for decomp pools (3 pools total)
 
!     + + + end specifications + + +
 
!     assign crop grain fraction values to temporary pool since
!     material may be transferred to the temporary pool without
!     a specific kill operation (really, how is that?)
btgrainf = bcgrainf
 
!     convert cut height based on cutflg and also change mm to meters
!     in this conversion, make it always from the ground up, and using
!     the max of either crop or temporary crop pools to make sure a
!     height greater than zero exists
SELECT CASE (cutflg)
CASE (0)
  cutht = cutht*mmtom
CASE (1)
  cutht = cutht*mmtom
  cutht = max(bczht,btzht) - cutht
  IF (cutht.LT.0.0) cutht = 0.0
CASE (2)
  cutht = (1.0-cutht)*max(bczht,btzht)
CASE DEFAULT
  WRITE (*,*) 'invalid cutht flag, nothing cut'
END SELECT
 
!***  print *, 'cut tflg: ', tflg
!***  print *, 'tflat before cutting: ', tflat
!***  print *, 'cutht/cstemht/tstemht: ', cutht,cstemht,tstemht
 
!!!!!!!!!!!!!!!!!!!!
!    for now, until the crop database can be updated to include some
!    indication of yield location, all yield will be available for removal
!    if the cut height gets at least the top quarter of the plant, otherwise
!    the amount will be linearly reduced until it is zero when cut height
!    equals crop height.
 
tot_mass_rem = 0.0
sel_mass_left = 0.0
 
      ! cut the living crop pool. note that cut material left on the
      ! field ends up in as flat in the same pool. the transfer from crop
      ! to temporary is then done after all pool accounting is complete.
      ! this accomplishes the assumption that all cut becomes killed flat.
CALL cut_pool(cutht,grainf,cropf,bcmstandstem,bcmstandleaf,bcmstandstore,bczht, &
            & bcgrainf,bchyfg,bcmflatstem,bcmflatleaf,bcmflatstore,tot_mass_rem,&
            & sel_mass_left)
 
      ! cut the temporary crop pool
CALL cut_pool(cutht,grainf,cropf,btmstandstem,btmstandleaf,btmstandstore,btzht, &
            & btgrainf,bchyfg,btmflatstem,btmflatleaf,btmflatstore,tot_mass_rem,&
            & sel_mass_left)
 
DO idy = 1,mnbpls
          ! cut the individual decomposition crop pools. note that standf
          ! is used instead of cropf, keeping plant material removal
          ! separate for living and dead crop. grain is harvested out of both
  CALL cut_pool(cutht,grainf,standf,bdmstandstem(idy),bdmstandleaf(idy),        &
              & bdmstandstore(idy),bdzht(idy),bdgrainf(idy),bdhyfg(idy),        &
              & bdmflatstem(idy),bdmflatleaf(idy),bdmflatstore(idy),            &
              & tot_mass_rem,sel_mass_left)
END DO
 
      ! transfer all crop flat material to temporary. this will end up in
      ! a decomp pool.
btmflatstem = btmflatstem + bcmflatstem
btmflatleaf = btmflatleaf + bcmflatleaf
btmflatstore = btmflatstore + bcmflatstore
bcmflatstem = 0.0
bcmflatleaf = 0.0
bcmflatstore = 0.0
 
      ! check that complete crop failure shows remaining biomass
IF (tot_mass_rem+sel_mass_left.LE.0.0) sel_mass_left = bcmstandstem +           &
  & bcmstandleaf + bcmstandstore + btmstandstem + btmstandleaf + btmstandstore +&
  & btmflatstem + btmflatleaf + btmflatstore
! 
END SUBROUTINE cut
!
SUBROUTINE cut_pool(poolcutht,grainf,cropf,poolmstandstem,poolmstandleaf,       &
                  & poolmstandstore,poolzht,poolgrainf,poolhyfg,poolmflatstem,  &
                  & poolmflatleaf,poolmflatstore,tot_mass_rem,sel_mass_left)
!
! generalized routine for cutting biomass from each pool. grain will
! now even be harvested from decomposition pools, so it is possible
! to kill a crop, transfer it to a decomposition pool, harvest
! the grain sucessfully, and get harvest index
!
IMPLICIT NONE
!
! Subroutine arguments
!
REAL :: cropf,grainf,poolcutht,poolgrainf,poolmflatleaf,poolmflatstem,          &
      & poolmflatstore,poolmstandleaf,poolmstandstem,poolmstandstore,poolzht,   &
      & sel_mass_left,tot_mass_rem
INTEGER :: poolhyfg
!
! Local variables
!
REAL :: mass_cut,mass_rem,rem_frac
INTEGER :: pool_flag
!
!     + + + argument declarations + + +
!
!     + + + local variables + + +
 
!     + + + local variable definitions + + +
!     pool_flag - if mass is removed, set true, so remaining biomass is
!                 summed so harvest index can be computed.
!     mass_cut - mass cut by cutting operation
!     mass_rem - mass removed from field by harvest operation
!     rem_frac - actual removal fraction calculated from combination of
!                grain fraction (grf) and removal fraction
 
pool_flag = 0
IF (poolcutht.LT.poolzht) THEN              ! cut crop pool
          ! above ground storage, reproductive fraction
          ! find amount cut
 
          ! disabled partial storage fraction removal due to cutting too high
          ! we now get all the storage regardless of cut height.
!          if( poolcutht.gt.0.75*poolzht ) then
!              ! yield assumed uniformly distributed in top 25% of plant
!              mass_cut = poolmstandstore                                &
!     &                  * ((poolzht-poolcutht)/(0.25*poolzht))
!          else
!              mass_cut = poolmstandstore
!          end if
 
          ! we get all of the standing storage material regardless of cut height
  mass_cut = poolmstandstore
 
  poolmstandstore = poolmstandstore - mass_cut
          ! find amount removed
  rem_frac = grainf
  IF (poolhyfg.LE.2) rem_frac = rem_frac*poolgrainf
  mass_rem = mass_cut*rem_frac
  mass_cut = mass_cut - mass_rem
          ! cut crop material left on field placed in temporary pool
  poolmflatstore = poolmflatstore + mass_cut
  IF (mass_rem.GT.0.0) THEN
     pool_flag = 1
     tot_mass_rem = tot_mass_rem + mass_rem
  END IF
 
          ! leaf fraction removal amounts
          ! find amount cut
  IF (poolcutht.GT.0.5*poolzht) THEN
              ! leaves assumed uniformly distributed in top 50% of plant
     mass_cut = poolmstandleaf*((poolzht-poolcutht)/(0.5*poolzht))
  ELSE
     mass_cut = poolmstandleaf
  END IF
  poolmstandleaf = poolmstandleaf - mass_cut
          ! find amount removed
  rem_frac = cropf
  IF (poolhyfg.EQ.3) rem_frac = rem_frac*poolgrainf
  mass_rem = mass_cut*rem_frac
  mass_cut = mass_cut - mass_rem
          ! cut crop material left on field placed in temporary pool
  poolmflatleaf = poolmflatleaf + mass_cut
  IF (mass_rem.GT.0.0) THEN
     pool_flag = 1
     tot_mass_rem = tot_mass_rem + mass_rem
  END IF
 
          ! stem fraction removal amounts
          ! find amount cut
  mass_cut = poolmstandstem*(1.0-(poolcutht/poolzht))
  poolmstandstem = poolmstandstem - mass_cut
          ! find amount removed
  rem_frac = cropf
  IF (poolhyfg.EQ.4) rem_frac = rem_frac*poolgrainf
  mass_rem = mass_cut*rem_frac
  mass_cut = mass_cut - mass_rem
          ! cut crop material left on field placed in temporary pool
  poolmflatstem = poolmflatstem + mass_cut
  IF (mass_rem.GT.0.0) THEN
     pool_flag = 1
     tot_mass_rem = tot_mass_rem + mass_rem
  END IF
 
!         stem height
  poolzht = poolcutht
END IF
 
!     add biomass to selected mass if biomass was removed from pool
 
IF (pool_flag.EQ.1) sel_mass_left = sel_mass_left + poolmstandstore +           &
                                  & poolmstandleaf + poolmstandstem +           &
                                  & poolmflatstem + poolmflatleaf +             &
                                  & poolmflatstore
! 
END SUBROUTINE cut_pool

SUBROUTINE thin(thinflg,thinval,grainf,cropf,standf,bcmstandstem,bcmstandleaf,  &
              & bcmstandstore,bcmflatstem,bcmflatleaf,bcmflatstore,bcdstm,      &
              & bcgrainf,bchyfg,btmstandstem,btmstandleaf,btmstandstore,        &
              & btmflatstem,btmflatleaf,btmflatstore,btdstm,btgrainf,           &
              & bdmstandstem,bdmstandleaf,bdmstandstore,bdmflatstem,bdmflatleaf,&
              & bdmflatstore,bddstm,bdgrainf,bdhyfg,tot_mass_rem,sel_mass_left)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
!
! Subroutine arguments
!
REAL :: bcdstm,bcgrainf,bcmflatleaf,bcmflatstem,bcmflatstore,bcmstandleaf,      &
      & bcmstandstem,bcmstandstore,btdstm,btgrainf,btmflatleaf,btmflatstem,     &
      & btmflatstore,btmstandleaf,btmstandstem,btmstandstore,cropf,grainf,      &
      & sel_mass_left,standf,thinval,tot_mass_rem
INTEGER :: bchyfg,thinflg
REAL,DIMENSION(mnbpls) :: bddstm,bdgrainf,bdmflatleaf,bdmflatstem,bdmflatstore, &
                        & bdmstandleaf,bdmstandstem,bdmstandstore
INTEGER,DIMENSION(mnbpls) :: bdhyfg
!
! Local variables
!
INTEGER :: idy
!
!     + + + purpose + + +
!     process # 37 called from doproc.for
 
!     this subroutine performs the biomass manipulation of thinning
!     biomass.  the component (either crop or a biomass pool) removed
!     is determined by flag which is set before the call to this
!     subroutine.
 
!     thinflg
!     0  - remove fraction of plants, thinval = fraction
!     1  - thin to plant population, thinval = population
 
!     note that biomass for any of these pools that are thinned is
!     either transferred to the coresponding flat pool or removed
!     depending on the three removal fraction values input
 
!     + + + keywords + + +
!     thin, transfer, biomass manipulation
! 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
! 
!     thinflg   - thinning value definition flag
!     thinval   - above ground height standing crop and/or
!                 residue is cut to (mm) or fraction
 
!     grainf    - of thinned material, fraction of reproductive mass removed
!     cropf     - of thinned material, fraction of standing crop plants removed
!     standf    - of thinned material, fraction of standing residue removed
 
!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
 
!     bcdstm   - crop stem count (# stems/m^2)
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
 
!     btdstm   - temporary crop stem count (# stems/m^2)
!     btgrainf - internally computed grain fraction of reproductive mass
 
!     bdmstandstem  - standing stem mass (kg/m^2)
!     bdmstandleaf  - standing leaf mass (kg/m^2)
!     bdmstandstore - standing storage mass (kg/m^2)
 
!     bdmflatstem  - flat stem mass (kg/m^2)
!     bdmflatleaf  - flat leaf mass (kg/m^2)
!     bdmflatstore - flat storage mass (kg/m^2)
 
!     bddstm   - residue pool stem count (# stems/m^2)
!     bdgrainf - internally computed grain fraction of reproductive mass
!     bdhyfg - flag indicating the part of plant to apply the "grain fraction",
!              grf, to when removing that plant part for yield
!         0     grf applied to above ground storage (seeds, reproductive)
!         1     grf times growth stage factor (see growth.for) applied to above ground storage (seeds, reproductive)
!         2     grf applied to all aboveground biomass (forage)
!         3     grf applied to leaf mass (tobacco)
!         4     grf applied to stem mass (sugarcane)
!         5     grf applied to below ground storage mass (potatoes, peanuts)
 
!     tot_mass_rem - mass of material removed by this harvest operation (kg/m^2)
!     sel_mass_left - mass of material left in pools from which mass is removed
!                     by this harvest operation (kg/m^2)
 
!     + + + accessed common block variable definitions + + +
!     mnbpls        - max number of decomposition pools (currently=3)
 
!     + + + parameters + + +
 
!     + + + local variables + + +
 
!     + + + local variable definitions + + +
!     idy     - loop variable for decomp pools (3 pools total)
 
!     + + + end specifications + + +
 
!     assign crop grain fraction values to temporary pool since
!     material may be transferred to the temporary pool without
!     a specific kill operation
btgrainf = bcgrainf
 
!     convert thinning value for all cases to fraction of plant
!     population to remain
SELECT CASE (thinflg)
CASE (0)
  thinval = 1.0 - thinval
CASE (1)
  IF (bcdstm.GT.0.0) THEN
     thinval = min(1.0,thinval/bcdstm)
  ELSE
     thinval = 0.0
  END IF
CASE DEFAULT
  WRITE (*,*) 'invalid thinning flag, nothing thinned'
END SELECT
 
!     this thinning is applied to all standing pools,
!     like a cutting device, it is not discriminate in any way
 
tot_mass_rem = 0.0
sel_mass_left = 0.0
 
      ! based on the structure used here, it is assumed that thinning
      ! is a live crop (standing and flat) and a temporary pool (standing only,
      ! since thinning dead flat biomass is meaningless)
      ! and multiple residue decomposition pools (standing only). removal
      ! is applied to the pools in the same manner.
 
      ! thin crop standing pool
CALL thin_pool(thinval,grainf,cropf,bcmstandstem,bcmstandleaf,bcmstandstore,    &
             & btmflatstem,btmflatleaf,btmflatstore,bcgrainf,bchyfg,            &
             & tot_mass_rem,sel_mass_left)
 
      ! if living crop flat pool has biomass, also transfer the correct
      ! proportions into temporary pool
      ! thin crop flat pool
CALL thin_pool(thinval,grainf,cropf,bcmflatstem,bcmflatleaf,bcmflatstore,       &
             & btmflatstem,btmflatleaf,btmflatstore,bcgrainf,bchyfg,            &
             & tot_mass_rem,sel_mass_left)
 
      ! modify stem count to reflect change
bcdstm = bcdstm*thinval
 
      ! thin temporary crop pool
CALL thin_pool(thinval,grainf,cropf,btmstandstem,btmstandleaf,btmstandstore,    &
             & btmflatstem,btmflatleaf,btmflatstore,btgrainf,bchyfg,            &
             & tot_mass_rem,sel_mass_left)
 
      ! modify stem count to reflect change
btdstm = btdstm*thinval
 
DO idy = 1,mnbpls
          ! thin residue decomposition crop pools
  CALL thin_pool(thinval,grainf,standf,bdmstandstem(idy),bdmstandleaf(idy),     &
               & bdmstandstore(idy),bdmflatstem(idy),bdmflatleaf(idy),          &
               & bdmflatstore(idy),bdgrainf(idy),bdhyfg(idy),tot_mass_rem,      &
               & sel_mass_left)
 
          ! modify stem count to reflect change
  bddstm(idy) = bddstm(idy)*thinval
END DO
! 
END SUBROUTINE thin
!
SUBROUTINE thin_pool(thinval,grainf,cropf,poolmstandstem,poolmstandleaf,        &
                   & poolmstandstore,poolmflatstem,poolmflatleaf,poolmflatstore,&
                   & poolgrainf,poolhyfg,tot_mass_rem,sel_mass_left)
!
! local subroutine to apply thinning fractions and biomass removal
! fractions to each pool. stem number reduction is done outside the
! the subroutine to allow the flat mass pool for a living crop to be
! handled the same as a standing pool for thinning purposes.
!
IMPLICIT NONE
!
! Subroutine arguments
!
REAL :: cropf,grainf,poolgrainf,poolmflatleaf,poolmflatstem,poolmflatstore,     &
      & poolmstandleaf,poolmstandstem,poolmstandstore,sel_mass_left,thinval,    &
      & tot_mass_rem
INTEGER :: poolhyfg
!
! Local variables
!
REAL :: mass_rem,mass_thin,rem_frac
INTEGER :: pool_flag
!
!     + + + argument declarations + + +
 
!     + + + local variable definitions + + +
 
!     + + + local variable definitions + + +
 
!     + + + end specifications + + +
 
pool_flag = 0
 
      ! yield mass
mass_thin = poolmstandstore*(1.0-thinval)
rem_frac = grainf
IF (poolhyfg.LE.2) rem_frac = rem_frac*poolgrainf
mass_rem = mass_thin*rem_frac
IF (mass_rem.GT.0.0) THEN
  pool_flag = 1
  tot_mass_rem = tot_mass_rem + mass_rem
END IF
poolmstandstore = poolmstandstore - mass_thin
poolmflatstore = poolmflatstore + mass_thin - mass_rem
 
      ! standing leaf mass
mass_thin = poolmstandleaf*(1.0-thinval)
rem_frac = cropf
IF (poolhyfg.EQ.3) rem_frac = rem_frac*poolgrainf
mass_rem = mass_thin*rem_frac
IF (mass_rem.GT.0.0) THEN
  pool_flag = 1
  tot_mass_rem = tot_mass_rem + mass_rem
END IF
poolmstandleaf = poolmstandleaf - mass_thin
poolmflatleaf = poolmflatleaf + mass_thin - mass_rem
 
      ! standing stem mass
mass_thin = poolmstandstem*(1.0-thinval)
rem_frac = cropf
IF (poolhyfg.EQ.4) rem_frac = rem_frac*poolgrainf
mass_rem = mass_thin*rem_frac
IF (mass_rem.GT.0.0) THEN
  pool_flag = 1
  tot_mass_rem = tot_mass_rem + mass_rem
END IF
poolmstandstem = poolmstandstem - mass_thin
poolmflatstem = poolmflatstem + mass_thin - mass_rem
 
      ! add biomass to selected mass if biomass was removed from pool
IF (pool_flag.EQ.1) sel_mass_left = sel_mass_left + poolmstandstem +            &
                                  & poolmstandleaf + poolmstandstore
! 
END SUBROUTINE thin_pool

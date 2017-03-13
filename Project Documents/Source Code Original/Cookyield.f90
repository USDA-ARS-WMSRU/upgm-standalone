SUBROUTINE cookyield(bchyfg,bnslay,dlfwt,dstwt,drpwt,drswt,bcmstandstem,        &
                   & bcmstandleaf,bcmstandstore,bcmflatstem,bcmflatleaf,        &
                   & bcmflatstore,bcmrootstorez,lost_mass,bcyld_coef,           &
                   & bcresid_int,bcgrf)
!
IMPLICIT NONE
!
! Subroutine arguments
!
REAL :: bcgrf,bcmflatleaf,bcmflatstem,bcmflatstore,bcmstandleaf,bcmstandstem,   &
      & bcmstandstore,bcresid_int,bcyld_coef,dlfwt,drpwt,drswt,dstwt,lost_mass
INTEGER :: bchyfg,bnslay
REAL,DIMENSION(*) :: bcmrootstorez
!
! Local variables
!
REAL :: ddm_adj,ddm_res_yld,store_mass,temp_tot
INTEGER :: idx
!
!     + + + purpose + + +
!     adjust incremental biomass allocation to leaf stem and reproductive
!     pools to match the input residue yield ratio and intercept value,
!     if running the model in that mode
 
!     + + + argument declarations + + +

!     + + + argument definitions + + +
!     bchyfg - flag indicating the part of plant to apply the "grain fraction",
!              grf, to when removing that plant part for yield
!         0     grf applied to above ground storage (seeds, reproductive)
!         1     grf times growth stage factor (see growth.for) applied to
!               above ground storage (seeds, reproductive)
!         2     grf applied to all aboveground biomass (forage)
!         3     grf applied to leaf mass (tobacco)
!         4     grf applied to stem mass (sugarcane)
!         5     grf applied to below ground storage mass (potatoes, peanuts)
!     bnslay - number of soil layers
!     dlfwt - increment in leaf dry weight (kg/m^2)
!     dstwt - increment in dry weight of stem (kg/m^2)
!     drpwt - increment in reproductive mass (kg/m^2)
!     drswt - biomass diverted from partitioning to root storage
!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
!     bcmflatstem  - crop flat stem mass (kg/m^2)
!     bcmflatleaf  - crop flat leaf mass (kg/m^2)
!     bcmflatstore - crop flat storage mass (kg/m^2)
!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     lost_mass - biomass that decayed (disappeared)
!     bcyld_coef - yield coefficient (kg/kg)     harvest_residue = bcyld_coef(kg/kg) * yield + bcresid_int (kg/m^2)
!     bcresid_int - residue intercept (kg/m^2)   harvest_residue = bcyld_coef(kg/kg) * yield + bcresid_int (kg/m^2)
!     bcgrf  - fraction of reproductive biomass that is yield
 
!     + + + common blocks + + +
 
!     + + + local variables + + +
 
!     + + + local variable definitions + + +
!     idx - array index used in loops
!     ddm_res_yld - increment in aboveground dry matter (kg/m^2)
!     temp_tot - temporary total biomass
!     store_mass - intermediate storage mass value
!     ddm_adj - adjusted increment in aboveground dry matter (kg/m^2)
 
!     + + + functions called + + +
 
!     + + + subroutines called + + +
 
!     + + + end of specifications + + +

      ! bchyfg = 0 - grf times  reproductive mass
      ! bchyfg = 1 - grf calculated in growth.for times reproductive mass (grain)
      ! bchyfg = 5 - grf times below ground storage mass

      ! method based on yield residue relationship
      ! sum yield mass increments
!      
SELECT CASE (bchyfg)
CASE (0,1)
          ! 0 - grf times  reproductive mass
          ! 1 - grf calculated in growth.for times reproductive mass (grain)
 
          ! change in residue + yield biomass
          ! (new mass (abovegound + yield) - lost scenesced mass)
  ddm_res_yld = dlfwt + dstwt + drpwt - lost_mass
CASE (5)
          ! 5 - grf times below ground storage mass
 
          ! change in residue + yield biomass
          ! (new mass (abovegound + yield) - lost scenesced mass)
  ddm_res_yld = dlfwt + dstwt + drpwt + drswt - lost_mass
CASE DEFAULT
          ! no adjustment
          ! variable must be initialized
  ddm_res_yld = 0.0
END SELECT
 
      ! find yield storage mass increment based on yield residue relationship
      ! sum present yield + residue biomass
temp_tot = 0.0
IF (bchyfg.EQ.5) THEN
          ! 5 - grf times below ground storage mass
  DO idx = 1,bnslay
     temp_tot = temp_tot + bcmrootstorez(idx)
  END DO
END IF
      ! add lost mass here to allow removing if mass was above threshold
temp_tot = temp_tot + lost_mass + bcmstandstem + bcmstandleaf + bcmstandstore + &
         & bcmflatstem + bcmflatleaf + bcmflatstore
IF (temp_tot+ddm_res_yld.LE.bcresid_int) THEN
  store_mass = 0.0
ELSE IF (temp_tot.LE.bcresid_int) THEN
  store_mass = (ddm_res_yld-(bcresid_int-temp_tot))/bcyld_coef/bcgrf
ELSE
  store_mass = ddm_res_yld/bcyld_coef/bcgrf
END IF
SELECT CASE (bchyfg)
CASE (0,1)
          ! 0 - grf times  reproductive mass
          ! 1 - grf calculated in growth.for times reproductive mass (grain)
 
          ! (new mass (abovegound + yield) - lost scenesced mass)
  ddm_adj = dlfwt + dstwt + drpwt
          ! set reproductive mass increment
  drpwt = store_mass
          ! find remainder of mass increment
  ddm_adj = ddm_adj - drpwt
          ! distribute remainder of mass increment between stem and leaf
          ! leaf increment gets priority
  IF (ddm_adj.GT.dlfwt) THEN
              ! set stem increment
     dstwt = ddm_adj - dlfwt
  ELSE
              ! not enough for both, leaf increment reduced
     dstwt = 0.0
     dlfwt = ddm_adj
  END IF
CASE (5)
          ! 5 - grf times below ground storage mass
 
  ddm_adj = dlfwt + dstwt + drpwt + drswt
          ! set reproductive mass increment
  drswt = store_mass
          ! find remainder of mass increment
  ddm_adj = ddm_adj - drswt
          ! distribute remainder of mass increment between stem and leaf
          ! leaf increment, then reproductive gets priority
  IF (ddm_adj.GT.dlfwt+drpwt) THEN
              ! set stem increment
     dstwt = ddm_adj - dlfwt - drpwt
  ELSE IF (ddm_adj.GT.dlfwt) THEN
              ! set stem increment
     dstwt = 0.0
              ! set reproductive increment
     drpwt = ddm_adj - dlfwt
  ELSE
              ! not enough for both, leaf increment reduced
     dstwt = 0.0
     drpwt = 0.0
     dlfwt = ddm_adj
  END IF
CASE DEFAULT
          ! no adjustment
END SELECT
! 
END SUBROUTINE cookyield
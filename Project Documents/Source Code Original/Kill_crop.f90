SUBROUTINE kill_crop(am0cgf,nlay,bcmstandstem,bcmstandleaf,bcmstandstore,       &
                   & bcmflatstem,bcmflatleaf,bcmflatstore,bcmrootstorez,        &
                   & bcmrootfiberz,bcmbgstemz,bczht,bcdstm,bcxstmrep,bczrtd,    &
                   & bcgrainf,btmstandstem,btmstandleaf,btmstandstore,          &
                   & btmflatstem,btmflatleaf,btmflatstore,btmbgrootstorez,      &
                   & btmbgrootfiberz,btmbgstemz,btzht,btdstm,btxstmrep,btzrtd,  &
                   & btgrainf)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
!
! Subroutine arguments
!
LOGICAL :: am0cgf
REAL :: bcdstm,bcgrainf,bcmflatleaf,bcmflatstem,bcmflatstore,bcmstandleaf,      &
      & bcmstandstem,bcmstandstore,bcxstmrep,bczht,bczrtd,btdstm,btgrainf,      &
      & btmflatleaf,btmflatstem,btmflatstore,btmstandleaf,btmstandstem,         &
      & btmstandstore,btxstmrep,btzht,btzrtd
INTEGER :: nlay
REAL,DIMENSION(mnsz) :: bcmbgstemz,bcmrootfiberz,bcmrootstorez,btmbgrootfiberz, &
                      & btmbgrootstorez,btmbgstemz
!
! Local variables
!
INTEGER :: lay
!
!     + + + purpose + + +
!
!     this subroutine performs the kill crop process and transferring of
!     biomass from crop to temporary pool.  transfer of biomass is performed
!     on above ground biomass and the root biomass.  the transfer is
!     from the crop pool to the "temporary" crop pool.
!
!     + + + keywords + + +
!     kill, transfer, biomass manipulation
 
!
!     + + + argument declarations + + +
!
 
 
 
 
 
 
 
 
 
 
 
 
 
!     + + + argument definitions + + +
 
!     am0cgf      - flag to start and stop crop growth submodel
!     nlay        - number of soil layers
 
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
!     bcxstmrep - a representative diameter so that acdstm*acxstmrep*aczht=acrsai
!     bczrtd  - crop root depth (m)
 
!     bcgrainf - internally computed grain fraction of reproductive mass
 
 
!     btmstandstem - crop standing stem mass (kg/m^2)
!     btmstandleaf - crop standing leaf mass (kg/m^2)
!     btmstandstore - crop standing storage mass (kg/m^2)
!                    (head with seed, or vegetative head (cabbage, pineapple))
 
!     btmflatstem  - crop flat stem mass (kg/m^2)
!     btmflatleaf  - crop flat leaf mass (kg/m^2)
!     btmflatstore - crop flat storage mass (kg/m^2)
 
!     btmbgrootstorez - crop root storage mass by layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     btmbgrootfiberz - crop root fibrous mass by layer (kg/m^2)
 
!     btmbgstemz  - crop buried stem mass by layer (kg/m^2)
 
!     btzht  - crop height (m)
!     btdstm - number of crop stems per unit area (#/m^2)
!            - it is computed by taking the tillering factor
!              times the plant population density.
!     btxstmrep - a representative diameter so that acdstm*acxstmrep*aczht=acrsai
!     btzrtd  - crop root depth (m)
 
!     btgrainf - internally computed grain fraction of reproductive mass
 
!     + + + accessed common block variable definitions + + +
 
!     mnsz          - max number of soil layers
 
!     + + + parameters + + +
 
!     + + + local variables + + +
 
 
!     + + + local variable definitions + + +
 
!     lay        - soil layer index
 
!     + + + end specifications + + +
 
!     need to stop the crop growth (ie. stop calling crop submodel)
am0cgf = .FALSE.
 
      ! once we move crop biomass into the "temporary" crop pool
      ! it is assumed to be dead biomass.
 
btmstandstem = btmstandstem + bcmstandstem
bcmstandstem = 0.0
btmstandleaf = btmstandleaf + bcmstandleaf
bcmstandleaf = 0.0
btmstandstore = btmstandstore + bcmstandstore
bcmstandstore = 0.0
 
btmflatstem = btmflatstem + bcmflatstem
bcmflatstem = 0.0
btmflatleaf = btmflatleaf + bcmflatleaf
bcmflatleaf = 0.0
btmflatstore = btmflatstore + bcmflatstore
bcmflatstore = 0.0
 
DO lay = 1,nlay
  btmbgrootstorez(lay) = btmbgrootstorez(lay) + bcmrootstorez(lay)
  bcmrootstorez(lay) = 0.0
  btmbgrootfiberz(lay) = btmbgrootfiberz(lay) + bcmrootfiberz(lay)
  bcmrootfiberz(lay) = 0.0
  btmbgstemz(lay) = btmbgstemz(lay) + bcmbgstemz(lay)
  bcmbgstemz(lay) = 0.0
END DO
 
btzht = max(btzht,bczht)
bczht = 0.0
btdstm = btdstm + bcdstm
bcdstm = 0.0
btxstmrep = bcxstmrep
      ! do not zero out this value.
      ! it is derived anyway and is displayed after the harvest
      ! bcxstmrep = 0.0
btzrtd = max(btzrtd,bczrtd)
bczrtd = 0.0
 
btgrainf = bcgrainf
      ! do not zero out this value.
      ! it is used until the temporary pool is transferred
      ! bcgrainf = 0.0
! 
END SUBROUTINE kill_crop

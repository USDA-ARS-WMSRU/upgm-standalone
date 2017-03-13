SUBROUTINE fall_mod_vt(rate_mult_vt,thresh_mult_vt,sel_pool,fracarea,bcrbc,     &
                     & bcdkrate,bcddsthrsh,bdrbc,bdkrate,bddsthrsh)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
!
! Subroutine arguments
!
REAL :: bcddsthrsh,fracarea
INTEGER :: bcrbc,sel_pool
REAL,DIMENSION(mndk) :: bcdkrate
REAL,DIMENSION(mnbpls) :: bddsthrsh
REAL,DIMENSION(mndk,mnbpls) :: bdkrate
INTEGER,DIMENSION(mnbpls) :: bdrbc
REAL,DIMENSION(mnrbc) :: rate_mult_vt,thresh_mult_vt
!
! Local variables
!
REAL,DIMENSION(mnrbc) :: area_adj_rate_mult,area_adj_thresh_mult
INTEGER :: idx,idy
!
!     + + + purpose + + +
!     this subroutine modifies the stem fall rate for standing crop and
!     residue material using a multiplier. the rate multiplier is
!     selected based on the toughness class and adjusted if the part of
!     the area is affected.
 
!     + + + keywords + + +
!     standing stem fall fate
 
!     + + + common blocks + + +
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     rate_mult_vt - standing stem fall rate multiplier
!     thresh_mult_vt - standing stem fall threshhold multiplier
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
!     fracarea - fraction of surface area affected by operation
!     bcrbc  - crop residue burial class (it exists in crop so
!              it can be carried into residue)
!         1   o fragile-very small (soybeans) residue
!         2   o moderately tough-short (wheat) residue
!         3   o non fragile-med (corn) residue
!         4   o woody-large residue
!         5   o gravel-rock
!     bcdkrate - crop array of decomposition rate parameters
!     bcddsthrsh  - crop decomposition days required for first stem fall
!     bdrbc  - residue burial class for each residue age pool
!         1   o fragile-very small (soybeans) residue
!         2   o moderately tough-short (wheat) residue
!         3   o non fragile-med (corn) residue
!         4   o woody-large residue
!         5   o gravel-rock
!     bdkrate - array of decomposition rate parameters for
!               each residue age class
!     bddsthrsh  - decomposition days required for first stem fall for
!                  each residue age class
 
!     + + + accessed common block variable definitions + + +
!     mnrbc         - number of residue burial classes
!     mndk          - number of residue decomposition parameters
!     mnbpls        - max number of decomposition pools (currently=3)
 
!     + + + parameters + + +
 
!     + + + local variables + + +
 
!     + + + local variable definitions + + +
!     idx - loop variable
!     area_adj_mult - adjust the multiplier based on area fraction
 
!     + + + end specifications + + +
 
DO idx = 1,mnrbc
  area_adj_rate_mult(idx) = 1.0 + fracarea*(rate_mult_vt(idx)-1.0)
  area_adj_thresh_mult(idx) = 1.0 + fracarea*(thresh_mult_vt(idx)-1.0)
END DO
 
      ! crop pool or temporary pool
IF (btest(sel_pool,0).OR.btest(sel_pool,1)) THEN
          ! adjust for proper residue burial class
  bcdkrate(5) = bcdkrate(5)*area_adj_rate_mult(bcrbc)
  bcddsthrsh = bcddsthrsh*area_adj_thresh_mult(bcrbc)
END IF
      ! residue pools
IF (btest(sel_pool,2)) THEN
        ! for each residue pool
  DO idy = 1,mnbpls
          ! adjust for proper residue burial class
     bdkrate(5,idy) = bdkrate(5,idy)*area_adj_rate_mult(bdrbc(idy))
     bddsthrsh(idy) = bddsthrsh(idy)*area_adj_thresh_mult(bdrbc(idy))
  END DO
END IF
! 
END SUBROUTINE fall_mod_vt

FUNCTION biodrag(bdrlai,bdrsai,bcrlai,bcrsai,bc0rg,bcxrow,bczht,bszrgh)
!
IMPLICIT NONE
!
INCLUDE 'p1unconv.inc'
!
! PARAMETER definitions
!
REAL,PARAMETER :: fur_dis = 0.5
!
! Function arguments
!
INTEGER :: bc0rg
REAL :: bcrlai,bcrsai,bcxrow,bczht,bdrlai,bdrsai,bszrgh
REAL :: biodrag
!
! Local variables
!
REAL :: red_fac,red_lai,red_sai
!
!     + + + purpose + + +
!     biodrag: combine effects of leaves and stems on drag coef.
!     calling subroutine needs b1glob.inc, c1gen.inc s1sgeo.inc
 
!     leaves are less effective at reducing the wind speed than
!     stems.  three effects are simulated: 1. streamlining of leaves,
!     2. leaf sheltered in furrow, and
!     3.leaf area confined in wide rows that act as wind barriers.
!     this function combines these effects into a single
!     value for use by other routines. may still be too large.
 
!     + + + keywords + + +
!     biodrag
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     biodrag  - drag coefficient (no units)
!     bdrlai   - residue leaf area index (sum of all pools)(m^2/m^2)
!     bdrsai   - residue stem silhouette area index (sum of all pools)(m^2/m^2)
!     bcrlai   - crop leaf area index (m^2/m^2)
!     bcrsai   - crop stem silhouette area index (m^2/m^2)
!     bc0rg    - crop seed location flag (0= in furrow, 1=on ridge)
!     bcxrow   - crop row spacing (m)(0 = broadcast)
!     bczht    - crop biomass height (m)
!     bszrgh   - ridge height (mm)
 
!     + + + parameters + + +
!     fur_dis  - coefficient for discounting drag of plant in furrow bottom
 
!     + + + global common blocks + + +
 
!     + + + local variables + + +
 
!     + + + local variable definitions + + +
!     red_lai     - reduced leaf area index (m^2/m^2)
!     red_sai     - reduced stem area index (m^2/m^2)
!     red_fac     - reduction factor
 
!     + + + end specifications + + +
 
      ! place crop values in temporary variables
red_lai = bcrlai
red_sai = bcrsai
 
      ! check for crop biomass position with respect to the ridge
IF (bc0rg.EQ.0) THEN
          ! biomass in furrow
          ! test plant height and ridge height for minimums
  IF (bczht.GT.(fur_dis*bszrgh*mmtom)) THEN
              ! sufficient height for some effect
     red_fac = (1.0-fur_dis*bszrgh*mmtom/bczht)
     red_lai = red_lai*red_fac
     red_sai = red_sai*red_fac
 
              ! check for row width effect
     IF (bcxrow.GT.bczht*5.0) THEN
        red_fac = 1.0/(0.92+0.021*bcxrow/(bczht-fur_dis*bszrgh*mmtom))
        red_lai = red_lai*red_fac
     END IF
 
  ELSE
              ! not tall enough to do anything
     red_lai = 0.0
     red_sai = 0.0
  END IF
          ! biomass not in furrow
          ! test plant height and ridge height for minimums
ELSE IF (bczht.GT.0.0) THEN
              ! check for row width effect
  IF (bcxrow.GT.bczht*5.0) THEN
     red_fac = 1.0/(0.92+0.021*bcxrow/bczht)
     red_lai = red_lai*red_fac
  END IF
ELSE
              ! not tall enough to do anything
  red_lai = 0.0
  red_sai = 0.0
END IF
 
      ! add discounted crop values to biomass values
red_lai = red_lai + bdrlai
red_sai = red_sai + bdrsai
 
      ! streamline effect for total leaf area
red_lai = red_lai*0.2*(1.0-exp(-red_lai))
 
      ! final result
biodrag = red_lai + red_sai
! 
END FUNCTION biodrag

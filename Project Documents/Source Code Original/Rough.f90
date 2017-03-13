SUBROUTINE rough(roughflg,rrimpl,till_i,tillf,rr,tillay,clayf,siltf,rootmass,   &
               & resmass,ldepth)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
!
! PARAMETER definitions
!
REAL,PARAMETER :: rrmin = 6.096
!
! Subroutine arguments
!
INTEGER :: roughflg,tillay
REAL :: rr,rrimpl,tillf,till_i
REAL,DIMENSION(mnsz) :: clayf,ldepth,resmass,rootmass,siltf
!
! Local variables
!
REAL :: biomass,rradj,soiladj
INTEGER :: laycnt
!
!     + + + purpose + + +
!
!     this subroutine performs a random roughness calculation
!     after a tillage operation.
!
!     + + + keywords + + +
!     random roughness (rr), tillage (primary/secondary)
!
!     + + + argument declarations + + +
!
!
!     + + + argument definitions + + +
!
!     tillf	    - fraction of the surface tilled (0-1)
!	    till_i	- tillage intensity factor (0-1)
!     rrimpl	- assigned nominal rr value for the tillage operation (mm)
!     rr		- current surface random roughness (mm)
!     tillay    - number of layers affected by tillage
!     clayf     - clay fraction of soil
!     siltf     - silt fraction of soil
!     rootmass  - mass of roots by layer, pools (kg/m^2)
!     resmass   - mass of buried crop residue by layer, pools (kg/m^2)
!     ldepth    - depth from soil surface of lower layer boundaries
!
!     + + + accessed common block variable definitions + + +
!
!
!     + + + parameters + + +
!
!     + + + local variables + + +
!
!     + + + local variable definitions + + +
!     laycnt   - counter for layers
!     rradj    - adjusted implement random roughness
!     soiladj  - soil texture adjustment multiplier
!     biomass  - total biomass in the tillage zone
!
!     + + + end specifications + + +
!
!	    perform the calculation of the surface rr after a tillage
!     operation.  check to see if the tillage intensity factor is
!     needed before performing the calculation.
!
!     adjust the input random roughness value based on flag
!     roughflg.eq.0 does nothing
rradj = rrimpl                    !(mm) = 0.24 inches
IF ((roughflg.EQ.1).OR.(roughflg.EQ.2)) THEN
!         adjust for soil type
  soiladj = 0.16*siltf(1)**0.25 + 1.47*clayf(1)**0.27
  soiladj = max(0.6,soiladj)
  rradj = rradj*soiladj
END IF
 
IF ((roughflg.EQ.1).OR.(roughflg.EQ.3)) THEN
!         adjust for buried residue amounts, handbook 703, eq 5-17
!         this equation is originally in lbs/ac/in
!         rradj = rrmin+(rradj-rrmin)*(0.8*(1-exp(-0.0012*biomass))+0.2)
!         this was modified in wagners correspondence with foster to use
!         the factor exp(-0.0015*biomass)
!         lbs/ac/in = 226692 * kg/m^2/mm
!         sum up total biomass in the tillage depth
  IF (rrimpl.GT.rrmin) THEN
     biomass = 0.0
     DO laycnt = 1,tillay
        biomass = biomass + rootmass(laycnt)
        biomass = biomass + resmass(laycnt)
     END DO
!             make it kg/m^2/mm
     biomass = biomass/ldepth(tillay)
!             if value is below min, don't adjust since it would
!             increase it with less residue.
!                 this equation uses biomass in kg/m^2/mm
     IF (rradj.GT.rrmin) rradj = rrmin + (rradj-rrmin)                          &
                               & *(0.8*(1-exp(-339.92*biomass))+0.2)
  END IF
END IF
 
      ! is rr going to be increased?  if so, then just do it.
IF (rradj.GE.rr) THEN
  rr = tillf*rradj + (1.0-tillf)*rr
ELSE
  rr = tillf*(till_i*rradj+(1.0-till_i)*rr) + (1.0-tillf)*rr
END IF
! 
END SUBROUTINE rough

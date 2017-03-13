SUBROUTINE resinit(resmass,resdepth,nlay,resarray,laythick)
!
IMPLICIT NONE
!
! Subroutine arguments
!
INTEGER :: nlay
REAL :: resdepth,resmass
REAL,DIMENSION(nlay) :: laythick,resarray
!
! Local variables
!
REAL :: depth
INTEGER :: ilay
!
!     + + + input variable declarations + + +
 
!     + + + input variable definitions + + +
!     resmass - residue mass (kg/m^2)
!     resdepth - depth residue is distributed in soil (mm)
!     nlay - number of soil layers
!     resarray(nlay) - soil residue array by layer (kg/m^2)
!     laythick(nlay) - soil layer thickness (mm)
 
!     + + + local variable declarations + + +
 
!     + + + local variable definitions + + +
!     ilay - array index
!     depth - accumulator for depth
!     thick - thickness of slice to which residue is to be added
 
depth = resdepth
DO ilay = 1,nlay
  IF (depth.GT.0.0) THEN
     resarray(ilay) = resmass*min(depth,laythick(ilay))/(resdepth)
     depth = depth - laythick(ilay)
  ELSE
     resarray(ilay) = 0.0
  END IF
END DO
! 
END SUBROUTINE resinit

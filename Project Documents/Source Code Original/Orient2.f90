SUBROUTINE orient2(dh,ds,impl_dh,impl_ds)
!
IMPLICIT NONE
!
! Subroutine arguments
!
REAL :: dh,ds,impl_dh,impl_ds
!
!     + + + purpose + + +
!
!     this subroutine performs an oriented roughness calculation
!     after a tillage operation.  it creates dikes, regardless
!     whether ridges already exist (it assumes they do).
!
!
!     + + + keywords + + +
!     oriented roughness (or), tillage (primary/secondary)
!
!     + + + argument declarations + + +
!
!
!     + + + argument definitions + + +
!
!
!     + + + accessed common block variable definitions + + +
!
!
!     + + + parameters + + +
!
!     + + + local variables + + +
!
!
!     + + + local variable definitions + + +
!
!     dh      - current dike height (mm)
!     ds      - current dike spacing (mm)
!     impl_ds - implement dike spacing (mm)
!     impl_dh - implement dike height (mm)
 
!     + + + end specifications + + +
!
!  perform the calculation of the oriented or after a tillage
!     operation.
!
ds = impl_ds
dh = impl_dh
! 
END SUBROUTINE orient2

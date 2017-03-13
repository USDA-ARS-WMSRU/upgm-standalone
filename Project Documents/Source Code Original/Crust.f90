SUBROUTINE crust(crustf_rm,tillf,crustf,lmosf,lmosm)
!
IMPLICIT NONE
!
! Subroutine arguments
!
REAL :: crustf,crustf_rm,lmosf,lmosm,tillf
!
!     + + + purpose + + +
!
!     this subroutine destroys the surface crust after a tillage event.
!
!     + + + keywords + + +
!     crust, tillage (primary/secondary)
 
!     include 'p1werm.inc'
!
!     + + + argument declarations + + +
!
!
!     + + + argument definitions + + +
!
!     lmosf - fraction of crusted surface containing loose erodible material
!     lmosm - mass of loose erodible material on crusted portion of surface
!     crustf - current fraction of surface crusted (before & after operation)
!     crustf_rm - fraction of crust removed (0 <= crustf_rm <= 1)
!     tillf - fraction of the surface tilled (0 <= tillf <= 1)
!
!     + + + accessed common block variable definitions + + +
!
!
!     + + + parameters + + +
!
!     + + + local variables + + +
!
!     + + + local variable definitions + + +
!
!     + + + end specifications + + +
!
!
!     crf = cri * ( (1.0 - tillf) + (tillf * (1.0-crustf_rm)))
 
      ! determine fraction of surface that remains crusted
crustf = crustf*(1.0-tillf*crustf_rm)
 
! currently the crust function doesn't modify the loose erodible
! material variables on the crusted surface.  that could be changed
! in the future if it was deemed necessary.
 
! the following should be removed.  need to check soil and erosion
! first to make sure they aren't adversely affected. - lew
! 8/25/1999
 
!     check to see if the loose material on the surface is still there
!     if enough of the crust is removed set lmosf to zero (loose material)
!     this was done according to l. hagen
 
      ! just clear them out if it close to zero
      ! (lh shouldn't have erosion or soil submodels this sensitive)
IF (crustf.LT.0.01) THEN
  lmosf = 0.0
  lmosm = 0.0
END IF
! 
END SUBROUTINE crust

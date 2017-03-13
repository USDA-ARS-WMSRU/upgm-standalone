SUBROUTINE mixproc(u,nlay,xcomp,cmass,mass)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
!
! Subroutine arguments
!
REAL :: cmass,mass,u
INTEGER :: nlay
REAL,DIMENSION(mnsz) :: xcomp
!
! Local variables
!
INTEGER :: i
REAL,DIMENSION(mnsz) :: mixed
!
!     + + + purpose + + +
!
!     this subroutine perfoms the actual mixing process.
!
!     + + + keywords + + +
!     mixing
!
!
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!
!     cmass	- total mass of a component contained in a subregion
!     mass	- total mass in a subregion
!     nlay	- number of layers to be mixed
!     u		- mixing coefficient
!	    xcomp	- component value that is mixed
 
!     + + + local variables + + +
!
!
!     + + + local variable definitions + + +
!
!     i		- index for layers in a subregion
!	    mixed	- temperory variable containing the mixed component
 
!     + + + end specifications + + +
 
!     do the mixing process.
!
DO i = 1,nlay
  mixed(i) = (1-u)*xcomp(i) + (u*cmass/mass)
  xcomp(i) = mixed(i)
END DO
!
END SUBROUTINE mixproc

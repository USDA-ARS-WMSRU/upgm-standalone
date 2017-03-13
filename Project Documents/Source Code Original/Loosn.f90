SUBROUTINE loosn(u,tillf,nlay,density,sbd,laythk)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
!
! Subroutine arguments
!
INTEGER :: nlay
REAL :: tillf,u
REAL,DIMENSION(mnsz) :: density,laythk,sbd
!
! Local variables
!
REAL,DIMENSION(mnsz) :: dum
INTEGER :: i
!
!     + + + purpose + + +
 
!     this subroutine reads in the array(s) containing the components
!     that need to be loosen/compact(ed).
 
!     + + + keywords + + +
!     loosen/compact, tillage
 
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
 
!     nlay     - number of soil layers used
!     u        - loosening coefficient
!     tillf    - fraction of soil area tilled by the machine
!     density  - present soil bulk density
!     sbd      - settled soil bulk density
!     laythk   - layer thickness
 
!     + + + accessed common block variable definitions + + +
!     mnsz        - max number of soil layers
 
!     + + + parameters + + +
 
!     + + + local variables + + +
 
!     + + + local variable definitions + + +
 
!     dum = dummy variable used in calculating the mass in a subregion
!     i = loop variable on layers
 
!     + + + end specifications + + +
 
!     perform the loosen/compact process on the layers in a subregion
 
DO i = 1,nlay
  dum(i) = density(i) - ((density(i)-(2.0/3.0)*sbd(i))*u*tillf)
  laythk(i) = laythk(i)*(density(i)/dum(i))
  density(i) = dum(i)
END DO
!
END SUBROUTINE loosn

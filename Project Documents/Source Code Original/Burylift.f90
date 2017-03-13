SUBROUTINE burylift(nlay,dflat,dstand,droot,dblwgnd,buryf,liftf,fltcoef)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
!
! Subroutine arguments
!
REAL :: buryf,fltcoef,liftf
INTEGER :: nlay
REAL,DIMENSION(mnbpls,mnsz) :: dblwgnd,droot
REAL,DIMENSION(mnbpls) :: dflat,dstand
!
! Local variables
!
REAL,DIMENSION(mnbpls) :: bury,lifttot
INTEGER :: i,lay
REAL,DIMENSION(mnbpls,mnsz) :: liftlay
!
!     + + + purpose + + +
!
!     this subroutine performs the biomass manipulation process of transfering
!     the above ground biomass into the soil or the inverse process of bringing
!     buried biomass to the surface.  it deals only with the biomass
!     pools (ie no live crop is involved)
!
!
!     + + + keywords + + +
!     bury, lift, biomass manipulation
 
!
!     + + + argument declarations + + +
!
!
!     + + + argument definitions + + +
!
!     buryp     - percent of flat material buried
!     dblwgnd   - (decomp) below ground residue / layer and decomp
!                 pool (kg / m^2)
!     dflat     - (decomp) flat residue pools (kg / m^2)
!     droot     - (decomp) root mass / layer and decomp pool
!     dstand    - (decomp) standing residue pools (kg/ m^2)
!     fltcoef   - flattening coefficient of an implement
!     liftp     - percent of buried material lifted to the surface
!     nlay      - number of soil layers used in the operation(s)
!
!     + + + accessed common block variable definitions + + +
!
!     mnbpls    - max number of biomass pools
!     mnsz      - max number of soil layers
!
!     + + + parameters + + +
!
!     + + + local variables + + +
!
!
!     + + + local variable definitions + + +
!
!     bury      - mass of biomass that is buried
!     i         - biomass pools (1-3)
!     lay       - number of layers in a specified subregion
!     liftlay   - buried material lifted to the surface in each layer
!     lifttot   - total buried material lifted to the surface
!
!     + + + end specifications + + +
!
!     perform the flatting of standing residue based upon the flatten
!     coefficient (fltcoef)
 
 
!     perform the lifting and burying of biomass simulataneously
 
DO i = 1,mnbpls
  dflat(i) = dflat(i) + dstand(i)*fltcoef
!     need to use temporary variables when performing the lifting
!     process.  this is done so we do not lift something that has
!     just been buried.
!
  bury(i) = dflat(i)*buryf
  DO lay = 1,nlay
     liftlay(i,lay) = dblwgnd(i,lay)*liftf
     lifttot(i) = lifttot(i) + droot(i,lay)*liftf + liftlay(i,lay)
  END DO
END DO
!     now let's update the 4 pool types using the temporary variables
!     we calculated above.
DO i = 1,mnbpls
  DO lay = 1,nlay
     dblwgnd(i,lay) = dblwgnd(i,lay) + bury(i)/nlay - liftlay(i,lay)
     droot(i,lay) = droot(i,lay)*(1.0-liftf)
  END DO
  dflat(i) = dflat(i) - bury(i) + lifttot(i)
  dstand(i) = dstand(i)*(1.0-fltcoef)
END DO
! 
END SUBROUTINE burylift

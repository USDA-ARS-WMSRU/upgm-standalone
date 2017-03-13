
SUBROUTINE liftvt(liftf,tillf,bdrbc,nlay,bdmflatstem,bdmflatleaf,bdmflatstore,  &
                & bdmflatrootstore,bdmflatrootfiber,bdmbgstemz,bdmbgleafz,      &
                & bdmbgstorez,bdmbgrootstorez,bdmbgrootfiberz,bflg)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
!
! Subroutine arguments
!
INTEGER :: bflg,nlay
REAL :: tillf
REAL,DIMENSION(mnsz,mnbpls) :: bdmbgleafz,bdmbgrootfiberz,bdmbgrootstorez,      &
                             & bdmbgstemz,bdmbgstorez
REAL,DIMENSION(mnbpls) :: bdmflatleaf,bdmflatrootfiber,bdmflatrootstore,        &
                        & bdmflatstem,bdmflatstore
INTEGER,DIMENSION(mnbpls) :: bdrbc
REAL,DIMENSION(mnrbc) :: liftf
!
! Local variables
!
INTEGER :: idy,lay,tflg
REAL,DIMENSION(mnsz) :: liftlay
REAL :: lifttot
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
 
 
 
 
 
 
!     + + + argument definitions + + +
!     liftf     - fraction of buried material lifted to the surface for
!                 different residue burial classes (m^2/m^2)
!     tillf    - fraction of soil area tilled by the machine
!     nlay      - number of soil layers used in the operation(s)
!     bdmflatstem  - flat stem mass (kg/m^2)
!     bdmflatleaf  - flat leaf mass (kg/m^2)
!     bdmflatstore - flat storage mass (kg/m^2)
 
!     bdmflatstore - flat storage root mass (kg/m^2)
!     bdmflatfiber - flat fibrous root mass (kg/m^2)
 
!     bdmbgstemz  - buried stem mass by layer (kg/m^2)
!     bdmbgleafz  - buried leaf mass by layer (kg/m^2)
!     bdmbgstorez - buried (from above ground) storage mass by layer (kg/m^2)
 
!     bdmbgrootstorez - buried storage root mass by layer (kg/m^2)
!     bdmbgrootfiberz - buried fibrous root mass by layer (kg/m^2)
 
!     bflg      - flag indicating what to manipulate
!       0 - all standing material is manipulate (both crop and residue)
!       1 - crop is cut
!       2 - 1'st residue pool
!       4 - 2'nd residue pool
!       ....
!       2**n - nth residue pool
 
!       note that any combination of pools or crop may be used
!       a bit test is done on the binary number to see what to modify
 
!     + + + accessed common block variable definitions + + +
!
!     mnrbc         - max number of residue burial classes
!     mnbpls        - max number of biomass pools
!     mnsz          - max number of soil layers
!
!     + + + parameters + + +
!
!     + + + local variables + + +
!
!
!     + + + local variable definitions + + +
!
!     idy       - biomass pools (1-3)
!     lay       - number of layers in a specified subregion
!     liftlay   - buried material lifted to the surface in each layer
!     lifttot   - total buried material lifted to the surface
!
!     + + + end specifications + + +
 
      !set tflg bits correctly for "all" pools if bflg=0
IF (bflg.EQ.0) THEN
  tflg = 1                          ! crop pool
  DO idy = 1,mnbpls
     tflg = tflg + 2**idy           ! decomp pools
  END DO
ELSE
  tflg = bflg
END IF
 
!     perform the lifting of biomass
DO idy = 1,mnbpls
!         check for proper indexes in bdrbc
  IF ((bdrbc(idy).GE.1).AND.(bdrbc(idy).LE.mnrbc)) THEN
!             lift it if biomass flag right
     IF (btest(tflg,idy)) THEN
 
                  ! stem
        lifttot = 0.0
        DO lay = 1,nlay
           liftlay(lay) = bdmbgstemz(lay,idy)*liftf(bdrbc(idy))*tillf
           lifttot = lifttot + liftlay(lay)
           bdmbgstemz(lay,idy) = bdmbgstemz(lay,idy) - liftlay(lay)
        END DO
        bdmflatstem(idy) = bdmflatstem(idy) + lifttot
 
                  ! leaf
        lifttot = 0.0
        DO lay = 1,nlay
           liftlay(lay) = bdmbgleafz(lay,idy)*liftf(bdrbc(idy))*tillf
           lifttot = lifttot + liftlay(lay)
           bdmbgleafz(lay,idy) = bdmbgleafz(lay,idy) - liftlay(lay)
        END DO
        bdmflatleaf(idy) = bdmflatleaf(idy) + lifttot
 
                  ! store
        lifttot = 0.0
        DO lay = 1,nlay
           liftlay(lay) = bdmbgstorez(lay,idy)*liftf(bdrbc(idy))*tillf
           lifttot = lifttot + liftlay(lay)
           bdmbgstorez(lay,idy) = bdmbgstorez(lay,idy) - liftlay(lay)
        END DO
        bdmflatstore(idy) = bdmflatstore(idy) + lifttot
 
                  ! rootstore
        lifttot = 0.0
        DO lay = 1,nlay
           liftlay(lay) = bdmbgrootstorez(lay,idy)*liftf(bdrbc(idy))*tillf
           lifttot = lifttot + liftlay(lay)
           bdmbgrootstorez(lay,idy) = bdmbgrootstorez(lay,idy) - liftlay(lay)
        END DO
        bdmflatrootstore(idy) = bdmflatrootstore(idy) + lifttot
 
                  ! rootfiber
        lifttot = 0.0
        DO lay = 1,nlay
           liftlay(lay) = bdmbgrootfiberz(lay,idy)*liftf(bdrbc(idy))*tillf
           lifttot = lifttot + liftlay(lay)
           bdmbgrootfiberz(lay,idy) = bdmbgrootfiberz(lay,idy) - liftlay(lay)
        END DO
        bdmflatrootfiber(idy) = bdmflatrootfiber(idy) + lifttot
 
     END IF
  END IF
END DO
! 
END SUBROUTINE liftvt

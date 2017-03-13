SUBROUTINE mburyvt(buryf,tillf,bcrbc,bdrbc,burydistflg,nlay,lthick,ldepth,      &
                 & btmflatstem,btmflatleaf,btmflatstore,btmflatrootstore,       &
                 & btmflatrootfiber,btmbgstemz,btmbgleafz,btmbgstorez,          &
                 & btmbgrootstorez,btmbgrootfiberz,bdmflatstem,bdmflatleaf,     &
                 & bdmflatstore,bdmflatrootstore,bdmflatrootfiber,bdmbgstemz,   &
                 & bdmbgleafz,bdmbgstorez,bdmbgrootstorez,bdmbgrootfiberz,bflg)
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
!
! Subroutine arguments
!
INTEGER :: bcrbc,bflg,burydistflg,nlay
REAL :: btmflatleaf,btmflatrootfiber,btmflatrootstore,btmflatstem,btmflatstore, &
      & tillf
REAL,DIMENSION(mnsz,mnbpls) :: bdmbgleafz,bdmbgrootfiberz,bdmbgrootstorez,      &
                             & bdmbgstemz,bdmbgstorez
REAL,DIMENSION(mnbpls) :: bdmflatleaf,bdmflatrootfiber,bdmflatrootstore,        &
                        & bdmflatstem,bdmflatstore
INTEGER,DIMENSION(mnbpls) :: bdrbc
REAL,DIMENSION(mnsz) :: btmbgleafz,btmbgrootfiberz,btmbgrootstorez,btmbgstemz,  &
                      & btmbgstorez,ldepth,lthick
REAL,DIMENSION(mnrbc) :: buryf
!
! Local variables
!
REAL :: burydist
REAL,DIMENSION(nlay) :: fracbury
INTEGER :: idy,lay,tflg
REAL :: tbury
!
!     + + + purpose + + +
!
!     this subroutine performs the biomass manipulation process of transfering
!     the above ground biomass into the soil or the inverse process of bringing
!     buried biomass to the surface.  it deals only with the biomass
!     pools (ie no live crop is involved)
!
!     + + + keywords + + +
!     bury, lift, biomass manipulation
 
!
!     + + + argument declarations + + +
 

!     + + + argument definitions + + +
 
!     buryf     - fraction of flat material buried for
!                 different residue burial classes (m^2/m^2)
!     tillf    - fraction of soil area tilled by the machine
!     bcrbc     - residue burial class for standing crop
!     bdrbc     - residue burial classes for residue
!     nlay      - number of soil layers used in the operation(s)
!     lthick    - distance from soil surface to bottom of layer
!                 for each soil layer
!     btmflatstem  - crop flat stem mass (kg/m^2)
!     btmflatleaf  - crop flat leaf mass (kg/m^2)
!     btmflatstore - crop flat storage mass (kg/m^2)
 
!     btmflatrootstore - crop flat root storage mass (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     btmflatrootfiber - crop flat root fibrous mass (kg/m^2)
 
!     btmbgflatstemz  - crop buried stem mass by layer (kg/m^2)
!     btmbgflatleafz  - crop buried leaf mass by layer (kg/m^2)
!     btmbgflatstorez - crop buried storage mass by layer (kg/m^2)
 
!     btmbgrootstorez - crop root storage mass by layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     btmbgrootfiberz - crop root fibrous mass by layer (kg/m^2)
 
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
!       1 - crop
!       2 - 1'st residue pool
!       4 - 2'nd residue pool
!       ....
!       2**n - nth residue pool
 
!       note that any combination of pools or crop may be used
!       a bit test is done on the binary number to see what to modify
!
!     + + + accessed common block variable definitions + + +
!
!     mnrbc         - max number of residue burial classes
!     mnbpls        - max number of biomass pools
!     mnsz          - max number of soil layers
!
!     + + + functions + + +
!
!     + + + local variables + + +
!
!
!     + + + local variable definitions + + +
!
!     bury      - mass of biomass that is buried
!     tbury     - mass of crop biomass that is buried
!     idy       - biomass pools (1-3)
!     lay       - number of layers in a specified subregion
!     tflg      - temporary biomass flag
!
!     + + + end specifications + + +
!
      !set tflg bits correctly for "all" pools if bflg=0
IF (bflg.EQ.0) THEN
  tflg = 1                          ! crop pool
  DO idy = 1,mnbpls
     tflg = tflg + 2**idy           ! decomp pools
  END DO
ELSE
  tflg = bflg
END IF
 
!     calculate fractions of total to be buried in each layer
DO lay = 1,nlay
  fracbury(lay) = burydist(lay,burydistflg,lthick,ldepth,nlay)
END DO
      !perform the burying of biomass
!      print *, 'mbury tflg/bflg: ', tflg, bflg
!      print *, 'tflat before mbury: ', tflat
!      print *, 'dflat before mbury: ', dflat(1), dflat(2),dflat(3)
 
!     check for proper indexes in bcrbc
IF ((bcrbc.GE.1).AND.(bcrbc.LE.mnrbc)) THEN
  IF (btest(tflg,0)) THEN               ! crop pool
              ! stem component
     tbury = btmflatstem*buryf(bcrbc)*tillf
     DO lay = 1,nlay
        btmbgstemz(lay) = btmbgstemz(lay) + tbury*fracbury(lay)
     END DO
     btmflatstem = btmflatstem - tbury
              ! leaf component
     tbury = btmflatleaf*buryf(bcrbc)*tillf
     DO lay = 1,nlay
        btmbgleafz(lay) = btmbgleafz(lay) + tbury*fracbury(lay)
     END DO
     btmflatleaf = btmflatleaf - tbury
              ! storage component
     tbury = btmflatstore*buryf(bcrbc)*tillf
     DO lay = 1,nlay
        btmbgstorez(lay) = btmbgstorez(lay) + tbury*fracbury(lay)
     END DO
     btmflatstore = btmflatstore - tbury
              ! root storage component
     tbury = btmflatrootstore*buryf(bcrbc)*tillf
     DO lay = 1,nlay
        btmbgrootstorez(lay) = btmbgrootstorez(lay) + tbury*fracbury(lay)
     END DO
     btmflatrootstore = btmflatrootstore - tbury
              ! root fiber component
     tbury = btmflatrootfiber*buryf(bcrbc)*tillf
     DO lay = 1,nlay
        btmbgrootfiberz(lay) = btmbgrootfiberz(lay) + tbury*fracbury(lay)
     END DO
     btmflatrootfiber = btmflatrootfiber - tbury
  END IF
END IF
 
DO idy = 1,mnbpls
!         check for proper indexes in bdrbc
  IF ((bdrbc(idy).GE.1).AND.(bdrbc(idy).LE.mnrbc)) THEN
     IF (btest(tflg,idy)) THEN
        tbury = bdmflatstem(idy)*buryf(bdrbc(idy))*tillf
        DO lay = 1,nlay
           bdmbgstemz(lay,idy) = bdmbgstemz(lay,idy) + tbury*fracbury(lay)
        END DO
        bdmflatstem(idy) = bdmflatstem(idy) - tbury
 
        tbury = bdmflatleaf(idy)*buryf(bdrbc(idy))*tillf
        DO lay = 1,nlay
           bdmbgleafz(lay,idy) = bdmbgleafz(lay,idy) + tbury*fracbury(lay)
        END DO
        bdmflatleaf(idy) = bdmflatleaf(idy) - tbury
 
        tbury = bdmflatstore(idy)*buryf(bdrbc(idy))*tillf
        DO lay = 1,nlay
           bdmbgstorez(lay,idy) = bdmbgstorez(lay,idy) + tbury*fracbury(lay)
        END DO
        bdmflatstore(idy) = bdmflatstore(idy) - tbury
 
        tbury = bdmflatrootstore(idy)*buryf(bdrbc(idy))*tillf
        DO lay = 1,nlay
           bdmbgrootstorez(lay,idy) = bdmbgrootstorez(lay,idy)                  &
                                    & + tbury*fracbury(lay)
        END DO
        bdmflatrootstore(idy) = bdmflatrootstore(idy) - tbury
 
        tbury = bdmflatrootfiber(idy)*buryf(bdrbc(idy))*tillf
        DO lay = 1,nlay
           bdmbgrootfiberz(lay,idy) = bdmbgrootfiberz(lay,idy)                  &
                                    & + tbury*fracbury(lay)
        END DO
        bdmflatrootfiber(idy) = bdmflatrootfiber(idy) - tbury
     END IF
  END IF
END DO
! 
!      print *, 'tflat after mbury: ', tflat
!      print *, 'dflat after mbury: ', dflat(1), dflat(2),dflat(3)
!
END SUBROUTINE mburyvt

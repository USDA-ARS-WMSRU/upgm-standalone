SUBROUTINE flatvt(fltcoef,tillf,bcrbc,bdrbc,bcmstandstem,bcmstandleaf,          &
                & bcmstandstore,btmflatstem,btmflatleaf,btmflatstore,bcdstm,    &
                & bdmstandstem,bdmstandleaf,bdmstandstore,bdmflatstem,          &
                & bdmflatleaf,bdmflatstore,bddstm,bflg)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
!
! Subroutine arguments
!
REAL :: bcdstm,bcmstandleaf,bcmstandstem,bcmstandstore,btmflatleaf,btmflatstem, &
      & btmflatstore,tillf
INTEGER :: bcrbc,bflg
REAL,DIMENSION(mnbpls) :: bddstm,bdmflatleaf,bdmflatstem,bdmflatstore,          &
                        & bdmstandleaf,bdmstandstem,bdmstandstore
INTEGER,DIMENSION(mnbpls) :: bdrbc
REAL,DIMENSION(mnrbc) :: fltcoef
!
! Local variables
!
REAL :: flatfrac
INTEGER :: idy,tflg
!
!     + + + purpose + + +
!     process # 33 called from doeffect.for
!
!     this subroutine performs the biomass manipulation process of transferring
!     standing biomass to flat biomass based upon a flatenning coefficient.
!     the standing component (either crop or a biomass pool) flattened
!     is determined by a flag which is set before the call to this
!     subroutine.  the flag may contain any number of combinations
!     found below.
 
!     the implicit assumption in this routine is that if you flatten it,
!     it is removed from the living crop and put into the temporary pool
!     to become residue
 
!            flags values (binary #'s actually)
!   bit no.                                          decimal value
!     x  - flatten standing material in all pools         (0)
!     0  - flatten standing crop                          (1)
!     1  - flatten standing residue in decomp pool #1     (2)
!     2  - flatten standing residue in decomp pool #2     (4)
!     3  - flatten standing residue in decomp pool #3     (8)
!
!     note that biomass for any of these pools that are flattened
!     is transfered to the cooresponding flat pool.
!
!     + + + keywords + + +
!     flatten, biomass manipulation
 
!
!     + + + argument declarations + + +
 
 
 
 
 
 
!
!     + + + argument definitions + + +
!
!     fltcoef   - flattening coefficients of implement for
!                 different residue burial classes (m^2/m^2)
!     tillf    - fraction of soil area tilled by the machine
!     bcrbc     - residue burial class for standing crop
!     bdrbc     - residue burial class for residue
 
!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bcmstandleaf - crop standing leaf mass (kg/m^2)
!     bcmstandstore - crop standing storage mass (kg/m^2)
 
!     btmflatstem  - crop flat stem mass (kg/m^2)
!     btmflatleaf  - crop flat leaf mass (kg/m^2)
!     btmflatstore - crop flat storage mass (kg/m^2)
 
!     bcdstm - number of crop stems per unit area (#/m^2)
 
!     bdmstandstem  - standing stem mass (kg/m^2)
!     bdmstandleaf  - standing leaf mass (kg/m^2)
!     bdmstandstore - standing storage mass (kg/m^2)
 
!     bdmflatstem  - flat stem mass (kg/m^2)
!     bdmflatleaf  - flat leaf mass (kg/m^2)
!     bdmflatstore - flat storage mass (kg/m^2)
 
!     bddstm - number of residue stems per unit area (#/m^2)
 
!     dstand    - (decomp pool) standing biomass by age pool (kg/m^2)
!     dflat     - (decomp pool) surface biomass by age pool  (kg/m^2)
!     dstems    - (decomp pool) number of standing residue stems (#/m^2)
!
!     bflg      - flag indicating what to flatten
!       0 - all standing material is flatttened (both crop and residue)
!       1 - crop is flattened
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
!
!     + + + parameters + + +
!
!     + + + local variables + + +
!
!
!     + + + local variable definitions + + +
 
!     idy   - loop variable for decomp pools
!     tflg  - temporary biomass flag
!     flatfrac - fraction of material to be flattened
 
!     + + + end specifications + + +
 
!     set tflg bits correctly for "all" pools if bflg=0
IF (bflg.EQ.0) THEN
  tflg = 1                           ! crop pool
  DO idy = 1,mnbpls
     tflg = tflg + 2**idy              ! decomp pools
  END DO
ELSE
  tflg = bflg
END IF
 
!     check for proper indexes in bcrbc
IF ((bcrbc.GE.1).AND.(bcrbc.LE.mnrbc)) THEN
  IF (btest(tflg,0)) THEN                      ! flatten standing crop
     flatfrac = fltcoef(bcrbc)*tillf
              ! increase flat pools
     btmflatstem = btmflatstem + bcmstandstem*flatfrac
     btmflatleaf = btmflatleaf + bcmstandleaf*flatfrac
     btmflatstore = btmflatstore + bcmstandstore*flatfrac
              ! decrease standing pools
     bcmstandstem = bcmstandstem*(1.0-flatfrac)
     bcmstandleaf = bcmstandleaf*(1.0-flatfrac)
     bcmstandstore = bcmstandstore*(1.0-flatfrac)
              ! reduce # of crop stems
     bcdstm = bcdstm*(1.0-flatfrac)
  END IF
END IF
 
DO idy = 1,mnbpls                     ! flatten standing residue
!         check for proper indexes in bdrbc
  IF ((bdrbc(idy).GE.1).AND.(bdrbc(idy).LE.mnrbc)) THEN
     IF (btest(tflg,idy)) THEN             ! from specified decomp pools
        flatfrac = fltcoef(bdrbc(idy))*tillf
        bdmflatstem(idy) = bdmflatstem(idy) + bdmstandstem(idy)*flatfrac
        bdmflatleaf(idy) = bdmflatleaf(idy) + bdmstandleaf(idy)*flatfrac
        bdmflatstore(idy) = bdmflatstore(idy) + bdmstandstore(idy)*flatfrac
        bdmstandstem(idy) = bdmstandstem(idy)*(1.0-flatfrac)
        bdmstandleaf(idy) = bdmstandleaf(idy)*(1.0-flatfrac)
        bdmstandstore(idy) = bdmstandstore(idy)*(1.0-flatfrac)
        bddstm(idy) = bddstm(idy)*(1.0-flatfrac)
     END IF
  END IF
END DO
! 
END SUBROUTINE flatvt

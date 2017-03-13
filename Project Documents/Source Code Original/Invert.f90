SUBROUTINE invert(nlay,density,laythk,sand,silt,clay,rock_vol,c_sand,m_sand,    &
                & f_sand,vf_sand,w_bd,organic,ph,calcarb,cation,lin_ext,aggden, &
                & drystab,soilwatr,satwatr,thrdbar,ftnbar,avawatr,soilcb,       &
                & soilair,satcond,root,blwgnd,massf)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
INCLUDE 'asd.inc'
!
! Subroutine arguments
!
INTEGER :: nlay
REAL,DIMENSION(mnsz) :: aggden,avawatr,calcarb,cation,clay,c_sand,density,      &
                      & drystab,ftnbar,f_sand,laythk,lin_ext,m_sand,organic,ph, &
                      & rock_vol,sand,satcond,satwatr,silt,soilair,soilcb,      &
                      & soilwatr,thrdbar,vf_sand,w_bd
REAL,DIMENSION(mnsz,mnbpls) :: blwgnd,root
REAL,DIMENSION(msieve+1,mnsz) :: massf
!
! Local variables
!
REAL,DIMENSION(mnsz) :: dum2
INTEGER :: i,j,k
!
!     the following subroutine arguments are not used: density  jcaii  8/08/08
!
!     + + + purpose + + +
!
!     this subroutine reads in the array(s) containing the components
!     that need to be inverted.  it then calls the subroutine invproc
!     and the actual inversion process is performed.
!
!
!
!     + + + keywords + + +
!     inversion, tillage
 
 
!
!     + + + argument declarations + + +
!
!
!     + + + argument definitions + + +
!
!     density     - soil density
!     laythk      - layer thickness
 
!     sand        - fraction of sand
!     silt        - fraction of silt
!     clay        - fraction of clay
!     rock_vol    - volume fraction of rock
!     c_sand      - fraction of course sand
!     m_sand      - fraction of medium sand
!     f_sand      - fraction of fine sand
!     vf_sand     - fraction of very fine sand
 
!     w_bd        - wet (1/3 bar) soil density
 
!     organic     - fraction of organic matter
!     ph          - soil ph
!     calcarb     - fraction of calcium carbonate
!     cation      - cation exchange capcity
 
!     lin_ext     - linear extensibility
 
!     aggden      - aggregrate density
!     drystab     - dry aggregrate stability
 
!     soilwatr    - soil water content (mass bases)
!     satwatr     - saturation soil water content
!     thrdbar     - 1/3 bar soil water content
!     ftnbar      - 15 bar soil water content
!     avawatr     - available soil water content
 
!     soilcbr     - soil cb value
!     soilair     - soil air entery potential
!     satcond     - saturated hydraulic conductivity
 
!     root        - root mass by layers
!     blwgnd      - below ground biomass
!     massf       - mass fractions for sieve cuts
 
!     nlay        - number of soil layers used
!
!     + + + accessed common block variable definitions + + +
!
!     mnsz	- max number of soil layers
!
!     + + + parameters + + +
!
!     + + + local variables + + +
!
!
!     + + + local variable definitions + + +
!
!     dum2    - dummy variable containing a variable array to
!               be passed to the inversion process routine
!     i       - loop variable on decomposition pools
!     j       - loop variable on asd sieves
!     k       - loop variable on the number of layers
!
!     + + + end specifications + + +
 
!  make calls to the inversion process for all variables that need
!  to be inverted.
!
!************************soil variables********************
CALL invproc(nlay,laythk,sand)
CALL invproc(nlay,laythk,silt)
CALL invproc(nlay,laythk,clay)
CALL invproc(nlay,laythk,rock_vol)
 
CALL invproc(nlay,laythk,c_sand)
CALL invproc(nlay,laythk,m_sand)
CALL invproc(nlay,laythk,f_sand)
CALL invproc(nlay,laythk,vf_sand)
 
CALL invproc(nlay,laythk,w_bd)
 
CALL invproc(nlay,laythk,organic)
CALL invproc(nlay,laythk,ph)
CALL invproc(nlay,laythk,calcarb)
CALL invproc(nlay,laythk,cation)
 
CALL invproc(nlay,laythk,lin_ext)
 
CALL invproc(nlay,laythk,aggden)
CALL invproc(nlay,laythk,drystab)
!************************soil variables********************
!
!************************hydrology variables********************
CALL invproc(nlay,laythk,soilwatr)
CALL invproc(nlay,laythk,satwatr)
CALL invproc(nlay,laythk,thrdbar)
CALL invproc(nlay,laythk,ftnbar)
CALL invproc(nlay,laythk,avawatr)
 
CALL invproc(nlay,laythk,soilcb)
CALL invproc(nlay,laythk,soilair)
CALL invproc(nlay,laythk,satcond)
!************************hydrology variables********************
!
!************************asd mass fractions********************
!   need to invert mass fractions for all sieve cuts and layers
!
DO j = 1,msieve
  DO k = 1,nlay
     dum2(k) = massf(j,k)
  END DO
  CALL invproc(nlay,laythk,dum2(1))
  DO k = 1,nlay
     massf(j,k) = dum2(k)
  END DO
END DO
!************************asd mass fractions********************
!
!************************decomposition variables********************
!   need to invert both pools and layers for these next two variables
 
DO i = 1,mnbpls
  DO k = 1,nlay
     dum2(k) = root(k,i)
  END DO
  CALL invproc(nlay,laythk,dum2(1))
  DO k = 1,nlay
     root(k,i) = dum2(k)
  END DO
END DO
 
DO i = 1,mnbpls
  DO k = 1,nlay
     dum2(k) = blwgnd(k,i)
  END DO
  CALL invproc(nlay,laythk,dum2(1))
  DO k = 1,nlay
     blwgnd(k,i) = dum2(k)
  END DO
END DO
!************************decomposition variables********************
!
END SUBROUTINE invert

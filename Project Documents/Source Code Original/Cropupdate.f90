SUBROUTINE cropupdate(bcmstandstem,bcmstandleaf,bcmstandstore,bcmflatstem,      &
                    & bcmflatleaf,bcmflatstore,bcmshoot,bcmbgstemz,             &
                    & bcmrootstorez,bcmrootfiberz,bczht,bcdstm,bczrtd,bcthucum, &
                    & bczgrowpt,bcmbgstem,bcmrootstore,bcmrootfiber,bcxstmrep,  &
                    & bcm,bcmst,bcmf,bcmrt,bcmrtz,bcrcd,bszrgh,bsxrgs,bsargo,   &
                    & bcrsai,bcrlai,bcrsaz,bcrlaz,bcffcv,bcfscv,bcftcv,         &
                    & bcfcancov,bc0rg,bcxrow,bnslay,bc0ssa,bc0ssb,bc0sla,       &
                    & bcovfact,bc0ck,bcxstm,bcdpop)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
!
! Subroutine arguments
!
REAL :: bc0ck,bc0sla,bc0ssa,bc0ssb,bcdpop,bcdstm,bcfcancov,bcffcv,bcfscv,bcftcv,&
      & bcm,bcmbgstem,bcmf,bcmflatleaf,bcmflatstem,bcmflatstore,bcmrootfiber,   &
      & bcmrootstore,bcmrt,bcmshoot,bcmst,bcmstandleaf,bcmstandstem,            &
      & bcmstandstore,bcovfact,bcrcd,bcrlai,bcrsai,bcthucum,bcxrow,bcxstm,      &
      & bcxstmrep,bczgrowpt,bczht,bczrtd,bsargo,bsxrgs,bszrgh
INTEGER :: bc0rg,bnslay
REAL,DIMENSION(mnsz) :: bcmbgstemz,bcmrootfiberz,bcmrootstorez,bcmrtz
REAL,DIMENSION(mncz) :: bcrlaz,bcrsaz
!
!     the following subroutine arguments are not used:
!     bc0ck, bc0rg, bc0sla, bcfcancov, bcffcv, bcfscv, bcftcv, bcm,
!     bcmbgstem, bcmbgstemz, bcmf, bcmflatleaf, bcmflatstem, bcmflatstore,
!     bcmrootfiber, bcmrootfiberz, bcmrootstore, bcmrootstorez, bcmrt, bcmrtz,
!     bcmshoot, bcmst, bcmstandleaf, bcmstandstore, bcovfact, bcrcd, bcrlai,
!     bcrlaz, bcrsaz, bcthucum, bcxrow, bcxstm, bczgrowpt, bczrtd, bnslay,
!     bsargo, bsxrgs, bszrgh    jcaii  8/08/08
!
!     include 'p1const.inc'
 
!     + + + argument definitions + + +
!     definitions are provided for only those variables that are actually
!     used. if others or if some of the above passed in variables not
!     currently used are later implemented, then definitions for these
!     variables will need to be added. the variable dictionary will also
!     need to be updated.
!
!     bc0ssa - stem area to mass coefficient a, result is m^2 per plant
!     bc0ssb - stem area to mass coefficient b, argument is kg per plant
!     bcdpop - number of plants per unit area; crop seeding density.
!              note: bcdstm/bcdpop gives the number of stems per plant
!     bcdstm - number of plant stems per unit area
!              note: bcdstm/bcdpop gives the number of stems per plant
!     bcmstandstem - crop standing stem mass
!     bcrsai - crop stem area index
!     bcxstmrep - a representative diameter so that
!                 acdstm*acxstmrep*aczht=acrsai
!     bczht - crop height
 
      ! calculate crop stem area index
      ! when exponent is not 1, must use mass for single plant stem to get stem area
      ! bcmstandstem, convert (kg/m^2) / (plants/m^2) = kg/plant
      ! result of ((m^2 of stem)/plant) * (# plants/m^2 ground area) = (m^2 of stem)/(m^2 ground area)
IF (bcdpop.GT.0.0) THEN
  bcrsai = bcdpop*bc0ssa*(bcmstandstem/bcdpop)**bc0ssb
ELSE
  bcrsai = 0.0
END IF
 
      ! (m^2 stem / m^2 ground) / ((stems/m^2 ground) * m) = m/stem
      ! this value not reset unless it is meaningful
IF ((bcdstm*bczht).GT.0.0) bcxstmrep = bcrsai/(bcdstm*bczht)
! 
END SUBROUTINE cropupdate

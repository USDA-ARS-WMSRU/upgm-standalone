subroutine cropupdate(bcmstandstem,bcmstandleaf,bcmstandstore,bcmflatstem,      &
                    & bcmflatleaf,bcmflatstore,bcmshoot,bcmbgstemz,             &
                    & bcmrootstorez,bcmrootfiberz,bczht,bcdstm,bczrtd,bcthucum, &
                    & bczgrowpt,bcmbgstem,bcmrootstore,bcmrootfiber,bcxstmrep,  &
                    & bcm,bcmst,bcmf,bcmrt,bcmrtz,bcrcd,bszrgh,bsxrgs,bsargo,   &
                    & bcrsai,bcrlai,bcrsaz,bcrlaz,bcffcv,bcfscv,bcftcv,         &
                    & bcfcancov,bc0rg,bcxrow,bnslay,bc0ssa,bc0ssb,bc0sla,       &
                    & bcovfact,bc0ck,bcxstm,bcdpop)
!
    use constants, only : mncz,mnsub,mnsz
implicit none
!
include 'c1glob.inc' !added to have access to variable 'acmrt'
!
! Dummy arguments
!
real :: bc0ck,bc0sla,bc0ssa,bc0ssb,bcdpop,bcdstm,bcfcancov,bcffcv,bcfscv,bcftcv,&
      & bcm,bcmbgstem,bcmf,bcmflatleaf,bcmflatstem,bcmflatstore,bcmrootfiber,   &
      & bcmrootstore,bcmrt,bcmshoot,bcmst,bcmstandleaf,bcmstandstem,            &
      & bcmstandstore,bcovfact,bcrcd,bcrlai,bcrsai,bcthucum,bcxrow,bcxstm,      &
      & bcxstmrep,bczgrowpt,bczht,bczrtd,bsargo,bsxrgs,bszrgh
integer :: bc0rg,bnslay
real,dimension(mnsz) :: bcmbgstemz,bcmrootfiberz,bcmrootstorez,bcmrtz
real,dimension(mncz) :: bcrlaz,bcrsaz

!local variables
real :: temp_fiber,temp_stem,temp_store
integer :: i
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

!     acmrt - total crop root mass (rootfiber + rootstore) (kg/m^2)
 
      ! calculate crop stem area index
      ! when exponent is not 1, must use mass for single plant stem to get stem area
      ! bcmstandstem, convert (kg/m^2) / (plants/m^2) = kg/plant
      ! result of ((m^2 of stem)/plant) * (# plants/m^2 ground area) = (m^2 of stem)/(m^2 ground area)
if (bcdpop>0.0) then
  bcrsai = bcdpop*bc0ssa*(bcmstandstem/bcdpop)**bc0ssb
else
  bcrsai = 0.0
end if
 
      ! (m^2 stem / m^2 ground) / ((stems/m^2 ground) * m) = m/stem
      ! this value not reset unless it is meaningful
if ((bcdstm*bczht)>0.0) bcxstmrep = bcrsai/(bcdstm*bczht)
!

!copied the following temp variables and for loop from growth.f90 to add temp variables and 
! set acmrt equal to the sum.
temp_store = 0.0
 
  temp_fiber = 0.0
  temp_stem = 0.0
  do i = 1,bnslay
     temp_store = temp_store + bcmrootstorez(i)
     temp_fiber = temp_fiber + bcmrootfiberz(i)
     temp_stem = temp_stem + bcmbgstemz(i)
  end do
  
  !assign the temproot variables to the 'bcm...' variables and add those to get the total
  !root amount.
  bcmrootstore = temp_store
  bcmrootfiber = temp_fiber
  bcmbgstem = temp_stem
  acmrt = bcmrootstore + bcmrootfiber
  
end subroutine cropupdate

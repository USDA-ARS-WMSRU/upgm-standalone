subroutine npcy
!
implicit none
!
include 'p1werm.inc'
include 'cgrow.inc'
include 'csoil.inc'
include 'chumus.inc'
!
! Local variables
!
integer :: j
real :: xx
!
! local variables
!
!     + + + purpose + + +
!     this subroutine is the master nutrient cycling subroutine.
!     calls npmin, nynit, nlch, nmnim, and ndnit for each soil
!     layer.
!
!     + + + kewords + + +
!     nutrient cycling
 
!     + + + common blocks + + +
 
 
! local includes
!     include 'cenvr.inc'
!     include 'cparm.inc'
!     include 'cfert.inc'
 
!     + + + local variables + + +
!
!     xx declared here but commented out in the code  jcaii  8/08/08
!
!     + + + local variable definitions + + +
!     ssfn - sum of n leached from each layer (kg/ha)
!     xx - depth to previous layer (m)
!
!     + + + subroutines called + + +
!     npmin
!     nynit
!     nlch
!     nmnim
!     ndnit
!
!     + + + output formats + + +
!
!     + + + end of specifications + + +
!
smr = 0.
shm = 0.
sim = 0.
sdn = 0.
smp = 0.
sip = 0.
tsfn = 0.
xx = 0.
do j = 1,ir
!        j1=lid(j)
!        j2=j1
!     calculate relative moisture content of each layer
!        sut=st(j)/(fc(j)+1.e-10)
  sut = .8
  if (sut>1.) sut = 1.
!     calculate mineral p transformations
  call npmin(j)
!        if (j1.ne.ld1) go to 2
!        calculate leaching from the top layer
!        call nynit (rq)
!        go to 3
!     calculate leaching from layers other than the top
!   2    call nlch (rq)
!   3    tsfn=tsfn+ssfn
!         if (t(j).le.0.) go to 5
!     calculate soil temperature factor for each layer
!         cdg=t(j)/(t(j)+20551.*exp(-.312*t(j)))
  cdg = 1.
!        if (rz.lt.xx) go to 4
!         if (rz.gt.xx) then
!      calculate organic n & p transformations in layers where there are roots
  call nmnim(j)
  shm = shm + hmn
  smr = smr + rmnr
  sim = sim + wim
  smp = smp + wmp
  sip = sip + wip
!         endif
!     calculate n denitrification
!   4    if (st(j1)/po(j1).lt..9) go to 5
!        if (st(j)/po(j).gt..9) then
!           call ndnit
!           sdn=sdn+wdn
!        endif
!         xx=z(j)
end do
!
end subroutine npcy

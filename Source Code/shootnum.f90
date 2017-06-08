subroutine shootnum(bnslay,bc0idc,bcdpop,bc0shoot,bcdmaxshoot,bcmtotshoot,      &
                  & bcmrootstorez,bcdstm)
!
    use constants, only: mgtokg
implicit none
!
!
! PARAMETER definitions
!
real,parameter :: per_release = 0.9,stage_release = 0.5
!
! Dummy arguments
!
integer :: bc0idc,bnslay
real :: bc0shoot,bcdmaxshoot,bcdpop,bcdstm,bcmtotshoot
real,dimension(*) :: bcmrootstorez
!
! Local variables
!
integer :: lay
real :: root_store_sum
!
!     + + + purpose + + +
!     determine the number of shoots that root storage mass can support,
!     and set the total mass to be released from root storage.
 
!     + + + keywords + + +
!     stem number, shoot growth
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     bnslay - number of soil layers
!     bc0idc - crop type:annual,perennial,etc
!     bcdpop - number of plants per unit area (#/m^2)
!            - note: bcdstm/bcdpop gives the number of stems per plant
!     bc0shoot - mass from root storage required for each shoot (mg/shoot)
!     bcdmaxshoot - maximum number of shoots possible from each plant
!     bcmtotshoot - total mass of shoot growing from root storage biomass (kg/m^2)
!                   in the period from beginning to completion of emergence heat units
!     bcmrootstorez - crop root storage mass by soil layer (kg/m^2)
!                   (tubers (potatoes, carrots), extended leaf (onion), seeds (peanuts))
!     bcdstm - number of crop stems per unit area (#/m^2)
 
!     + + + local variables + + +
 
!     + + + local variable definitions + + +
!     lay - layer index for summing root storage
!     root_store_sum - sum of root storage
 
!     + + + parameters + + +
 
!     + + + parameter definitions + + +
!     per_release - fraction of available root storage mass released to
!                   grow new shoots. default is set to 90% of available
 
!     + + + global common blocks + + +
 
!     + + + common block variables definitions + + +
!     mgtokg - parameter (mgtokg = 0.000001); to convert milligrams to
!              kilograms, multiply by 0.000001
 
      ! find number of shoots (stems) that can be supported from
      ! root storage mass up to the maximum
root_store_sum = 0.0
do lay = 1,bnslay
  root_store_sum = root_store_sum + bcmrootstorez(lay)
end do
 
      ! determine number of regrowth shoots
      ! units are kg/m^2 / kg/shoot = shoots/m^2
if ((bc0idc==3).or.(bc0idc==6)) then
          ! perennials hold some mass in reserve
  bcdstm = max(bcdpop,                                                          &
         & min(bcdmaxshoot*bcdpop,(1.0-per_release)*root_store_sum/(bc0shoot*   &
         & mgtokg)))
else if (bc0idc==8) then
  ! this perennial stages its bud release, putting out less after each cutting
  bcdstm = max(bcdpop,stage_release*root_store_sum/(bc0shoot*mgtokg))
else
          ! all others go for broke
  bcdstm = max(bcdpop,min(bcdmaxshoot*bcdpop,root_store_sum/(bc0shoot*mgtokg)))
end if
 
      ! set the mass of root storage that is released (for use in shoot grow)
      ! units are shoots/m^2 * kg/shoot = kg/m^2
bcmtotshoot = min(root_store_sum,bcdstm*bc0shoot*mgtokg)
!
end subroutine shootnum

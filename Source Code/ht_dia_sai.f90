subroutine ht_dia_sai(bcdpop,bcmstandstem,bc0ssa,bc0ssb,bcdstm,bcxstm,bczmxc,   &
                    & bczht,dht,bcxstmrep,bcrsai)
!
implicit none
!
! Dummy arguments
!
real :: bc0ssa,bc0ssb,bcdpop,bcdstm,bcmstandstem,bcrsai,bcxstm,bcxstmrep,bczht, &
      & bczmxc,dht
intent (in) bc0ssa,bc0ssb,bcdpop,bcdstm,bcmstandstem,bcxstm,bczht,bczmxc
intent (out) bcrsai,bcxstmrep
intent (inout) dht
!
!     + + + purpose + + +
! this routine checks for consistency between plant height and biomass
! accumulation, using half and double the stem diameter (previously unused)
! as check points. the representative stem diameter is set to show where
! within the range the actual stem diameter is.
 
!     + + + argument declarations + + +
 
!     + + + argument definitions + + +
!     bcdpop - crop seeding density (#/m^2)
!     bcmstandstem - crop standing stem mass (kg/m^2)
!     bc0ssa - stem area to mass coefficient a, result is m^2 per plant
!     bc0ssb - stem area to mass coefficient b, argument is kg per plant
!     bcdstm - number of crop stems per unit area (#/m^2)
!     bcxstm - crop stem diameter (m)
!     bczmxc - maximum potential plant height (m)
!     bczht  - crop height (m)
!     dht - daily height increment (m)
!     bcxstmrep - a representative diameter so that acdstm*acxstmrep*aczht=acrsai
!     bcrsai - crop stem area index (m^2/m^2)
 
!     + + + local variables + + +
!
!     min_dia, max_dia, min_height, max_height, new_height set here but
!     commented out in the code  jcaii 8/08/08
!
 
!     + + + local variable definitions + + +
!     min_dia - minimum stem diameter
!     max_dia - maximum stem diameter
!     min_height - minimum plant height
!     max_height - maximum plant height
!     new_height - plant height plus increment
 
!     + + + local parameters + + +
!
!     multmin, multmax set here but commented out in the code  jcaii 8/08/08
!
 
!     + + + local parameter definitions + + +
!     multmin - multiplier to find minimum stem diameter from set stem diameter
!     multmax - multiplier to find maximum stem diameter from set stem diameter
 
!     + + + end of specifications + + +
 
      ! calculate crop stem area index
      ! when exponent is not 1, must use mass for single plant stem to get stem area
      ! bcmstandstem, convert (kg/m^2) / (plants/m^2) = kg/plant
      ! result of ((m^2 of stem)/plant) * (# plants/m^2 ground area) = (m^2 of stem)/(m^2 ground area)
if (bcdpop>0.0) then
  bcrsai = bcdpop*bc0ssa*(bcmstandstem/bcdpop)**bc0ssb
else
  bcrsai = 0.0
end if
 
!      if( dht .lt. 0.0 ) then
!          write(*,*) 'error - this should never appear'
!          stop
!      if( dht .gt. 0.0 ) then
!          ! only adjust height during period of height increase
!          ! back calculate height limits
!          ! min diameter, max height
!          min_dia = multmin * bcxstm
!          max_height = bcrsai / (bcdstm * min_dia)
!
!          ! max diameter, min height
!          max_dia = multmax * bcxstm
!          min_height = bcrsai / (bcdstm * max_dia)
!
!          ! check proposed height increase
!          if( dht .gt. 0.0 ) then
!              new_height = bczht + dht
!              if( new_height .gt. max_height ) then
!                  ! stem is too thin, slow height increase, no less than zero
!                  dht = max(0.0, max_height - bczht)
!              else if( new_height .lt. min_height ) then
!                  ! stem is too thick, speed height increase
!                  dht = min(bczmxc - bczht, min_height - bczht)
!              end if
!          end if
!      end if
 
      ! (m^2 stem / m^2 ground) / ((stems/m^2 ground) * m) = m/stem
      ! this value not reset unless it is meaningful
if ((bcdstm*(bczht+dht))>0.0) then
  bcxstmrep = bcrsai/(bcdstm*(bczht+dht))
else
  bcxstmrep = 0.0
end if
!
end subroutine ht_dia_sai

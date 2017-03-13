function frac_lay(top_loc,bot_loc,top_lay,bot_lay)
!
! this function determines the fraction of a location which
! is contained in a layer. it could also be viewed as the
! fraction of "overlap" of the linear location with a layer
! depth slice. it was written assuming that top values are
! less than bottom values
!
implicit none
!
! Dummy arguments
!
real :: bot_lay,bot_loc,top_lay,top_loc
real :: frac_lay
!
! function arguments
!
!
if (top_lay<=top_loc.and.bot_lay>top_loc) then
          ! top location is in layer
  if (bot_lay>=bot_loc) then
              ! bottom location is also in layer
     frac_lay = 1.0
  else
              ! bottom location is below layer, proportion
     frac_lay = (bot_lay-top_loc)/(bot_loc-top_loc)
  end if
else if (top_lay<bot_loc.and.bot_lay>=bot_loc) then
          ! bottom location is in layer
          ! if we are here, top location is not in layer so proportion
  frac_lay = (bot_loc-top_lay)/(bot_loc-top_loc)
else if (top_lay>top_loc.and.bot_lay<bot_loc) then
          ! location completely spans layer
  frac_lay = (bot_lay-top_lay)/(bot_loc-top_loc)
else
          ! location is not in the layer at all
  frac_lay = 0.0
end if
!
end function frac_lay

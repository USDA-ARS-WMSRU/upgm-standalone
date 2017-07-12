subroutine cdbug(isr, slay, ctrl, clidat, soils, bio)
!
    use upgm_simdata, only : controls
    use climate, only : climate_data
    use soil, only : soildata
    use constants, only : mnsz, mnsub, mnhhrs, mncz,mndk
    use biomaterial
    use datetime, only : caldat
implicit none
!
! Dummy arguments
!
    type(controls) :: ctrl
    type(climate_data) :: clidat
    type(soildata) :: soils
    type(biomatter) :: bio
integer :: isr,slay
!
! Local variables
!
integer :: cd,cm,cy,l
!
!     + + + purpose + + +
!     this program prints out many of the global variables before
!     and after the call to crop provide a comparison of values
!     which may be changed by crop
 
!     author: john tatarko
!     version: 09/01/92
 
!     + + + key words + + +
!     wind, erosion, hydrology, tillage, soil, crop, decomposition
!     management
 
!     + + + global common blocks + + +
 
!     include 'm1subr.inc'
!     include 'c1info.inc'
!     include 'd1glob.inc'
 
!     + + + local common blocks + + +
!     include 'cenvr.inc'
 
!     + + + local variables + + +
 
 
!     + + + local definitions + + +
 
!   cd        - the current day of simulation month.
!   cm        - the current month of simulation year.
!   cy        - the current year of simulation run.
!   isr       - this variable holds the subregion index.
!   l         - this variable is an index on soil layers.
 
!     + + + subroutines called + + +
 
!     + + + functions called + + +
 
!     + + + unit numbers for input/output devices + + +
!     * = screen and keyboard
!     27 = debug crop
 
!     + + + data initializations + + +
 
if (bio%growth%am0cif.eqv..true.) then
  ctrl%sim%tday = -1
  ctrl%sim%tmo = -1
  ctrl%sim%tyr = -1
  ctrl%sim%tisr = -1
end if
call caldat(ctrl%sim%juldate,cd,cm,cy)
 
!     + + + end specifications + + +
 
!          write weather cligen and windgen variables
if ((cd==ctrl%sim%tday).and.(cm==ctrl%sim%tmo).and.(cy==ctrl%sim%tyr).and.(isr==ctrl%sim%tisr)) then
  write (ctrl%handles%cdbugfile,1000) cd,cm,cy,isr
else
  write (ctrl%handles%cdbugfile,1100) cd,cm,cy,isr
end if
write (ctrl%handles%cdbugfile,1200)
write (ctrl%handles%cdbugfile,1300) clidat%awzdpt, clidat%awtdmx, clidat%awtdmn, clidat%aweirr
 
!      write(27,2045) isr
 
write (ctrl%handles%cdbugfile,1400) isr,isr,isr,isr,isr,isr,isr
! admf(isr) is not dimensioned correctly anymore - lew 04/23/99
! just commenting it out for now since it is a debug routine
!      write(27,2051) amrslp(isr), acftcv(isr), acrlai(isr), aczrtd(isr),
!     &               admf(isr), ahfwsf(isr), ac0nam(isr)
write (ctrl%handles%cdbugfile,1600) isr,isr,isr,isr
write (ctrl%handles%cdbugfile,1700) bio%database%tdtm,bio%growth%thucum,bio%deriv%mst,bio%deriv%mrt
! write (ctrl%handles%cdbugfile,1800) isr,isr,isr,isr
write (ctrl%handles%cdbugfile,1800) isr,isr
!write (ctrl%handles%cdbugfile,1900) ahzea,ahzep,ahzptp,actmin(isr),actopt(isr),as0rrk(isr),         &
!              & aslrr(isr)
write (ctrl%handles%cdbugfile,1900) bio%database%tmin,bio%database%topt
write (ctrl%handles%cdbugfile,2000)
 
do l = 1,slay
  write (ctrl%handles%cdbugfile,2100) l,soils%spp%aszlyt(l),      &
                & soils%spp%ahtsmn(l)
end do
write (ctrl%handles%cdbugfile,2200)
 
do l = 1,slay
  write (ctrl%handles%cdbugfile,2300) l,soils%spp%asfcla(l),soils%scp%asfom(l),     &
                & soils%spp%asdblk(l)
end do
 
ctrl%sim%tisr = isr
ctrl%sim%tday = cd
ctrl%sim%tmo = cm
ctrl%sim%tyr = cy
 
!     + + + input formats + + +
 
!     + + + output formats + + +
 1000 format ('**',1x,2(i2,'/'),i4,                                             &
             &'    after  call to crop         subregion no. ',i3)
 1100 format ('**',1x,2(i2,'/'),i4,                                             &
             &'    before call to crop         subregion no. ',i3)
 1200 format (' awzdpt  awtdmx  awtdmn  aweirr')
1300         format (f7.2,3F8.2)
! 2045 format ('subregion number',i3)
 1400 format ('amrslp(',i2,') acftcv(',i2,') acrlai(',i2,')',' aczrtd(',i2,     &
             &') admf(',i2,') ahfwsf(',i2,')',' ac0nam(',i2,')')
 1500 format (2F10.2,2F10.5,2x,f10.2,f10.2,3x,a12)
 1600 format ('actdtm(',i2,') sum-phu(',i2,') acmst(',i2,')','  acmrt(',i2,     &
             &')')
 1700 format (i10,4F10.2,2F12.2)
!1800 format ('      ahzea     ahzep    ahzptp ',' actmin(',i2,') actopt(',i2,  &
!            &') as0rrk(',i2,')',' aslrr(',i2,')')
 1800 format (' actmin(',i2,') actopt(',i2,')')
!1900 format (2f10.2,2f10.3,3f12.2)
 1900 format (2F10.2)
 2000 format ('layer aszlyt ahtsmn')
 2100 format (i4,1x,f7.2,1x,1F7.2)
 2200 format (' layer  asfcla asfom asdblk')
 2300 format (i4,2x,1F7.2,f7.3,F7.2)
 
end subroutine cdbug

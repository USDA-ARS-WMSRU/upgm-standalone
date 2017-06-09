subroutine cdbug(isr,slay, julday)
!
implicit none
!
include 'file.fi'

include 'p1werm.inc'
include 'm1flag.inc'
include 's1layr.inc'
include 's1phys.inc'
include 's1agg.inc'
include 's1dbh.inc'
include 's1dbc.inc'
include 's1sgeo.inc'
include 'c1db1.inc'
include 'c1db2.inc'
include 'c1glob.inc'
include 'w1clig.inc'
include 'h1et.inc'
include 'h1hydro.inc'
include 'h1db1.inc'
include 'h1temp.inc'
include 'tcdbug.inc'
!
! Dummy arguments
!
integer :: isr,slay
integer :: julday
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
!     include 'main.inc'
 
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
 
if (am0cif.eqv..true.) then
  tday = -1
  tmo = -1
  tyr = -1
  tisr = -1
end if
call caldat(0,cd,cm,cy)
 
!     + + + end specifications + + +
 
!          write weather cligen and windgen variables
if ((cd==tday).and.(cm==tmo).and.(cy==tyr).and.(isr==tisr)) then
  write (cdbugfile,1000) cd,cm,cy,isr
else
  write (cdbugfile,1100) cd,cm,cy,isr
end if
write (cdbugfile,1200)
write (cdbugfile,1300) awzdpt,awtdmx,awtdmn,aweirr,awtdpt,awrrh
 
!      write(27,2045) isr
 
write (cdbugfile,1400) isr,isr,isr,isr,isr,isr,isr
! admf(isr) is not dimensioned correctly anymore - lew 04/23/99
! just commenting it out for now since it is a debug routine
!      write(27,2051) amrslp(isr), acftcv(isr), acrlai(isr), aczrtd(isr),
!     &               admf(isr), ahfwsf(isr), ac0nam(isr)
write (cdbugfile,1600) isr,isr,isr,isr
write (cdbugfile,1700) actdtm(isr),acthucum(isr),acmst(isr),acmrt(isr),ahzeta,ahzetp,  &
              & ahzpta
! write (cdbugfile,1800) isr,isr,isr,isr
write (cdbugfile,1800) isr,isr,isr
!write (cdbugfile,1900) ahzea,ahzep,ahzptp,actmin(isr),actopt(isr),as0rrk(isr),         &
!              & aslrr(isr)
write (cdbugfile,1900) ahzea,ahzep,ahzptp,actmin(isr),actopt(isr),as0rrk(isr)
write (cdbugfile,2000)
 
do l = 1,slay
  write (cdbugfile,2100) l,aszlyt(l,isr),ahrsk(l,isr),ahrwc(l,isr),ahrwcs(l,isr),      &
                & ahrwca(l,isr),ahrwcf(l,isr),ahrwcw(l,isr),ah0cb(l,isr),       &
                & aheaep(l,isr),ahtsmx(l,isr),ahtsmn(l,isr)
end do
write (cdbugfile,2200)
 
do l = 1,slay
  write (cdbugfile,2300) l,asfsan(l,isr),asfsil(l,isr),asfcla(l,isr),asfom(l,isr),     &
                & asdblk(l,isr),aslagm(l,isr),as0ags(l,isr),aslagn(l,isr),      &
                & aslagx(l,isr),aseags(l,isr)
end do
 
tisr = isr
tday = cd
tmo = cm
tyr = cy
 
!     + + + input formats + + +
 
!     + + + output formats + + +
 1000 format ('**',1x,2(i2,'/'),i4,                                             &
             &'    after  call to crop         subregion no. ',i3)
 1100 format ('**',1x,2(i2,'/'),i4,                                             &
             &'    before call to crop         subregion no. ',i3)
 1200 format (' awzdpt  awtdmx  awtdmn  aweirr  awtdpt  awrrh')
1300         format (f7.2,5F8.2)
! 2045 format ('subregion number',i3)
 1400 format ('amrslp(',i2,') acftcv(',i2,') acrlai(',i2,')',' aczrtd(',i2,     &
             &') admf(',i2,') ahfwsf(',i2,')',' ac0nam(',i2,')')
 1500 format (2F10.2,2F10.5,2x,f10.2,f10.2,3x,a12)
 1600 format ('actdtm(',i2,') sum-phu(',i2,') acmst(',i2,')','  acmrt(',i2,     &
             &')  ahzeta      ahzetp     ',' ahzpta ')
 1700 format (i10,4F10.2,2F12.2)
!1800 format ('      ahzea     ahzep    ahzptp ',' actmin(',i2,') actopt(',i2,  &
!            &') as0rrk(',i2,')',' aslrr(',i2,')')
 1800 format ('      ahzea     ahzep    ahzptp ',' actmin(',i2,') actopt(',i2,  &
             &') as0rrk(',i2,')')
!1900 format (2f10.2,2f10.3,3f12.2)
 1900 format (2F10.2,2F10.3,2F12.2)
 2000 format ('layer aszlyt  ahrsk ahrwc ahrwcs ahrwca',                        &
             &' ahrwcf ahrwcw ah0cb aheaep ahtsmx ahtsmn')
 2100 format (i4,1x,f7.2,1x,e7.1,f6.2,4F7.2,f6.2,3F7.2)
 2200 format (' layer  asfsan asfsil asfcla asfom asdblk aslagm  as0ags',       &
             &' aslagn  aslagx  aseags')
 2300 format (i4,2x,3F7.2,f7.3,2F7.2,f8.2,f7.3,2F8.2)
 
end subroutine cdbug

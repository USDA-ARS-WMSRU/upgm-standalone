SUBROUTINE cdbug(isr,slay)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
INCLUDE 'm1flag.inc'
INCLUDE 's1layr.inc'
INCLUDE 's1phys.inc'
INCLUDE 's1agg.inc'
INCLUDE 's1dbh.inc'
INCLUDE 's1dbc.inc'
INCLUDE 's1sgeo.inc'
INCLUDE 'c1db1.inc'
INCLUDE 'c1db2.inc'
INCLUDE 'c1glob.inc'
INCLUDE 'w1clig.inc'
INCLUDE 'w1wind.inc'
INCLUDE 'h1et.inc'
INCLUDE 'h1hydro.inc'
INCLUDE 'h1db1.inc'
INCLUDE 'h1temp.inc'
INCLUDE 'tcdbug.inc'
!
! Subroutine arguments
!
INTEGER :: isr,slay
!
! Local variables
!
INTEGER :: cd,cm,cy,l
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
 
IF (am0cif.EQV..TRUE.) THEN
  tday = -1
  tmo = -1
  tyr = -1
  tisr = -1
END IF
CALL caldatw(cd,cm,cy)
 
!     + + + end specifications + + +
 
!          write weather cligen and windgen variables
IF ((cd.EQ.tday).AND.(cm.EQ.tmo).AND.(cy.EQ.tyr).AND.(isr.EQ.tisr)) THEN
  WRITE (27,1000) cd,cm,cy,isr
ELSE
  WRITE (27,1100) cd,cm,cy,isr
END IF
WRITE (27,1200)
WRITE (27,1300) awzdpt,awtdmx,awtdmn,aweirr,awudmx,awudmn,awtdpt,awadir,awhrmx, &
              & awrrh
 
!      write(27,2045) isr
 
WRITE (27,1400) isr,isr,isr,isr,isr,isr,isr
! admf(isr) is not dimensioned correctly anymore - lew 04/23/99
! just commenting it out for now since it is a debug routine
!      write(27,2051) amrslp(isr), acftcv(isr), acrlai(isr), aczrtd(isr),
!     &               admf(isr), ahfwsf(isr), ac0nam(isr)
WRITE (27,1600) isr,isr,isr,isr
WRITE (27,1700) actdtm(isr),acthucum(isr),acmst(isr),acmrt(isr),ahzeta,ahzetp,  &
              & ahzpta
WRITE (27,1800) isr,isr,isr,isr
WRITE (27,1900) ahzea,ahzep,ahzptp,actmin(isr),actopt(isr),as0rrk(isr),         &
              & aslrr(isr)
WRITE (27,2000)
 
DO l = 1,slay
  WRITE (27,2100) l,aszlyt(l,isr),ahrsk(l,isr),ahrwc(l,isr),ahrwcs(l,isr),      &
                & ahrwca(l,isr),ahrwcf(l,isr),ahrwcw(l,isr),ah0cb(l,isr),       &
                & aheaep(l,isr),ahtsmx(l,isr),ahtsmn(l,isr)
END DO
WRITE (27,2200)
 
DO l = 1,slay
  WRITE (27,2300) l,asfsan(l,isr),asfsil(l,isr),asfcla(l,isr),asfom(l,isr),     &
                & asdblk(l,isr),aslagm(l,isr),as0ags(l,isr),aslagn(l,isr),      &
                & aslagx(l,isr),aseags(l,isr)
END DO
 
tisr = isr
tday = cd
tmo = cm
tyr = cy
 
!     + + + input formats + + +
 
!     + + + output formats + + +
 1000 FORMAT ('**',1x,2(i2,'/'),i4,                                             &
             &'    after  call to crop         subregion no. ',i3)
 1100 FORMAT ('**',1x,2(i2,'/'),i4,                                             &
             &'    before call to crop         subregion no. ',i3)
 1200 FORMAT (' awzdpt  awtdmx  awtdmn  aweirr  awudmx  awudmn ',               &
             &' awtdpt  awadir  awhrmx   awrrh ')
 1300 FORMAT (f7.2,9F8.2)
! 2045 format ('subregion number',i3)
 1400 FORMAT ('amrslp(',i2,') acftcv(',i2,') acrlai(',i2,')',' aczrtd(',i2,     &
             &') admf(',i2,') ahfwsf(',i2,')',' ac0nam(',i2,')')
 1500 FORMAT (2F10.2,2F10.5,2x,f10.2,f10.2,3x,a12)
 1600 FORMAT ('actdtm(',i2,') sum-phu(',i2,') acmst(',i2,')','  acmrt(',i2,     &
             &')  ahzeta      ahzetp     ',' ahzpta ')
 1700 FORMAT (i10,4F10.2,2F12.2)
 1800 FORMAT ('      ahzea     ahzep    ahzptp ',' actmin(',i2,') actopt(',i2,  &
             &') as0rrk(',i2,')',' aslrr(',i2,')')
 1900 FORMAT (2F10.2,2F10.3,3F12.2)
 2000 FORMAT ('layer aszlyt  ahrsk ahrwc ahrwcs ahrwca',                        &
             &' ahrwcf ahrwcw ah0cb aheaep ahtsmx ahtsmn')
 2100 FORMAT (i4,1x,f7.2,1x,e7.1,f6.2,4F7.2,f6.2,3F7.2)
 2200 FORMAT (' layer  asfsan asfsil asfcla asfom asdblk aslagm  as0ags',       &
             &' aslagn  aslagx  aseags')
 2300 FORMAT (i4,2x,3F7.2,f7.3,2F7.2,f8.2,f7.3,2F8.2)
 
END SUBROUTINE cdbug

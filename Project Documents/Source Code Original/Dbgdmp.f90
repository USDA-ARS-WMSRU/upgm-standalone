SUBROUTINE dbgdmp(day,sr)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
INCLUDE 'm1subr.inc'
INCLUDE 'm1sim.inc'
INCLUDE 's1layr.inc'
INCLUDE 's1surf.inc'
INCLUDE 's1phys.inc'
INCLUDE 's1agg.inc'
INCLUDE 's1dbh.inc'
INCLUDE 's1dbc.inc'
INCLUDE 's1sgeo.inc'
INCLUDE 'c1gen.inc'
INCLUDE 'c1db1.inc'
INCLUDE 'c1db2.inc'
INCLUDE 'c1db3.inc'
INCLUDE 'c1glob.inc'
INCLUDE 'd1glob.inc'
INCLUDE 'b1glob.inc'
INCLUDE 'w1clig.inc'
INCLUDE 'w1wind.inc'
INCLUDE 'w1pavg.inc'
INCLUDE 'h1hydro.inc'
INCLUDE 'h1scs.inc'
INCLUDE 'h1db1.inc'
INCLUDE 'h1temp.inc'
!
! PARAMETER definitions
!
REAL,PARAMETER :: tstmin = 1E-10,tstmax = 1E10
!
! Subroutine arguments
!
INTEGER :: day,sr
!
! Local variables
!
LOGICAL :: dmpflg
INTEGER :: idx,jdx
!
! ****************************************************************** wjr
!     the dumps variables that have gone out of range
 
!       edit history
!       01-mar-99       wjr     original coding
 
 
!     + + + global common blocks + + +
 
!     include 'p1unconv.inc'
!     include 'wpath.inc'
!     include 'm1geo.inc'
!     include 'm1flag.inc'
!     include 'm1dbug.inc'
!     include 'c1info.inc'
 
!     + + + local common blocks + + +
!     include 'main.inc'
!     include 'man.inc'
!     include 'm2geo.inc'
!     include 'e2erod.inc'
 
!
!
!
DATA dmpflg/.TRUE./
! prototype, remove before compiling
!      if (#(sr).lt.tstmin.or.#(sr).gt.tstmax)
!     &  write(*,*) 'day ',day,' # ', #(sr)
 
 
! s1surf
 
IF (dmpflg) WRITE (*,*) 's1surf'
!
IF (aszcr(sr).LT.0.0.OR.aszcr(sr).GT.23.0) WRITE (*,*) 'day ',day,' aszcr ',    &
  & aszcr(sr)
!
IF (asfcr(sr).LT.0.0.OR.asfcr(sr).GT.1.0) WRITE (*,*) 'day ',day,' asfcr ',     &
  & asfcr(sr)
!
IF (asmlos(sr).LT.0.0.OR.asmlos(sr).GT.2.0) WRITE (*,*) 'day ',day,' asmlos ',  &
  & asmlos(sr)
!
IF (asflos(sr).LT.0.0.OR.asflos(sr).GT.1.0) WRITE (*,*) 'day ',day,' asflos ',  &
  & asflos(sr)
!
! wjr,  test values based on definition
IF (asdcr(sr).LT.0.6.OR.asdcr(sr).GT.2.0) WRITE (*,*) 'day ',day,' asdcr ',     &
  & asdcr(sr)
!
IF (asecr(sr).LT.0.1.OR.asecr(sr).GT.7.0) WRITE (*,*) 'day ',day,' asecr ',     &
  & asecr(sr)
!
IF (asfald(sr).LT.0.05.OR.asfald(sr).GT.0.25) WRITE (*,*) 'day ',day,' asfald ',&
  & asfald(sr)
!
IF (asfalw(sr).LT.0.05.OR.asfalw(sr).GT.0.2) WRITE (*,*) 'day ',day,' asfalw ', &
  & asfalw(sr)
!
! w1info
!
! ***      if (aw0cln.lt.70.0.or.aw0cln.gt.170.0)
! ***     *  write(*,*) 'day ',day,' aw0cln ', aw0cln
!
! ***      if (aw0clt.lt.15.0.or.aw0clt.gt.75.0)
! ***     *  write(*,*) 'day ',day,' aw0clt ', aw0clt
!
! ***      if (aw0wln.lt.70.0.or.aw0wln.gt.170.0)
! ***     *  write(*,*) 'day ',day,' aw0wln ', aw0wln
!
! ***      if (aw0wlt.lt.15.0.or.aw0wlt.gt.75.0)
! ***     *  write(*,*) 'day ',day,' aw0wlt ', aw0wlt
!
! s1sgeo
!
IF (dmpflg) WRITE (*,*) 's1sgeo'
!
IF (aszrgh(sr).LT.0.0.OR.aszrgh(sr).GT.500.0) WRITE (*,*) 'day ',day,' aszrgh ',&
  & aszrgh(sr)
!
IF (asxrgw(sr).LT.10.0.OR.asxrgw(sr).GT.4000.0) WRITE (*,*) 'day ',day,         &
   &' asxrgw ',asxrgw(sr)
!
IF (asxrgs(sr).LT.10.0.OR.asxrgs(sr).GT.2000.0) WRITE (*,*) 'day ',day,         &
   &' asxrgs ',asxrgs(sr)
!
IF (asargo(sr).LT.0.0.OR.asargo(sr).GT.179.0) WRITE (*,*) 'day ',day,' asargo ',&
  & asargo(sr)
!
! wjr,  test values based on definition
IF (asxdks(sr).LT.0.0.OR.asxdks(sr).GT.1000.0) WRITE (*,*) 'day ',day,          &
  & ' asxdks ',asxdks(sr)
!
! wjr,  test values based on definition
IF (asxdkh(sr).LT.0.0.OR.asxdkh(sr).GT.1000.0) WRITE (*,*) 'day ',day,          &
  & ' asxdkh ',asxdkh(sr)
!
IF (as0rrk(sr).LT.tstmin.OR.as0rrk(sr).GT.tstmax) WRITE (*,*) 'day ',day,       &
   &' as0rrk ',as0rrk(sr)
!
IF (aslrrc(sr).LT.tstmin.OR.aslrrc(sr).GT.tstmax) WRITE (*,*) 'day ',day,       &
   &' aslrrc ',aslrrc(sr)
!
IF (aslrr(sr).LT.1.0.OR.aslrr(sr).GT.30.0) WRITE (*,*) 'day ',day,' aslrr ',    &
  & aslrr(sr)
!
! w1wind
!
IF (dmpflg) WRITE (*,*) 'w1wind'
!
! wjr,  test values based on definition
IF (awadir.LT.0.0.OR.awadir.GT.360.0) WRITE (*,*) 'day ',day,' awadir ',awadir
!
IF (awhrmx.LT.1.0.OR.awhrmx.GT.24.0) WRITE (*,*) 'day ',day,' awhrmx ',awhrmx
!
IF (awrmxn.LT.0.0.OR.awrmxn.GT.tstmax) WRITE (*,*) 'day ',day,' awrmxn ',awrmxn
!
! wjr,  test values based on definition
IF (awudmx.LT.0.0.OR.awudmx.GT.50.0) WRITE (*,*) 'day ',day,' awudmx ',awudmx
!
! wjr,  test values based on definition
IF (awudmn.LT.0.0.OR.awudmn.GT.25.0) WRITE (*,*) 'day ',day,' awudmn ',awudmn
!
! wjr,  test values based on definition
IF (awudav.LT.0.0.OR.awudav.GT.35.0) WRITE (*,*) 'day ',day,' awudav ',awudav
!
DO idx = 1,mntime
! wjr,  test values based on definition
  IF (awu(idx).LT.0.0.OR.awu(idx).GT.35.0) WRITE (*,*) 'day ',day,' awu(',idx,  &
     &') ',awu(idx)
END DO
!
! w1pagv
!
! wjr,  test values based on definition
IF (awtmmx.LT.-10.0.OR.awtmmx.GT.40.0) WRITE (*,*) 'day ',day,' awtmmx ',awtmmx
!
! wjr,  test values based on definition
IF (awtmmn.LT.-20.0.OR.awtmmn.GT.30.0) WRITE (*,*) 'day ',day,' awtmmn ',awtmmn
!
! wjr,  test values based on definition
IF (awdair.LT.0.0.OR.awdair.GT.tstmax) WRITE (*,*) 'day ',day,' awdair ',awdair
!
! wjr,  test values based on definition
IF (awztpt.LT.0.0.OR.awztpt.GT.500.0) WRITE (*,*) 'day ',day,' awztpt ',awztpt
!
! wjr,  test values based on definition
IF (awtpav.LT.-20.0.OR.awtpav.GT.40) WRITE (*,*) 'day ',day,' awtpav ',awtpav
!
! ***      if (awepir.lt.tstmin.or.awepir.gt.tstmax)
! ***     *  write(*,*) 'day ',day,' awepir ', awepir
!
! wjr,  test values based on definition
IF (awupav.LT.0.0.OR.awupav.GT.30.0) WRITE (*,*) 'day ',day,' awupav ',awupav
!
! wjr,  test values based on definition
IF (awnuet.LT.0.OR.awnuet.GT.31.0) WRITE (*,*) 'day ',day,' awnuet ',awnuet
!
! wjr,  test values based on definition
IF (aweuet.LT.0.0.OR.aweuet.GT.tstmax) WRITE (*,*) 'day ',day,' aweuet ',aweuet
!
! b1geom
!
IF (dmpflg) WRITE (*,*) 'b1geom'
!
! wjr,  test values based on definition
IF (abrsai(sr).LT.0.0.OR.abrsai(sr).GT.1.0) WRITE (*,*) 'day ',day,' abrsai ',  &
  & abrsai(sr)
!
! wjr,  test values based on definition
IF (abrlai(sr).LT.0.0.OR.abrlai(sr).GT.1.0) WRITE (*,*) 'day ',day,' abrlai ',  &
  & abrlai(sr)
!
DO idx = 1,mncz
!
! wjr,  test values based on definition
  IF (abrsaz(idx,sr).LT.0.0.OR.abrsaz(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' abrsaz(',idx,') ',abrsaz(idx,sr)
!
! wjr,  test values based on definition
  IF (abrlaz(idx,sr).LT.0.0.OR.abrlaz(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' abrlaz(',idx,') ',abrlaz(idx,sr)
!
END DO
!
! wjr,  test values based on definition
IF (abffcv(sr).LT.0.0.OR.abffcv(sr).GT.1.0) WRITE (*,*) 'day ',day,' abffcv ',  &
  & abffcv(sr)
!
! wjr,  test values based on definition
IF (abfscv(sr).LT.0.0.OR.abfscv(sr).GT.1.0) WRITE (*,*) 'day ',day,' abfscv ',  &
  & abfscv(sr)
!
! wjr,  test values based on definition
IF (abftcv(sr).LT.0.0.OR.abftcv(sr).GT.1.0) WRITE (*,*) 'day ',day,' abftcv ',  &
  & abftcv(sr)
!
! w1clig
!
IF (dmpflg) WRITE (*,*) 'w1clig'
!
! wjr,  test values based on definition
IF (awrrh.LT.0.0.OR.awrrh.GT.100.0) WRITE (*,*) 'day ',day,' awrrh ',awrrh
!
! wjr,  test values based on definition
IF (awtdav.LT.-20.0.OR.awtdav.GT.50.0) WRITE (*,*) 'day ',day,' awtdav ',awtdav
!
! wjr,  test values based on definition
IF (awtyav.LT.0.0.OR.awtyav.GT.30.0) WRITE (*,*) 'day ',day,' awtyav ',awtyav
!
DO idx = 1,12
! wjr,  test values based on definition
  IF (awtmav(idx).LT.-10.0.OR.awtmav(idx).GT.40.0) WRITE (*,*) 'day ',day,      &
     &' awtmav(',idx,') ',awtmav(idx)
END DO
!
! wjr,  test values based on definition
IF (awtdmx.LT.0.0.OR.awtdmx.GT.50.0) WRITE (*,*) 'day ',day,' awtdmx ',awtdmx
!
! wjr,  test values based on definition
IF (awtdmn.LT.-20.0.OR.awtdmn.GT.40.0) WRITE (*,*) 'day ',day,' awtdmn ',awtdmn
!
! wjr,  test values based on definition
IF (awtdpt.LT.0.0.OR.awtdpt.GT.40.0) WRITE (*,*) 'day ',day,' awtdpt ',awtdpt
!
! wjr,  test values based on definition
IF (awzdpt.LT.0.0.OR.awzdpt.GT.1000.0) WRITE (*,*) 'day ',day,' awzdpt ',awzdpt
!
! wjr,  test values based on definition
IF (aweirr.LT.0.0.OR.aweirr.GT.tstmax) WRITE (*,*) 'day ',day,' aweirr ',aweirr
!
! s1psd
!
! ***      do 40 idx=1,mnsz
! ***      if (aslsgm(idx, sr).lt.0.0.or.aslsgm(idx, sr).gt.10.0)
! ***     *  write(*,*) 'day ',day,' aslsgm ', aslsgm(idx, sr)
!
! ***      if (as0sgs(idx, sr).lt.0.0.or.as0sgs(idx, sr).gt.10.0)
! ***     *  write(*,*) 'day ',day,' as0sgs ', as0sgs(idx, sr)
! ***   40 continue
!
! s1layd
!
IF (dmpflg) WRITE (*,*) 's1layd'
!
DO idx = 1,nslay(sr)
  IF (asdsblk(idx,sr).LT.tstmin.OR.asdsblk(idx,sr).GT.tstmax) WRITE (*,*)       &
    & 'day ',day,' asdsblk(',idx,') ',asdsblk(idx,sr)
!
  IF (aszlyd(idx,sr).LT.tstmin.OR.aszlyd(idx,sr).GT.tstmax) WRITE (*,*) 'day ', &
    & day,' aszlyd(',idx,') ',aszlyd(idx,sr)
!
! ***      if (aszlym(idx, sr).lt.tstmin.or.aszlym(idx, sr).gt.tstmax)
! ***     *  write(*,*) 'day ',day,' aszlym(',idx,') ', aszlym(idx, sr)
!
! ***      if (aszmpt(idx, sr).lt.tstmin.or.aszmpt(idx, sr).gt.tstmax)
! ***     *  write(*,*) 'day ',day,' aszmpt(',idx,') ', aszmpt(idx, sr)
END DO
!
! s1layr
!
IF (dmpflg) WRITE (*,*) 's1layr'
!
IF (nslay(sr).LT.1.OR.nslay(sr).GT.10) WRITE (*,*) 'day ',day,' nslay ',        &
  & nslay(sr)
!
IF (aszlyt(1,sr).LT.10.0.OR.aszlyt(1,sr).GT.10.0) WRITE (*,*) 'day ',day,       &
   &' aszlyt(1) ',aszlyt(1,sr)
!
IF (nslay(sr).GT.1.AND.(aszlyt(2,sr).LT.40.0.OR.aszlyt(2,sr).GT.40.0))          &
  & WRITE (*,*) 'day ',day,' aszlyt(2) ',aszlyt(2,sr)
!
IF (nslay(sr).GT.2.AND.(aszlyt(3,sr).LT.50.0.OR.aszlyt(3,sr).GT.100.0))         &
  & WRITE (*,*) 'day ',day,' aszlyt(3) ',aszlyt(3,sr)
!
IF (nslay(sr).GT.3.AND.(aszlyt(4,sr).LT.50.0.OR.aszlyt(4,sr).GT.100.0))         &
  & WRITE (*,*) 'day ',day,' aszlyt(4) ',aszlyt(4,sr)
!
DO idx = 5,mnsz + 1
  IF (nslay(sr).GE.idx.AND.(aszlyt(idx,sr).LT.1.0.OR.aszlyt(idx,sr).GT.1000.0)) &
    & WRITE (*,*) 'day ',day,' aszlyt(',idx,') ',aszlyt(idx,sr)
END DO
!
! s1phys
!
IF (dmpflg) WRITE (*,*) 's1phys'
!
DO idx = 0,mnsz
  IF (asdblk(idx,sr).LT.0.50.OR.asdblk(idx,sr).GT.2.5) WRITE (*,*) 'day ',day,  &
     &' asdblk(',idx,') ',asdblk(idx,sr)
END DO
!
! s1dbh
!
IF (dmpflg) WRITE (*,*) 's1dbh'
!
DO idx = 0,mnsz
  IF (asfsan(idx,sr).LT.0.0.OR.asfsan(idx,sr).GT.1.0) WRITE (*,*) 'day ',day,   &
     &' asfsan(',idx,') ',asfsan(idx,sr)
!
  IF (asfsil(idx,sr).LT.0.0.OR.asfsil(idx,sr).GT.1.0) WRITE (*,*) 'day ',day,   &
     &' asfsil(',idx,') ',asfsil(idx,sr)
!
  IF (asfcla(idx,sr).LT.0.0.OR.asfcla(idx,sr).GT.1.0) WRITE (*,*) 'day ',day,   &
     &' asfcla(',idx,') ',asfcla(idx,sr)
!
  IF (asvroc(idx,sr).LT.0.0.OR.asvroc(idx,sr).GT.1.0) WRITE (*,*) 'day ',day,   &
     &' asvroc(',idx,') ',asvroc(idx,sr)
END DO
!
! s1agg
!
IF (dmpflg) WRITE (*,*) 's1agg'
!
DO idx = 0,mnsz
  IF (asdagd(idx,sr).LT.0.6.OR.asdagd(idx,sr).GT.2.5) WRITE (*,*) 'day ',day,   &
     &' asdagd(',idx,') ',asdagd(idx,sr)
!
  IF (aseags(idx,sr).LT.0.1.OR.aseags(idx,sr).GT.7.0) WRITE (*,*) 'day ',day,   &
     &' aseags(',idx,') ',aseags(idx,sr)
!
  IF (aslagm(idx,sr).LT.0.03.OR.aslagm(idx,sr).GT.30.0) WRITE (*,*) 'day ',day, &
     &' aslagm(',idx,') ',aslagm(idx,sr)
!
  IF (aslagn(idx,sr).LT.0.001.OR.aslagn(idx,sr).GT.5.0) WRITE (*,*) 'day ',day, &
     &' aslagn(',idx,') ',aslagn(idx,sr)
!
  IF (aslagx(idx,sr).LT.1.0.OR.aslagx(idx,sr).GT.1000.0) WRITE (*,*) 'day ',day,&
     &' aslagx(',idx,') ',aslagx(idx,sr)
!
  IF (as0ags(idx,sr).LT.1.0.OR.as0ags(idx,sr).GT.20.0) WRITE (*,*) 'day ',day,  &
     &' as0ags(',idx,') ',as0ags(idx,sr)
END DO
!
! s1dbc
!
IF (dmpflg) WRITE (*,*) 's1dbc'
!
IF (asfom(0,sr).LT.tstmin.OR.asfom(0,sr).GT.tstmax) WRITE (*,*) 'day ',day,     &
   &' asfom(0) ',asfom(0,sr)
!
DO idx = 1,mnsz
  IF (as0ph(idx,sr).LT.0.0.OR.as0ph(idx,sr).GT.14.0) WRITE (*,*) 'day ',day,    &
     &' as0ph(',idx,') ',as0ph(idx,sr)
!
  IF (ascmg(idx,sr).LT.0.0.OR.ascmg(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,  &
     &' ascmg(',idx,') ',ascmg(idx,sr)
!
  IF (ascna(idx,sr).LT.0.0.OR.ascna(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,  &
     &' ascna(',idx,') ',ascna(idx,sr)
!
  IF (asfcce(idx,sr).LT.0.0.OR.asfcce(idx,sr).GT.100.0) WRITE (*,*) 'day ',day, &
     &' asfcce(',idx,') ',asfcce(idx,sr)
!
  IF (asfcec(idx,sr).LT.0.0.OR.asfcec(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' asfcec(',idx,') ',asfcec(idx,sr)
!
  IF (asfesp(idx,sr).LT.0.0.OR.asfesp(idx,sr).GT.100.0) WRITE (*,*) 'day ',day, &
     &' asfesp(',idx,') ',asfesp(idx,sr)
!
  IF (asfom(idx,sr).LT.0.0.OR.asfom(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,  &
     &' asfom(',idx,') ',asfom(idx,sr)
!
  IF (asfnoh(idx,sr).LT.0.0.OR.asfnoh(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' asfnoh(',idx,') ',asfnoh(idx,sr)
!
  IF (asfpoh(idx,sr).LT.0.0.OR.asfpoh(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' asfpoh(',idx,') ',asfpoh(idx,sr)
!
  IF (asfpsp(idx,sr).LT.0.0.OR.asfpsp(idx,sr).GT.1.0) WRITE (*,*) 'day ',day,   &
     &' asfpsp(',idx,') ',asfpsp(idx,sr)
!
  IF (asfsmb(idx,sr).LT.0.0.OR.asfsmb(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' asfsmb(',idx,') ',asfsmb(idx,sr)
!
  IF (asftap(idx,sr).LT.0.0.OR.asftap(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' asftap(',idx,') ',asftap(idx,sr)
!
  IF (asftan(idx,sr).LT.0.0.OR.asftan(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' asftan(',idx,') ',asftan(idx,sr)
END DO
!
IF (asmno3(sr).LT.0.0.OR.asmno3(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' asmno3 ',asmno3(sr)
!
! m1sim
!
IF (dmpflg) WRITE (*,*) 'm1sim'
!
IF (ntstep.LT.1.OR.ntstep.GT.96) WRITE (*,*) 'day ',day,' ntstep ',ntstep
!
IF (am0jd.NE.day) WRITE (*,*) 'day ',day,' am0jd ',am0jd
!
IF (amalat.LT.15.0.OR.amalat.GT.75.0) WRITE (*,*) 'day ',day,' amalat ',amalat
!
IF (amalon.LT.70.0.OR.amalon.GT.170.0) WRITE (*,*) 'day ',day,' amalon ',amalon
!
IF (amzele.LT.0.0.OR.amzele.GT.2500.0) WRITE (*,*) 'day ',day,' amzele ',amzele
!
! m1subr
!
IF (dmpflg) WRITE (*,*) 'm1subr'
!
IF (nsubr.LT.1.OR.nsubr.GT.4) WRITE (*,*) 'day ',day,' nsubr ',nsubr
!
IF (am0csr.LT.1.OR.am0csr.GT.4) WRITE (*,*) 'day ',day,' am0csr ',am0csr
!
IF (amnryr(sr).LT.1.OR.amnryr(sr).GT.10) WRITE (*,*) 'day ',day,' amnryr ',     &
  & amnryr(sr)
!
IF (amrslp(sr).LT.0.0.OR.amrslp(sr).GT.1.0) WRITE (*,*) 'day ',day,' amrslp ',  &
  & amrslp(sr)
!
! h1temp
!
IF (dmpflg) WRITE (*,*) 'h1temp'
!
DO idx = 1,mnsz
  IF (ahtsav(idx,sr).LT.-20.0.OR.ahtsav(idx,sr).GT.50.0) WRITE (*,*) 'day ',day,&
     &' ahtsav(',idx,') ',ahtsav(idx,sr)
!
  IF (ahtsmx(idx,sr).LT.-20.0.OR.ahtsmx(idx,sr).GT.50.0) WRITE (*,*) 'day ',day,&
     &' ahtsmx(',idx,') ',ahtsmx(idx,sr)
!
  IF (ahtsmn(idx,sr).LT.-20.0.OR.ahtsmn(idx,sr).GT.50.0) WRITE (*,*) 'day ',day,&
     &' ahtsmn(',idx,') ',ahtsmn(idx,sr)
END DO
!
! h1et
!
! ***      if (ahzea.lt.0.0.or.ahzea.gt.50.0)
! ***     *  write(*,*) 'day ',day,' ahzea ', ahzea
!
! ***      if (ahzep.lt.0.0.or.ahzep.gt.50.0)
! ***     *  write(*,*) 'day ',day,' ahzep ', ahzep
!
! ***      if (ahzeta.lt.0.0.or.ahzeta.gt.50.0)
! ***     *  write(*,*) 'day ',day,' ahzeta ', ahzeta
!
! ***      if (ahzetp.lt.0.0.or.ahzetp.gt.50.0)
! ***     *  write(*,*) 'day ',day,' ahzetp ', ahzetp
!
! ***      if (ahzpta.lt.0.0.or.ahzpta.gt.50.0)
! ***     *  write(*,*) 'day ',day,' ahzpta ', ahzpta
!
! ***      if (ahzptp.lt.0.0.or.ahzptp.gt.50.0)
! ***     *  write(*,*) 'day ',day,' ahzptp ', ahzptp
!
! ***      if (ah0drat.lt.0.0.or.ah0drat.gt.1.0)
! ***     *  write(*,*) 'day ',day,' ah0drat ', ah0drat
!
! h1hydro
!
IF (dmpflg) WRITE (*,*) 'h1hydro'
!
DO idx = 1,mnsz
  IF (ahrwc(idx,sr).LT.0.011.OR.ahrwc(idx,sr).GT.0.379) WRITE (*,*) 'day ',day, &
     &' ahrwc(',idx,') ',ahrwc(idx,sr)
!
  IF (aheaep(idx,sr).LT.-17.91.OR.aheaep(idx,sr).GT.0.0) WRITE (*,*) 'day ',day,&
     &' aheaep(',idx,') ',aheaep(idx,sr)
!
  IF (ahrsk(idx,sr).LT.0.0.OR.ahrsk(idx,sr).GT.0.001) WRITE (*,*) 'day ',day,   &
     &' ahrsk(',idx,') ',ahrsk(idx,sr)
!
  IF (ah0cb(idx,sr).LT.0.917.OR.ah0cb(idx,sr).GT.27.927) WRITE (*,*) 'day ',day,&
     &' ah0cb(',idx,') ',ah0cb(idx,sr)
END DO
!
IF (ahfwsf(sr).LT.tstmin.OR.ahfwsf(sr).GT.tstmax) WRITE (*,*) 'day ',day,       &
   &' ahfwsf ',ahfwsf(sr)
!
IF (ahzsno(sr).LT.0.0.OR.ahzsno(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ahzsno ',ahzsno(sr)
!
IF (ahzirr(sr).LT.0.0.OR.ahzirr(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ahzirr ',ahzirr(sr)
!
IF (ahzper(sr).LT.0.0.OR.ahzper(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ahzper ',ahzper(sr)
!
IF (ahzrun(sr).LT.0.0.OR.ahzrun(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ahzrun ',ahzrun(sr)
!
IF (ahzsmt(sr).LT.0.0.OR.ahzsmt(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ahzsmt ',ahzsmt(sr)
!
! h1scs
!
IF (dmpflg) WRITE (*,*) 'h1scs'
!
IF (ah0cng(sr).LT.6.0.OR.ah0cng(sr).GT.91.0) WRITE (*,*) 'day ',day,' ah0cng ', &
  & ah0cng(sr)
!
IF (ah0cnp(sr).LT.45.0.OR.ah0cnp(sr).GT.94.0) WRITE (*,*) 'day ',day,' ah0cnp ',&
  & ah0cnp(sr)
!
! h1db1
!
IF (dmpflg) WRITE (*,*) 'h1db1'
!
DO idx = 1,mnsz
  IF (ahrwcw(idx,sr).LT.0.005.OR.ahrwcw(idx,sr).GT.0.242) WRITE (*,*) 'day ',   &
    & day,' ahrwcw(',idx,') ',ahrwcw(idx,sr)
!
  IF (ahrwcf(idx,sr).LT.0.012.OR.ahrwcf(idx,sr).GT.0.335) WRITE (*,*) 'day ',   &
    & day,' ahrwcf(',idx,') ',ahrwcf(idx,sr)
!
  IF (ahrwcs(idx,sr).LT.0.208.OR.ahrwcs(idx,sr).GT.0.440) WRITE (*,*) 'day ',   &
    & day,' ahrwcs(',idx,') ',ahrwcs(idx,sr)
!
  IF (ahrwca(idx,sr).LT.0.0.OR.ahrwca(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' ahrwca(',idx,') ',ahrwca(idx,sr)
END DO
!
DO idx = 1,mnhhrs
  IF (ahrwc0(idx,sr).LT.0.0.OR.ahrwc0(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' ahrwc0(',idx,') ',ahrwc0(idx,sr)
END DO
!
IF (ahzsnd(sr).LT.0.0.OR.ahzsnd(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ahzsnd ',ahzsnd(sr)
!
! p1werm
!
DO idx = 1,mnbpls
  IF (admf(idx,sr).LT.0.0.OR.admf(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,    &
     &' admf(',idx,')',admf(idx,sr)
END DO
!
! not used anymore
!      do 150 idx=0,mncz
!      if (adma(idx, sr).lt.0.0.or.adma(idx, sr).gt.tstmax)
!     &  write(*,*) 'day ',day,' adma(',idx,') ', adma(idx, sr)
!  150 continue
!
!      if (admbt(sr).lt.0.0.or.admbt(sr).gt.tstmax)
!     &  write(*,*) 'day ',day,' admbt ', admbt(sr)
!
DO idx = 1,mnsz
  DO jdx = 1,mnbpls
     IF (admbgz(idx,jdx,sr).LT.0.0.OR.admbgz(idx,jdx,sr).GT.tstmax) WRITE (*,*) &
        & 'day ',day,' admbgz(',idx,jdx,') ',admbgz(idx,jdx,sr)
!
     IF (admrtz(idx,jdx,sr).LT.0.0.OR.admrtz(idx,jdx,sr).GT.tstmax) WRITE (*,*) &
        & 'day ',day,' admrtz(',idx,jdx,') ',admrtz(idx,jdx,sr)
  END DO
END DO
!
! d1glob
!
IF (dmpflg) WRITE (*,*) 'd1glob'
!
DO idx = 1,mnbpls
  IF (adzht(idx,sr).LT.tstmin.OR.adzht(idx,sr).GT.tstmax) WRITE (*,*) 'day ',   &
    & day,' adzht(',idx,') ',adzht(idx,sr)
 
  IF (adm(idx,sr).LT.0.0.OR.adm(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,      &
     &' adm(',idx,') ',adm(idx,sr)
 
  IF (admst(idx,sr).LT.0.0.OR.admst(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,  &
     &' admst(',idx,') ',admst(idx,sr)
 
  IF (admf(idx,sr).LT.0.0.OR.admf(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,    &
     &' admf(',idx,') ',admf(idx,sr)
 
  IF (admbg(idx,sr).LT.0.0.OR.admbg(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,  &
     &' admbg(',idx,') ',admbg(idx,sr)
 
  IF (admrt(idx,sr).LT.0.0.OR.admrt(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,  &
     &' admrt(',idx,') ',admrt(idx,sr)
 
  IF (addstm(idx,sr).LT.0.0.OR.addstm(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' addstm(',idx,') ',addstm(idx,sr)
END DO
 
! c1db3
!
IF (dmpflg) WRITE (*,*) 'c1db3'
!
IF (ac0bn1(sr).LT.0.0.OR.ac0bn1(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0bn1 ',ac0bn1(sr)
!
IF (ac0bn2(sr).LT.0.0.OR.ac0bn2(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0bn2 ',ac0bn2(sr)
!
IF (ac0bn3(sr).LT.0.0.OR.ac0bn3(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0bn3 ',ac0bn3(sr)
!
IF (ac0bp1(sr).LT.0.0.OR.ac0bp1(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0bp1 ',ac0bp1(sr)
!
IF (ac0bp2(sr).LT.0.0.OR.ac0bp2(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0bp2 ',ac0bp2(sr)
!
IF (ac0bp3(sr).LT.0.0.OR.ac0bp3(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0bp3 ',ac0bp3(sr)
!
IF (acfny(sr).LT.0.0.OR.acfny(sr).GT.1.0) WRITE (*,*) 'day ',day,' acfny ',     &
  & acfny(sr)
!
IF (acfpy(sr).LT.0.0.OR.acfpy(sr).GT.1.0) WRITE (*,*) 'day ',day,' acfpy ',     &
  & acfpy(sr)
!
IF (acfwy(sr).LT.0.0.OR.acfwy(sr).GT.1.0) WRITE (*,*) 'day ',day,' acfwy ',     &
  & acfwy(sr)
!
! c1gen
!
IF (dmpflg) WRITE (*,*) 'c1gen'
!
IF (ac0rg(sr).LT.0.OR.ac0rg(sr).GT.1) WRITE (*,*) 'day ',day,' ac0rg ',ac0rg(sr)
!
IF (acdpop(sr).LT.0.0.OR.acdpop(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' acdpop ',acdpop(sr)
!
IF (acxrow(sr).LT.0.0.OR.acxrow(sr).GT.1.0) WRITE (*,*) 'day ',day,' acxrow ',  &
  & acxrow(sr)
!
! c1geom
!
IF (dmpflg) WRITE (*,*) 'c1geom'
!
IF (acrsai(sr).LT.0.0.OR.acrsai(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' acrsai ',acrsai(sr)
!
IF (acrlai(sr).LT.0.0.OR.acrlai(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' acrlai ',acrlai(sr)
!
DO idx = 1,mncz
  IF (acrsaz(idx,sr).LT.0.0.OR.acrsaz(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' acrsaz(',idx,') ',acrsaz(idx,sr)
!
  IF (acrlaz(idx,sr).LT.0.0.OR.acrlaz(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' acrlaz(',idx,') ',acrlaz(idx,sr)
END DO
!
IF (acffcv(sr).LT.0.0.OR.acffcv(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' acffcv ',acffcv(sr)
!
IF (acfscv(sr).LT.0.0.OR.acfscv(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' acfscv ',acfscv(sr)
!
IF (acftcv(sr).LT.0.0.OR.acftcv(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' acftcv ',acftcv(sr)
!
! c1glob
!
IF (dmpflg) WRITE (*,*) 'c1glob'
!
IF (aczht(sr).LT.0.0.OR.aczht(sr).GT.3.0) WRITE (*,*) 'day ',day,' aczht ',     &
  & aczht(sr)
!
IF (aczrtd(sr).LT.0.0.OR.aczrtd(sr).GT.3.0) WRITE (*,*) 'day ',day,' aczrtd ',  &
  & aczrtd(sr)
!
IF (acm(sr).LT.0.0.OR.acm(sr).GT.tstmax) WRITE (*,*) 'day ',day,' acm ',acm(sr)
!
IF (acmst(sr).LT.0.0.OR.acmst(sr).GT.tstmax) WRITE (*,*) 'day ',day,' acmst ',  &
  & acmst(sr)
!
IF (acmrt(sr).LT.0.0.OR.acmrt(sr).GT.tstmax) WRITE (*,*) 'day ',day,' acmrt ',  &
  & acmrt(sr)
!
DO idx = 1,nslay(sr)
  IF (acmrtz(idx,sr).LT.0.0.OR.acmrtz(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' acmrtz ',acmrtz(idx,sr)
END DO
!
IF (acrsai(sr).LT.0.0.OR.acrsai(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' acrsai ',acrsai(sr)
!
IF (acrlai(sr).LT.0.0.OR.acrlai(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' acrlai ',acrlai(sr)
!
DO idx = 1,mncz
  IF (acrsaz(idx,sr).LT.0.0.OR.acrsaz(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' acrsaz ',acrsaz(idx,sr)
  IF (acrlaz(idx,sr).LT.0.0.OR.acrlaz(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' acrlaz ',acrlaz(idx,sr)
END DO
!
IF (acffcv(sr).LT.0.0.OR.acffcv(sr).GT.1.0) WRITE (*,*) 'day ',day,' acffcv ',  &
  & acffcv(sr)
!
IF (acfscv(sr).LT.0.0.OR.acfscv(sr).GT.1.0) WRITE (*,*) 'day ',day,' acfscv ',  &
  & acfscv(sr)
!
IF (acftcv(sr).LT.0.0.OR.acftcv(sr).GT.1.0) WRITE (*,*) 'day ',day,' acffcv ',  &
  & acftcv(sr)
!
IF (acdstm(sr).LT.0.0.OR.acdstm(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' acdstm ',acdstm(sr)
 
! cldb2
!
IF (dmpflg) WRITE (*,*) 'c1db2'
!
IF (actopt(sr).LT.0.0.OR.actopt(sr).GT.40.0) WRITE (*,*) 'day ',day,' actopt ', &
  & actopt(sr)
!
IF (actmin(sr).LT.0.0.OR.actmin(sr).GT.20.0) WRITE (*,*) 'day ',day,' actmin ', &
  & actmin(sr)
!
IF (acfdla(sr).LT.0.0.OR.acfdla(sr).GT.1.0) WRITE (*,*) 'day ',day,' acfdla ',  &
  & acfdla(sr)
!
IF (acrdla(sr).LT.0.0.OR.acrdla(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' acrdla ',acrdla(sr)
!
IF (ac0caf(sr).LT.0.0.OR.ac0caf(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0caf ',ac0caf(sr)
!
IF (ac0psf(sr).LT.0.0.OR.ac0psf(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0psf ',ac0psf(sr)
!
DO idx = 1,2
  IF (ac0pt1(idx,sr).LT.0.0.OR.ac0pt1(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' ac0pt1(',idx,') ',ac0pt1(idx,sr)
!
  IF (ac0pt2(idx,sr).LT.0.0.OR.ac0pt2(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' ac0pt2(',idx,') ',ac0pt2(idx,sr)
!
  IF (ac0fd1(idx,sr).LT.0.0.OR.ac0fd1(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' ac0fd1(',idx,') ',ac0fd1(idx,sr)
!
  IF (ac0fd2(idx,sr).LT.0.0.OR.ac0fd2(idx,sr).GT.1.0) WRITE (*,*) 'day ',day,   &
     &' ac0fd2(',idx,') ',ac0fd2(idx,sr)
END DO
!
IF (ac0ck(sr).LT.0.0.OR.ac0ck(sr).GT.1.0) WRITE (*,*) 'day ',day,' ac0ck ',     &
  & ac0ck(sr)
!
! c1mass
!
! ***      do 210 idx=1,mnsz
! ***      if (acmbgr(idx, sr).lt.tstmin.or.acmbgr(idx, sr).gt.tstmax)
! ***     &  write(*,*) 'day ',day,' acmbgr ', acmbgr(idx, sr)
! ***  210 continue
!
! c1db1
!
IF (dmpflg) WRITE (*,*) 'c1db1'
!
IF (acrcn(sr).LT.0.0.OR.acrcn(sr).GT.tstmax) WRITE (*,*) 'day ',day,' acrcn ',  &
  & acrcn(sr)
!
IF (actdtm(sr).LT.0.0.OR.actdtm(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' actdtm ',actdtm(sr)
!
IF (aczmrt(sr).LT.0.0.OR.aczmrt(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' aczmrt ',aczmrt(sr)
!
IF (aczmxc(sr).LT.0.0.OR.aczmxc(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' aczmxc ',aczmxc(sr)
!
IF (acrbe(sr).LT.0.0.OR.acrbe(sr).GT.tstmax) WRITE (*,*) 'day ',day,' acrbe ',  &
  & acrbe(sr)
!
IF (acrbed(sr).LT.0.0.OR.acrbed(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' acrbed ',acrbed(sr)
!
DO idx = 1,mncz
  IF (ac0lad(idx,sr).LT.0.0.OR.ac0lad(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' ac0lad(',idx,') ',ac0lad(idx,sr)
!
  IF (ac0sad(idx,sr).LT.0.0.OR.ac0sad(idx,sr).GT.tstmax) WRITE (*,*) 'day ',day,&
     &' ac0sad(',idx,') ',ac0sad(idx,sr)
END DO
!
IF (acehu0(sr).LT.0.0.OR.acehu0(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' acehu0 ',acehu0(sr)
!
IF (ac0alf(sr).LT.0.0.OR.ac0alf(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0alf ',ac0alf(sr)
!
IF (ac0blf(sr).LT.0.0.OR.ac0blf(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0blf ',ac0blf(sr)
!
IF (ac0clf(sr).LT.0.0.OR.ac0clf(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0clf ',ac0clf(sr)
!
IF (ac0dlf(sr).LT.0.0.OR.ac0dlf(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0dlf ',ac0dlf(sr)
!
IF (ac0arp(sr).LT.0.0.OR.ac0arp(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0arp ',ac0arp(sr)
!
IF (ac0brp(sr).LT.0.0.OR.ac0brp(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0brp ',ac0brp(sr)
!
IF (ac0crp(sr).LT.0.0.OR.ac0crp(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0crp ',ac0crp(sr)
!
IF (ac0drp(sr).LT.0.0.OR.ac0drp(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0drp ',ac0drp(sr)
!
IF (ac0aht(sr).LT.0.0.OR.ac0aht(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0aht ',ac0aht(sr)
!
IF (ac0bht(sr).LT.0.0.OR.ac0bht(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0bht ',ac0bht(sr)
!
IF (ac0ssa(sr).LT.0.0.OR.ac0ssa(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0ssa ',ac0ssa(sr)
!
IF (ac0ssb(sr).LT.0.0.OR.ac0ssb(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0ssb ',ac0ssb(sr)
!
IF (ac0sla(sr).LT.0.0.OR.ac0sla(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0sla ',ac0sla(sr)
!
IF (ac0hue(sr).LT.0.0.OR.ac0hue(sr).GT.tstmax) WRITE (*,*) 'day ',day,          &
  & ' ac0hue ',ac0hue(sr)
!
IF (dmpflg) WRITE (*,*) 'end dbgdmp'
!
END SUBROUTINE dbgdmp

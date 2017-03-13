SUBROUTINE nuse(bn1,bn2,bn3,bp1,bp2,bp4)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
INCLUDE 'cgrow.inc'
INCLUDE 'csoil.inc'
INCLUDE 'cfert.inc'
INCLUDE 'cparm.inc'
INCLUDE 'cenvr.inc'
INCLUDE 'chumus.inc'
!
! Subroutine arguments
!
REAL :: bn1,bn2,bn3,bp1,bp2,bp4
!
! Local variables
!
REAL :: clp,flu,vt,xx,xy
INTEGER :: j,k
!
!     + + + purpose + + +
!     this subroutine calculates the daily soil supply of n & p from each
!     soil layer in which there are roots.
 
!     + + + argument declarations + + +
 
!     + + + common blocks + + +
 
 
! local includes
!     include 'p1crop.inc'
 
!     + + + local variables + + +
!
!     + + + local variable definitions + + +
!     note:variable names in brackets are the names used in the epic manual
!     cnt(cnb) - optimal plant n concentration(kg/t) on day i
!     bn1,bn2,... - crop parameters for plant n concentration equation
!     hui         - heat unit index(0-1) on day i
!     un2(undef.) - optimal crop n concentration (kg/ha) on day i
!     un1(undef.) - sum of soil supplied plus fixed n (kg/ha) on day i ??
!     uno3(und)   - n demand rate for crop (kg/ha/d)
!     dm(b)       - accumulated plant biomass(tops+roots) (t/ha)
!     ddm(undef.) - daily plant biomass accumulation (t/ha/d)
!     cpt(cpb)    - optimal plant p concentration(kg/t) on day i
!     bp          - crop parameters for plant p concentration equation
!     hui         - heat unit index(0-1) on day i
!     up2(undef.) - optimal crop p concentration (kg/ha) on day i
!     up1(undef.) - actual crop p concentration  (kg/ha) on day i
!     upp(upd)    - p demand rate for crop (kg/ha/d)
!     dm(b)       - accumulated plant biomass(tops+roots) (t/ha)
!     upp(upd)  = p demand rate (kg/ha/d)
!     rw(rwt)   = total root weight upto day i (t/ha)
!     ir        = deepest layer number to which roots have extended
!     un        = rate of n supplied by the soil from layer j (kg/ha/d)
!     wno3      = amount of n in a layer (kg/ha)
!     u         = water uptake from a layer by evpt (mm)
!     st(sw)    = soil water content of a layer (mm)
!     sunn(uns)  = sum of n supplied from all rooted layers (kg/ha)
!     clp(clp)  = p concentration in a layer(g/ton)
!     flu(lfu)  = labile p factor for crop uptake(0-1)
!     up        = soil supply of p from a layer (kg/ha)
!     sup(ups)  = soil supply of p from all rooted layers (kg/ha)
!     ap        = amount of labile p in a layer (kg/ha)
!     xy        =
 
!     + + + output formats + + +
!2000 format(1x,i3,1x,9(f7.3,1x))
 
!     + + + end of specifications + + +
!     this section is the epic subroutine nup
!      dimension fr(10)
un1 = un2
!      sunn=0.
sup = 0.
!     calculate optimal n concentration for a crop using a modified version of
!     eq. 2.215 in the next 2 lines.
cnt = bn1 + bn2*exp(-bn3*hui)
un2 = cnt*dm*1000.
!      if (un2.lt.un1) un2=un1
!     allow positive n demand late in the season ?
!      uno3=amin1(4000.*.0023*ddm,un2-un1)
uno3 = un2 - un1
IF (uno3.LE.0.) uno3 = 0.
vt = uno3
!     this section is the epic subroutine npup
!     calculate p concentration for a crop using a modified form of eq.2.229.
cpt = bp2 + bp1*exp(-bp4*hui)
up2 = cpt*dm*1000.
IF (up2.LT.up1) up2 = up1
upp = up2 - up1
!
!     this section is the epic subroutine nuse
!     calculate parts of eq. 2.231 --- p demand rate
IF (rw.NE.0.) THEN
  xx = 1.5*upp/rw
!     loop for computing soil supply of n and p from each layer
  DO j = 1,ir
!        compute soil supply of n in the next 2 lines
!        next line commented out unitl water use data is available
!        un(j)=wno3(j)*u(j)/(st(j)+.001)
!         un(j)=wno3(j)*0.05
!         sunn=sunn+un(j)
!        above 3 lines replaced by the following 5 lines
!         un(j)=uno3*rwt(j)/rw
     IF (vt.GT.0.) THEN
        IF (wno3(j).NE.0.) THEN
           xy = wno3(j) - vt
           IF (xy.GE.0.) THEN
              wno3(j) = wno3(j) - vt
              sunn = sunn + vt
              vt = 0.
           END IF
           IF (xy.LT.0.) THEN
              sunn = sunn + wno3(j)
              vt = vt - wno3(j)
              wno3(j) = 0.
           END IF
        END IF
     END IF
!         xy=wno3(j)-un(j)
!         if (xy.le.0.) un(j)=wno3(j)
!         sunn=sunn+un(j)
!         wno3(j)=wno3(j)-un(j)
!        compute soil supply of p in the next 8 lines
!        f in the next line replaced by clp
     clp = 1000.*ap(j)/wt(j)
!        f in the next and subsequent lines replaced by flu --- eq 2.232
!        *********** re-arranged to avoid the goto statement **************
     flu = clp/(clp+exp(a_s11-b_s11*clp))
     IF (clp.GT.30.) flu = 1.
!!       use equation 2.231
!         rwt(j) = ???  if this is root mass by layer - change to bcmbgr(j)
!         bcmbgr should also be passed as an argument
     up(j) = xx*flu*rwt(j)
     IF (up(j).GE.ap(j)) up(j) = .9*ap(j)
     sup = sup + up(j)
  END DO
!     the following algorithms may be temporary depending whether subroutine
!     najn & najp are eliminated.
!     adjust soil supply and plant demand for n
!      sum=0.
!      rt=uno3/(sunn+1.e-20)
!      if (rt.lt.1.) then
!         do 2 k=1,ir
!            un(k)=un(k)*rt
!            sum=sum+un(k)
!   2     continue
!        sunn=sum
!      endif
!c     adjust soil supply and plant demand for p
!      sum=0.
!      rt=upp/(sup+1.e-20)
!      if (rt.lt.1.) then
!         do 3 k=1,ir
!            up(k)=up(k)*rt
!            sum=sum+up(k)
!   3     continue
!        sup=sum
!      endif
!     acummulate plant uptake of n and p
!      un1=un1+sunn
  up1 = up1 + sup
!     a new variable un3 added for debugging purposes
  suno3 = suno3 + uno3
!     update remaining no3 and labile p in the soil and ouput for debugging
  DO k = 1,ir
!         wno3(k)=wno3(k)-un(k)
     ap(k) = ap(k) - un(k)
!        write(310,2133) jd,k,ap(k),rwt(k),wno3(k),un(k),up(k)
!2133 format (i3,1x,i3,1x,5(f8.4,1x))
  END DO
  tno3 = tno3 - sunn + rmnr
  tap = tap - sup + wmp
  j = j - 1
END IF
!     write(39,2000)jd,cnt,un1,un2,uno3,suno3,sunn,vt,dm,ddm
!
END SUBROUTINE nuse

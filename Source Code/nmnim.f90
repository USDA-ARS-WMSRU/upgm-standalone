subroutine nmnim(k)
!
implicit none
!
include 'p1werm.inc'
include 'chumus.inc'
include 'cfert.inc'
include 'csoil.inc'
!
! Dummy arguments
!
integer :: k
!
! Local variables
!
real :: amin1
real :: ca,cnr,cnrf,cpr,cprf,cs,decr,hmp,r4,rdc,rm2,rmn,rmp,rwn,tkg,xx
!
!     + + + purpose + + +
!     this subroutine estimates daily n and p mineralization and immobilization
!     considering fresh organic material (crop residue) and active and stable
!     humus material. goto statements in the original code were replaced by if-
!     then-else statements as required by wepp coding convention.
!
!     + + + keywords + + +
!     mineralization
!
!     + + + common blocks + + +
 
 
! local includes
!     include 'cenvr.inc'
 
!     + + + local variables + + +
 
!
!     + + + end of specifications + + +
!
!     convert residue to kg/ha.
cmn = 0.0003
tkg = rsd(k)*1000.
!     calculate parts of eq. 2.153
cs = sqrt(cdg*sut)
!     this section of code calculates amount of n&p mineralized from humus.
!     calculate n that becomes part of the stable n pool using eq. 2.159.
!     this eq. is not the same as in the manual--->(-1.0) added.
rwn = .1E-4*(wmn(k)*(1./rtn(k)-1.)-wn(k))
!     add rwn to the stable organic n pool (wn(k)).
wn(k) = wn(k) + rwn
wim = 0.
wip = 0.
!     calculate amount of n mineralized from the active n pool. this eq is
!     not the same as in the manual--->bd*bd left out.
!     next line replaced by the line following it
!     hmn=cmn*cs*wmn(k)/(bdp(k)*bdp(k))
hmn = cmn*cs*wmn(k)
!     calculate mineralized p (hmp) in the following 2 lines.
xx = wn(k) + wmn(k)
hmp = 1.4*hmn*wp(k)/xx
 
!     calculate n&p mineralization from humus when there is not enough residue
!      if (tkg.le.1.) then
!        calculate remaining amount of humus
!         hum(k)=hum(k)*(1.-hmn/xx)
!        subtract n flow to stable pool(rwn) and humus mineralized n(hmn) from
!        the active n pool (wmn)
!         wmn(k)=wmn(k)-hmn-rwn
!        subtract humus p from stable organic pool
!         wp(k)=wp(k)-hmp
!         rmnr=hmn
!        add humus n to the no3_n supply
!         wno3(k)=wno3(k)+rmnr
!        add humus p to the labile p supply
!         ap(k)=ap(k)+hmp
!         wmp=hmp
!      else
!        calculate n&p mineral./immobil. from fresh residue and humus material
!        this section of code calculates n & p mineralization from fresh om
!        numerator of eq 2.155
r4 = .58*tkg
!        calculate c:n ratio
cnr = r4/(fon(k)+wno3(k))
!        calculate c:p ratio
cpr = r4/(fop(k)+ap(k))
!        calculate cnp (c:n and c:p ratio factor)--eq 2.154
cnrf = 1.
if (cnr>25.) cnrf = exp(-.693*(cnr-25.)/25.)
cprf = 1.
if (cpr>200.) cprf = exp(-.693*(cpr-200.)/200.)
ca = amin1(cnrf,cprf)
!        calculate the decay rate constant using eq. 2.153
!        rc=0.05 is it constant ? when does it become 0.8 and 0.0095 ?
!        new code added to determine residue composition factor
!         if (rsdi(k).le.0.) rsdi(k)=1.
!         rfom=rsd(k)/rsdi(k)
!         if (rfom.ge.0.8) rc=0.8
!         if (rfom.lt.0.8) rc=0.05
!         if (rfom.lt.0.1) rc=0.0095
rc = 0.05
!        end of new code additions
decr = rc*ca*cs
!        calculate n mineralization rate using eq. 2.152
rmn = decr*fon(k)
!        calculate p mineralization rate using eq. 2.152
rmp = decr*fop(k)
!        calculate 20% of fresh om n
rm2 = .2*rmn
!        calculate amount of remaining humus
hum(k) = hum(k)*(1.+(rm2-hmn)/xx)
!        update amount of active humus n pool
wmn(k) = wmn(k) + rm2 - hmn - rwn
!        update amount of stable organic n pool
wp(k) = wp(k) - hmp + .2*rmp
!        calulate amount of decayed residue
rdc = decr*tkg
!        update amount of residue and convert to t/ha
rsd(k) = .001*(tkg-rdc)
!        calculate net minerlized n
rmnr = .8*rmn + hmn
!        calculate net mineralized p
wmp = .8*rmp + hmp
 
!        wim=amax1(.0232*rdc-rmn,0.)
!        wim=amin1(rmnr+wno3(k),wim)
!        wip=amax1(.0029*rdc-rmp,0.)
!        wip=amin1(wmp+ap(k),wip)
 
!        add immobilized p and subtract mineralized p to fresh organic p pool
fop(k) = fop(k) + wip - rmp
!        add immobilized n and subtract mineralized n to fresh organic n pool
fon(k) = fon(k) + wim - rmn
!        update total no3_n in soil layer
wno3(k) = wno3(k) - wim + rmnr
!        update total labile p in soil layer
ap(k) = ap(k) - wip + wmp
!        keep running totals of mineralized n & p from fresh residue(rmn*.8,
!        rmp*.8) and humus(hmn,hmp)
trmn = trmn + .8*rmn
trmp = trmp + .8*rmp
thmn = thmn + hmn
thmp = thmp + hmp
 
!
!     + + + local variable definitions + + +
!     ca (cnp) - takes the value of 1.0,cnrf or cprf whichever is the smallest
!     cmn (cmn) - humus rate constant(1/d) - 2.160
!     cnr (cnr) - c:n ratio in a soil layer -  2.155
!     cnrf (cnp) - c:n ratio factor -  2.154
!     cpr (cpr) - c:p ratio in a soil layer -  2.156
!     cprf (cnp) - c:p ratio factor -  2.154
!     cs - temp and soil moisture factor of - 2.153
!     decr (dcr) - decay rate constant for fresh organic matter -  2.153
!     hmp (hmp) - amount of p mineralized from humus - kg/ha/d
!     r4 - numerator of - 2.155
!     rc (rc) - residue decomposition factor (.8,.05,.0095)
!     rdc - amount of decayed residue from fresh residue - kg/ha
!     rm2 - 20% of n mineralized from fersh residue - kg/ha/day
!     rmn (rmn) - amount of n mineralized from fresh residue - kg/ha/d - 2.152
!     rmp - amount of p mineralized from fresh residue - kg/ha/d - 2.152
!     rwn (ron) - flow rate between active and stable humus n pools - kg/ha/d
!     tkg - residue - kg/ha
!     xx - sum of active and stable n pools
!
!
!     + + + output formats + + +
 1000 format (1x,2(i3,1x),11(f7.3,1x))
!      endif
 
!     write(37,2000)jd,k,wno3(k),ap(k),fon(k),rmn,rmp,hmn,hmp,trmn,trmp,
!    1thmn,thmp
!     write(312,2001)jd,k,cnr,cpr,cnrf,cprf,ca,decr,cs,cdg,sut
!     write(313,2002)jd,k,hum(k),wmn(k),wn(k),wp(k),rwn,rmnr,wmp
!2001 format (1x,2(i3,1x),9(f8.3,1x))
!2002 format (1x,2(i3,1x),7(f10.5,1x))
!
end subroutine nmnim

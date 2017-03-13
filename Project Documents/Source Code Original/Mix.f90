SUBROUTINE mix(u,tillf,nlay,density,laythk,sand,silt,clay,rock_vol,c_sand,      &
             & m_sand,f_sand,vf_sand,w_bd,organic,ph,calcarb,cation,lin_ext,    &
             & aggden,drystab,soilwatr,satwatr,thrdbar,ftnbar,avawatr,soilcb,   &
             & soilair,satcond,root,blwgnd,massf)
!
IMPLICIT NONE
!
INCLUDE 'p1werm.inc'
INCLUDE 'asd.inc'
!
! Subroutine arguments
!
INTEGER :: nlay
REAL :: tillf,u
REAL,DIMENSION(mnsz) :: aggden,avawatr,calcarb,cation,clay,c_sand,density,      &
                      & drystab,ftnbar,f_sand,laythk,lin_ext,m_sand,organic,ph, &
                      & rock_vol,sand,satcond,satwatr,silt,soilair,soilcb,      &
                      & soilwatr,thrdbar,vf_sand,w_bd
REAL,DIMENSION(mnsz,mnbpls) :: blwgnd,root
REAL,DIMENSION(msieve+1,mnsz) :: massf
!
! Local variables
!
REAL :: cmass,mass,tillmix
REAL,DIMENSION(mnsz) :: dum,dum1,dum2
INTEGER :: i,j
!
!     + + + purpose + + +
!
!     this subroutine reads in the array(s) containing the components
!     that need to be mixed.  it then calls the subroutine mixproc
!     and the actual mixing process is performed.
!
!     + + + keywords + + +
!     mixing
 
 
!     + + + argument declarations + + +
 
 
 
!     + + + argument definitions + + +
!     u           - mixing coefficient
!     tillf       - fraction of the soil area tilled by the machine
!     density     - soil density
!     laythk      - layer thickness
 
!     sand        - fraction of sand
!     silt        - fraction of silt
!     clay        - fraction of clay
!     rock_vol    - volume fraction of rock
!     c_sand      - fraction of course sand
!     m_sand      - fraction of medium sand
!     f_sand      - fraction of fine sand
!     vf_sand     - fraction of very fine sand
 
!     w_bd        - wet (1/3 bar) soil density
 
!     organic     - fraction of organic matter
!     ph          - soil ph
!     calcarb     - fraction of calcium carbonate
!     cation      - cation exchange capcity
 
!     lin_ext     - linear extensibility
 
!     aggden      - aggregrate density
!     drystab     - dry aggregrate stability
 
!     soilwatr    - soil water content (mass bases)
!     satwatr     - saturation soil water content
!     thrdbar     - 1/3 bar soil water content
!     ftnbar      - 15 bar soil water content
!     avawatr     - available soil water content
 
!     soilcbr     - soil cb value
!     soilair     - soil air entery potential
!     satcond     - saturated hydraulic conductivity
 
!     root        - root mass by layers
!     blwgnd      - below ground biomass
!     massf       - mass fractions for sieve cuts
 
!     nlay        - number of soil layers used
 
!     + + + accessed common block variable definitions + + +
!     mnsz        - max number of soil layers
!     mnbpls      - max number of decomposition pools
 
!     + + + parameters + + +
 
!     + + + local variables + + +
 
!     + + + local variable definitions + + +
!     tillmix - combination of mixing coefficient and tilled area fraction
!     cmass = total mass of a component contained in a subregion
!     dum = dummy variable used in calculating the mass in a subregion
!     dum1 = dummy variable used in calculating mass of a component
!            in a subregion
!     lay = number of layers in a specified subregion
!     mass = total mass in a subregion
 
!     + + + end specifications + + +
 
!     print the initial masses calculated above
 
!      print*,'initial data - before mixing'
!         do 230 i=1,5
!            print*, root(i,1)
!230      continue
 
!     find combination coefficient based on fraction of area and mixing
tillmix = u*tillf
!
!     calculate the total mass in all layers within a subregion
!
mass = 0.0
DO i = 1,nlay
  dum(i) = density(i)*laythk(i) + mass
  mass = dum(i)
END DO
 
!     make calls to the mixing process.  first need to calculate
!     the total mass of the component to be mixed.  this is then passed
!     in the call.
 
!     need to calculate the component mass before making the call
!     to mixproc for each and every component.  this is then passed to
!     mixproc and used in the mix calculation.
 
!************************soil variables********************
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*sand(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,sand,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*silt(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,silt,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*clay(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,clay,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*rock_vol(i) + cmass         ! note: we are mixing rock vol on mass not volume ratio
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,rock_vol,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*c_sand(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,c_sand,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*m_sand(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,m_sand,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*f_sand(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,f_sand,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*vf_sand(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,vf_sand,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*w_bd(i) + cmass            !note: mixed w_bd on mass basis (not entirely correct)
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,w_bd,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*organic(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,organic,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*ph(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,ph,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*calcarb(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,calcarb,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*cation(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,cation,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*lin_ext(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,lin_ext,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*aggden(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,aggden,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*drystab(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,drystab,cmass,mass)
 
!************************soil variables********************
!
!**********************hydrology variables********************
!
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*soilwatr(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,soilwatr,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*satwatr(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,satwatr,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*thrdbar(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,thrdbar,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*ftnbar(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,ftnbar,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*avawatr(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,avawatr,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*soilcb(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,soilcb,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*soilair(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,soilair,cmass,mass)
 
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*satcond(i) + cmass
  cmass = dum1(i)
END DO
CALL mixproc(tillmix,nlay,satcond,cmass,mass)
!************************hydrology variables********************
!
!
!********************decomposition variables********************
!   need to mix both pools and layers for these next two variables
DO j = 1,mnbpls
  cmass = 0.0
  DO i = 1,nlay
     dum1(i) = density(i)*laythk(i)*root(i,j) + cmass
     cmass = dum1(i)
     dum2(i) = root(i,j)
  END DO
  CALL mixproc(tillmix,nlay,dum2(1),cmass,mass)
  DO i = 1,nlay
     root(i,j) = dum2(i)
  END DO
END DO
!         do 231 i=1,5
!            print*, root(i,1)
!231      continue
 
DO j = 1,mnbpls
  cmass = 0.0
  DO i = 1,nlay
     dum1(i) = density(i)*laythk(i)*blwgnd(i,j) + cmass
     cmass = dum1(i)
     dum2(i) = blwgnd(i,j)
  END DO
  CALL mixproc(tillmix,nlay,dum2(1),cmass,mass)
  DO i = 1,nlay
     blwgnd(i,j) = dum2(i)
  END DO
END DO
!********************decomposition variables********************
!
!
!************************asd mass fractions********************
DO j = 1,msieve
  cmass = 0.0
  DO i = 1,nlay
     dum1(i) = density(i)*laythk(i)*massf(j,i) + cmass
     cmass = dum1(i)
     dum2(i) = massf(j,i)
  END DO
  CALL mixproc(tillmix,nlay,dum2(1),cmass,mass)
  DO i = 1,nlay
     massf(j,i) = dum2(i)
  END DO
END DO
!************************asd mass fractions********************
!
!
!****************************bulk density**********************
!     a weight based adjustment of bulk density results in a
!     change in layer thickness. this must be done last!!!
cmass = 0.0
DO i = 1,nlay
  dum1(i) = density(i)*laythk(i)*density(i) + cmass
  cmass = dum1(i)
  dum(i) = density(i)
END DO
CALL mixproc(tillmix,nlay,dum,cmass,mass)
DO i = 1,nlay
  laythk(i) = laythk(i)*(density(i)/dum(i))
  density(i) = dum(i)
END DO
! 
END SUBROUTINE mix

    submodule (nitrogen) nitrogen_nuse
    contains
        module procedure nuse
        use constants, only : mnsz
        use climate
        implicit none
        !
        !
        ! Local variables
        !
        real :: clp,flu,vt,xx,xy
        integer :: j,k
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
        ndat%un1 = ndat%un2
        !      sunn=0.
        ndat%sup = 0.
        !     calculate optimal n concentration for a crop using a modified version of
        !     eq. 2.215 in the next 2 lines.
        ndat%cnt = bn1 + bn2*exp(-bn3*clidat%hui)
        ndat%un2 = ndat%cnt*dm*1000.
        !      if (un2.lt.un1) un2=un1
        !     allow positive n demand late in the season ?
        !      uno3=amin1(4000.*.0023*ddm,un2-un1)
        ndat%uno3 = ndat%un2 - ndat%un1
        if (ndat%uno3<=0.) ndat%uno3 = 0.
        vt = ndat%uno3
        !     this section is the epic subroutine npup
        !     calculate p concentration for a crop using a modified form of eq.2.229.
        ndat%cpt = bp2 + bp1*exp(-bp4*clidat%hui)
        ndat%up2 = ndat%cpt*dm*1000.
        if (ndat%up2<ndat%up1) ndat%up2 = ndat%up1
        ndat%upp = ndat%up2 - ndat%up1
        !
        !     this section is the epic subroutine nuse
        !     calculate parts of eq. 2.231 --- p demand rate
        if (rw/=0.) then
            xx = 1.5*ndat%upp/rw
            !     loop for computing soil supply of n and p from each layer
            do j = 1,ir
                !        compute soil supply of n in the next 2 lines
                !        next line commented out unitl water use data is available
                !        un(j)=wno3(j)*u(j)/(st(j)+.001)
                !         un(j)=wno3(j)*0.05
                !         sunn=sunn+un(j)
                !        above 3 lines replaced by the following 5 lines
                !         un(j)=uno3*rwt(j)/rw
                if (vt>0.) then
                    if (ndat%wno3(j)/=0.) then
                        xy = ndat%wno3(j) - vt
                        if (xy>=0.) then
                            ndat%wno3(j) = ndat%wno3(j) - vt
                            ndat%sunn = ndat%sunn + vt
                            vt = 0.
                        end if
                        if (xy<0.) then
                            ndat%sunn = ndat%sunn + ndat%wno3(j)
                            vt = vt - ndat%wno3(j)
                            ndat%wno3(j) = 0.
                        end if
                    end if
                end if
                !         xy=wno3(j)-un(j)
                !         if (xy.le.0.) un(j)=wno3(j)
                !         sunn=sunn+un(j)
                !         wno3(j)=wno3(j)-un(j)
                !        compute soil supply of p in the next 8 lines
                !        f in the next line replaced by clp
                clp = 1000.*ndat%ap(j)/ndat%wt(j)
                !        f in the next and subsequent lines replaced by flu --- eq 2.232
                !        *********** re-arranged to avoid the goto statement **************
                flu = clp/(clp+exp(ndat%a_s11-ndat%b_s11*clp))
                if (clp>30.) flu = 1.
                !!       use equation 2.231
                !         rwt(j) = ???  if this is root mass by layer - change to bcmbgr(j)
                !         bcmbgr should also be passed as an argument
                ndat%up(j) = xx*flu*rwt(j)
                if (ndat%up(j)>=ndat%ap(j)) ndat%up(j) = .9*ndat%ap(j)
                ndat%sup = ndat%sup + ndat%up(j)
            end do
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
            ndat%up1 = ndat%up1 + ndat%sup
            !     a new variable un3 added for debugging purposes
            ndat%suno3 = ndat%suno3 + ndat%uno3
            !     update remaining no3 and labile p in the soil and ouput for debugging
            do k = 1,ir
                !         wno3(k)=wno3(k)-un(k)
                ndat%ap(k) = ndat%ap(k) - ndat%un(k)
                !        write(310,2133) jd,k,ap(k),rwt(k),wno3(k),un(k),up(k)
                !2133 format (i3,1x,i3,1x,5(f8.4,1x))
            end do
            ndat%tno3 = ndat%tno3 - ndat%sunn + ndat%rmnr
            ndat%tap = ndat%tap - ndat%sup + ndat%wmp
            j = j - 1
        end if
        !     write(39,2000)jd,cnt,un1,un2,uno3,suno3,sunn,vt,dm,ddm
        !
        end procedure 
    end submodule
    

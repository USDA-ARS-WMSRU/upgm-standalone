    submodule (nitrogen) nitrogen_nuts
    contains
        module procedure nuts
        !
        implicit none
        !
        ! Local variables
        !
        real :: yy
        !
        !     + + + purpose + + +
        !     this subroutine calculates a nutrient stress factor caused by limited
        !     supply of n or p.

        !     + + + keywords + + +
        !     nutrient stress

        !     + + + common blocks + + +


        !     + + + local variables + + +

        !     + + + local variable definitions + + +
        !     y1 - cummulative amount of n or p taken by the plant (kg/ha) - supply
        !     y2 - potential amount of n or p needed by the plant (kg/ha) - demand
        !     yy - scaled ratio of supply over demand
        !     uu - n or p stress factor
        !     yy replaces uu where appropriate to minimize confusion
        !     a_s8,b_s8 are used instead of scrp(8,1) and scrp(8,2)

        !     + + + end of specifications + + +

        if (y2/=0.) then
            yy = 200.*(y1/y2-.5)
            if (yy>0.) then
                uu = yy/(yy+exp(ndat%a_s8-ndat%b_s8*yy))
            else
                uu = 0.
            end if
        end if
        !
        end procedure nuts
    end submodule

!------------------------------------------------------------------------------
! 
!   author: Roger Marquez
!   
!   purpose: the nitrogen module. This module contains all the processes for calculating N in the soil.
!
!   use: include and call the main routine.
!
!------------------------------------------------------------------------------
    module nitrogen
    use climate
    implicit none
    type nitrogen_data
        real :: a_s11   !parameter in P uptake eqn. (corresponding to scrp(11,1) in epic)
        real :: b_s11   !parameter in P uptake eqn. (corresponding to scrp(11,2) in epic)
        real :: a_s8    !parameter in N or P stress eqn. (corresponds to scrp(8,1) in epic)
        real :: b_s8    !parameter in N or P stress eqn. (corresponds to scrp(8,2) in epic)
    end type nitrogen_data

  
    interface
    
        module subroutine nuse(clidat, ndat, bn1, bn2, bn3, bp1, bp2, bp4)
        ! Argument Defs
        type (nitrogen_data), intent(in) :: ndat
        type (climate_data), intent(in) :: clidat
        real, intent(in) :: bn1, bn2, bn3
        real, intent(in) :: bp1, bp2, bp4
        end subroutine nuse
    
        module subroutine nuts(ndat, y1, y2, uu)
        ! Argument Defs
        type (nitrogen_data), intent(in) :: ndat
        real, intent(in) :: y1, y2
        real, intent(out) :: uu
        end subroutine nuts
    
    end interface

    end module nitrogen
    
!-------------------------------------------------------------------------
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
        real :: a_s11
        real :: b_s11
        real :: a_s8
        real :: b_s8
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
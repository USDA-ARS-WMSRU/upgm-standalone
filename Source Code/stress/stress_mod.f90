!------------------------------------------------------------------------------
! 
!   author: Roger Marquez
!   
!   purpose: the stress module. This module contains stress for a particular crop
!
!   use: include and call the main routine.
!
!------------------------------------------------------------------------------
    module stress
    implicit none
    
    type crop_stress
        real :: ahfwsf                  !Crop growth water stress factor (unitless)
    end type crop_stress
        
    contains
    ! calculations here later for other stresses
    
    end module stress
    
!------------------------------------------------------------------------------  
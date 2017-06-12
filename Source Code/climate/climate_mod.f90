!------------------------------------------------------------------------------
! 
!   author: Roger Marquez
!   
!   purpose: module that contains climate data and associated functions
!
!   use: make appropriate calls to subroutines as necessary
!
!------------------------------------------------------------------------------
    module climate
    use constants, only : num_months, mndayr
    implicit none
    
    integer, parameter :: n_header = 16
    
    type climate_data
        integer :: wcd(mndayr)
        integer :: wcm(mndayr)
        integer :: wcy(mndayr)
        real :: awtmnav(12) !used for monthly min air temp
        real :: awtmxav(12) !used for monthly max air temp
        real :: awtdmn	! used in call crop
        real :: awtdmx ! used in call crop
        real :: awzdpt ! used in call crop
        real :: aweirr ! used in call crop
        real :: wwzdpt(mndayr) !used for emergence
        real :: wwtdmx(mndayr)
        real :: wwtdmn(mndayr)
        real :: wgrad(mndayr)
    end type climate_data
    
    
    type(climate_data) :: cli_data

    
    
    end module climate
    
!------------------------------------------------------------------------------  
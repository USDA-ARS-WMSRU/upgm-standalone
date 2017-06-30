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
        real :: awtmnav(num_months) !used for monthly min air temp
        real :: awtmxav(num_months) !used for monthly max air temp
        real :: awtdmn	! used in call crop
        real :: awtdmx ! used in call crop
        real :: awzdpt ! used in call crop
        real :: aweirr ! used in call crop
        real :: wwzdpt(mndayr) !used for emergence
        real :: wwtdmx(mndayr)
        real :: wwtdmn(mndayr)
        real :: wgrad(mndayr)
        
        real :: hrlt    !day length on day i (h)
        real :: hrlty   ! day length on day (i-1)
        real :: hui     !heat unit index (ratio of acthucum to acthum)
        real :: huiy    !heat unit index (ratio of acthucum to acthum) on day (i-1)
        real :: huirt   !heat unit index used to drive root growth (no delays)
        real :: huirty  !heat unit index used to drive root growth (no delays) on day (i-1)
        real :: ehu     !heat units required from planting seedling emergence
        real :: ts      !temperature stress factor
        real :: xlat    !latitude of a location (deg.)
        real :: phu     !potential heat units for crop maturity (deg. c)
        real :: co2atmos             ! the atmospheric level of CO2.
    end type climate_data
    
    end module climate
    
!------------------------------------------------------------------------------  
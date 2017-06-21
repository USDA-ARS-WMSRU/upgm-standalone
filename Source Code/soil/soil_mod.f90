!------------------------------------------------------------------------------
! 
!   author: Roger Marquez
!   
!   purpose: module that contains climate data and associated functions
!
!   use: make appropriate calls to subroutines as necessary
!
!------------------------------------------------------------------------------
    module soil
    use constants, only : num_months, mndayr
    implicit none

    !integer, parameter :: mnsz = 100

    type soil_phys_props
        real, dimension (100) :: asdblk !used
        real :: aszrgh(4) !used
        real :: asxrgs(4) !used
        real :: asargo(4) !used
        integer :: nslay ! Number of soil layers being used for each subregion.
        real :: aszlyt(100+1, 4) ! Number of soil layers being used for each subregion.
        real :: aszlyd(100, 4) ! Depth to bottom of each soil layer for each subregion (mm)
        real :: asfcla(0:100, 4) !used
    end type soil_phys_props


    type(soil_phys_props) :: spp_data



    end module soil
    
!------------------------------------------------------------------------------  
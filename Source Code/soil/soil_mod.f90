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
        real :: aszlyt(100+1) ! Number of soil layers being used for each subregion.
        real :: aszlyd(100) ! Depth to bottom of each soil layer for each subregion (mm)
        real :: asfcla(0:100) !used
        real :: ahtsmn(100) 
    end type soil_phys_props
    
    type soil_chem_props
        real :: as0ph(100) !used
        real :: asfcce(100) !used
        real :: asfcec(100) !used
        real :: asfom(0:100) !used
        real :: asfsmb(100) !used
        real :: asftap(100) !used
        real :: asftan(100) !used
        real :: asmno3      !used
            
    end type soil_chem_props
    

    type soildata
        type(soil_phys_props) :: spp
        type(soil_chem_props) :: scp
    end type soildata
    
    end module soil
    
!------------------------------------------------------------------------------  
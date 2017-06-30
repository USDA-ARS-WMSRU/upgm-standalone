    !------------------------------------------------------------------------------
    !
    !   author: Roger Marquez
    !
    !   purpose: module that contains simulation data/controls for UPGM
    !
    !   use: module should be declared in main initialization area, passed around
    !
    !------------------------------------------------------------------------------

    module upgm_simdata
    use stress, only : crop_stress
    use nitrogen
    implicit none

    ! simulation data
    type simulation
        integer :: plant_day    ! planting day
        integer :: plant_mon    ! planting month
        integer :: plant_year   ! planting year.  currently, not the calendar year.

        integer :: start_jday
        integer :: end_jday
        integer :: plant_jday
        integer :: harvest_jday
        integer :: juldate       ! julian date

        logical :: growcrop_flg

        integer :: tisr         ! The last accessed day of simulation month.
        integer :: tday         ! The last accessed month of simulation year.
        integer :: tmo          ! The last accessed year of simulation run.
        integer :: tyr          ! The last accessed subregion index.

        real(kind=4) :: amalat  !  Latitude of simulation site (degrees)

        integer :: cook_yield = 1 ! default to using functional yield/residue ratio info
        !     cook_yield - flag setting which uses input from crop record to
        !                  guarantee a fixed yield/redsidue ratio at harvest
        !                  (this is cooking the books :-(
        real :: water_stress_max = 0.0
        !     water_stress_max - Cap water stress at some maximum value
        !                  (note maximum stress occurs at 0.0 and minimum stress at 1.0)
        !                   water_stress_max = x.xx   ! specified stress limit
        integer :: winter_ann_root = 0 ! RMarquez 06/13/2017 -> changed this to 1, has some impact on wheat.
        !select root growth option for winter annuals
        !winter_ann_root = 0                                    ! root depth grows at same rate as height
        !winter_ann_root = 1                                    ! root depth grows with fall heat units
        integer :: am0cdb  ! flag to print CROP variables before and after call to CROP
        character(80) :: cliname              ! the name of the location for the climate data
    end type simulation

    type files
        integer luicli
        integer luocrop
        integer luoshoot
        integer luoseason
        integer luoinpt
        integer luoallcrop
        integer luoemerge
        integer luophenol
        integer luocanopyht
        integer cropxml
        integer upgmmgt
        integer upgmstress
        integer upgmcli
        integer upgmcrop
        integer upgmco2
        integer upgmco2atmos
        integer cdbugfile
        integer soilprofile
    end type files


    type controls
        type(files) :: handles
        type(simulation) :: sim
        type(crop_stress) :: cropstress
        type(nitrogen_data) :: ndat
    end type controls


    interface ! interface

    module function initialize_ctrl(offset) result(ctrl)
    integer, intent (in) :: offset
    type(controls) :: ctrl
    end function

    module subroutine open_inputfiles(ctrl)
    type(controls), intent(inout) :: ctrl
    end subroutine

    module subroutine open_outputfiles(ctrl)
    type(controls), intent(inout) :: ctrl
    end subroutine

    end interface ! end interface

    contains

    module function initialize_ctrl(offset) result(ctrl)
    implicit none
    integer, intent(in) :: offset
    type(controls) :: ctrl
    ! setup file handle offsets
    ctrl%handles%upgmmgt = 10000 + offset
    ctrl%handles%upgmstress = 20000 + offset
    ctrl%handles%upgmcli = 30000 + offset
    ctrl%handles%upgmcrop = 40000 + offset
    ctrl%handles%upgmco2 = 50000 + offset
    ctrl%handles%upgmco2atmos = 60000 + offset
    ctrl%handles%luicli = 70000 + offset
    ctrl%handles%luocrop = 80000 + offset
    ctrl%handles%luoshoot = 90000 + offset
    ctrl%handles%luoseason = 100000 + offset
    ctrl%handles%luoinpt = 110000 + offset
    ctrl%handles%luoemerge = 120000 + offset
    ctrl%handles%luophenol = 130000 + offset
    ctrl%handles%luocanopyht = 140000 + offset
    ctrl%handles%luoallcrop = 150000 + offset
    ctrl%handles%cdbugfile = 160000 + offset
    ctrl%handles%cropxml = 170000 + offset
    !Rmarquez 2.10.17 -> added new offset value
    ctrl%handles%soilprofile = 180000 + offset
    end function initialize_ctrl

    module subroutine open_inputfiles(ctrl)
    implicit none
    type(controls), intent(inout) :: ctrl
    !
    ! open required input files
    !
    call fopenk(ctrl%handles%cropxml,'cropxml.dat','old')      ! open weps crop parameter file
    ! call fopenk(luicli,'cligen.cli','old')  ! open cligen climate
    call fopenk(ctrl%handles%upgmmgt,'upgm_mgmt.dat','old')    ! open management file
    call fopenk(ctrl%handles%upgmstress,'upgm_stress.dat','old')  ! open water stress file
    call fopenk(ctrl%handles%upgmcli,'upgm_cli.dat','old')     ! open historical climate file
    call fopenk(ctrl%handles%upgmcrop,'upgm_crop.dat','old')    ! open upgm crop file
    call fopenk(ctrl%handles%upgmco2,'upgm_co2.dat','old')     ! open upgm co2 file. DE added for co2 effects
    call fopenk(ctrl%handles%upgmco2atmos,'upgm_co2atmos.dat','old') ! open upgm daily atmospheric co2 file.
    !Rmarquez 2.10.17 -> added new profile file
    call fopenk(ctrl%handles%soilprofile, 'upgm_soil_profile.dat', 'old') ! open soil profile file to initialize profile
    end subroutine open_inputfiles

    module subroutine open_outputfiles(ctrl)
    implicit none
    type(controls), intent(inout) :: ctrl
    call fopenk(ctrl%handles%luocrop,'crop.out','unknown')    ! daily crop output of most state variables
    call fopenk(ctrl%handles%luoseason,'season.out','unknown')  ! seasonal summaries of yield and biomass
    call fopenk(ctrl%handles%luoinpt,'inpt.out','unknown')    ! echo crop input data
    call fopenk(ctrl%handles%luoshoot,'shoot.out','unknown')   ! crop shoot output
    call fopenk(ctrl%handles%luoemerge,'emerge.out','unknown')  ! debe added for emergence output
    call fopenk(ctrl%handles%luophenol,'phenol.out','unknown')  ! debe added for phenology output
    call fopenk(ctrl%handles%luocanopyht,'canopyht.out','unknown')
    end subroutine open_outputfiles

    end module upgm_simdata

    !------------------------------------------------------------------------------
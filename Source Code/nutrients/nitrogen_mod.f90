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
    use constants, only : mnsz
    implicit none
    type nitrogen_data
        real :: a_s11   !parameter in P uptake eqn. (corresponding to scrp(11,1) in epic)
        real :: b_s11   !parameter in P uptake eqn. (corresponding to scrp(11,2) in epic)
        real :: a_s8    !parameter in N or P stress eqn. (corresponds to scrp(8,1) in epic)
        real :: b_s8    !parameter in N or P stress eqn. (corresponds to scrp(8,2) in epic)
        real, dimension (mnsz) :: wt
        real, dimension (mnsz) :: psp
        integer :: ids
        real :: cmn     ! ???
        real :: cnt     ! optimal plant n concentration(kg/t) on day i
        real :: cpt     ! optimal plant p concentration(kg/t) on day i
        real :: suno3   ! ??
        real :: sut     ! soil moisture factor
        real :: cdg     ! soil temperature factor for nutrient cycling
        real :: trsd    ! sum of residue of all layers (t/ha) 
        real :: tfon    ! total amount of N from fresh organic matter (kg/ha)
        real :: tfop    ! total amount of P from fresh organic matter (kg/ha)
        real :: tmp     ! total active mineral P from all layers (kg/ha)
        real :: top     ! total stable mineral P from all layers (kg/ha)
        real :: twn     ! total organic N from humus (kg/ha)
        real :: twmn    ! total organic N from active humus pool (kg/ha)   
        real :: tp      ! total P from all humus (kg/ha)
        real :: tap     ! total labile P (kg/ha)
        real :: tno3    ! total no3_n (kg/ha)   
        real :: yc      ! period of cultivation before simualtion starts (yr)
        reaL :: rmnr    ! net mineralized n from all sources of o.m. (kg/ha)
        real :: wmp     ! active humus p pool (kg/ha)
        real :: wim     ! immobilized n (kg/ha)
        real :: wip     ! immobilized p 9kg/ha)
        real :: hmn     !amount of N mineralized from active N pool (kg/ha/d)
        reaL :: rc      ! residue composition factor (0.8, or 0.05, or 0.0095)
        real :: trmn    ! total mineralized N from fresh residue (kg/ha)
        real :: trmp    ! total mineralized N from fresh residue (kg/ha)
        real :: thmn    ! total mineralized N from humus (kg/ha)
        real :: thmp    ! total mineralized P from humus (kg/ha)
        real :: shm     ! sum of mineralized N from humus (kg/ha)
        real :: smr     ! sum of mineralized N from humus & fresh residue (kg/ha)
        real :: sim     ! sum of immobilized N (kg/ha)
        real :: sdn     ! sum of denitrified N (kg/ha)
        real :: smp     ! sum of mineralized P from humus and fresh residue (kg/ha)
        real :: sip     ! sum of immobilized P (kg/ha)
        real :: tsfn    ! sum of N leached from all layers (kg/ha)
        
        
        
        real :: rsd(mnsz) ! current amount of residue (t/ha) in a layer
        real :: rtn(mnsz) !active pool fraction
        real :: fon(mnsz)   ! amount of N in residue (kg/ha)
        real :: fop(mnsz)   ! amount of P in residue (kg/ha)
        real :: wn(mnsz)    ! organic N concentration of humus (g/t)
        real :: wp(mnsz)    ! organic P concentration of humus (g/t)
        real :: wmn(mnsz)   ! active humus n pool (kg/ha)
        real :: hum(mnsz)   ! amount of humus (t/ha)
    end type nitrogen_data

  
    interface
    
        module subroutine nuse(clidat, ndat, bn1, bn2, bn3, bp1, bp2, bp4)
        ! Argument Defs
        type (nitrogen_data), intent(inout) :: ndat
        type (climate_data), intent(in) :: clidat
        real, intent(in) :: bn1, bn2, bn3
        real, intent(in) :: bp1, bp2, bp4
        end subroutine nuse
    
        module subroutine nuts(ndat, y1, y2, uu)
        ! Argument Defs
        type (nitrogen_data), intent(inout) :: ndat
        real, intent(in) :: y1, y2
        real, intent(out) :: uu
        end subroutine nuts
    
    end interface

    end module nitrogen
    
!-------------------------------------------------------------------------
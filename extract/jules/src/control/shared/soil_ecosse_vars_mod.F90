#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology, 2017.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237] 
!******************************COPYRIGHT**************************************

MODULE soil_ecosse_vars_mod


IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Module holding variables for the ECOSSE soil model.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Module variables
!-----------------------------------------------------------------------------

PUBLIC  ! Everything is public.

!-----------------------------------------------------------------------------
! Prognostic variables.
! Note that ECOSSE also uses organic C and N variables from the prognostics
! module.
!-----------------------------------------------------------------------------
REAL, ALLOCATABLE ::                                                        &
  n_amm_soilt(:,:,:),                                                       &
    ! N in soil ammonium (kg m-2).
  n_nit_soilt(:,:,:)
    ! N in soil nitrate (kg m-2).

!-----------------------------------------------------------------------------
! Time-averaged driver variables.
!-----------------------------------------------------------------------------
REAL, ALLOCATABLE ::                                                        &
  deposition_n_driver(:),                                                   &
    ! Atmospheric deposition of N to land surface (kg m-2 s-1).
  qbase_l_driver(:,:,:),                                                    &
    ! Lateral flux of water from each soil layer (kg m-2 s-1).
  sthf_driver(:,:,:),                                                       &
    ! Frozen soil moisture content as a fraction of saturation.
  sthu_driver(:,:,:),                                                       &
    ! Unfrozen soil moisture content as a fraction of saturation.
  tsoil_driver(:,:,:),                                                      &
    ! Soil temperature (K).
  wflux_driver(:,:,:)
    ! Downward water flux at bottom of each soil layer (kg m-2 s-1).

!-----------------------------------------------------------------------------
! Diagnostics.
!-----------------------------------------------------------------------------
REAL, ALLOCATABLE ::                                                          &
  !---------------------------------------------------------------------------
  ! CO2 fluxes.
  !---------------------------------------------------------------------------
  co2_soil_gb(:),                                                             &
    ! C in CO2 flux from soil to atmosphere (kg m-2 s-1).
  !---------------------------------------------------------------------------
  ! Litter input diagnostics.
  !---------------------------------------------------------------------------
  plant_input_c_gb(:),                                                        &
    ! Plant input of carbon to soil by litterfall (kg m-2 s-1).
  plant_input_n_gb(:),                                                        &
    ! Plant input of nitrogen to soil by litterfall (kg m-2 s-1).
  !---------------------------------------------------------------------------
  ! Diagnostics of soil adjustments.
  ! At present these variables could be local to the routines that use them
  ! but they are added here in anticipation of future use as diagnostics.
  !---------------------------------------------------------------------------
  soil_c_add(:),                                                              &
    ! C added to soil during adjustments (kg m-2).
  soil_n_add(:)
    ! N added to soil during adjustments (kg m-2).

END MODULE soil_ecosse_vars_mod
#endif

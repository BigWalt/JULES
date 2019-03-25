! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Description: Module containing the variables for the FLake lake scheme
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.
!
!   Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
!   This file belongs in section: Land
!
MODULE lake_mod
IMPLICIT NONE

! parameters following values in FLake routine flake_parameters
REAL, PARAMETER  :: h_snow_min_flk = 1.0e-5  & ! (m)
                   ,h_ice_min_flk  = 1.0e-9  & ! (m)
                   ,h_ice_max      = 3.0       ! (m)

! parameters following values in FLake routine flake_albedo_ref
REAL, PARAMETER  :: albedo_whiteice_ref =  0.60 & ! White ice
                   ,albedo_blueice_ref  =  0.10 & ! Blue ice
                   ,c_albice_MR         = 95.6
                      ! Constant in the interpolation formula for
                      ! the ice albedo (Mironov and Ritter 2004)

! parameters for empirical initialisation of lake-ice thickness
REAL, PARAMETER :: lake_h_deltaT =  25.0 ! (K)
REAL, PARAMETER :: lake_h_scale  = 100.0 ! dimensionless

! approximate lengthscale for exponential DWSW attenuation in snow
REAL, PARAMETER :: h_snow_sw_att = 0.25 ! (m)

REAL, DIMENSION(:,:), ALLOCATABLE :: surf_ht_flux_lake_ij
                            ! Net downward heat flux at surface over
                            ! lake fraction of gridbox, all points (W/m2)

REAL, DIMENSION(:), ALLOCATABLE :: SURF_HT_FLUX_LK_gb
                            ! Net downward heat flux at surface over
                            ! lake fraction of gridbox, land points (W/m2)
REAL, DIMENSION(:), ALLOCATABLE :: U_S_LAKE_gb
                            ! lake subsurface friction velocity (m/s)
REAL, DIMENSION(:), ALLOCATABLE :: SW_DOWN_gb
                            ! downwelling shortwave irradiance [W m^{-2}]
REAL, DIMENSION(:), ALLOCATABLE :: CORIOLIS_PARAM_gb
                            ! Coriolis parameter (s^-1)
REAL, DIMENSION(:), ALLOCATABLE :: LAKE_DEPTH_gb
                            ! lake depth (m)
REAL, DIMENSION(:), ALLOCATABLE :: LAKE_FETCH_gb
                            ! Typical wind fetch (m)
REAL, DIMENSION(:), ALLOCATABLE, SAVE :: LAKE_ALBEDO_gb
                            ! lake albedo
REAL, DIMENSION(:), ALLOCATABLE :: LAKE_T_SNOW_gb
                            ! temperature at the air-snow interface (K)
REAL, DIMENSION(:), ALLOCATABLE :: LAKE_T_ICE_gb
                            ! temperature at upper boundary of lake ice(K)
REAL, DIMENSION(:), ALLOCATABLE :: LAKE_T_MEAN_gb
                            ! lake mean temperature (K)
REAL, DIMENSION(:), ALLOCATABLE :: LAKE_T_MXL_gb
                            ! lake mixed-layer temperature (K)
REAL, DIMENSION(:), ALLOCATABLE :: LAKE_SHAPE_FACTOR_gb
                            ! thermocline shape factor (dimensionless?)
REAL, DIMENSION(:), ALLOCATABLE :: LAKE_H_SNOW_gb
                            ! snow thickness (m)
REAL, DIMENSION(:), ALLOCATABLE :: LAKE_H_ICE_gb
                            ! lake ice thickness (m)
REAL, DIMENSION(:), ALLOCATABLE :: LAKE_H_MXL_gb
                            ! lake mixed-layer thickness (m)
REAL, DIMENSION(:), ALLOCATABLE :: LAKE_T_SFC_gb
                            ! temperature (of water, ice or snow) at
                            !surface of lake (K)
REAL, DIMENSION(:), ALLOCATABLE :: TS1_LAKE_gb
                            ! "average" temperature of
                            ! lake-ice, lake and soil sandwich
                            ! (K).
REAL, DIMENSION(:), ALLOCATABLE :: NUSSELT_gb
                                  ! Nusselt number
REAL, DIMENSION(:), ALLOCATABLE :: G_DT_gb
                            ! ground heat flux over delta T [W m-2 K-1]

! initial values
REAL :: lake_depth_0  =    5.0  ! (m)
REAL :: lake_fetch_0  =   25.0  ! (m)
REAL :: lake_h_mxl_0  =    2.0  ! (m)
REAL :: lake_shape_0  =    0.5  ! shape factor, dimensionless
REAL :: nusselt_0     = 1000.0  ! Nusselt number, dimensionless
REAL :: g_dt_0        = 1.0e-10 ! very small [W m-2 K-1]

! trap counters
INTEGER :: trap_frozen   = 0
INTEGER :: trap_unfrozen = 0

END MODULE

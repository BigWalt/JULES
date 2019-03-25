! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Module containing surface fluxes.
!
! Code Description:
!   Language: FORTRAN 90
!   This code is written to UMDP3 v8.2 programming standards.
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in section: Land
!
! Changes to variable names to enable soil tiling
! Anything named _tile is now ambiguous, so the following convention is
! adopted:
! _gb for variables on land points
! _ij for variables with i and j indices
! Surface heterogeneity...
! _surft for surface tiled variables (generally size n_surft)
! _pft for plant functional type surface tiled variables (generally sized n_pft)
! _sicat for sea ice catergories (generally size nice_use)
! Sub-surface heterogeneity...
! _soilt for soil tiled variables

MODULE fluxes

IMPLICIT NONE

!-----------------------------------------------------------------------------
! anthrop_heat is required by both the UM and standalone configurations
!-----------------------------------------------------------------------------
REAL, ALLOCATABLE :: anthrop_heat_surft(:,:)
! Additional heat source on surface tiles used for anthropgenic urban heat
! source (W/m2)

REAL, ALLOCATABLE :: surf_ht_store_surft(:,:)
! Diagnostic to store values of C*(dT/dt) during calculation of energy
! balance

REAL, ALLOCATABLE, SAVE :: sw_sicat(:,:)
! Net SW on sea ice categories
REAL, ALLOCATABLE, SAVE :: sw_rts_sicat(:,:)
! Net SW on sea ice categories on the radiative timestep
REAL, ALLOCATABLE, SAVE :: swup_rts_sicat(:,:)
! Upward SW on sea ice categories on the radiative timestep
REAL, ALLOCATABLE, SAVE :: swdn_rts_sicat(:,:)
! Downward SW on sea ice categories on the radiative timestep
REAL, ALLOCATABLE, SAVE :: alb_sicat(:,:,:)
! Albedo of sea ice categories (ij point, sicat, band- see below)
REAL, ALLOCATABLE, SAVE :: sw_sea(:)
! Net SW on open sea
REAL, ALLOCATABLE, SAVE :: sw_rts_sea(:)
! Net SW on open sea on the radiative timestep

!-----------------------------------------------------------------------------
! Everything else is required only in standalone configuration
!-----------------------------------------------------------------------------
#if !defined(UM_JULES)
REAL, ALLOCATABLE :: alb_surft(:,:,:)
!   Albedo for surface tiles
!     (:,:,1) direct beam visible
!     (:,:,2) diffuse visible
!     (:,:,3) direct beam near-IR
!     (:,:,4) diffuse near-IR
REAL, ALLOCATABLE :: e_sea_ij(:,:)
!   Evaporation from sea times leads fraction. Zero over land
!                                (kg per square metre per sec)
REAL, ALLOCATABLE :: ecan_ij(:,:)
!   Gridbox mean evaporation from canopy/surface store (kg/m2/s)
!     Zero over sea
REAL, ALLOCATABLE :: ecan_surft(:,:)
!   Canopy evaporation from for snow-free land tiles
REAL, ALLOCATABLE :: ei_ij(:,:)
!   Sublimation from lying snow or sea-ice (kg/m2/s)
REAL, ALLOCATABLE :: ei_surft(:,:)
!   EI for land tiles
REAL, ALLOCATABLE :: esoil_ij_soilt(:,:,:)
!   Surface evapotranspiration from soil moisture store (kg/m2/s)
REAL, ALLOCATABLE :: esoil_surft(:,:)
!   ESOIL for snow-free land tiles
REAL, ALLOCATABLE :: ext_soilt(:,:,:)
!   Extraction of water from each soil layer (kg/m2/s)
REAL, ALLOCATABLE :: fqw_1_ij(:,:)
!   Moisture flux between layers (kg per square metre per sec)
!   FQW(,1) is total water flux from surface, 'E'
REAL, ALLOCATABLE :: fqw_surft(:,:)
!   Surface FQW for land tiles
REAL, ALLOCATABLE :: fqw_sicat(:,:,:)
!   Surface FQW for sea-ice
REAL, ALLOCATABLE :: fsmc_pft(:,:)
!   Moisture availability factor.
REAL, ALLOCATABLE :: ftl_1_ij(:,:)
!   FTL(,K) contains net turbulent sensible heat flux into layer K from below
!   so FTL(,1) is the surface sensible heat, H.(W/m2)
REAL, ALLOCATABLE :: ftl_sicat(:,:,:)
!   Surface FTL for sea-ice
REAL, ALLOCATABLE :: ftl_surft(:,:)
!   Surface FTL for land tiles
REAL, ALLOCATABLE :: h_sea_ij(:,:)
!   Surface sensible heat flux over sea times leads fraction (W/m2)
REAL, ALLOCATABLE :: hf_snow_melt_gb(:)
!   Gridbox snowmelt heat flux (W/m2)
REAL, ALLOCATABLE :: land_albedo_ij(:,:,:)
!   GBM albedo
!     (:,:,1) direct beam visible
!     (:,:,2) diffuse visible
!     (:,:,3) direct beam near-IR
!     (:,:,4) diffuse near-IR
REAL, ALLOCATABLE :: le_surft(:,:)
!   Surface latent heat flux for land tiles
REAL, ALLOCATABLE :: melt_surft(:,:)
!   Snowmelt on land tiles (kg/m2/s)
REAL, ALLOCATABLE :: sea_ice_htf_sicat(:,:,:)
!   Heat flux through sea-ice (W/m2, positive downwards)
REAL, ALLOCATABLE :: snomlt_sub_htf_gb(:)
!   Sub-canopy snowmelt heat flux (W/m2)
REAL, ALLOCATABLE :: snow_melt_gb(:)
!   snowmelt on land points (kg/m2/s)
REAL, ALLOCATABLE :: snowmelt_ij(:,:)
!   Snowmelt (kg/m2/s)
REAL, ALLOCATABLE :: sub_surf_roff_gb(:)
!   Sub-surface runoff (kg/m2/s)
REAL, ALLOCATABLE :: surf_ht_flux_ij(:,:)
!   Net downward heat flux at surface over land and sea-ice fraction of
!gridbox (W/m2)
REAL, ALLOCATABLE ::  snow_soil_htf(:,:)
!   Heat flux under snow to subsurface on tiles (W/m2)
REAL, ALLOCATABLE :: surf_htf_surft(:,:)
!   Surface heat flux on land tiles (W/m2)
REAL, ALLOCATABLE :: surf_roff_gb(:)
!   Surface runoff (kg/m2/s)
REAL, ALLOCATABLE :: radnet_surft(:,:)
!   Surface net radiation on tiles ( W/m2)
REAL, ALLOCATABLE :: taux_1_ij(:,:)
!   W'ly component of surface wind stress (N/sq m)
REAL, ALLOCATABLE :: tauy_1_ij(:,:)
!   S'ly component of surface wind stress (N/sq m)
!   On V-grid; comments as per TAUX
REAL, ALLOCATABLE :: tot_tfall_gb(:)
!   Total throughfall (kg/m2/s)
REAL, ALLOCATABLE :: tstar_ij(:,:)
!   GBM surface temperature (K)
REAL, ALLOCATABLE :: emis_surft(:,:)
!   Tile emissivity
REAL, ALLOCATABLE :: sw_surft(:,:)
!   Surface net shortwave on tiles (W/m2)
REAL, ALLOCATABLE :: rflow_gb(:)
!   River outflow on model grid (kg/m2/s)
REAL, ALLOCATABLE :: rrun_gb(:)
!   Runoff after river routing on model grid (kg/m2/s)
REAL, ALLOCATABLE :: z0m_surft(:,:)
!   Tile roughness lengths for momentum.
REAL, ALLOCATABLE :: z0h_surft(:,:)
!   Tile roughness lengths for heat and moisture (m).
#endif

END MODULE fluxes

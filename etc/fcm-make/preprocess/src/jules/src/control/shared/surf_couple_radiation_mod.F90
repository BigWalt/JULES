










! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************

MODULE surf_couple_radiation_mod

IMPLICIT NONE

PRIVATE
PUBLIC :: surf_couple_radiation

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='SURF_COUPLE_RADIATION_MOD'

CONTAINS

!===============================================================================
! Public subroutine
!===============================================================================
SUBROUTINE surf_couple_radiation(                                             &
  !Fluxes INTENT(IN)
  tstar,                                                                      &
  !Misc INTENT(IN)
  ws10m, chloro,                                                              &
  n_band, max_n_swbands, wavelength_short, wavelength_long,                   &
  !Misc INTENT(OUT)
  sea_ice_albedo,                                                             &
  !Fluxes INTENT(OUT)
  alb_surft, land_albedo_ij                                                   &
  )

!Module imports
USE ftsa_mod,        ONLY: ftsa
USE tile_albedo_mod, ONLY: tile_albedo

!Common modules
USE missing_data_mod,         ONLY:                                           &
  rmdi
USE ereport_mod,              ONLY:                                           &
  ereport

USE ancil_info,               ONLY:                                           &
  nsoilt

USE jules_sea_seaice_mod,     ONLY:                                           &
  nice, nice_use,                                                             &
  alpham, alphac, alphab, dtice, dt_bare, dalb_bare_wet,                      &
  pen_rad_frac, sw_beta,                                                      &
  albicev_cice, albicei_cice, albsnowv_cice, albsnowi_cice,                   &
  albpondv_cice, albpondi_cice,                                               &
  ahmax, dalb_mlt_cice, dalb_mlts_v_cice, dalb_mlts_i_cice,                   &
  dt_bare_cice, dt_snow_cice, pen_rad_frac_cice, sw_beta_cice,                &
  snowpatch
USE jules_surface_types_mod,  ONLY:                                           &
  ntype, npft

!Potential troublemaker
USE theta_field_sizes,        ONLY:                                           &
  t_i_length, t_j_length

!for testing LSM switch
USE jules_print_mgr,          ONLY: jules_message, jules_print

!JULES-only modules
USE ancil_info,               ONLY:                                         &
  ice_fract_ij, ice_fract_ncat_sicat, pond_frac_cat_sicat,                  &
  pond_depth_cat_sicat, land_pts, land_index, nsurft, surft_pts,            &
  surft_index, frac_surft, row_length, rows
USE p_s_parms,                ONLY:                                         &
  cosz_ij, albsoil_soilt, albobs_sw_gb, albobs_vis_gb, albobs_nir_gb,       &
  z0_surft
USE coastal,                  ONLY:                                         &
  flandg, tstar_sice_sicat
USE orog,                     ONLY:                                         &
  ho2r2_orog_gb
USE prognostics,              ONLY:                                         &
  snow_mass_ij, snow_mass_sea_sicat, di_ncat_sicat, lai_pft, canht_pft,     &
  rgrain_surft, snow_surft, soot_ij, tstar_surft
USE lsm_switch_mod,         ONLY:                                           & 
  lsm_id, jules, cable

!Dr Hook
USE parkind1,                 ONLY:                                           &
  jprb, jpim

USE yomhook,                  ONLY:                                           &
  lhook, dr_hook


IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Coupling routine between the UM or JULES system code and land surface
!   radiation science routines. Calls the appropriate LSM-specific code.
!
!   Some variables exist in modules only in JULES, others only in the UM
!   Options in order of preference
!   -UM and JULES share the same module names.
!   -UM and JULES have different module names and USE statements go on an ifdef
!   -The UM flavour of the variable does not live in a module. Pass in using a
!    an ifdef'ed argument list
!
!   If there are lots of ifs, then we could cosider splitting lsm_couple into
!   jules_couple and lsm_couple
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------


! Subroutine arguments

! Dimensioning variables
INTEGER, INTENT(IN) ::                                                        &
  n_band,                                                                     &
  max_n_swbands

!UM-only arguments
REAL ::                                                                     &
    albobs_sc_ij(t_i_length,t_j_length,ntype,2),                            &
      !albedo scaling factors to obs
    open_sea_albedo(row_length,rows,2,max_n_swbands)
      !Surface albedo for Open Sea (direct and diffuse components, for each
      !band, with zeros for safety where no value applies)

!Fluxes INTENT(IN)
REAL, INTENT(IN) ::                                                           &
  tstar(row_length,rows)            !Surface temperature

!Misc INTENT(IN)
REAL, INTENT(IN) ::                                                           &
  ws10m(row_length,rows),                                                     &
                                    !10m wind speed
  chloro(row_length,rows)           !nr surface chlorophyll content

REAL, INTENT(IN)    ::                                                        &
  wavelength_short(n_band),                                                   &
  wavelength_long(n_band)

!Misc INTENT(OUT)
REAL, INTENT(OUT) ::                                                          &
  sea_ice_albedo(row_length,rows,4)   !Surface Albedo for sea ice
                                      ! (*,1) - direct beam visible
                                      ! (*,2) - diffuse visible
                                      ! (*,3) - direct beam near-ir
                                      ! (*,4) - diffuse near-ir

!Fluxes INTENT(OUT)
REAL, INTENT(OUT) ::                                                          &
  alb_surft(land_pts,nsurft,4),                                               &
                                      !Albedos for surface tiles.
                                      ! (*,*,1) - Direct beam visible
                                      ! (*,*,2) - Diffuse visible
                                      ! (*,*,3) - Direct beam near-IR
                                      ! (*,*,4) - Diffuse near-IR
  land_albedo_ij(t_i_length,t_j_length,4) !GBM albedos.

!-----------------------------------------------------------------------------
! Local variables
!-----------------------------------------------------------------------------

!Land point only versions of ij variables
REAL :: soot_gb(land_pts)
REAL :: cosz_gb(land_pts)

!Counters
INTEGER :: i,j,l,band
INTEGER :: errorstatus

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='SURF_COUPLE_RADIATION'

!Only required until model switch implemented for coupled mode

!-----------------------------------------------------------------------------
!End of header

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

SELECT CASE( lsm_id )
CASE ( jules )
  CALL ftsa (                                                             &
    !INTENT(IN)
    !input fields
    flandg, ice_fract_ij, ice_fract_ncat_sicat, tstar, tstar_sice_sicat,  &
    cosz_ij, ws10m, chloro,                                               &
    snow_mass_ij, snow_mass_sea_sicat, di_ncat_sicat,                     &
    pond_frac_cat_sicat, pond_depth_cat_sicat,                            &
    !max and min sea ice albedo specifications
    alpham, alphac, alphab, dtice,                                        &
    dt_bare, dalb_bare_wet, pen_rad_frac, sw_beta,                        &
    ! parameters for CICE multi-band albedo scheme:
    albicev_cice, albicei_cice, albsnowv_cice, albsnowi_cice,             &
    albpondv_cice, albpondi_cice,                                         &
    ahmax, dalb_mlt_cice, dalb_mlts_v_cice, dalb_mlts_i_cice,             &
    dt_bare_cice, dt_snow_cice,                                           &
    pen_rad_frac_cice, sw_beta_cice, snowpatch,                           &
    !size and control variables
    row_length * rows, max_n_swbands,                                       &
    n_band, nice, nice_use,                                               &
    !spectral boundaries
    wavelength_short,                                                     &
    wavelength_long,                                                      &
    !INTENT(OUT)
    !output arguments
    sea_ice_albedo,                                                       &
    open_sea_albedo)

  !Compress gridded variables to land point only
  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    soot_gb(l)    = soot_ij(i,j)
    cosz_gb(l)    = cosz_ij(i,j)
  END DO

  CALL tile_albedo (                                                      &
    !INTENT(IN)
    t_i_length * t_j_length,                                                &
    land_pts, nsurft,                                                     &
    land_index, surft_pts, surft_index,                                   &
    albsoil_soilt, albobs_sw_gb, albobs_vis_gb, albobs_nir_gb,            &
    cosz_gb, soot_gb, ho2r2_orog_gb,                                      &
    lai_pft, canht_pft,                                                   &
    rgrain_surft, snow_surft, tstar_surft, z0_surft, frac_surft,          &
    !INTENT(OUT)
    alb_surft,albobs_sc_ij,land_albedo_ij)

CASE ( cable )
  ! for testing LSM 
  WRITE(jules_message,'(A)') "CABLE not yet implemented"
  CALL jules_print('surf_couple_radiation', jules_message)

  ! initialise all INTENT(OUT) fields for now until CABLE is implemented
  sea_ice_albedo(:,:,:) = 0.0
  alb_surft(:,:,:) = 0.0
  land_albedo_ij(:,:,:) = 0.0

CASE DEFAULT
  errorstatus = 101
  CALL ereport('surf_couple_radiation', errorstatus,                      &
               'Unrecognised surface scheme')

END SELECT

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE surf_couple_radiation
END MODULE surf_couple_radiation_mod


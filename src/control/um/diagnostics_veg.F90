#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
! Subroutine diagnostics_veg ----------------------------------------
!
! Purpose : Calculates diagnostics for dynamic vegetation and
!           outputs them.
!
! -----------------------------------------------------------------
!
! Subroutine diagnostics_veg
!
! Code Owner: Please refer to ModuleLeaders.txt and UM file CodeOwners.txt
! This file belongs in TECHNICAL, VEGETATION

SUBROUTINE diagnostics_veg(                                             &
                       row_length, rows, n_rows                         &
,                      global_row_length, global_rows                   &
,                      dim_cs1, dim_cs2                                 &
,                      halo_i, halo_j, off_x, off_y, me                 &
,                      n_proc, n_procx, n_procy                         &
,                      g_rows, g_row_length                             &
,                      at_extremity                                     &
,                      land_pts                                         &
,                      land_index                                       &
,                      ntype,npft                                       &
,                      c_veg,cv,g_leaf_phen                             &
,                      lit_c,lit_c_mn,g_leaf_day                        &
,                      lai_phen,g_leaf_dr_out,npp_dr_out                &
,                      resp_w_dr_out,resp_s_dr_out,frac_disturb         &
,                      disturb_veg_prev,wood_prod_fast_gb               &
,                      wood_prod_med_gb,wood_prod_slow_gb               &
,                      frac,lai,ht,cs                                   &
,                      stashwork                                  )


! Purpose:
!          Calculates diagnostics and outputs them.
!
USE submodel_mod, ONLY: atmos_im
USE stash_array_mod, ONLY:                                              &
    sf, si, stlist, stindex, len_stlist, stash_pseudo_levels,           &
    num_stash_pseudo, stash_levels, num_stash_levels
USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
USE ereport_mod, ONLY: ereport
USE missing_data_mod, ONLY: rmdi
USE prognostics, ONLY: ns_pool_gb, n_inorg_gb, triffid_co2_gb
USE errormessagelength_mod, ONLY: errormessagelength
USE trif_vars_mod, ONLY: wp_fast_in_gb,  wp_med_in_gb,  wp_slow_in_gb,  &
                         wp_fast_out_gb, wp_med_out_gb, wp_slow_out_gb, &
                         deposition_n_gb, dpm_ratio_gb,                 &
                         n_fix_pft, n_fix_gb, n_leach_soilt, n_gas_gb,  &
                         n_loss_gb, harvest_n_pft, harvest_n_gb,        &
                         lit_n_ag_pft_diag, lit_n_pft_diag, n_luc,      &
                         n_stem_trif_pft, n_root_trif_pft,              &
                         n_leaf_trif_pft,  n_leaf_alloc_trif_pft,       &
                         n_leaf_labile_trif_pft, n_veg_pft, n_veg_gb,   &
                         n_demand_growth_pft, n_demand_spread_pft,      &
                         n_demand_pft, n_demand_gb, n_demand_lit_pft,   &
                         n_uptake_growth_pft, n_uptake_spread_pft,      &
                         n_uptake_pft, n_uptake_gb, fn_gb,              &
                         immob_n_pot_gb, immob_n_gb, minl_n_pot_gb,     &
                         minl_n_gb, n_fertiliser_pft, n_fertiliser_gb,  &
                         lit_n_t_gb, leafc_pft, woodc_pft, rootc_pft,   &
                         leafc_gbm, woodc_gbm, rootc_gbm,               &
                         root_abandon_pft,root_abandon_n_pft,           &
                         root_abandon_gb,root_abandon_n_gb,             &
                         cnsrv_veg_triffid_gb, cnsrv_soil_triffid_gb,   &
                         cnsrv_prod_triffid_gb, cnsrv_carbon_triffid_gb,&
                         cnsrv_carbon_veg2_gb, resp_s_to_atmos_gb,      &
                         cnsrv_vegn_triffid_gb, cnsrv_soiln_triffid_gb, &
                         cnsrv_n_inorg_triffid_gb,                      &
                         cnsrv_nitrogen_triffid_gb,                     &
                         exudates_pft, exudates_gb, npp_n_gb, npp_n,    &
                         resp_p_actual_pft, resp_p_actual_gb,           &
                         gpp_pft_out, gpp_gb_out,                       &
                         gpp_pft_acc, gpp_gb_acc, n_leach_gb_acc,       &
                         harvest_pft, harvest_gb
 
USE jules_vegetation_mod, ONLY: triffid_period

USE conversions_mod, ONLY: rsec_per_day

IMPLICIT NONE

LOGICAL ::                                                              &
  at_extremity(4)  ! Indicates if this processor is at north,
                         ! south, east or west of the processor grid

! Arguments with Intent IN. ie: Input variables.

INTEGER ::                                                              &
  row_length                                                            &
                   ! number of points on a row
, rows                                                                  &
                   ! number of rows in a theta field
, n_rows                                                                &
                   ! number of rows in a v field
, number_format
                   ! switch controlling number format diagnostics
                   ! are written out in. See PP_WRITE for details.


INTEGER ::                                                              &
  global_row_length                                                     &
                      !IN. NUMBER OF points on a global row
, global_rows                                                           &
                      !IN. NUMBER OF global rows
, me                                                                    &
                      !IN. Processor number
, halo_i                                                                &
                      !IN. size of large halo in x direction
, halo_j                                                                &
                      !IN. size of large halo in y direction
, off_x                                                                 &
                      !IN. size of small halo in x direction
, off_y                                                                 &
                      !IN. size of small halo in y direction
, n_proc                                                                &
, n_procx                                                               &
, n_procy                                                               &
, g_rows(0:n_proc-1)                                                    &
, g_row_length(0:n_proc-1)                                              &
, land_pts                                                              &
           ! No.of land points being processed, can be 0.
, ntype                                                                 &
              ! Max. No. of land surface tiles
, npft                                                                  &
              ! No. of plant functional types
, dim_cs1, dim_cs2     ! soil carbon dimensions

REAL ::                                                                 &
  lat_rot_np                                                            &
, long_rot_np


! Primary Arrays used in all models
INTEGER ::                                                              &
  land_index(land_pts)      ! set from land_sea_mask


REAL ::                                                                 &
 c_veg(land_pts,npft)                                                   &
                      ! Total carbon content of vegetation
!                              ! (kg C/m2).
,cv(land_pts)                                                           &
                      ! Gridbox mean veg carbon (kg C/m2).
,lit_c(land_pts,npft)                                                   &
                      ! Carbon Litter (kg C/m2/360days).
,lit_c_mn(land_pts)                                                     &
                      ! Gridbox mean carbon litter
!                              ! (kg C/m2/360days)
,g_leaf_day(land_pts,npft)                                              &
                             ! Mean leaf turnover rate for
!                                     ! input to PHENOL (/360days).
,g_leaf_phen(land_pts,npft)                                             &
                             ! Mean leaf turnover rate over
!                                     ! phenology period (/360days).
,g_leaf_dr_out(land_pts,npft)                                           &
                             ! Mean leaf turnover rate for
!                                     ! driving TRIFFID (/360days).
,lai_phen(land_pts,npft)                                                &
                             ! LAI of PFTs after phenology.
,npp_dr_out(land_pts,npft)                                              &
                             ! Mean NPP for driving TRIFFID
!                                     ! (kg C/m2/360days).
,resp_w_dr_out(land_pts,npft)                                           &
                             ! Mean wood respiration for
!                                     ! driving TRIFFID
!                                     ! (kg C/m2/360days).
,resp_s_dr_out(land_pts,dim_cs1+1)                                      &
                                   ! Mean soil respiration for
!                                     ! driving TRIFFID
!                                     ! (kg C/m2/360days).
,frac_disturb(land_pts)                                                 &
                             ! Fraction of gridbox in which
!                                     !    vegetation is disturbed.
,disturb_veg_prev(land_pts)                                             &
                             ! Previous disturbed fraction
,wood_prod_fast_gb(land_pts)                                            &
                             ! Wood product pool - fast decomposition
,wood_prod_med_gb(land_pts)                                             &
                             ! Wood product pool - medium rate of 
                             !                          decomposition
,wood_prod_slow_gb(land_pts)                                            &
                             ! Wood product pool - slow decomposition
,frac(land_pts,ntype)                                                   &
                             ! Fractions of surface types.
,lai(land_pts,npft)                                                     &
                             ! LAI of plant functional
!                                     !       types.
,ht(land_pts,npft)                                                      &
                             ! Height of plant functional
!                                     !       types (m).
,cs(land_pts,dim_cs1)   ! Soil carbon content
!                                     !       (kg C/m2).

! Diagnostic variables
REAL ::                                                                &
 stashwork( * )    ! STASH workspace


! Local variables

LOGICAL ::                                                              &
 plltype(ntype)                                                         &
                    ! pseudolevel list for surface types
,pllpft(npft)       ! pseudolevel list for PFTs

INTEGER ::                                                              &
 pslevel                                                                &
               !  loop counter for pseudolevels
,pslevel_out   !  index for pseudolevels sent to STASH

INTEGER ::                                                              &
  i, j, k, l                                                            &
,    icode                ! Return code  =0 Normal exit  >1 Error

CHARACTER(LEN=errormessagelength) :: cmessage
CHARACTER(LEN=*) :: routinename
PARAMETER ( routinename='DIAGNOSTICS_VEG')

INTEGER ::                                                              &
  im_index        ! internal model index

REAL ::                                                                 &
  interp_data(row_length,rows)

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

! ----------------------------------------------------------------------
! Section 1.  Initialisation.
! ----------------------------------------------------------------------

IF (lhook) CALL dr_hook(RoutineName,zhook_in,zhook_handle)
icode = 0 ! Initialise error status
im_index = 1

! ----------------------------------------------------------------------

!  Item 1: VEGETATION CARBON ON PLANT FUNCTIONAL TYPES

IF (sf(1,19)) THEN

  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,1,19,im_index)),                              &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 1 = c_veg)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = c_veg(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(1,19,im_index) + (pslevel_out-1)      &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,1,                                               &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 1901)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(1,19)


!  Item 2: GRIDBOX MEAN VEGETATION CARBON

IF (sf(2,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = cv(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(2,19,im_index)),interp_data,              &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,2,                                                   &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 1902)"
    GO TO 9999
  END IF

END IF     !   sf(2,19)

!  Item 3: PHENOLOGICAL LEAF TURNOVER RATE PFTS

IF (sf(3,19)) THEN

  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,3,19,im_index)),                              &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 3 = g_leaf_phen)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = g_leaf_phen(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(3,19,im_index) + (pslevel_out-1)      &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,3,                                               &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 1903)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(3,19)


!  Item 4: LITTER CARBON ON PLANT FUNCTIONAL TYPES

IF (sf(4,19)) THEN

  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,4,19,im_index)),                              &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 4 = lit_c)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = lit_c(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(4,19,im_index) + (pslevel_out-1)      &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,4,                                               &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 1904)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(4,19)


!  Item 5: GRIDBOX MEAN LITTER CARBON

IF (sf(5,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = lit_c_mn(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(5,19,im_index)),interp_data,              &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,5,                                                   &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 1905)"
    GO TO 9999
  END IF

END IF     !   sf(5,19)

!  Item 6: MEAN LEAF TURNOVER RATE ON PFTS FOR PHENOLOGY

IF (sf(6,19)) THEN

  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,6,19,im_index)),                              &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 6 = g_leaf_day)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = g_leaf_day(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(6,19,im_index) + (pslevel_out-1)      &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,6,                                               &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 1906)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(6,19)


!  Item 7: LEAF AREA INDEX ON PLANT FUNCTIONAL TYPES AFTER PHENOLOGY

IF (sf(7,19)) THEN

  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,7,19,im_index)),                              &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 7 = lai_phen)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = lai_phen(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(7,19,im_index) + (pslevel_out-1)      &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,7,                                               &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 1907)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(7,19)


!  Item 8: MEAN LEAF TURNOVER RATE ON PFTS FOR TRIFFID

IF (sf(8,19)) THEN

  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,8,19,im_index)),                              &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 8 = g_leaf_dr_out)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = g_leaf_dr_out(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(8,19,im_index) + (pslevel_out-1)      &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,8,                                               &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 1908)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(8,19)


!  Item 9: MEAN NPP ON PFTS FOR TRIFFID

IF (sf(9,19)) THEN

  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,9,19,im_index)),                              &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 9 = npp_dr_out)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = npp_dr_out(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(9,19,im_index) + (pslevel_out-1)      &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,9,                                               &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 1909)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(9,19)


!  Item 10: MEAN WOOD RESPIRATION ON PFTS FOR TRIFFID

IF (sf(10,19)) THEN

  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,10,19,im_index)),                             &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 10 = resp_w_dr_out)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = resp_w_dr_out(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(10,19,im_index) + (pslevel_out-1)     &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,10,                                              &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 1910)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(10,19)


!  Item 11: MEAN SOIL RESPIRATION FOR TRIFFID

IF (sf(11,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = resp_s_dr_out(l,5)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(11,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,11,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 1911)"
    GO TO 9999
  END IF

END IF     !   sf(11,19)

!  Item 12: DISTURBED FRACTION OF VEGETATION

IF (sf(12,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = frac_disturb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(12,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,12,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 1912)"
    GO TO 9999
  END IF

END IF     !   sf(12,19)

!  Item 13: SURFACE TYPE FRACTIONS AFTER TRIFFID

IF (sf(13,19)) THEN

  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(ntype,len_stlist,                                &
       stlist(1,stindex(1,13,19,im_index)),                             &
       plltype,stash_pseudo_levels,num_stash_pseudo,                    &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 13 = frac)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,ntype
    IF (plltype(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = frac(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(13,19,im_index) + (pslevel_out-1)     &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,13,                                              &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 1913)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(13,19)


!  Item 14: LEAF AREA INDEX ON PLANT FUNCTIONAL TYPES AFTER TRIFFID

IF (sf(14,19)) THEN

  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,14,19,im_index)),                             &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 14 = lai)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = lai(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(14,19,im_index) + (pslevel_out-1)     &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,14,                                              &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 1914)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(14,19)


!  Item 15: CANOPY HEIGHT ON PLANT FUNCTIONAL TYPES AFTER TRIFFID

IF (sf(15,19)) THEN

  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,15,19,im_index)),                             &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 15 = ht)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = ht(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(15,19,im_index) + (pslevel_out-1)     &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,15,                                              &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 1915)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(15,19)

!  Item 16: SOIL CARBON CONTENT AFTER TRIFFID

IF (sf(16,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = cs(l,1) + cs(l,2) + cs(l,3) + cs(l,4)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(16,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,16,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 1916)"
    GO TO 9999
  END IF

END IF     !   sf(16,19)


!  Item 17-20: MEAN SOIL RESPIRATION FOR TRIFFID, INDIVID. POOLS
! 17: DPM
IF (sf(17,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = resp_s_dr_out(l,1)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(17,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,17,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 1917)"
    GO TO 9999
  END IF
END IF

! 18: RPM
IF (sf(18,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = resp_s_dr_out(l,2)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(18,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,18,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 1918)"
    GO TO 9999
  END IF
END IF

! 19: BIO
IF (sf(19,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = resp_s_dr_out(l,3)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(19,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,19,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 1919)"
    GO TO 9999
  END IF
END IF

! 20: HUM
IF (sf(20,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = resp_s_dr_out(l,4)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(20,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,20,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 1920)"
    GO TO 9999
  END IF
END IF

!  Item 21-24: SOIL CARBON CONTENT AFTER TRIFFID, INDIVID. POOLS kgC/m2
! 21: DPM
IF (sf(21,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = cs(l,1)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(21,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,21,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 1921)"
    GO TO 9999
  END IF
END IF

! 22: RPM
IF (sf(22,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = cs(l,2)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(22,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,22,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 1922)"
    GO TO 9999
  END IF
END IF

! 23: BIO
IF (sf(23,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = cs(l,3)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(23,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,23,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 1923)"
    GO TO 9999
  END IF
END IF

! 24: HUM
IF (sf(24,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = cs(l,4)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(24,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,24,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 1924)"
    GO TO 9999
  END IF
END IF

! 19025 LEAF CARBON on PFTS, kgC/m2

IF (sf(25,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,25,19,im_index)),                             &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19025)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = leafc_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(25,19,im_index) + (pslevel_out-1)     &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,25,                                              &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19025)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(25,19)

! Item 19 026 LEAF CARBON (GBM) kgC/m2

IF (sf(26,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = leafc_gbm(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(26,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,26,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19026)"
    GO TO 9999
  END IF

END IF     !   sf(26,19)



! Item 19027 WOOD CARBON on PFTS, kgC/m2

IF (sf(27,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,27,19,im_index)),                             &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19027)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = woodc_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(27,19,im_index) + (pslevel_out-1)     &
          * row_length * rows),interp_data,                             &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,27,                                              &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19027)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(27,19)

! Item 19 028 WOOD CARBON (GBM) kgC/m2

IF (sf(28,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = woodc_gbm(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(28,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,28,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19028)"
    GO TO 9999
  END IF

END IF     !   sf(28,19)


! Item 19029 ROOT CARBON on PFTS, kgC/m2

IF (sf(29,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,29,19,im_index)),                             &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19029)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = rootc_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(29,19,im_index) + (pslevel_out-1)     &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,29,                                              &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19029)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(29,19)

! Item 19 030 ROOT CARBON (GBM) kgC/m2

IF (sf(30,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = rootc_gbm(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(30,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,30,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19030)"
    GO TO 9999
  END IF

END IF     !   sf(30,19)



! Item 19031: Previous agricultural fraction
! Agricultural fraction from previous call to TRIFFID.

IF (sf(31,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = disturb_veg_prev(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(31,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,31,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19031)"
    GO TO 9999
  END IF

END IF     !   sf(31,19)


! Item 19032: Wood product pool Carbon (FAST turnover rate pool), kgC/m2
! Fast-turnover wood product carbon pool from cleared vegetation

IF (sf(32,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = wood_prod_fast_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(32,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,32,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19032)"
    GO TO 9999
  END IF

END IF     !   sf(32,19)


! Item 19033: Wood product pool Carbon (MEDIUM turnover rate pool), kgC/m2
! Medium-turnover wood product carbon pool from cleared vegetation

IF (sf(33,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = wood_prod_med_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(33,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,33,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19033)"
    GO TO 9999
  END IF

END IF     !   sf(33,19)

! Item 19034: Wood product pool Carbon (SLOW turnover rate pool), kgC/m2
! Slow-turnover wood product carbon pool from cleared vegetation

IF (sf(34,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = wood_prod_slow_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(34,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,34,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19034)"
    GO TO 9999
  END IF

END IF     !   sf(34,19)


! Item 19036:  LIT C FLUX TO FAST POOL kgC/m2/YR
! Carbon flux into the fast-turnover wood product pool from cleared vegetation

IF (sf(36,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = wp_fast_in_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(36,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,36,                                                  &
  !#include <argppx/argppx.h>
         icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19036)"
    GO TO 9999
  END IF

END IF     !   sf(36,19)

! Item 19037:  LIT C FLUX TO MED POOL kgC/m2/YR
! Carbon flux into the medium-turnover wood product pool from cleared vegetation

IF (sf(37,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = wp_med_in_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(37,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,37,                                                  &
  !#include <argppx/argppx.h>
         icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19037)"
    GO TO 9999
  END IF

END IF     !   sf(37,19)


! Item 19038:  LIT C FLUX TO SLOW POOL kgC/m2/YR
! Carbon flux into the slow-turnover wood product pool from cleared vegetation

IF (sf(38,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = wp_slow_in_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(38,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,38,                                                  &
  !#include <argppx/argppx.h>
         icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19038)"
    GO TO 9999
  END IF

END IF     !   sf(38,19)


! Item 19039: FAST WP POOL DECOMP C FLUX kgC/m2/YR
! CO2 flux from decomposition of the fast-turnover wood product pool

IF (sf(39,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = wp_fast_out_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(39,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,39,                                                  &
  !#include <argppx/argppx.h>
         icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19039)"
    GO TO 9999
  END IF

END IF     !   sf(39,19)


! Item 19040: MED WP POOL DECOMP C FLUX kgC/m2/YR
! CO2 flux from decomposition of the medium-turnover wood product pool

IF (sf(40,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = wp_med_out_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(40,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,40,                                                  &
  !#include <argppx/argppx.h>
         icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19040)"
    GO TO 9999
  END IF

END IF     !   sf(40,19)


! Item 19041: SLOW WP POOL DECOMP C FLUX kgC/m2/YR
! CO2 flux from decomposition of the slow-turnover wood product pool

IF (sf(41,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = wp_slow_out_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(41,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,41,                                                  &
  !#include <argppx/argppx.h>
         icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19041)"
    GO TO 9999
  END IF

END IF     !   sf(41,19)


! Item 19042: TOTAL WP POOL DECOMP C FLUX kgC/m2/YR
! Total CO2 flux from decomposition of all three wood product pools combined

IF (sf(42,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = wp_fast_out_gb(l) + wp_med_out_gb(l) +          &
                       wp_slow_out_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(42,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,42,                                                  &
  !#include <argppx/argppx.h>
         icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19042)"
    GO TO 9999
  END IF

END IF     !   sf(42,19)

! 19043: Harvest carbon flux on PFTs kgC/m2/yr
! Carbon flux from harvested crop PFTs, available when
! L_TRIF_CROP=.TRUE. 

IF (sf(43,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                         &
       stlist(1,stindex(1,43,19,im_index)),                     &
       pllpft,stash_pseudo_levels,num_stash_pseudo,             &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage =                                                &
    "dagveg  : error in set_pseudo_list(item 19043)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = harvest_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (STASHwork(si(43,19,im_index) +         &
           (pslevel_out-1)                                &
           * row_length * rows),interp_data,                &
           row_length,rows,0,0,0,0, at_extremity,           &
           atmos_im,19,43,                                  &
           icode,cmessage)

      IF (icode > 0) THEN
        cmessage="Error in copydiag (item 19043)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(43,19)

! 19044: Harvest carbon flux (GBM) kgC/m2/yr
! Carbon flux from harvested crop PFTs (gridbox mean), 
! available when L_TRIF_CROP=.TRUE. 

IF (sf(44,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = harvest_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (STASHwork(si(44,19,im_index)),interp_data,     &
       row_length,rows,0,0,0,0, at_extremity,                   &
       atmos_im,19,44,                                          &
       icode,cmessage)

  IF (icode > 0) THEN
    cmessage="Error in copydiag (item 19044)"
    GO TO 9999
  END IF

END IF     !   sf(44,19)


! Item 19 45 LANDUSE C TO SOIL ON PFTS kg/m2/360d

IF (sf(45,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,45,19,im_index)),                             &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19045)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = root_abandon_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(45,19,im_index) + (pslevel_out-1)     &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,45,                                              &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19045)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(45,19)

! Item 19 46 LANDUSE C TO SOIL (GBM) kgC/m2/360d

IF (sf(46,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = root_abandon_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(46,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,46,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19046)"
    GO TO 9999
  END IF

END IF     !   sf(46,19)

! 19047: CO2 correction term added to the atmosphere's 3D CO2 tracer from the 
! land surface in emissions-driven runs. Consists of the sum of the
! crop harvest flux, exudates (the portion of NPP not assimilable by plants 
! due to nitrogen limitation) and the total wood product pool flux. 
! UNITS: kgC/m2/yr
!
IF (sf(47,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = triffid_co2_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (STASHwork(si(47,19,im_index)),interp_data,       &
       row_length,rows,0,0,0,0, at_extremity,                     &
       atmos_im,19,47,                                            &
  !#include <argppx/argppx.h>
               icode,cmessage)

  IF (icode > 0) THEN
    cmessage="Error in copydiag (item 19047)"
    GO TO 9999
  END IF

END IF     !   sf(47,19)

! Item 19 48 carbon error in veg2 kg m-2

IF (sf(48,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = cnsrv_carbon_veg2_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(48,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,48,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19048)"
    GO TO 9999
  END IF

END IF     !   sf(48,19)

! Item 19 49 carbon error in triffid kg m-2

IF (sf(49,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = cnsrv_carbon_triffid_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(49,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,49,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19049)"
    GO TO 9999
  END IF

END IF     !   sf(49,19)

! Item 19 50 veg carbon error in triffid kg m-2

IF (sf(50,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = cnsrv_veg_triffid_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(50,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,50,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19050)"
    GO TO 9999
  END IF

END IF     !   sf(50,19)

! Item 19 51 soil carbon error in triffid kg m-2

IF (sf(51,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = cnsrv_soil_triffid_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(51,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,51,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19051)"
    GO TO 9999
  END IF

END IF     !   sf(51,19)

! Item 19 52 wood product carbon error in triffid kg m-2

IF (sf(52,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = cnsrv_prod_triffid_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(52,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,52,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19052)"
    GO TO 9999
  END IF

END IF     !   sf(52,19)

! Item 19 53 soil to atmosphere respiration flux kg m-2 (360 day)-1

IF (sf(53,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = resp_s_to_atmos_gb(l,1)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(53,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,53,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19053)"
    GO TO 9999
  END IF

END IF     !   sf(53,19)

! Item 19054: Fraction of total litter going to Decomposable Plant 
!             Material (DPM) pool. 

IF (sf(54,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = dpm_ratio_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(54,19,im_index)),interp_data,             &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,54,                                                  &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19054)"
    GO TO 9999
  END IF

END IF     !   sf(54,19)

! Stashcode (19,101):  NPP after N limitation on PFTS (KGC/M2/YR)
! NPP after the removal of the exudates term as a result of Nitrogen
! limitation (on PFTs)

IF (sf(101,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                               &
                       stlist(1,stindex(1,101,19,im_index)),          &
                       pllpft,stash_pseudo_levels,num_stash_pseudo,   &
                       icode,cmessage)
  IF (icode > 0) THEN
    cmessage=                                                       &
    "dagveg  : error in set_pseudo_list(item 101 = npp_n)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1,rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1,land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = npp_n(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (STASHwork(si(101,19,im_index) + (pslevel_out-1)  &
                     * row_length * rows),interp_data,                  &
                     row_length,rows,0,0,0,0, at_extremity,             &
                     atmos_im,19,101,                                   &
                     icode,cmessage)

      IF (icode > 0) THEN
        cmessage="Error in copydiag (item 19101)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(101,19)

! Stashcode (19,102):  NPP after N limitation (GBM) KGC/M2/YR
! NPP after the removal of the exudates term as a result of Nitrogen
! limitation (gridbox mean)

IF (sf(102,19)) THEN
  DO j = 1,rows
    DO i = 1,row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1,land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = npp_n_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (STASHwork(si(102,19,im_index)),interp_data,        &
                 row_length,rows,0,0,0,0, at_extremity,             &
                 atmos_im,19,102,                                   &
                 icode,cmessage)

  IF (icode > 0) THEN
    cmessage="Error in copydiag (item 19102)"
    GO TO 9999
  END IF

END IF     !   sf(102,19)

! Stashcode (19,103): Exudates on PFTs (KGC/M2/YR)
! Exudates term - the amount by which NPP is reduced due to Nitrogen limitation
! (on PFTs)

IF (sf(103,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
                       stlist(1,stindex(1,103,19,im_index)),            &
                       pllpft,stash_pseudo_levels,num_stash_pseudo,     &
                       icode,cmessage)
  IF (icode > 0) THEN
    cmessage=                                                         &
    "dagveg  : error in set_pseudo_list(item 103 = exudates)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1,rows
        DO i = 1,row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1,land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = exudates_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (STASHwork(si(103,19,im_index) + (pslevel_out-1)  &
                     * row_length * rows),interp_data,                  &
                     row_length,rows,0,0,0,0, at_extremity,             &
                     atmos_im,19,103,                                   &
                     icode,cmessage)

      IF (icode > 0) THEN
        cmessage="Error in copydiag (item 19103)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(103,19)

! Stashcode (19,104): Gridbox mean exudates term (KGC/M2/YR)
! Exudates term - the amount by which NPP is reduced due to Nitrogen
! limitation (gridbox mean)

IF (sf(104,19)) THEN
  DO j = 1,rows
    DO i = 1,row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1,land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = exudates_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (STASHwork(si(104,19,im_index)),interp_data,            &
                 row_length,rows,0,0,0,0, at_extremity,                 &
                 atmos_im,19,104,                                       &
                 icode,cmessage)

  IF (icode > 0) THEN
    cmessage="Error in copydiag (item 19104)"
    GO TO 9999
  END IF

END IF     !   sf(104,19)

! Item 19 111 NITROGEN DEPOSITION kgN/m2/360d

IF (sf(111,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    ! Convert to per 360 days
    interp_data(i,j) = deposition_n_gb(l) * rsec_per_day * 360.0
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(111,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,111,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19111)"
    GO TO 9999
  END IF

END IF     !   sf(111,19)

! Item 19 112 NITROGEN FIXATION PFTS kgC/m2/360d

IF (sf(112,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,112,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19112)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = n_fix_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(112,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,112,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19112)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(112,19)

! Item 19 113 NITROGEN FIXATION (GBM) kgN/m2/360d

IF (sf(113,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = n_fix_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(113,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,113,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19113)"
    GO TO 9999
  END IF

END IF     !   sf(113,19)

! Stashcode (19,114) NITROGEN LEACHING (GBM) KGN/M2/YR
! This is calculated every timestep in JULES subroutine HYDROL and 
! accumulated between calls to TRIFFID so that it can be
! sampled and meaned over TRIFFID timesteps like all other section 19 
! diagnostics. The * 360.0/REAL(triffid_period) converts the 
! accumulation since the last TRIFFID call to units of kgN/m2/yr;
! TRIFFID is constrained to a 360 day calendar.

IF (sf(114,19)) THEN

  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = n_leach_gb_acc(l) * 360.0 / REAL(triffid_period)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (STASHwork(si(114,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,114,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag (item 19114)"
    GO TO 9999
  END IF

END IF     !   sf(114,19)

! Item 19 115 NITROGEN MINERAL GAS kgN/m2/360d

IF (sf(115,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = n_gas_gb(l,1)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(115,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,115,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19115)"
    GO TO 9999
  END IF

END IF     !   sf(115,19)

! Item 19 116 INORG NITROGEN LOSS kgN/m2/360d

IF (sf(116,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = n_loss_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(116,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,116,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19116)"
    GO TO 9999
  END IF

END IF     !   sf(116,19)

! Item 19 117 NITROGEN ATM GAS LOSS kgN/m2/360d

IF (sf(117,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = n_loss_gb(l) + n_gas_gb(l,1)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(117,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,117,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19117)"
    GO TO 9999
  END IF

END IF     !   sf(117,19)

! Item 19 118 NITROGEN TOTAL LOSS kgN/m2/360d

IF (sf(118,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = n_loss_gb(l) + n_gas_gb(l,1) +                   &
                       n_leach_gb_acc(l) * 360.0 / REAL(triffid_period)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(118,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,118,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19118)"
    GO TO 9999
  END IF

END IF     !   sf(118,19)

! Item 19 119 HARVEST N ON PFTS kgN/m2/360d

IF (sf(119,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,119,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19119)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = harvest_n_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(119,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,119,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19119)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(119,19)

! Item 19 120 HARVEST N (GBM) kgN/m2/360d

IF (sf(120,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = harvest_n_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(120,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,120,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19120)"
    GO TO 9999
  END IF

END IF     !   sf(120,19)

! Item 19 121 NITROGEN LUC ON PFTS kgN/m2/360d

IF (sf(121,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,121,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19121)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = lit_n_ag_pft_diag(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(121,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,121,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19121)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(121,19)

! Item 19 122 NITROGEN LUC (GBM) kgN/m2/360d

IF (sf(122,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = n_luc(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(122,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,122,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19122)"
    GO TO 9999
  END IF

END IF     !   sf(122,19)

! Item 19 123 NITROGEN LITTER ON PFTS kgN/m2/360d

IF (sf(123,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,123,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19123)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = lit_n_pft_diag(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(123,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,123,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19123)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(123,19)

! Item 19 124 NITROGEN LITTER (GBM) kgN/m2/360d

IF (sf(124,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = lit_n_t_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(124,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,124,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19124)"
    GO TO 9999
  END IF

END IF     !   sf(124,19)

! Item 19 125 NITROGEN FERTILISER ON PFTS kgN/m2/360d

IF (sf(125,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,125,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19125)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = n_fertiliser_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(125,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,125,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19125)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(125,19)

! Item 19 126 NITROGEN FERTILISER (GBM) kgN/m2/360d

IF (sf(126,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = n_fertiliser_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(126,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,126,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19126)"
    GO TO 9999
  END IF

END IF     !   sf(126,19)

! Item 19 127 LANDUSE N TO SOIL ON PFTS kg/m2/360d

IF (sf(127,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,127,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19127)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = root_abandon_n_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(127,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,127,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19127)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(127,19)

! Item 19 128 LANDUSE N TO SOIL (GBM) kgC/m2/360d

IF (sf(128,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = root_abandon_n_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(128,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,128,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19128)"
    GO TO 9999
  END IF

END IF     !   sf(128,19)

! Item 19 131 NITROGEN STEM/WOOD kgN/m2

IF (sf(131,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,131,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19131)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = n_stem_trif_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(131,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,131,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19131)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(131,19)

! Item 19 132 NITROGEN LEAF kgN/m2

IF (sf(132,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,132,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19132)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = n_leaf_trif_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(132,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,132,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19132)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(132,19)

! Item 19 133 NITROGEN ROOT kgN/m2

IF (sf(133,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,133,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19133)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = n_root_trif_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(133,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,133,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19133)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(133,19)

! Item 19 134 NITROGEN LEAF LABILE kgN/m2

IF (sf(134,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,134,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19134)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = n_leaf_labile_trif_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(134,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,134,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19134)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(134,19)

! Item 19 135 NITROGEN LEAF ALLOCATED kgN/m2

IF (sf(135,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,135,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19135)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = n_leaf_alloc_trif_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(135,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,135,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19135)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(135,19)

! Item 19 136 NITROGEN VEG PFT kgN/m2

IF (sf(136,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,136,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19136)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = n_veg_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(136,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,136,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19136)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(136,19)

! Item 19 137 NITROGEN VEG (GBM) kgN/m2

IF (sf(137,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = n_veg_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(137,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,137,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19137)"
    GO TO 9999
  END IF

END IF     !   sf(137,19)

! Item 19 141 NITROGEN SOIL RPM kgN/m2

IF (sf(141,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = ns_pool_gb(l,1,1)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(141,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,141,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19141)"
    GO TO 9999
  END IF

END IF     !   sf(141,19)

! Item 19 142 NITROGEN SOIL DPM kgN/m2

IF (sf(142,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = ns_pool_gb(l,1,2)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(142,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,142,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19142)"
    GO TO 9999
  END IF

END IF     !   sf(142,19)

! Item 19 143 NITROGEN SOIL BIO kgN/m2

IF (sf(143,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = ns_pool_gb(l,1,3)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(143,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,143,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19143)"
    GO TO 9999
  END IF

END IF     !   sf(143,19)

! Item 19 144 NITROGEN SOIL HUM kgN/m2

IF (sf(144,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = ns_pool_gb(l,1,4)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(144,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,144,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19144)"
    GO TO 9999
  END IF

END IF     !   sf(144,19)

! Item 19 145 NITROGEN SOIL TOTAL kgN/m2

IF (sf(145,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = SUM(ns_pool_gb(l,1,:))
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(145,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,145,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19145)"
    GO TO 9999
  END IF

END IF     !   sf(145,19)

! Item 19 146 INORGANIC NITROGEN kgN/m2

IF (sf(146,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = n_inorg_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(146,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,146,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19146)"
    GO TO 9999
  END IF

END IF     !   sf(146,19)

! Item 19 147 NITROGEN TOTAL ECOSYSTEM kgN/m2

IF (sf(147,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) =                                                  &
         SUM(ns_pool_gb(l,1,:)) + n_inorg_gb(l) + n_veg_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(147,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,147,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19147)"
    GO TO 9999
  END IF

END IF     !   sf(147,19)

! Item 19 152 N DEMAND GROWTH kgN/m2/360d

IF (sf(152,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,152,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19152)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = n_demand_growth_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(152,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,152,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19152)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(152,19)

! Item 19 153 N DEMAND SPREAD kgN/m2/360d

IF (sf(153,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,153,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19153)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = n_demand_spread_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(153,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,153,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19153)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(153,19)

! Item 19 154 N DEMAND TOTAL kgN/m2/360d

IF (sf(154,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,154,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19154)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = n_demand_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(154,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,154,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19154)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(154,19)

! Item 19 155 N DEMAND TOTAL (GBM) kgN/m2/360D

IF (sf(155,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = n_demand_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(155,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,155,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19155)"
    GO TO 9999
  END IF

END IF     !   sf(155,19)

! Item 19 156 N UPTAKE TURNOVER kgN/m2/360d

IF (sf(156,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,156,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19156)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = n_demand_lit_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(156,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,156,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19156)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(156,19)

! Item 19 157 N UPTAKE GROWTH kgN/m2/360d

IF (sf(157,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,157,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19157)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = n_uptake_growth_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(157,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,157,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19157)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(157,19)

! Item 19 158 N UPTAKE SPREAD kgN/m2/360d

IF (sf(158,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,158,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19158)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = n_uptake_spread_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(158,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,158,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19158)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(158,19)

! Item 19 159 N UPTAKE TOTAL kgN/m2/360d

IF (sf(159,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                                 &
       stlist(1,stindex(1,159,19,im_index)),                            &
       pllpft,stash_pseudo_levels,num_stash_pseudo,                     &
       icode,cmessage)
  IF (icode >  0) THEN
    cmessage=                                                         &
  "dagveg  : error in set_pseudo_list(Item 19159)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1, rows
        DO i = 1, row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1, land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = n_uptake_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (stashwork(si(159,19,im_index) + (pslevel_out-1)    &
           * row_length * rows),interp_data,                            &
           row_length,rows,0,0,0,0, at_extremity,                       &
           atmos_im,19,159,                                             &
           icode,cmessage)

      IF (icode  >   0) THEN
        cmessage="Error in copydiag( Item 19159)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(159,19)

! Item 19 160 N UPTAKE TOTAL (GBM) kgN/m2/360D

IF (sf(160,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = n_uptake_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(160,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,160,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19160)"
    GO TO 9999
  END IF

END IF     !   sf(160,19)

! Item 19 161 N SOIL DECOMPOSITION RATE MODIFIER

IF (sf(161,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = fn_gb(l,1)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(161,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,161,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19161)"
    GO TO 9999
  END IF

END IF     !   sf(161,19)

! Item 19 162 N IMMOBILIS POTEN DPM kgN/m2/360D

IF (sf(162,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = immob_n_pot_gb(l,1,1)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(162,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,162,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19162)"
    GO TO 9999
  END IF

END IF     !   sf(162,19)

! Item 19 163 N IMMOBILIS POTEN RPM kgN/m2/360D

IF (sf(163,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = immob_n_pot_gb(l,1,2)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(163,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,163,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19163)"
    GO TO 9999
  END IF

END IF     !   sf(163,19)

! Item 19 164 N IMMOBILIS POTEN BIO kgN/m2/360D

IF (sf(164,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = immob_n_pot_gb(l,1,3)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(164,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,164,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19164)"
    GO TO 9999
  END IF

END IF     !   sf(164,19)

! Item 19 165 N IMMOBILIS POTEN HUM kgN/m2/360D

IF (sf(165,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = immob_n_pot_gb(l,1,4)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(165,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,165,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19165)"
    GO TO 9999
  END IF

END IF     !   sf(165,19)

! Item 19 166 N IMMOBILIS POTEN TOT kgN/m2/360D

IF (sf(166,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = immob_n_pot_gb(l,1,5)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(166,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,166,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19166)"
    GO TO 9999
  END IF

END IF     !   sf(166,19)

! Item 19 167 N IMMOBILIS DPM kgN/m2/360D

IF (sf(167,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = immob_n_gb(l,1,1)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(167,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,167,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19167)"
    GO TO 9999
  END IF

END IF     !   sf(167,19)

! Item 19 168 N IMMOBILIS RPM kgN/m2/360D

IF (sf(168,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = immob_n_gb(l,1,2)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(168,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,168,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19168)"
    GO TO 9999
  END IF

END IF     !   sf(168,19)

! Item 19 169 N IMMOBILIS BIO kgN/m2/360D

IF (sf(169,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = immob_n_gb(l,1,3)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(169,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,169,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19169)"
    GO TO 9999
  END IF

END IF     !   sf(169,19)

! Item 19 170 N IMMOBILIS HUM kgN/m2/360D

IF (sf(170,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = immob_n_gb(l,1,4)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(170,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,170,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19170)"
    GO TO 9999
  END IF

END IF     !   sf(170,19)

! Item 19 171 N IMMOBILIS TOT kgN/m2/360D

IF (sf(171,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = immob_n_gb(l,1,5)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(171,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,171,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19171)"
    GO TO 9999
  END IF

END IF     !   sf(171,19)

! Item 19 172 N MINERALIS POTEN DPM kgN/m2/360D

IF (sf(172,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = minl_n_pot_gb(l,1,1)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(172,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,172,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19172)"
    GO TO 9999
  END IF

END IF     !   sf(172,19)

! Item 19 173 N MINERALIS POTEN RPM kgN/m2/360D

IF (sf(173,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = minl_n_pot_gb(l,1,2)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(173,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,173,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19173)"
    GO TO 9999
  END IF

END IF     !   sf(173,19)

! Item 19 174 N MINERALIS POTEN BIO kgN/m2/360D

IF (sf(174,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = minl_n_pot_gb(l,1,3)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(174,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,174,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19174)"
    GO TO 9999
  END IF

END IF     !   sf(174,19)

! Item 19 175 N MINERALIS POTEN HUM kgN/m2/360D

IF (sf(175,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = minl_n_pot_gb(l,1,4)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(175,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,175,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19175)"
    GO TO 9999
  END IF

END IF     !   sf(175,19)

! Item 19 176 N MINERALIS POTEN TOT kgN/m2/360D

IF (sf(176,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = minl_n_pot_gb(l,1,5)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(176,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,176,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19176)"
    GO TO 9999
  END IF

END IF     !   sf(176,19)

! Item 19 177 N MINERALIS DPM kgN/m2/360D

IF (sf(177,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = minl_n_gb(l,1,1)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(177,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,177,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19177)"
    GO TO 9999
  END IF

END IF     !   sf(177,19)

! Item 19 178 N MINERALIS RPM kgN/m2/360D

IF (sf(178,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = minl_n_gb(l,1,2)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(178,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,178,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19178)"
    GO TO 9999
  END IF

END IF     !   sf(178,19)

! Item 19 179 N MINERALIS BIO kgN/m2/360D

IF (sf(179,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = minl_n_gb(l,1,3)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(179,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,179,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19179)"
    GO TO 9999
  END IF

END IF     !   sf(179,19)

! Item 19 180 N MINERALIS HUM kgN/m2/360D

IF (sf(180,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = minl_n_gb(l,1,4)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(180,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,180,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19180)"
    GO TO 9999
  END IF

END IF     !   sf(180,19)

! Item 19 181 N MINERALIS TOT kgN/m2/360D

IF (sf(181,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = minl_n_gb(l,1,5)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(181,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,181,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19181)"
    GO TO 9999
  END IF

END IF     !   sf(181,19)

! Stashcode (19,182): Gross Primary Productivity on PFTs (KGC/M2/YR)
! GPP on PFTs: total amount of Carbon taken up during photosynthesis, some of 
! which will be respired back into the atmosphere, so the net plant carbon 
! uptake is the Net Primary Productivity. Computed by the surface code but 
! copied here for output via section 19 to aid carbon budget calculations on 
! TRIFFID timesteps. The * 360.0/REAL(triffid_period) converts the 
! accumulation since the last TRIFFID call to units of kgC/m2/yr; TRIFFID 
! is constrained to a 360 day calendar.

IF (sf(182,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                               &
                       stlist(1,stindex(1,182,19,im_index)),          &
                       pllpft,stash_pseudo_levels,num_stash_pseudo,   &
                       icode,cmessage)
  IF (icode > 0) THEN
    cmessage=                                                       &
    "dagveg  : error in set_pseudo_list(item 182 = gpp_pft_acc)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1,rows
        DO i = 1,row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1,land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = gpp_pft_acc(l,pslevel_out) *         &
                           360.0 / REAL(triffid_period)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (STASHwork(si(182,19,im_index) +              &
                    (pslevel_out-1) * row_length * rows),       &
                     interp_data,                                 &
                     row_length,rows,0,0,0,0, at_extremity,       &
                     atmos_im,19,182,                             &
                     icode,cmessage)

      IF (icode > 0) THEN
        cmessage="Error in copydiag (item 19182)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(182,19)


! Stashcode (19,183): Gross Primary Productivity, GBM (KGC/M2/YR)
! GPP (gridbox mean): total amount of Carbon taken up during photosynthesis, 
! some of which will be respired back into the atmosphere, so the net plant 
! carbon uptake is the Net Primary Productivity. Computed by the surface code 
! but copied here for output via section 19 to aid carbon budget calculations 
! on TRIFFID timesteps. The * 360.0/REAL(triffid_period) converts the 
! accumulation since the last TRIFFID call to units of kgC/m2/yr; TRIFFID is 
! constrained to a 360 day calendar.

IF (sf(183,19)) THEN

  DO j = 1,rows
    DO i = 1,row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1,land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = gpp_gb_acc(l) * 360.0 / REAL(triffid_period)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (STASHwork(si(183,19,im_index)),interp_data,          &
                 row_length,rows,0,0,0,0, at_extremity,               &
                 atmos_im,19,183,                                     &
                 icode,cmessage)

  IF (icode > 0) THEN
    cmessage="Error in copydiag (item 19183)"
    GO TO 9999
  END IF

END IF     !   sf(183,19)


! Stashcode (19,184): Actual plant respiration on PFTs (KGC/M2/YR)
! This is plant respiration adjusted to take into account the exudates term, 
! i.e. the amount by which potential Net Primary Productivity is reduced to 
! account for limitation by Nitrogen availablity. 
 
IF (sf(184,19)) THEN
  ! DEPENDS ON: set_pseudo_list
  CALL set_pseudo_list(npft,len_stlist,                              &
                       stlist(1,stindex(1,184,19,im_index)),         &
                       pllpft,stash_pseudo_levels,num_stash_pseudo,  &
                       icode,cmessage)
  IF (icode > 0) THEN
    cmessage=                                                      &
    "dagveg: error in set_pseudo_list(item 184 = resp_p_actual_pft)"
    GO TO 9999
  END IF
  pslevel_out = 0

  DO pslevel = 1,npft
    IF (pllpft(pslevel)) THEN
      pslevel_out = pslevel_out + 1
      DO j = 1,rows
        DO i = 1,row_length
          interp_data(i,j) = rmdi
        END DO
      END DO

      DO l = 1,land_pts
        j = (land_index(l) - 1) / row_length + 1
        i = land_index(l) - (j-1) * row_length
        interp_data(i,j) = resp_p_actual_pft(l,pslevel_out)
      END DO

      ! DEPENDS ON: copydiag
      CALL copydiag (STASHwork(si(184,19,im_index) +             &
                    (pslevel_out-1) * row_length * rows),      &
                     interp_data,                                &
                     row_length,rows,0,0,0,0, at_extremity,      &
                     atmos_im,19,184,                            &
                     icode,cmessage)

      IF (icode > 0) THEN
        cmessage="Error in copydiag (item 19184)"
        GO TO 9999
      END IF
    END IF
  END DO

END IF     !   sf(184,19)


! Stashcode (19,185): Actual plant respiration, gridbox mean  (KGC/M2/YR)
! This is plant respiration adjusted to take into account the exudates term, 
! i.e. the amount by which potential Net Primary Productivity is reduced to 
! account for limitation by Nitrogen availablity. 

IF (sf(185,19)) THEN
  DO j = 1,rows
    DO i = 1,row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1,land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = resp_p_actual_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (STASHwork(si(185,19,im_index)),interp_data,          &
                 row_length,rows,0,0,0,0, at_extremity,               &
                 atmos_im,19,185,                                     &
                 icode,cmessage)

  IF (icode > 0) THEN
    cmessage="Error in copydiag (item 19185)"
    GO TO 9999
  END IF

END IF     !   sf(185,19)

! Item 19 186 veg nitrogen error in triffid kg m-2

IF (sf(186,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = cnsrv_vegn_triffid_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(186,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,186,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19186)"
    GO TO 9999
  END IF

END IF     !   sf(186,19)

! Item 19 187 soil nitrogen error in triffid kg m-2

IF (sf(187,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = cnsrv_soiln_triffid_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(187,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,187,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19187)"
    GO TO 9999
  END IF

END IF     !   sf(49,19)

! Item 19 188 soil nitrogen error in triffid kg m-2

IF (sf(188,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = cnsrv_n_inorg_triffid_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(188,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,188,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19188)"
    GO TO 9999
  END IF

END IF     !   sf(188,19)

! Item 19 189 nitrogen error in triffid kg m-2

IF (sf(189,19)) THEN
  DO j = 1, rows
    DO i = 1, row_length
      interp_data(i,j) = rmdi
    END DO
  END DO

  DO l = 1, land_pts
    j = (land_index(l) - 1) / row_length + 1
    i = land_index(l) - (j-1) * row_length
    interp_data(i,j) = cnsrv_nitrogen_triffid_gb(l)
  END DO

  ! DEPENDS ON: copydiag
  CALL copydiag (stashwork(si(189,19,im_index)),interp_data,            &
       row_length,rows,0,0,0,0, at_extremity,                           &
       atmos_im,19,189,                                                 &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( Item 19189)"
    GO TO 9999
  END IF

END IF     !   sf(189,19)





9999 CONTINUE
IF (icode /= 0) THEN

  CALL ereport(routinename,icode,cmessage)
END IF

IF (lhook) CALL dr_hook(routinename,zhook_out,zhook_handle)
RETURN
END SUBROUTINE diagnostics_veg
#endif












!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237] 
!******************************COPYRIGHT**************************************

MODULE overbank_inundation_mod

!-----------------------------------------------------------------------------
! Description:
!   Contains river overbank inundation variables and switches
!
!  Code Owner: Please refer to ModuleLeaders.txt
!  This file belongs in section: Hydrology
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE missing_data_mod, ONLY: imdi, rmdi

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Overbank inundation parameters
!-----------------------------------------------------------------------------

REAL ::                                                                       &
   riv_a = rmdi,                                                              &
        ! Parameter in Leopold & Maddock (1953: eqn1)
   riv_b = rmdi,                                                              &
        ! Parameter in Leopold & Maddock (1953: eqn1)
   riv_c = rmdi,                                                              &
        ! Parameter in Leopold & Maddock (1953: eqn2)
   riv_f = rmdi,                                                              &
        ! Parameter in Leopold & Maddock (1953: eqn2)
   coef_b = rmdi,                                                             &
        ! Parameter to calculate the bankflow discharge power-law
        ! relationship from "Flood Modeling, Prediction and Mitigation" 
        ! by Şen 2018.
   exp_c = rmdi,                                                              &
        ! Parameter to calculate the bankflow discharge power-law
        ! relationship from "Flood Modeling, Prediction and Mitigation" 
        ! by Şen 2018.
   ent_ratio = rmdi
        ! Entrenchment ratio (Rosgen 1994). Ratio of the width of
        ! flood-prone area to surface width of bankfull channel.
        ! The flood-prone area width is measured at the elevation that
        ! corresponds to twice the maximum depth of the bankfull channel.

LOGICAL ::                                                                    &
   l_riv_hypsometry = .TRUE.,                                                 &
                            ! TRUE indicates to calculate inundated area  
                            ! using hypsometry, which requires additional
                            ! ancillaries logn_mean and logn_stdev
                            ! FALSE indicates to use a simpler width scaling
                            ! method (generally only to be used for testing
                            ! when those ancillaries are not available).
   use_rosgen = .FALSE.
                            ! Modify floodplain width using the Rosgen
                            ! entrenchment ratio (n.b. only for local use)

!-----------------------------------------------------------------------------
! Array variables defined on land points updated in overbank inundation
!-----------------------------------------------------------------------------

REAL, ALLOCATABLE ::                                                          &
  frac_fplain_lp(:)
                          ! fraction of gridcell that is temporarily-
                          ! inundated open water surface (for overbank
                          ! inundation)

!-----------------------------------------------------------------------------
! Array variables defined on full 2D rivers grid (as read in from ancillary)
!----------------------------------------------------------------------------

REAL, ALLOCATABLE ::                                                          &
  logn_mean(:,:),                                                             &
                          ! ln(mean(elevation - elev_min)) for each gridcell
                          ! (elevation is relative to minimum elevation) 
                          !in grid box)
  logn_stdev(:,:)
                          ! ln(SD(elevation - elev_min)) for each gridcell

!-----------------------------------------------------------------------------
! Other array variables
!----------------------------------------------------------------------------

REAL, ALLOCATABLE ::                                                          &
  logn_mean_rp(:),                                                            &
           ! as logn_mean but on river points
  logn_stdev_rp(:),                                                           &
           ! as logn_stdev but on river points
  qbf(:),                                                                     &
           ! bankfull discharge rate in m3/s
  dbf(:),                                                                     &
           ! channel depth when at bankfull discharge rate (m)
  wbf(:),                                                                     &
           ! channel width when at bankfull discharge rate (m)
  frac_fplain_rp(:)
           ! Fraction of gridcell predicted to have overbank inundation
           ! (on rivers grid)

CONTAINS

SUBROUTINE check_jules_overbank()

USE ereport_mod, ONLY: ereport

USE jules_rivers_mod, ONLY: l_rivers, l_riv_overbank

USE jules_vegetation_mod, ONLY: l_triffid

USE logging_mod, ONLY: log_fatal

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_OVERBANK switches for consistency
!
! Current Code Owner: Toby Marthews (CEH)
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER :: errcode

IF ( ( .NOT. l_rivers) .AND. l_riv_overbank)                               &
   errcode = 101
CALL ereport("check_jules_overbank", errcode,                              &
            'l_riv_overbank=T requires l_rivers=T ')

!! Further sanity checks (e.g. parameters within range) possible here....
!! see check_jules_rivers()

END SUBROUTINE check_jules_overbank

END MODULE overbank_inundation_mod

!-----------------------------------------------------------------------------

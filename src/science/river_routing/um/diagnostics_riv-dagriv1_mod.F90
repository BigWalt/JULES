#if defined(UM_JULES)
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: River Routing
MODULE diagnostics_riv_mod

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='DIAGNOSTICS_RIV_MOD'

CONTAINS



SUBROUTINE diagnostics_riv(                                             &
                       row_length, rows                                 &
,                       river_row_length, river_rows                    &
,                      at_extremity                                     &
,                      at_extremity_riv                                 &
,                      riverout                                         &
,                      box_outflow, box_inflow                          &
! Add inland basin outflow to arguments
      ,                      twatstor, inlandout_riv                          &
      ,                      stashwork                                  )

! Description:
!   Calculates river-related diagnostics (held in STASH section 26).
!
! Method:
!   Each diagnostic is simply copied into the STASHwork array
!   to be passed on to STASH for output processing.
!
!   Diagnostics currently available (in order calculated):
!   Item  Description
!    1    River water storage  (river grid)
!    2    gridbox outflow       (   "     )
!    3    gridbox runoff        (   "     )
!    4    coastal outflow       (ATMOS grid)
!         6    inland basin outflow       (river grid)


USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
USE ereport_mod, ONLY: ereport

USE submodel_mod, ONLY: atmos_im
USE stash_array_mod, ONLY: sf, si

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

LOGICAL ::                                                              &
  at_extremity(4)                                                       &
                   ! Indicates if this processor is at north,
!     &                   ! south, east or west of the processor grid
      ,  at_extremity_riv(4) ! Indicates if this processor is at north,
!     &                   ! south, east or west of the processor grid
! Arguments with Intent IN. ie: Input variables.

INTEGER ::                                                              &
  row_length                                                            &
                   ! number of points on a row
, rows                                                                  &
                   ! number of rows in a theta field
, river_row_length                                                      &
                    ! river row length
, river_rows        ! river rows

LOGICAL ::                                                              &
 l_rivers           ! IN rivers switched on if .TRUE.

REAL ::                                                                 &
 riverout(row_length, rows)                                             &
,box_outflow(river_row_length, river_rows)                              &
,box_inflow(river_row_length, river_rows)                               &
,twatstor(river_row_length, river_rows)                                 &

! Declare inland basin outflow variable
      ,inlandout_riv(river_row_length, river_rows)


! Local variables

INTEGER ::                                                              &
  i, j, k, l                                                            &
,    icode                ! Return code  =0 Normal exit  >1 Error

INTEGER, PARAMETER :: Sect = 26  !  Section No for RR diagnostics

CHARACTER(LEN=errormessagelength) :: cmessage
CHARACTER(LEN=*) :: RoutineName
PARAMETER ( RoutineName='DIAGNOSTICS_RIV')

INTEGER ::                                                              &
  im_index        ! internal model index

REAL ::                                                                 &
  interp_data(row_length,rows)

! Diagnostic variables
REAL ::                                                                &
 STASHwork( * )    ! STASH workspace

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

! ------------------------------------------------------------------
! Section 1.  Initialisation.
! ------------------------------------------------------------------

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,          &
                        zhook_handle)

icode = 0 ! Initialise error status
im_index = 1

! --------------------------------------------------------------------
! River outflow (at seapoints)
! --------------------------------------------------------------------
! Item 26 004 riverout
IF (sf(004,sect)) THEN
  ! DEPENDS ON: copydiag
  CALL copydiag(STASHwork(si(004,sect,im_index)),riverout,             &
       row_length,rows,0,0,0,0, at_extremity,                          &
       atmos_im,sect,004,                                              &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( item 004)"
    GO TO 9999
  END IF
END IF

! ----------------------------------------------------------------------
! River outflow (at seapoints)
! -------------------------------------------------------------------
! Item 26 001 riverout
IF (sf(001,sect)) THEN
  ! DEPENDS ON: copydiag
  CALL copydiag(STASHwork(si(001,sect,im_index)),twatstor,             &
       river_row_length,river_rows,0,0,0,0, at_extremity_riv,          &
       atmos_im,sect,001,                                              &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( item 001)"
    GO TO 9999
  END IF
END IF
!----------------------------------------------------------------------
! --------------------------------------------------------------------
! River outflow (at seapoints)
! --------------------------------------------------------------------
! Item 26 002 riverout
IF (sf(002,sect)) THEN
  ! DEPENDS ON: copydiag
  CALL copydiag(STASHwork(si(002,sect,im_index)),box_outflow,          &
       river_row_length,river_rows,0,0,0,0, at_extremity_riv,          &
       atmos_im,sect,002,                                              &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( item 002)"
    GO TO 9999
  END IF
END IF
!-------------------------------------------------------------------
! ------------------------------------------------------------------
! River outflow (at seapoints)
! ------------------------------------------------------------------
! Item 26 003 riverout
IF (sf(003,sect)) THEN
  ! DEPENDS ON: copydiag
  CALL copydiag(STASHwork(si(003,sect,im_index)), box_inflow,          &
       river_row_length,river_rows,0,0,0,0, at_extremity_riv,          &
       atmos_im,sect,003,                                              &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( item 003)"
    GO TO 9999
  END IF
END IF
!---------------------------------------------------------------------

! Output inland basin outflow on TRIP grid

! ------------------------------------------------------------------
! Inland basin outflow
! ------------------------------------------------------------------
! Item 26 006 inlandout_riv
IF (sf(006,sect)) THEN
  ! DEPENDS ON: copydiag
  CALL copydiag(STASHwork(si(006,sect,im_index)),                      &
   inlandout_riv,                                                      &
       river_row_length,river_rows,0,0,0,0, at_extremity_riv,          &
       atmos_im,sect,006,                                              &
       icode,cmessage)

  IF (icode  >   0) THEN
    cmessage="Error in copydiag( item 006)"
    GO TO 9999
  END IF
END IF
!---------------------------------------------------------------------

9999 CONTINUE
IF (icode /= 0) THEN

  CALL Ereport(RoutineName,icode,Cmessage)
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,         &
                        zhook_handle)
RETURN
END SUBROUTINE diagnostics_riv

END MODULE diagnostics_riv_mod
#endif

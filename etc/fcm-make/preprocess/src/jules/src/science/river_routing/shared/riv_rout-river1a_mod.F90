










! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************

MODULE riv_rout_mod_1A

USE routedbl_mod, ONLY: routedbl

USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='RIV_ROUT_MOD_1A'

CONTAINS


SUBROUTINE riv_rout_1a(global_row_length, global_rows,                        &
     surf_runoffin, sub_runoffin, row_length, rows, invert_atmos, xua,        &
     yva, river_row_length, river_rows, frac, rmdi, regrid, ru,               &
     ratmed, dt, cyclic_trip, gridbox_areas, global_trip, invert_trip,        &
     twatstor,                                                                &
     riverdir, riverseq, riverout_trip, runoff_trip_kgs, riverout_inv,        &
     inlandout_trip, inlandout_atm, g_river_row_length, g_river_rows,         &
     global_land_frac,first, cmessage)

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
USE conversions_mod, ONLY: rsec_per_day
USE riv_directions, ONLY: open_sea_dirs, river_outflow_dir,                 &
     inland_outflow_dir
USE rivers_utils, ONLY:                                                    &
   rivers_route_regrid, rivers_route_regrid_invert

IMPLICIT NONE

!-----------------------------------------------------------------------------
!
! Subroutine: RIV_ROUT_1A
!
! Description:
!  Perform the river routing of surface and sub-surface runoff in parallel
!  using the Total Runoff Integrating Pathways model (TRIP)
!
! Note:
!   This code was transferred from the UM repository at UM vn9.2 / JULES vn 4.1.
!   Future developments will supercede these subroutines, and as such they
!   should be considered deprecated. They will be retained in the codebase to
!   maintain backward compatibility with functionality prior to
!   UM vn10.0 / JULES vn 4.2, until such time as they become redundant.
!
! Method:
!   This routine regrids the total runoff to the river routing grid
!   and passes it to the TRIP routines to be routed, then maps the
!   outflow to seapoints on the ATMOS grid and passes back the
!   updated water storage. Regridding from the ATMOS to the TRIP grid
!   uses area weighting, whilst outflows (kg/s) are mapped back onto
!   the ATMOS grid points with which they overlap the most.
!   This is then converted to the usual kg/m2/s using the ATMOS
!   gridbox areas.
!
! Current Code Owner: Please refer to the JULES science module leaders
!   This file belongs in module: Hydrology
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

INTEGER, INTENT(IN) ::                                                      &

       rows                                                                 &
     ! atmos rows
     , row_length                                                           &
     ! atmos row length
     , global_rows                                                          &
     ! total rows in global grid across all procs
     , global_row_length                                                    &
     ! total row length in global grid across all procs
     , river_row_length                                                     &
     ! river row length
     , river_rows                                                           &
     ! river rows
     , g_river_row_length                                                   &
     ! global river row length
     , g_river_rows
     ! global river rows

REAL, INTENT(IN) ::                                                         &

       surf_runoffin(row_length, rows)                                      &
     ! atmos grid subdomain surface runoffin
     , sub_runoffin(row_length, rows)                                       &
     ! atmos grid subdomain surface runoffin
     , frac(row_length, rows)                                               &
     ! land fractions
     , riverdir(river_row_length, river_rows)                               &
     ! values of river direction
     , riverseq(river_row_length, river_rows)                               &
     ! values of river sequence
     , gridbox_areas(row_length, rows)                                      &
     ! atmos gridbox areas
     , global_land_frac(global_row_length, global_rows)
     ! land seak fraction across all processes

REAL, INTENT(INOUT) ::                                                      &

       riverout_inv(row_length, rows)                                       &
     ! river flow out from each gridbox
     , riverout_trip(river_row_length, river_rows)                          &
     ! gridbox outflow on river grid (Kg/s)
     , runoff_trip_kgs(river_row_length, river_rows)                        &
     ! gridbox runoff on river grid
     , inlandout_trip(river_row_length, river_rows)                         &
     ! trip outflow from inland basins on trip grid (kg/s)
     , twatstor(river_row_length, river_rows)                               &
     ! initial water storage file
     , inlandout_atm(row_length, rows)
     ! trip outflow from inland basins on atmos grid kg/m2/s

REAL, INTENT(IN) ::                                                         &

       xua(0:global_row_length)                                             &
     ! atmos grid global domain lat coords
     , yva(0:global_rows)
     ! atmos grid global domain long coords

LOGICAL, INTENT(IN) ::                                                      &

       invert_atmos                                                         &
     ! invert rows
     , regrid                                                               &
     ! whether to regrid atmos to river grid
     , cyclic_trip                                                          &
     ! indicates if trips grid is cyclic
     , global_trip                                                          &
     ! indicates if atmos grid is cyclic
     , invert_trip

LOGICAL, INTENT(IN) :: first
! true if first entry into this routine

REAL, INTENT(IN) ::                                                         &

       rmdi                                                                 &
     ! missing value indicator
     , ru                                                                   &
     ! effective river flow velocity (m/s)
     , ratmed                                                               &
     ! river meander ratio
     , dt
     ! river routing time step (s)

CHARACTER(LEN=errormessagelength), INTENT(OUT) :: cmessage
! error message

! local variables

REAL :: runoff_trip(river_row_length, river_rows)                              &
               ! total runoff regridded to trip grid
     , trip_outflow(river_row_length, river_rows)
               ! total outflow on river grid (including inland basins)

REAL, DIMENSION(:), ALLOCATABLE ::                                          &
       weight
! for weighting of source grid point
INTEGER, DIMENSION(:), ALLOCATABLE ::                                       &
      index_arav
! indexes global src grid points

REAL :: runoffin(row_length,rows)                                              &
                              ! atmos runoffin with halos for swap bounds
     , riverout(row_length,rows)                                            &
     , inlandout(row_length, rows)

INTEGER ::                                                                  &

      count_tr(river_row_length, river_rows)                                &
                              ! no. of source boxes in target
     ,base_tr(river_row_length, river_rows)                                 &
     ,trivdir(river_row_length, river_rows)                                 &
                              ! river direction file in integers
     ,trivseq(river_row_length, river_rows)                                 &
                              ! river sequence file in integers
     , trivdir_inv(river_row_length, river_rows)
LOGICAL :: amask_all(global_row_length, global_rows)                           &
                              ! do not mask any atmos points when
                              ! regridding runoff to trip grid and
                              ! inland basin flow back to atm grid
     , trmask_all(river_row_length, river_rows)                             &
                              ! do not mask any TRIP points when
                              ! regridding runoff to trip grid
     , trmask_riverout(river_row_length, river_rows)                        &
                              ! mask off all trip points other than
                              ! river outflow points, sea points and
                              ! inland basin flow points for
                              ! mapping river outflow back to atm grid
     , trmask_inlandout(river_row_length, river_rows)                       &
                              ! mask off all trip points other than
                              ! inland basin points when mapping
                              ! inland basin flow back to atm grid
     , amask_sea_global(global_row_length, global_rows)
                              ! global mask of atmospheric points with
                              ! no sea fraction when mapping river
                              ! outflow back to atm grid

INTEGER :: lenl
! the maximum number of source boxes in weights for all targets

CHARACTER(LEN=*) :: routinename
PARAMETER ( routinename='RIV_ROUT_1A')

INTEGER ::                                                                  &
       local_rstart_rl, local_rstart_rows                                   &
     , local_rstop_rl, local_rl, local_pstop_rl                             &
     , local_rstop_rows, stop_phi, start_lambda, local_rows                 &
     , local_pstart_row, local_pstart_rl, local_pstop_row

REAL ::                                                                     &

       yut(river_rows+1)                                                    &
                              ! TRIP latitude coordinates (limited)
     , xut(0:river_row_length)
! TRIP longitude coordinates (limited)
REAL ::                                                                     &
      trip_areas(river_row_length, river_rows)

INTEGER :: i, j, k, l
! loop counter variables

INTEGER :: jmax
! max. no. of j values for TRIP

INTEGER :: ndev               ! No. of TRIP timesteps/day
INTEGER :: error

! debugging variables
INTEGER :: ierror, my_comm, info, icode
REAL :: total_weight_sum, weight_sum

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

inlandout = 0.0
inlandout_trip = 0.0
inlandout_atm = 0.0
error = 0
info = 0

! calculate runoff

! Sum the surface and subsurface runoffs
! Sum the two types of runoff

DO j = 1, rows
  DO i = 1, row_length
    IF (surf_runoffin(i,j) /= rmdi                                           &
                .AND. sub_runoffin(i,j) /= rmdi) THEN
      runoffin(i,j) = (surf_runoffin(i,j)                                   &
                                     + sub_runoffin(i,j)) * frac(i,j)
    ELSE
      runoffin(i,j) = rmdi
    END IF
  END DO
END DO

IF (regrid) THEN


  lenl = (global_row_length + river_row_length) * (global_rows + river_rows)
  CALL rivers_route_regrid( lenl, runoffin, runoff_trip )


ELSE

  DO j = 1, river_rows
    DO i = 1, river_row_length
      runoff_trip(i,j) = runoffin(i,j)
    END DO
  END DO

END IF ! regridding

DO j = 1,river_rows
  DO i = 1, river_row_length
    ! Change the runoff from Kg/m2/sec (mm/sec) to mm/day for TRIP routines
    ! Allow Runoff to be at TRIP seapoints for conserve.

    IF (runoff_trip(i,j) >  rmdi) THEN
      runoff_trip(i,j) = runoff_trip(i,j) * rsec_per_day
    ELSE
      runoff_trip(i,j) = 0.0
    END IF

  END DO
END DO


! *****************************************************************
! Change river direction and sequence arrays to integer as expected by
! river routing scheme (including missing data points -> INT(rmdi))

DO j = 1, river_rows
  DO i = 1, river_row_length
    trivdir(i,j) = INT(riverdir(i,j))
    trivseq(i,j) = INT(riverseq(i,j))
  END DO
END DO

! Set the number of river routing timesteps per day to 1
ndev = 1


jmax = river_rows

! need to pass offset values for routine, remember to lat rotate grid
CALL routedbl(runoff_trip, ru, ratmed, ndev, dt                           &
   , river_row_length, river_rows, trivdir, trivseq, twatstor             &
   , jmax, rmdi, riverout_trip, runoff_trip_kgs, trip_areas               &
   , 0, 0)

! Invert the TRIP OUTFLOW for 'mapping' and remove TRIP_rmdi values

! Copy non-missing data outflow values at river mouths, sea points
! and inland basin flow points ahead of mapping onto ATMOS grid.
! All other points are replaced with zeros.

DO j = 1, river_rows
  DO i = 1, river_row_length
    IF ( (ANY(trivdir(i,j) == open_sea_dirs)                                &
         .OR. trivdir(i,j) == river_outflow_dir                             &
         .OR. trivdir(i,j) == inland_outflow_dir     )                      &
         .AND. riverout_trip(i,j) /= rmdi ) THEN
      trip_outflow(i,j) = riverout_trip(i,j)
    ELSE
      trip_outflow(i,j) = 0.0
    END IF
  END DO
END DO

IF (regrid) THEN

  ! Prepare for mapping and count river mouths on TRIP grid as sea.
  ! Instead of regridding, use count_tr, base_tr and weight to 'map' the
  ! TRIP sea points onto the ATMOS grid where the major part of the TRIP
  ! box lies using do_map_max

  DO j = 1, river_rows
    DO i = 1, river_row_length
      trmask_riverout(i,j) = (ALL(trivdir(i,j) /= open_sea_dirs)            &
                             .AND. trivdir(i,j) /= river_outflow_dir        &
                             .AND. trivdir(i,j) /= inland_outflow_dir)
    END DO
  END DO

  riverout = 0
  count_tr = 0
  base_tr = 0

  lenl = (global_row_length + river_row_length) * (global_rows + river_rows)


  ! Second regrid outflow to inland basin outflow points where
  ! the nearest atmospheric grid box has 100% land fraction.
  ! This is achieved by copying the total outflow from trip_outflow
  ! where count_tr=0 (i.e. where outflow was not mapped into river
  ! outflow as amask_sea_global == .TRUE.)

  DO j = 1, river_rows
    DO i = 1, river_row_length

      IF (invert_atmos) THEN
        IF ( (ANY(trivdir(i,j) == open_sea_dirs)                            &
             .OR. trivdir(i,j) == river_outflow_dir                         &
             .OR. trivdir(i,j) == inland_outflow_dir     )                  &
             .AND. count_tr(i,j) == 0 ) THEN

          inlandout_trip(i,j) = trip_outflow(i,j)

        ELSE

          inlandout_trip(i,j) = 0.0

        END IF

      END IF

    END DO
  END DO

  ! Mask out all points that aren't inland basin points
  DO j = 1, river_rows
    DO i = 1, river_row_length
      trmask_inlandout(i,j) = (trivdir(i,j) /= inland_outflow_dir)
    END DO
  END DO

  ! call pre_areaver to regrid inland basin outflow
  ! swap src field data


  lenl = (global_row_length + river_row_length) * (global_rows + river_rows)
  CALL rivers_route_regrid_invert( lenl, riverout_trip, riverout )
  !    CALL rivers_route_regrid_invert( lenl, inlandout_trip, inlandout )

END IF ! end of regridding


! *****************************************************************


! Change river outflow to mm/s (as expected by the ocean later)

DO j = 1,rows
  DO i = 1,row_length
    IF (riverout(i,j) /= rmdi) THEN
      riverout_inv(i,j) =                                                   &
                      riverout(i,j) / gridbox_areas(i,j)
    ELSE
      riverout_inv(i,j) = rmdi
    END IF
  END DO
END DO


! convert inland basin outflow from kg/s to kg/m2/s
! as expected by hydrol7a

DO j = 1, rows
  DO i = 1, row_length

    IF (inlandout(i,j) /= rmdi) THEN
      inlandout_atm(i,j)= inlandout(i,j) / gridbox_areas(i,j)
    ELSE

      !      set to zero, not rmdi since hydrol7a cannot
      !      use rmdi values

      inlandout_atm(i,j) = 0.0
    END IF

  END DO
END DO

! Set non-land points values of inlandout_trip to rmdi post regridding

DO j = 1, river_rows
  DO i = 1, river_row_length

    IF (riverout_trip(i,j) == rmdi) inlandout_trip(i,j) = rmdi

  END DO
END DO

! Set rmdi values in river outflow and runoff on river grid to 0.0 as
! points with rmdi are not constant as runoff greater than 0.0 is
! 'added in' at some TRIP seapoints due to regridding from
! non-coincident grids.
! Otherwise postprocessing of fields can give negative values.

DO j = 1,river_rows
  DO i = 1, river_row_length
    IF (riverout_trip(i,j) == rmdi) riverout_trip(i,j) = 0.0
  END DO
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE riv_rout_1a

! ********************************************************************

END MODULE riv_rout_mod_1A












!Huw Lewis (MO), Jan 2015
!DEPRECATED CODE
!This code was transferred from the UM repository at UM vn9.2 / JULES vn 4.1.
!Future developments will supercede these subroutines, and as such they
!should be considered deprecated. They will be retained in the codebase to
!maintain backward compatibility with functionality prior to
!UM vn10.0 / JULES vn 4.2, until such time as they become redundant.
!
!
! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Code Owner: Please refer to the UM file CodeOwners.txt
! This file belongs in section: River Routing
MODULE riv_intctl_mod_2A

USE riv_rout_mod_2A, ONLY: riv_rout_2A


USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE


CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='RIV_INTCTL_MOD_2A'

CONTAINS

SUBROUTINE riv_intctl_2a(                                              &
xpa, xua, xva, ypa, yua, yva,                                          &
g_p_field, g_r_field, n_proc, me, rmdi,                                &
gather_pe_trip,land_points,land_index,                                 &
invert_atmos, row_length, rows,                                        &
global_row_length, global_rows,                                        &
river_row_length, river_rows,                                          &
global_river_row_length, global_river_rows,                            &
flandg, riv_step, riv_vel, riv_mcoef,                                  &
trivdir, trivseq, twatstor, a_boxareas,                                &
delta_phi,first,                                                       &
r_area, slope, flowobs1,r_inext,r_jnext,r_land,                        &
substore,surfstore,flowin,bflowin,                                     &
! IN/OUT accumulated runoff
       tot_surf_runoff, tot_sub_runoff,                                       &
! OUT
       box_outflow, box_inflow, riverout_atmos,                               &
! Optional arguments from 1A subroutine needed for interface checking
       inlandout_atmos,inlandout_riv,                                         &
       dsm_levels,acc_lake_evap,smvcst,smvcwt,smcl,sthu)

! Purpose:
! New Control routine for River routing for Regional Model.
!
! Code Description:
!   Language: FORTRAN 77 + common extensions.
!   This code is written to UMDP3 v6 programming standards.
!-----------------------------------------------------------------

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
USE ereport_mod, ONLY: ereport
IMPLICIT NONE


INTEGER ::                                                              &
 row_length                                                             &
                           ! IN NO. OF COLUMNS IN ATMOSPHERE
,rows                                                                   &
                           ! IN NO. OF ROWS IN ATMOSPHERE
, global_row_length                                                     &
                           ! number of points on a row
, global_rows                                                           &
                           ! NUMBER OF global rows
,land_points                                                            &
                           ! IN number of landpoints
,river_row_length                                                       &
                           ! IN no. of columns in river grid
,river_rows                                                             &
                           ! IN no. of rows in river grid
,global_river_row_length                                                &
                           ! IN global river row length
,global_river_rows                                                      &
                           ! IN GLOBAL river rows
,gather_pe_trip                                                         &
                           ! IN pe River routing to be run on
, n_proc                                                                &
                           ! IN Total number of processors
, me                       ! IN My processor number

INTEGER ::                                                              &
  g_p_field                                                             &
                            ! IN size of global ATMOS field
, g_r_field                                                             &
                            ! IN Size of global river field
, land_index (land_points)  ! IN index of land to global points
REAL ::                                                                 &
 tot_surf_runoff(land_points)                                           &
                             !IN Surf RUNOFF on land pts(KG/M2/S)
,tot_sub_runoff(land_points)                                            &
                             !IN Subsurf.RUNOFF (KG/M2/S)
,rmdi                                                                   &
                            ! IN real missing data indicator
,xua(0:row_length)                                                      &
                            ! IN Atmosphere UV longitude coords
,yua(rows)                                                              &
                            ! IN Atmosphere latitude coords
,xpa(row_length+1)                                                      &
                            ! IN Atmosphere longitude coords
,ypa(rows)                                                              &
                            ! IN Atmosphere latitude coords
,xva(row_length+1)                                                      &
                            ! IN Atmosphere longitude coords
,yva(0:rows)                                                            &
                            ! IN Atmosphere latitude coords
,a_boxareas(row_length,rows)                                            &
                            !IN ATMOS gridbox areas
,flandg(row_length,rows)                                                &
                            ! IN Land fraction on global field.
,delta_phi                  ! RCM gridsize (radians)


REAL ::                                                                 &
 trivdir(river_row_length, river_rows)                                  &
                                         !IN river direction
,trivseq(river_row_length, river_rows)                                  &
                                         !IN river sequence
,twatstor(river_row_length, river_rows)                                 &
                                         !IN/OUT water store(Kg)
,riv_vel                                                                &
                            ! IN river velocity
,riv_mcoef                                                              &
                            ! IN meandering coefficient
,riv_step                   ! IN river timestep (secs)

LOGICAL ::                                                              &
 invert_atmos               ! IN True if ATMOS fields are S->N
!                                 ! for regridding runoff from ATMOS.

REAL ::                                                                 &
 riverout_atmos(row_length,rows)                                        &
                                ! OUT river flow out from each
!                           ! gridbox (kg/m2/s)
      ,box_outflow(river_row_length, river_rows)                              &
                                                 ! OUT gridbox outflow
!                                ! river grid (kg/s)
      ,box_inflow(river_row_length, river_rows)   ! OUT gridbox runoff
!                                ! river grid (kg/s)


! ancillary variables for river routing model

REAL ::                                                                &
r_area(global_row_length,global_rows),                                 &
                !ACCUMULATED AREAS FILE
r_inext(global_row_length,global_rows),                                &
                ! X-COORDINATE OF DOWNSTREAM GRID PT
r_jnext(global_row_length,global_rows),                                &
                ! Y-COORDINATE OF DOWNSTREAM GRID PT
slope(global_row_length,global_rows),                                  &
                ! SLOPES (NOT USED YET)
flowobs1(global_row_length,global_rows),                               &
                ! OPTIONAL INITIALISATION FOR FLOWS
r_land(global_row_length,global_rows)
                !LAND/RIVER DEPENDS ON VALUE OF A_THRESH

! PROGNOSTIC VARIABLES FOR GRID-TO-GRID MODEL

REAL ::                                                                &
 substore(global_row_length,global_rows)                               &
                ! ROUTING SUB_SURFACE STORE (MM)
,surfstore(global_row_length,global_rows)                              &
                ! ROUTING SURFACE STORE (MM)
,flowin(global_row_length,global_rows)                                 &
                !SURFACE LATERAL INFLOW (MM)
,bflowin(global_row_length,global_rows)
                ! SUB-SURFACE LATERAL INFLOW (MM)

LOGICAL :: first    ! First call to river routing ? (T/F)

! Start of optional arguments from 1A subroutine
INTEGER, OPTIONAL :: dsm_levels
REAL,    OPTIONAL :: inlandout_riv (river_row_length,river_rows)
REAL,    OPTIONAL :: inlandout_atmos (row_length,rows)
REAL,    OPTIONAL :: acc_lake_evap (row_length,rows)
REAL,    OPTIONAL :: smvcwt(land_points)
REAL,    OPTIONAL :: smvcst(land_points)
REAL,    OPTIONAL :: sthu(land_points)
REAL,    OPTIONAL :: smcl(land_points)
! End of optional arguments from 1A subroutine

INTEGER ::                                                              &
 i,j,l


LOGICAL ::                                                              &
 invert_trip                                                            &
                          ! TRUE WHEN ROW INVERSION IS REQUIRED
, regrid                                                                &
                          ! TRUE if TRIP grid different to ATMOS
, cyclic_trip                                                           &
                          ! TRUE WHEN THE TRIP MODEL HAS CYCLIC
, global_trip             ! TRUE WHEN TRIP GRID SURFACE IS SPHER
PARAMETER(invert_trip = .FALSE.,cyclic_trip = .TRUE.,                      &
global_trip = .TRUE.,regrid = .TRUE.)

!      REAL rmdi_trip
!      PARAMETER(rmdi_trip=-999)

INTEGER :: iarea(row_length,rows)
INTEGER :: inext(row_length,rows)
INTEGER :: jnext(row_length,rows)
INTEGER :: land(row_length,rows)

REAL ::                                                                 &
 surf_runoffin(row_length,rows)                                         &
                               !IN TOTAL RATE OF RUNOFF (KG/M2/S)
,sub_runoffin(row_length,rows) !IN TOTAL RATE OF RUNOFF (KG/M2/S)


INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='RIV_INTCTL_2A'



! gather the TRIP variables to PE0 and call the TRIP river routing
! 1. Gather coupling fields from distributed processors onto a single
!    processor for input to river routing routines.

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,          &
                        zhook_handle)
DO j = 1,rows
  DO i = 1,row_length
    surf_runoffin(i,j) = 0.0
  END DO
END DO
DO j = 1,rows
  DO i = 1,row_length
    sub_runoffin(i,j) = 0.0
  END DO
END DO

! Copy land points output back to full fields array.
DO l = 1, land_points
  j=(land_index(l) - 1) / row_length + 1
  i = land_index(l) - (j-1) * row_length
  surf_runoffin(i,j) = tot_surf_runoff(l)
  sub_runoffin(i,j) = tot_sub_runoff(l)
END DO



! Set river routing to run on the last PE
IF (me == gather_pe_trip) THEN

  DO j = 1,global_rows
    DO i = 1,global_row_length
      iarea(i,j) = NINT( r_area(i,j) )
      land(i,j)  = NINT( r_land(i,j) )
      inext(i,j) = NINT( r_inext(i,j) )
      jnext(i,j) = NINT( r_jnext(i,j) )
    END DO
  END DO

  ! Call the Grid-to-grid river routing scheme

  !------------------------------------------------------------------

  CALL riv_rout_2a(                                                       &
     surf_runoffin, sub_runoffin,                                         &
     global_row_length, global_rows,                                      &
     a_boxareas,                                                          &
     delta_phi,first,riv_step,                                            &
  ! ancillary variables
           iarea, slope, flowobs1,                                              &
           inext,jnext,land,                                                    &
  ! prognostic variables
           substore,surfstore,                                                  &
           flowin,bflowin,                                                      &
           riverout_atmos)

  ! Convert prognostics from INTEGER to REAL before scattering
  DO l = 1, global_rows * global_row_length
    j=(l-1) / global_row_length + 1
    i= l - (j-1) * global_row_length
    r_land(i,j) = REAL( land(i,j) )
  END DO

  ! Set all total land pts to 0.0

END IF                         ! Single processor
!



IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,         &
                        zhook_handle)
RETURN
END SUBROUTINE riv_intctl_2a
END MODULE riv_intctl_mod_2A

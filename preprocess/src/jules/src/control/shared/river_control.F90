










! *****************************COPYRIGHT****************************************
! (c) Crown copyright, Met Office. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms and
! conditions set out therein.
!
! [Met Office Ref SC0237]
! *****************************COPYRIGHT****************************************
MODULE river_control_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='RIVER_CONTROL_MOD'

CONTAINS
SUBROUTINE river_control(                                                     &
   !INTEGER, INTENT(IN)
   land_pts,                                                                  &
   ! REAL, INTENT(IN)
   sub_surf_roff, surf_roff,                                                  &
   ! LOGICAL, INTENT(IN)
   srflow, srrun,                                                             &
   ! REAL, INTENT (OUT)
   rflow, rrun                                                                &
  )

!Module imports

!Common modules
USE riv_intctl_mod_1A,        ONLY: riv_intctl_1A
USE riv_intctl_mod_2A,        ONLY: riv_intctl_2A

USE missing_data_mod,         ONLY: rmdi
USE jules_surface_types_mod,  ONLY: lake
USE jules_soil_mod,           ONLY: sm_levels

USE ancil_info,               ONLY:  nsoilt

USE ereport_mod,              ONLY: ereport

!Module imports - Variables required only in UM-mode

! imported module routines
USE jules_rivers_trip_mod, ONLY: rivers_drive_trip, regrid_routestore
USE jules_rivers_rfm_mod, ONLY: rivers_drive_rfm

USE jules_rivers_mod, ONLY:                                                   &
!  imported scalar parameters
     rivers_trip, rivers_rfm, rivers_type, l_riv_overbank                     &
!  imported scalars with intent(in)
    ,rivers_timestep,rivers_type                                              &
    ,np_rivers                                                                &
!  imported scalars with intent(inout)
    ,rivers_count,rivers_step                                                 &
!  imported arrays with intent(inout)
    ,tot_surf_runoff_gb, tot_sub_runoff_gb, acc_lake_evap_gb                  

USE overbank_inundation_mod, ONLY:                                            &
   frac_fplain_lp

USE overbank_rivers_drive_mod, ONLY: overbank_rivers_drive

USE model_grid_mod, ONLY: latitude, longitude

USE coastal, ONLY: flandg

USE timestep_mod, ONLY: timestep

USE ancil_info, ONLY: frac_surft, nx=>row_length, ny=>rows, land_index

USE fluxes, ONLY:  fqw_surft

USE p_s_parms, ONLY: smvcst_soilt, smvcwt_soilt, sthu_soilt

USE prognostics, ONLY: smcl_soilt

USE theta_field_sizes, ONLY: t_i_length, t_j_length

USE jules_print_mgr, ONLY:                                                 &
  jules_message,                                                            &
  jules_print

USE model_grid_mod, ONLY: global_land_pts

USE parallel_mod, ONLY: master_task_id, is_master_task,                    &
  gather_land_field, scatter_land_field

USE jules_vegetation_mod, ONLY: l_irrig_limit


USE parkind1, ONLY: jprb, jpim
USE yomhook, ONLY: lhook, dr_hook

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Control routine for rivers
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Subroutine arguments

!-----------------------------------------------------------------------------
! Jules-standalone only arguments

! Array arguments with intent(in)
INTEGER, INTENT(IN) :: land_pts              !  number of land points

! Array arguments with intent(in)
REAL, INTENT(IN) :: sub_surf_roff(land_pts)  ! Sub-surface runoff (kg m-2 s-1)
REAL, INTENT(IN) :: surf_roff(land_pts)      ! Surface runoff (kg m-2 s-1)

! Array arguments with intent(inout)
LOGICAL, INTENT(IN) :: srflow         ! Flag for river flow diagnostic
LOGICAL, INTENT(IN) :: srrun          ! Flag for runoff diagnostic

REAL, INTENT(OUT) :: rflow(land_pts)  ! River flow diagnostic on land points
REAL, INTENT(OUT) :: rrun(land_pts)   ! Runoff diagnostic on land points

! Local array variables.
REAL ::                                                                     &
   riv_step
                          ! IN river timestep (secs)

REAL, ALLOCATABLE :: global_tot_sub_runoff(:)
REAL, ALLOCATABLE :: global_tot_surf_runoff(:)
REAL, ALLOCATABLE :: global_rrun(:)
REAL, ALLOCATABLE :: global_rflow(:)
REAL, ALLOCATABLE :: global_frac_fplain(:)
       ! Fraction of inundated floodplain predicted by the overbank
       ! inundation routine (=0 if l_riv_overbank=F), defined on
       ! global_land_pts

LOGICAL :: rivers_call

!-----------------------------------------------------------------------------
!UM-only arguments

!Local variables
INTEGER ::                                                                    &
  error,                                                                      &
    ! Error status from each call to ALLOCATE.
  error_sum,                                                                  &
    ! Accumulated error status.
  nstep_rivers,                                                               &
  gather_pe_rivers,                                                           &
  l,i,j

LOGICAL ::                                                                    &
  first_routing,                                                              &
  invert_atmos

!Error reporting
CHARACTER(LEN=256)       :: message
INTEGER                  :: errorstatus
CHARACTER(LEN=*), PARAMETER  :: RoutineName = 'RIVER_CONTROL'

!Dr Hook variables
INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

!-----------------------------------------------------------------------------
!end of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)


!Initialise the accumulated surface and subsurface runoff to zero
!at the beginning of river routing timestep
IF ( rivers_count == 0 ) THEN
  tot_surf_runoff_gb = 0.0
  tot_sub_runoff_gb  = 0.0
  acc_lake_evap_gb   = 0.0
END IF

! Increment counters.
!  rivers_step = rivers_step + 1
rivers_count = rivers_count + 1
nstep_rivers = INT(rivers_timestep)

IF (rivers_count == nstep_rivers) THEN
  rivers_call = .TRUE.
ELSE
  rivers_call = .FALSE.
END IF

!Accumulate the runoff as Kg/m2/s over the River Routing period
DO l = 1, land_pts
  IF (surf_roff(l) <  0.0) THEN
    !      WRITE(umMessage,*)'surf_roff(',l,')= ',surf_roff(l)
    !      CALL umPrint(umMessage,src='river_control')
  ELSE
    tot_surf_runoff_gb(l) = tot_surf_runoff_gb(l) +                         &
                         (surf_roff(l) / REAL(nstep_rivers))
  END IF
  IF (sub_surf_roff(l) <  0.0) THEN
    !      WRITE(umMessage,*)'sub_surf_roff(',l,')= ',sub_surf_roff(L)
    !      CALL umPrint(umMessage,src='river_control')
  ELSE
    tot_sub_runoff_gb(l) = tot_sub_runoff_gb(l) +                           &
                        (sub_surf_roff(l) / REAL(nstep_rivers))
  END IF
END DO


!-------------------------------------------------------------------------------
! STANDALONE versions
!-------------------------------------------------------------------------------

IF ( rivers_call ) THEN

  !If ATMOS fields are as Ocean (i.e. inverted NS) set invert_atmos
  invert_atmos = .FALSE.

  !-------------------------------------------------------------------------------
  !   Gather runoff information from all processors
  !-------------------------------------------------------------------------------

  IF ( is_master_task() ) THEN
    ALLOCATE(global_tot_sub_runoff(global_land_pts), stat = error)
    error_sum = error
    ALLOCATE(global_tot_surf_runoff(global_land_pts), stat = error)
    error_sum = error_sum + error
    ALLOCATE(global_rrun(global_land_pts), stat = error)
    error_sum = error_sum + error
    ALLOCATE(global_rflow(global_land_pts), stat = error)
    error_sum = error_sum + error
  ELSE
    ALLOCATE(global_tot_sub_runoff(1), stat = error)
    error_sum = error
    ALLOCATE(global_tot_surf_runoff(1), stat = error)
    error_sum = error_sum + error
    ALLOCATE(global_rrun(1), stat = error)
    error_sum = error_sum + error
    ALLOCATE(global_rflow(1), stat = error)
    error_sum = error_sum + error
  END IF

  IF ( error_sum /= 0 ) THEN
    errorstatus = 10
    CALL ereport( RoutineName, errorstatus,                                 &
                   "Error related to allocation of runoff variables." )
  END IF
        
  CALL gather_land_field(tot_sub_runoff_gb, global_tot_sub_runoff)
  CALL gather_land_field(tot_surf_runoff_gb, global_tot_surf_runoff)

  !-------------------------------------------------------------------------------
  !   If inundation is on, gather surface open water fraction from all
  !   processors.
  !-------------------------------------------------------------------------------
  IF ( l_riv_overbank ) THEN

    IF ( is_master_task() ) THEN
      ALLOCATE(global_frac_fplain(global_land_pts), stat = error)
      error_sum = error_sum + error
    ELSE
      ALLOCATE(global_frac_fplain(1), stat = error)
      error_sum = error_sum + error
    END IF

    IF ( error_sum /= 0 ) THEN
      errorstatus = 10
      CALL ereport( RoutineName, errorstatus,                               &
                    "Error related to allocation of global_frac_fplain." )
    END IF

    global_frac_fplain(:) = rmdi

  END IF  !  l_riv_overbank

  !-------------------------------------------------------------------------
  ! Call RFM or TRIP routing driver on single processor
  !-------------------------------------------------------------------------
  IF ( is_master_task() ) THEN

    SELECT CASE ( rivers_type )

      !---------------------------------------------------------------------
      ! RFM ('standalone' version)
      !---------------------------------------------------------------------
    CASE ( rivers_rfm )
      CALL rivers_drive_rfm( global_land_pts,                             &
                             global_tot_sub_runoff,                       &
                             global_tot_surf_runoff,                      &
                             global_rrun, global_rflow ) 

      !---------------------------------------------------------------------
      ! TRIP ('standalone' version)
      !---------------------------------------------------------------------
    CASE ( rivers_trip )
      CALL rivers_drive_trip( global_land_pts,                            &
                              global_tot_sub_runoff,                      &
                              global_tot_surf_runoff,                     &
                              global_rrun, global_rflow )

      !---------------------------------------------------------------------
      ! Default case for rivers_type.
      !---------------------------------------------------------------------
    CASE DEFAULT

      WRITE(jules_message,*)'ERROR: rivers_drive: ' //                    &
                            'do not recognise rivers_type=',              &
                            TRIM(rivers_type)
      CALL jules_print('rivers_route_drive_standalone',jules_message)

    END SELECT

    !-----------------------------------------------------------------------
    ! Compute overbank inundation
    !-----------------------------------------------------------------------
    IF ( l_riv_overbank ) THEN
      CALL overbank_rivers_drive( global_land_pts, global_frac_fplain )
    END IF

  END IF    ! end is_master

  !-------------------------------------------------------------------------
  ! Update output diagnostics
  !-------------------------------------------------------------------------
  IF (srflow .OR. srrun) THEN
    CALL scatter_land_field(global_rrun, rrun)
    CALL scatter_land_field(global_rflow, rflow)

    IF ( l_irrig_limit ) THEN
      CALL regrid_routestore()
    END IF
  END IF

  !-------------------------------------------------------------------------
  ! Update overbank inundation (on processors) if calculated.
  !-------------------------------------------------------------------------
  IF ( l_riv_overbank ) THEN
    CALL scatter_land_field(global_frac_fplain, frac_fplain_lp)
  END IF

  DEALLOCATE(global_rflow)
  DEALLOCATE(global_rrun)
  DEALLOCATE(global_tot_surf_runoff)
  DEALLOCATE(global_tot_sub_runoff)

  IF ( l_riv_overbank ) THEN
    DEALLOCATE(global_frac_fplain)
  END IF

  !-------------------------------------------------------------------------
  !   Reset counters after a call to routing.
  !-------------------------------------------------------------------------
  !Mult RIVEROUT by the number of physics timesteps per River routing
  !timestep as DAGHYD stores RIVEROUT every timestep. Non-routing
  !timestep vals are passed in as 0.0
  rivers_count = 0

END IF ! rivers_call


IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE river_control

END MODULE river_control_mod


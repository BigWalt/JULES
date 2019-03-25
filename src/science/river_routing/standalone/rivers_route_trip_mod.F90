! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Description:
!     Driver and science routines for calculating river flow routing
!     using the TRIP model
!     see Oki et al 1999 J.Met.Soc.Japan, 77, 235-255.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

MODULE jules_rivers_trip_mod

CONTAINS

!###############################################################################
! subroutine rivers_drive_trip
! Driver routine for runoff routing by the TRIP model.

SUBROUTINE rivers_drive_trip( global_land_pts, sub_runoffin, surf_runoffin,   &
                              runoff_out, rivflow )

!-------------------------------------------------------------------------------
!
! Description:
!   Perform the routing of total surface runoff using TRIP
!
!   This routine regrids the total surface runoff to the river grid and passes
!   it to the TRIP routines to be routed.
!
!-------------------------------------------------------------------------------
! Modules used:

USE jules_rivers_mod, ONLY:                                                &
!  imported scalars with intent(in)
     np_rivers,rivers_regrid                                                  &
!  imported arrays with intent(in)
    ,rivers_index_rp, rivers_lat_rp, rivers_lon_rp                            &
    ,rivers_boxareas_rp, il_river_grid                                        &
!  imported arrays with intent(inout)
    ,rivers_next_rp

USE rivers_utils, ONLY:                                                    &
!  imported procedures
     get_rivers_len_rp, rivers_regrid_to_land, rivers_regrid_from_land

!-------------------------------------------------------------------------------
USE jules_print_mgr, ONLY:                                                 &
  jules_message,                                                            &
  jules_print

IMPLICIT NONE

! Scalar arguments with intent(in)

INTEGER, INTENT(IN) :: global_land_pts
                             ! Size of GLOBAL runoff arrays on landpts only

! Array arguments with intent(in)

REAL, INTENT(IN) :: sub_runoffin(global_land_pts)
                             ! Sub surface runoff on landpts in kg m-2 s-1
REAL, INTENT(IN) :: surf_runoffin(global_land_pts)
                             ! Surface runoff on landpts in kg m-2 s-1

! Array arguments with intent(out)

REAL, INTENT(OUT) :: runoff_out(global_land_pts)
                             ! Runoff diagnostic on landpts
REAL, INTENT(OUT) :: rivflow(global_land_pts)
                             ! River flow diagnostic on landpts

! Local scalar variables.

INTEGER :: ip                ! loop counters

! Local array variables.

REAL :: runoff_grid(global_land_pts)
                             !  average rate of runoff since last
                             !  rivers call (kg m-2 s-1), on a grid
                             !  that is a superset of (can be equal
                             !  to) the model grid REAL :: outflow(np_rivers)
                             !  rate of channel flow leaving gridbox
                             !  (kg m-2 s-1)
REAL :: outflow_flux(np_rivers)
                             !  rate of channel flow leaving gridbox (kg s-1)
REAL :: runoff_rivers(np_rivers)
                             !  average rate of runoff, on river grid
                             !  (kg m-2 s-1)
REAL :: rflux_rivers(np_rivers)
                             ! average rate of runoff flux (kg s-1)
REAL :: riverslength(np_rivers)
                             !  distance to next downstream gridpoint (m)
LOGICAL :: rivers_mask(np_rivers)
                             !  TRUE at land points on river grid, else FALSE

!-------------------------------------------------------------------------------
! Initialisation
!-------------------------------------------------------------------------------

rivflow(:)      = 0.0
runoff_out(:)   = 0.0
outflow_flux(:) = 0.0

!-------------------------------------------------------------------------------
! Sum the surface and subsurface runoffs
!-------------------------------------------------------------------------------

runoff_grid(:) = surf_runoffin(:) + sub_runoffin(:)

!-------------------------------------------------------------------------------
! Regrid runoff from land grid to river routing grid, and convert from
! flux density (kg m-2 s-1) to flux (kg s-1).
!-------------------------------------------------------------------------------

runoff_rivers(:) = 0.0
rflux_rivers(:)  = 0.0

! If regridding required, call routine
IF ( rivers_regrid ) THEN

  CALL rivers_regrid_from_land( global_land_pts, runoff_grid,               &
                                np_rivers, runoff_rivers )

  ! If not regridding, translate between land and river point vectors
ELSE IF (global_land_pts /= np_rivers) THEN

  DO ip = 1,np_rivers
    IF (il_river_grid(ip) > 0) THEN
      runoff_rivers(ip) = runoff_grid(il_river_grid(ip))
    END IF
  END DO
    
  ! If grids identical, including land/riv points in same order, no need to regrid
ELSE

  runoff_rivers(:) = runoff_grid(:)

END IF

! Convert runoff to kg s-1 flux
rflux_rivers(:) = runoff_rivers(:) * rivers_boxareas_rp(:)

!-------------------------------------------------------------------------------
! Calculate distance between grid points.
!-------------------------------------------------------------------------------
CALL get_rivers_len_rp( np_rivers, rivers_next_rp,                          &
                        rivers_lat_rp, rivers_lon_rp, riverslength )

!-------------------------------------------------------------------------------
! Call the river routing routine.
!-------------------------------------------------------------------------------
CALL rivers_trip( riverslength, rflux_rivers, outflow_flux )
  
!-------------------------------------------------------------------------------
! Add runoff on TRIP sea points to the river flow variable.
! Regridding may have resulted in some land runoff appearing in TRIP sea
! gridboxes, so we need to account for this water. Even without regridding,
! a land point that is sea in TRIP will not have been added to flow.
!-------------------------------------------------------------------------------
!  WHERE( .NOT. rivers_mask(:,:) ) outflow(:,:) = rflux_rivers(:,:)

!-------------------------------------------------------------------------------
!   Regrid from rivers to land grid
!-------------------------------------------------------------------------------

! If regridding required, call routine
IF ( rivers_regrid ) THEN

  outflow_flux(:) = outflow_flux(:) / rivers_boxareas_rp(:)
  CALL rivers_regrid_to_land( np_rivers, outflow_flux,                      & 
                              global_land_pts, rivflow )
  ! Sanity check regridding routines - expect input = output
  CALL rivers_regrid_to_land( np_rivers, runoff_rivers,                     & 
                              global_land_pts, runoff_out )

  ! If not regridding, translate between land and river point vectors
ELSE IF (global_land_pts /=  np_rivers) THEN
    
  DO ip = 1,np_rivers
    IF (il_river_grid(ip) > 0) THEN
      rivflow(il_river_grid(ip)) = outflow_flux(ip) / rivers_boxareas_rp(ip)
      runoff_out(il_river_grid(ip)) = rflux_rivers(ip) / rivers_boxareas_rp(ip) 
    END IF
  END DO

  ! If grids identical, including land/riv points in same order, no need to regrid
ELSE

  rivflow(:)    = outflow_flux(:) / rivers_boxareas_rp(:)
  runoff_out(:) = runoff_rivers(:)

END IF

END SUBROUTINE rivers_drive_trip
!###############################################################################

!###############################################################################
! subroutine rivers_trip
!
!-----------------------------------------------------------------------------
! Description:
!   Calculates river outflow (kg s-1) and updates channel storage for the
!   TRIP river routing model.
!
! Method:
!   See Oki et al. 1999, J.Met.Soc.Japan, 77, 235-255.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

SUBROUTINE rivers_trip( riverslength,runoff,outflow )

USE jules_rivers_mod, ONLY:                                                &
!  imported scalars with intent(in)
     np_rivers,nx_rivers,ny_rivers,                                           &
     rivers_timestep,nseqmax                                                  &
!  imported arrays with intent(in)
    ,rivers_index_rp,rivers_next_rp,rivers_seq_rp,rivers_sto_rp

USE jules_riversparm, ONLY:                                                &
!  imported parameters
     rivers_meander, rivers_speed

USE rivers_utils, ONLY:                                                    &
!  imported procedures
     rivers_get_xy_pos

USE timestep_mod, ONLY:                                                     &
   timestep

USE jules_print_mgr, ONLY:                                                 &
  jules_message,                                                            &
  jules_print

IMPLICIT NONE

!-------------------------------------------------------------------------------

REAL, INTENT(IN) ::                                                         &
!  arrays with intent(in)
     riverslength(np_rivers)                                                  &
     !  distance between gridpoints (m)
     ,runoff(np_rivers)
     !  rate of runoff generation in each gridbox (kg s-1)

REAL, INTENT(OUT) ::                                                        &
!  arrays with intent(out)
     outflow(np_rivers)
     !  rate of channel flow leaving gridbox (kg s-1)

INTEGER ::                                                                  &
!  local scalars (work/loop counters)
     i,ip,ir,iseq       

REAL ::                                                                     &
!  local scalars
     coeff                                                                    &
     !  coefficient in the routing model (s-1)
     ,exp_coeffdt                                                             &
     !  working variable exp[c*dt]
     ,dt                                                                      &
     !  timestep of routing model (s)
     ,store_old
     !  channel storage (kg)

REAL ::                                                                     &
!  local arrays
    inflow(np_rivers)
    !  rate of channel flow entering gridbox (kg s-1)

!-------------------------------------------------------------------------------

dt = REAL(rivers_timestep) * timestep

! Initialise inflow with runoff generated over each gridbox.
inflow(:) = runoff(:)

! Initialise outflow (for clarity at non-rivers points).
outflow(:) = 0.0

!-------------------------------------------------------------------------------
! Loop over rivers points with valid flow direction
!-------------------------------------------------------------------------------

DO iseq = 1, nseqmax

  DO ip = 1,np_rivers

    !   Get index (location in rivers vector) of the point to consider.      
    IF (NINT(rivers_seq_rp(ip)) == iseq) THEN
        
      !-------------------------------------------------------------------------------
      !   Calculate the coefficient "c" of the model.
      !   c=u/(d*r), where u is effective flow speed,
      !   d is distance between gridpoints, and r is meander ratio.
      !-------------------------------------------------------------------------------
      coeff = rivers_speed / ( riverslength(ip) * rivers_meander )
      exp_coeffdt = EXP(-(coeff * dt))

      !-------------------------------------------------------------------------------
      !   Save value of channel storage at start of timestep.
      !-------------------------------------------------------------------------------
      store_old = rivers_sto_rp(ip)

      !-------------------------------------------------------------------------------
      !   Calculate channel storage at end of timestep.
      !   Eqn.4 of Oki et al, 1999, J.Met.Soc.Japan, 77, 235-255.
      !-------------------------------------------------------------------------------
      rivers_sto_rp(ip) = store_old * exp_coeffdt                           &
                          + ( 1.0 - exp_coeffdt ) * inflow(ip) / coeff

      !-------------------------------------------------------------------------------
      !   Calculate outflow as inflow minus change in storage.
      !-------------------------------------------------------------------------------
      outflow(ip) = inflow(ip) + (store_old - rivers_sto_rp(ip)) / dt

      !-------------------------------------------------------------------------------
      !   Add outflow to inflow of next downstream point.
      !-------------------------------------------------------------------------------
      IF ( rivers_next_rp(ip) > 0 ) THEN
        !     Get location in grid of next downstream point.
        inflow(rivers_next_rp(ip)) = inflow(rivers_next_rp(ip)) + outflow(ip)
      END IF

    END IF

  END DO !points
END DO !river sequences

END SUBROUTINE rivers_trip

!###############################################################################

SUBROUTINE regrid_routestore

USE jules_rivers_mod, ONLY:                                                &
!  imported scalars with intent(in)
    np_rivers, rivers_regrid                                                  &
!  imported array with intent(in) 
    ,il_river_grid, rivers_sto_rp,rivers_boxareas_rp                         &
!  imported array with intent(out)
    ,rivers_sto_per_m2_on_landpts

USE parallel_mod, ONLY: is_master_task, scatter_land_field

USE rivers_utils, ONLY:                                                    &
!  imported procedures
     rivers_regrid_to_land

USE model_grid_mod, ONLY: global_land_pts

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Uses to the river storage on the river points array (a prognostic) to
!   fill the river storage on land points array, which is used to calculate
!   the water available for irrigation.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

! Local arrays
REAL, ALLOCATABLE :: global_rivers_sto_per_m2_on_landpts(:)
          ! Water storage (kg m-2) on global model land points
REAL :: rivers_sto_per_m2_rgrid(np_rivers)
          ! water storage on routing grid in kg m-2
INTEGER :: ip ! array indices

rivers_sto_per_m2_on_landpts(:) = 0.0

IF ( is_master_task() ) THEN
  ALLOCATE(global_rivers_sto_per_m2_on_landpts(global_land_pts))
ELSE
  ALLOCATE(global_rivers_sto_per_m2_on_landpts(1))
END IF

global_rivers_sto_per_m2_on_landpts(:) = 0.0

IF ( is_master_task() ) THEN

  rivers_sto_per_m2_rgrid(:) = rivers_sto_rp(:) / rivers_boxareas_rp(:)

  !-------------------------------------------------------------------------------
  !   Regrid from rivers to land grid
  !-------------------------------------------------------------------------------

  ! If regridding required, call routine
  IF ( rivers_regrid ) THEN

    CALL rivers_regrid_to_land( np_rivers, rivers_sto_per_m2_rgrid,         & 
                                global_land_pts,                            &
                                global_rivers_sto_per_m2_on_landpts )

    ! If not regridding, translate between land and river point vectors
  ELSE IF (global_land_pts /= np_rivers) THEN
      
    DO ip = 1,np_rivers
      IF (il_river_grid(ip) > 0) THEN
        global_rivers_sto_per_m2_on_landpts(il_river_grid(ip)) =            &
           rivers_sto_per_m2_rgrid(ip)
      END IF
    END DO
      
    ! If grids identical, including land/riv points in same order, no need to regrid
  ELSE
      
    global_rivers_sto_per_m2_on_landpts(:) = rivers_sto_per_m2_rgrid(:)
      
  END IF
  
END IF
  
CALL scatter_land_field(global_rivers_sto_per_m2_on_landpts,                &
                        rivers_sto_per_m2_on_landpts)

DEALLOCATE(global_rivers_sto_per_m2_on_landpts)

END SUBROUTINE regrid_routestore

!###############################################################################

SUBROUTINE adjust_routestore

!-----------------------------------------------------------------------------
! Description:
!   Uses to the river adjustment factor on land points (which accounts for
!   water extracted from the river storage for irrigation)
!   to adjust the river storage on river points array (a prognostic).
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE jules_rivers_mod, ONLY:                                                &
!  imported scalars with intent(in)
    np_rivers, rivers_regrid          &
!  imported array with intent(in)
    ,il_river_grid, rivers_sto_rp, rivers_adj_on_landpts

USE rivers_utils, ONLY:                                                    &
!  imported procedures
     rivers_regrid_from_land, rivers_route_regrid

USE parallel_mod, ONLY: is_master_task, gather_land_field

USE model_grid_mod, ONLY: global_land_pts

IMPLICIT NONE

REAL, ALLOCATABLE :: global_rivers_adj_on_landpts(:)
          ! rivers adjustment factor on global model land points

REAL :: rivers_adj_rgrid(np_rivers)
          ! rivers adjustment factor on routing grid
INTEGER :: ip ! array indices

IF ( is_master_task() ) THEN
  ALLOCATE(global_rivers_adj_on_landpts(global_land_pts))
ELSE
  ALLOCATE(global_rivers_adj_on_landpts(1))
END IF

CALL gather_land_field(rivers_adj_on_landpts, global_rivers_adj_on_landpts)

IF ( is_master_task() ) THEN

  rivers_adj_rgrid(:) = 1.0

  !-------------------------------------------------------------------------------
  !   Regrid from land to rivers grid
  !-------------------------------------------------------------------------------
 
  ! If regridding required, call routine
  IF ( rivers_regrid ) THEN

    CALL rivers_regrid_from_land( global_land_pts,                          &
                                  global_rivers_adj_on_landpts,             &
                                  np_rivers, rivers_adj_rgrid )

    ! If not regridding, translate between land and river point vectors
  ELSE IF (global_land_pts /= np_rivers) THEN

    DO ip = 1,np_rivers
      IF (il_river_grid(ip) > 0) THEN
        rivers_adj_rgrid(ip) = global_rivers_adj_on_landpts(il_river_grid(ip))
      END IF
    END DO
      
    ! If grids identical, including land/riv points in same order, no need to regrid
  ELSE

    rivers_adj_rgrid(:) = global_rivers_adj_on_landpts(:)

  END IF

  !-------------------------------------------------------------------------------
  ! Apply the correction factor to the route storage, which is a prognostic
  !-------------------------------------------------------------------------------

  DO ip = 1,np_rivers
    rivers_sto_rp(ip) = rivers_adj_rgrid(ip) * rivers_sto_rp(ip)
  END DO

END IF

DEALLOCATE(global_rivers_adj_on_landpts)

END SUBROUTINE adjust_routestore

!###############################################################################

END MODULE jules_rivers_trip_mod


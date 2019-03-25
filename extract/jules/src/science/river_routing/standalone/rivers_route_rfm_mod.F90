! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************
!
! Description:
!     Driver and science routines for calculating river flow routing
!     using the RFM kinematic wave model
!     see Bell et al. 2007 Hydrol. Earth Sys. Sci. 11. 532-549
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

MODULE jules_rivers_rfm_mod

CONTAINS

!###############################################################################
! subroutine rivers_drive_rfm
! Driver routine for runoff routing by the RFM (kinematic wave) model.

SUBROUTINE rivers_drive_rfm( global_land_pts, sub_runoffin, surf_runoffin,     &
                             runoff_out, rivflow )

!-------------------------------------------------------------------------------
!
! Description:
!   Perform the routing of surface and sub-surface runoff using RFM on landpts
!
!   This routine regrids the total surface runoff to the river grid and passes
!   it to the RFM routines to be routed.
!
!-------------------------------------------------------------------------------
! Modules used:

USE jules_rivers_mod, ONLY:                                                &
!  imported scalars with intent(in)
    np_rivers,rivers_regrid, il_river_grid                                    

USE rivers_utils, ONLY:                                                    &
!  imported procedures
     rivers_regrid_from_land, rivers_regrid_to_land

IMPLICIT NONE

!-------------------------------------------------------------------------------

! Scalar arguments with intent(in)
INTEGER, INTENT(IN) :: global_land_pts
                             ! Size of GLOBAL runoff arrays on land points 
! Array arguments with intent(in)
REAL, INTENT(IN) :: sub_runoffin(global_land_pts)
                             ! Average rate of sub surface runoff since
                             ! last rivers call on land_pts in kg m-2 s-1
REAL, INTENT(IN) :: surf_runoffin(global_land_pts)
                             ! Average rate of surface runoff since last
                             ! rivers call on land_pts in kg m-2 s-1

! Array arguments with intent(out)
REAL, INTENT(OUT) :: runoff_out(global_land_pts)
                             ! Total runoff diagnostic on land_pts
                             ! in kg m-2 s-1
REAL, INTENT(OUT) :: rivflow(global_land_pts)
                             ! River flow diagnostic on land_pts
                             ! in kg m-2 s-1

! Local scalar variables
INTEGER :: ip                !  loop counter

! Local array variables.
REAL :: outflow(np_rivers)
                             !  rate of channel surface flow leaving gridbox
                             !  (kg m-2 s-1)
REAL :: baseflow(np_rivers)
                             !  rate of channel base flow leaving
                             !  gridbox (kg m-2 s-1)
REAL :: surf_runoff_rivers(np_rivers)
                             !  average rate of surface runoff since last
                             !  rivers call (kg m-2 s-1) on rivers grid.
REAL :: sub_runoff_rivers(np_rivers)
                             !  average rate of sub-surface runoff since last
                             !  rivers call (kg m-2 s-1) on rivers grid.
REAL :: tot_runoff_rivers(np_rivers)
                             ! runoff diagnostic on rivers grid (kg m-2 s-1)

!-------------------------------------------------------------------------------
! Initialisation
!-------------------------------------------------------------------------------

outflow(:)    = 0.0
baseflow(:)   = 0.0
rivflow(:)    = 0.0
runoff_out(:) = 0.0

!-------------------------------------------------------------------------------
! Regrid surface and subsurface runoff from land points to rivers points
!-------------------------------------------------------------------------------

sub_runoff_rivers(:)  = 0.0
surf_runoff_rivers(:) = 0.0

! If regridding required, call routine
IF ( rivers_regrid ) THEN

  CALL rivers_regrid_from_land( global_land_pts, sub_runoffin,              &
                                np_rivers, sub_runoff_rivers)
  CALL rivers_regrid_from_land( global_land_pts, surf_runoffin,             &
                                np_rivers, surf_runoff_rivers )

  ! If not regridding, translate between global_land and river point vectors
ELSE IF (global_land_pts /= np_rivers) THEN

  DO ip = 1,np_rivers
    IF (il_river_grid(ip) > 0) THEN
      sub_runoff_rivers(ip) = sub_runoffin(il_river_grid(ip))
      surf_runoff_rivers(ip) = surf_runoffin(il_river_grid(ip))
    END IF
  END DO
    
  ! If grids identical, including land/riv points in same order, no need to regrid
ELSE

  sub_runoff_rivers(:)  = sub_runoffin(:)
  surf_runoff_rivers(:) = surf_runoffin(:)

END IF

!-------------------------------------------------------------------------------
! Call the routing routine.
!-------------------------------------------------------------------------------

CALL rivers_rfm_vector( surf_runoff_rivers, sub_runoff_rivers,             &
                        outflow, baseflow )

!-------------------------------------------------------------------------------
!   Regrid from rivers to land grid
!-------------------------------------------------------------------------------

! Sum the surface and subsurface runoffs
tot_runoff_rivers(:) = surf_runoff_rivers(:) + sub_runoff_rivers(:)

! If regridding required, call routine
IF ( rivers_regrid ) THEN

  CALL rivers_regrid_to_land( np_rivers, outflow, global_land_pts, rivflow)
  ! Sanity check regridding routines - expect input = output
  CALL rivers_regrid_to_land( np_rivers, tot_runoff_rivers,                &
                              global_land_pts, runoff_out)

  ! If not regridding, translate between land and river point vectors
ELSE IF (global_land_pts /= np_rivers) THEN
    
  DO ip = 1,np_rivers
    IF (il_river_grid(ip) > 0) THEN
      rivflow(il_river_grid(ip))    = outflow(ip)
      runoff_out(il_river_grid(ip)) = tot_runoff_rivers(ip)
    END IF
  END DO

  ! If grids identical, including land/riv points in same order, no need to regrid
ELSE
 
  rivflow(:)    = outflow(:)
  runoff_out(:) = tot_runoff_rivers(:)

END IF

END SUBROUTINE rivers_drive_rfm

!###############################################################################
! subroutine rivers_rfm_vector
!
!-----------------------------------------------------------------------------
! Description:
!   Perform the routing of surface and sub-surface runoff for Regional
!   model.
!   Calculates river outflow (kg m-2 s-1) and baseflow (kg m-2 s-1) for the
!   RFM kinematic wave river routing model.
!
! Method:
!   See Bell et al. 2007 Hydrol. Earth Sys. Sci. 11. 532-549
!   This subroutine routes surface and subsurface runoff
!   for the RCM. The routing procedure is performed on the
!   RCM grid, so no regridding is needed. The routing
!   procedure is currently based on the Kinematic Wave
!   routing model used at CEH Wallingford.
!
! Author: V.A.Bell, CEH Wallingford, 21.08.03
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!
! Code History:
!    Modified from river2a.f by sjd 13/05/05
!    Modified from UM routine riv_rout-river2a.F90 by hl 13/04/14
!    Updated to route runoff on riv_pts vector only by hl 24/04/14
!
!  MODEL            MODIFICATION HISTORY FROM MODEL VERSION 5.5:
! VERSION  DATE
!   6.0   12/09/03  Change DEF from A20 to A26. D. Robinson
!   6.0   12.09.03  Routing code added. V.A.Bell
!   x.x   02/05/12  Additional implementation and developments S. Dadson
!   x.x   06/01/15  Formal implementation within JULES code base H. Lewis
!
! NOTE ON UNITS:
!   This routine, based on Simon Dadson's work, includes a modification to the
!   'standard' RFM routines which assume a regular x/y-grid based
!   implementation to account for potential variable grid box areas (e.g. from
!   lat/lon grid).
!   Stores are calculated in units of m x m2 rather than mm, and flows are
!   initially calculated in units of m3/s.
!   For consistency with other routines (for now), the output is converted
!   again to a flux density kg/m2/s

SUBROUTINE rivers_rfm_vector( sfc_runoff, sub_sfc_runoff,                   &
                              outflow, baseflow )

USE jules_rivers_mod, ONLY:                                                &
!  imported scalar parameters
       rivers_timestep, np_rivers,rivers_dlat, rivers_first, rivers_dx        &
!  imported scalars with intent (inout)
!       ,l_rfmFirst, l_rfm_overbank &
!  imported arrays with intent(in)
       ,rfm_flowobs1_rp                                                       &
!  imported arrays with intent(inout)
       ,rivers_next_rp, rfm_iarea_rp, rfm_land_rp                             &
       ,rfm_substore_rp, rfm_surfstore_rp, rfm_flowin_rp, rfm_bflowin_rp      &
       ,rfm_rivflow_rp, rfm_baseflow_rp, rivers_boxareas_rp

USE timestep_mod, ONLY: timestep

USE planet_constants_mod, ONLY: planet_radius  ! the Earth's radius (m)

USE conversions_mod, ONLY: pi_over_180

USE water_constants_mod, ONLY: rho_water

USE jules_riversparm, ONLY: cland, criver, cbland, cbriver,                &
                             runoff_factor, retl, retr, slfac, a_thresh

USE logging_mod, ONLY: log_warn, log_fatal

USE jules_print_mgr, ONLY:                                                 &
   jules_message,                                                           &
   jules_print

!-------------------------------------------------------------------------------

IMPLICIT NONE

! IN Arguments

! Array arguments with intent(in)
REAL, INTENT(IN) :: sfc_runoff(np_rivers)
       !  average rate of surface runoff since last rivers call (kg m-2 s-1)
REAL, INTENT(IN) :: sub_sfc_runoff(np_rivers)
       !  average rate of sub-surface runoff since last call (kg m-2 s-1)

REAL, INTENT(OUT) :: outflow(np_rivers)
       !  rate of channel surface flow leaving gridbox (kg m-2 s-1)
REAL, INTENT(OUT) :: baseflow(np_rivers)
       !  rate of channel base flow leaving gridbox (kg m-2 s-1)

! internal variables
INTEGER ::                                                                  &
     landtype                                                               &
       !  local for land type
     ,rn                                                                    &
       !  local co-ords of downstream point
     ,ip
       !  co-ordinate counters in do loops

REAL ::                                                                     &
   landtheta, rivertheta                                                    &
       !  surface wave speed factors
   ,sublandtheta, subrivertheta                                             &
       !  sub-surface wave speed factors
   ,returnflow                                                              &
       !  returnflow (m3 per timestep)
   ,flowobs1_m3s                                                            &
       !  initial river flow [m3/s]
   ,dt                                                                      &
       !  river routing model timestep (s)
   ,dx
       !  distance between midpoints of neighbouring cells (m)

REAL ::                                                                     &
   substore_n(np_rivers)                                                    &
       !   subsurface store at next timestep (m3 per timestep)
   ,surfstore_n(np_rivers)                                                  &
       !   surface store at next timestep (m3 per timestep)
   ,flowin_n(np_rivers)                                                     &
       !   surface lateral inflow next time (m3 per timestep)
   ,bflowin_n(np_rivers)                                                    &
       !   sub-surface lateral inflow next time (m3 per timestep)
   ,rarea(np_rivers)                                                        &
       !   accumulated area (iarea) (m2)
   ,surf_roff(np_rivers)                                                    &
       !   INTERNAL surf_runoff (m3 per timestep)
   ,sub_surf_roff(np_rivers)
       !   INTERNAL sub_surf_runoff (m3 per timestep)

INTEGER :: nland, nsea, nriv

!-------------------------------------------------------------------------------
! Set up rivers parameters
!-------------------------------------------------------------------------------

! rivers model timestep(s)
dt = rivers_timestep * timestep                     

! horizontal gridsize (m)
IF (rivers_dx <= 0) THEN
  dx = planet_radius * (ABS(rivers_dlat) * pi_over_180)  
ELSE
  dx = rivers_dx
END IF

! Wave speed factors (dimensionless)
rivertheta    = criver  * dt / dx
landtheta     = cland   * dt / dx
sublandtheta  = cbland  * dt / dx
subrivertheta = cbriver * dt / dx

! Check condition for numerical stability
IF (landtheta > 1.0 .OR. sublandtheta > 1.0 .OR.                            &
    rivertheta > 1.0 .OR. subrivertheta > 1.0) THEN
  CALL log_warn("rivers_rfm",                                               &
                "Finite difference method will be unstable in RFM routing:" &
                // " setting thetas to zero")
  rivertheta    = 0.0
  landtheta     = 0.0
  sublandtheta  = 0.0
  subrivertheta = 0.0
END IF

!-------------------------------------------------------------------------------
! Initialise variables at first timestep
!-------------------------------------------------------------------------------

nland = 0
nsea  = 0
nriv  = 0

IF (rivers_first) THEN

  ! From the cumulative catchment areas dataset (rfm_iarea), determine
  ! which grid cells are land or river
  !------------------------------------------------------------------
  ! rfm_land_rp() is set at the first entry, and the values must be
  ! retained for subsequent timesteps
  !-------------------------------------------------------------------

  rivers_first = .FALSE.

  DO ip = 1, np_rivers

    IF (rfm_iarea_rp(ip) < 0) THEN
      rfm_land_rp(ip) = 0     !Sea
      nsea = nsea + 1
    ELSE IF (rfm_iarea_rp(ip) > a_thresh) THEN
      rfm_land_rp(ip) = 1     !river
      nriv = nriv + 1
    ELSE
      rfm_land_rp(ip) = 2     !land
      nland = nland + 1
    END IF
    !      rarea(ip) = REAL(rfm_iarea_rp(ip)) + 1.0
          ! include current point, so add 1

          ! set to sea if top or left edge drains outside model domain
    IF (rivers_next_rp(ip) < 1 .OR. rivers_next_rp(ip) > np_rivers) THEN
      rfm_land_rp(ip) = 0  !Sea
    END IF

    ! Initialise surface and sub-surface stores using flow observations if available
    IF ( rfm_flowobs1_rp(ip) > 0.0 ) THEN
      flowobs1_m3s = rfm_flowobs1_rp(ip) * rivers_boxareas_rp(ip) / rho_water
      rfm_surfstore_rp(ip) = flowobs1_m3s * dt / rivertheta
      rfm_substore_rp(ip) = flowobs1_m3s * dt / subrivertheta
    END IF
  END DO

END IF   ! end riversFirst

!-------------------------------------------------------------------------------
! Processing for each timestep
!-------------------------------------------------------------------------------

!-------------------------------------------------------------------------------
! Convert runoff from (kg m-2 s-1) to (m3 per gridcell per timestep)
!-------------------------------------------------------------------------------

DO ip = 1,np_rivers

  IF ( sfc_runoff(ip) >= 0.0 .AND. sub_sfc_runoff(ip) >= 0.0 ) THEN   

    surf_roff(ip) = runoff_factor * sfc_runoff(ip) *                        &
                          dt * rivers_boxareas_rp(ip) / rho_water
    sub_surf_roff(ip) = runoff_factor * sub_sfc_runoff(ip) *                &
                          dt * rivers_boxareas_rp(ip) / rho_water

  ELSE

    ! ignore no data (-1.00e20) and other negative values
    surf_roff(ip) = 0.0
    sub_surf_roff(ip) = 0.0

  END IF

END DO

!-------------------------------------------------------------------------------
! Initialise accumulated inflows and stores for the next timestep
!-------------------------------------------------------------------------------

flowin_n(:)    = 0.0
bflowin_n(:)   = 0.0
surfstore_n(:) = 0.0
substore_n(:)  = 0.0

!-------------------------------------------------------------------------------
! Rivers runoff using simple kinematic wave model (see Lewis et al. 2018:Appx.B)
!-------------------------------------------------------------------------------

DO ip = 1,np_rivers

  rn = rivers_next_rp(ip)
  landtype = NINT( rfm_land_rp(ip) )

  IF (landtype == 2) THEN  !land

    ! land surface
    surfstore_n(ip) = (1.0 - landtheta) * rfm_surfstore_rp(ip) +            &
                             rfm_flowin_rp(ip) + surf_roff(ip)

    ! land subsurface
    substore_n(ip) = (1.0 - sublandtheta) * rfm_substore_rp(ip) +           &
                             rfm_bflowin_rp(ip) + sub_surf_roff(ip)

    ! return flow
    IF (retl > 0) THEN ! changed to allow for -ve retr/l (sjd: 4/6/09)
      returnflow = MAX( ABS( substore_n(ip) * retl ), 0.0 )
    ELSE
      returnflow = -1.0 * MAX( ABS( surfstore_n(ip) * retl ), 0.0 )
    END IF

    substore_n(ip)  = substore_n(ip)  - returnflow
    surfstore_n(ip) = surfstore_n(ip) + returnflow

    IF ( rn > 0 ) THEN
      flowin_n(rn)  = flowin_n(rn)  + landtheta    * rfm_surfstore_rp(ip)
      bflowin_n(rn) = bflowin_n(rn) + sublandtheta * rfm_substore_rp(ip)
    END IF
      
    rfm_rivflow_rp(ip)  = rfm_surfstore_rp(ip) * (landtheta    / dt)
    rfm_baseflow_rp(ip) = rfm_substore_rp(ip)  * (sublandtheta / dt)
      
  ELSE IF (landtype == 1) THEN  !river

    ! river subsurface
    substore_n(ip) = (1.0 - subrivertheta) * rfm_substore_rp(ip) +          &
                          rfm_bflowin_rp(ip) + sub_surf_roff(ip)

    ! river surface
    surfstore_n(ip) = (1.0 - rivertheta) * rfm_surfstore_rp(ip) +           &
                            rfm_flowin_rp(ip) + surf_roff(ip)

    ! return flow
    IF (retr > 0) THEN ! changed to allow for -ve retr/l (sjd: 4/6/09)
      returnflow = MAX( ABS( substore_n(ip) * retr ), 0.0 )
    ELSE
      returnflow = -1.0 * MAX( ABS( surfstore_n(ip) * retr ), 0.0 )
    END IF
    substore_n(ip)  = substore_n(ip)  - returnflow
    surfstore_n(ip) = surfstore_n(ip) + returnflow

    IF ( rn > 0 ) THEN
      flowin_n(rn)  = flowin_n(rn)  + rivertheta    * rfm_surfstore_rp(ip)
      bflowin_n(rn) = bflowin_n(rn) + subrivertheta * rfm_substore_rp(ip)
    END IF

    rfm_rivflow_rp(ip)  = rfm_surfstore_rp(ip) * (rivertheta    / dt)
    rfm_baseflow_rp(ip) = rfm_substore_rp(ip)  * (subrivertheta / dt)

  END IF ! land or river

  ! accumulate flow into the sea if the next point is sea
  ! (assume it's equal to river flow in the adjacent land pt)
  IF ( rn > 0 ) THEN
    IF ( NINT(rfm_land_rp(rn)) == 0) THEN
      rfm_rivflow_rp(rn) = rfm_rivflow_rp(rn) + rfm_rivflow_rp(ip)
    END IF
  END IF
    
END DO !end of rivers loop, ip

!-------------------------------------------------------------------------------
! Housekeeping for next timestep
!-------------------------------------------------------------------------------

  ! keep inflows for next timestep
rfm_flowin_rp(:)  = flowin_n(:)
rfm_bflowin_rp(:) = bflowin_n(:)

! keep rivers stores for next timestep (m3)
rfm_surfstore_rp(:) = surfstore_n(:)
rfm_substore_rp(:)  = substore_n(:)

!-------------------------------------------------------------------------------
! Return flows in flux density units kg/m2/s
!-------------------------------------------------------------------------------
! HL: N.B. Add option to select what output units required for river flow

outflow(:)  = rfm_rivflow_rp(:)  * rho_water / rivers_boxareas_rp(:)
baseflow(:) = rfm_baseflow_rp(:) * rho_water / rivers_boxareas_rp(:)

END SUBROUTINE rivers_rfm_vector

!###############################################################################
!###############################################################################

END MODULE jules_rivers_rfm_mod












!###############################################################################
!###############################################################################

MODULE rivers_utils

USE conversions_mod, ONLY: pi, pi_over_180

USE planet_constants_mod, ONLY: planet_eq_radius,                          &
                                ! Earth's equatorial radius 'a'
                                eccensq,                                    &
                                ! square of eccentricity of Earth spheroid
                                eccen
                                ! eccentricity of Earth spheroid

!-----------------------------------------------------------------------------
! Description:
!   Contains river routing utility functions for standalone running
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

CONTAINS

!###############################################################################
! subroutine rivers_get_xy_pos
!
! Internal procedure in module grid_utils
!    Given a point number and the extents (size) of a 2-D (xy) grid, returns
!    the x and y indices (coords) of the point. All coords are relative to (1,1)
!    at bottom left of grid, with numbering running left to right, bottom to
!    top.

SUBROUTINE rivers_get_xy_pos( i, nx, ny, ix, iy )

IMPLICIT NONE

! Scalar arguments with intent (in)
INTEGER, INTENT(IN) ::                                                      &
                       i,                                                   &
                   !  the point number
                       nx,                                                  &
                   !  the extent (size) of the grid in the x direction
                       ny
                   !  the extent (size) of the grid in the y direction
                   !  NB ny is only required for (cursory) error checking.

! Scalars with intent (out)
INTEGER, INTENT(OUT) ::  ix,iy   !  the x and y coordinates of the point

! Assume that i>=1!

iy = ( i - 1 ) / nx + 1

ix = i - (iy-1) * nx

! If locations are out of range (grid is too small for this point number),
! set to -1.
IF ( ix > nx .OR. iy > ny ) THEN
  ix = -1
  iy = -1
END IF

END SUBROUTINE rivers_get_xy_pos
!###############################################################################


!###############################################################################
! subroutine rivers_remap_match
! Computes regridding between land points and main model grid where both 
! grids overlap (i.e. rivers_regrid=False)
SUBROUTINE rivers_remap_match
!-----------------------------------------------------------------------------
!
! Description:
!   Computes remapping between land points and river routing grid
!
!   If the "main" model grid is 2-D, this target grid is the 2-D grid.
!   If the "main" grid is a vector (in offline applications of JULES this is
!   possible if points from a larger grid have been compressed - e.g. land
!   points selected from a larger grid.), the target grid is the larger grid,
!   across which the points are to be scattered.
!
!------------------------------------------------------------------------------
! Modules used:

USE jules_rivers_mod, ONLY:                                                &
!  imported arrays with intent(in)
     rivers_lat_rp, rivers_lon_rp, np_rivers, ir_land_grid, il_river_grid

USE model_grid_mod, ONLY: global_land_pts, latitude_of_land_pts,           &
    longitude_of_land_pts

USE parallel_mod, ONLY: master_task_id, is_master_task, gather_land_field

IMPLICIT NONE

REAL, ALLOCATABLE :: global_lat_of_land_pts(:)
REAL, ALLOCATABLE :: global_lon_of_land_pts(:)

! Local scalar variables.

INTEGER :: l      !  loop counter (land point)
INTEGER :: ip     !  work

!------------------------------------------------------------------------------
! Compute full land_pts grid lat/lon
!------------------------------------------------------------------------------

IF ( is_master_task() ) THEN
  ALLOCATE(global_lat_of_land_pts(global_land_pts))
  ALLOCATE(global_lon_of_land_pts(global_land_pts))
ELSE
  ALLOCATE(global_lat_of_land_pts(1))
  ALLOCATE(global_lon_of_land_pts(1))
END IF
 
global_lat_of_land_pts(:) = 0.0
global_lon_of_land_pts(:) = 0.0

CALL gather_land_field(latitude_of_land_pts, global_lat_of_land_pts)
CALL gather_land_field(longitude_of_land_pts, global_lon_of_land_pts)

!------------------------------------------------------------------------------
! Compute remap between land points and full river grid 
!         general case, covers all grid types when rivers_regrid=False
!------------------------------------------------------------------------------

IF ( is_master_task() ) THEN
  DO l = 1,global_land_pts
    DO ip = 1,np_rivers
      IF (rivers_lat_rp(ip) == global_lat_of_land_pts(l) .AND.        &
         rivers_lon_rp(ip) == global_lon_of_land_pts(l)) THEN
        il_river_grid(ip) = l
        ir_land_grid(l) = ip
        EXIT
      END IF
    END DO
  END DO
END IF
  
DEALLOCATE(global_lat_of_land_pts)
DEALLOCATE(global_lon_of_land_pts)

END SUBROUTINE rivers_remap_match
!###############################################################################
!###############################################################################
! subroutine rivers_remap_unmatch
! Setup initial arrays for regridding between land points and main model grid 
! where both grids DO NOT overlap
SUBROUTINE rivers_remap_unmatch
!-----------------------------------------------------------------------------
!
! Description:
!
!------------------------------------------------------------------------------
! Modules used:

USE jules_rivers_mod, ONLY:                                                &
!  imported scalars with intent(in) - defining grid
     nx_grid, ny_grid, reg_dlat, reg_dlon, reg_lon1, reg_lat1,                &
     rivers_reglatlon, rivers_regrid,                                         &
!  imported arrays with intent(in)
     rivers_lat_rp, rivers_lon_rp, np_rivers, ir_land_grid, il_river_grid

USE model_grid_mod, ONLY: global_land_pts, latitude_of_land_pts,           &
    longitude_of_land_pts

USE parallel_mod, ONLY: master_task_id, is_master_task, gather_land_field

IMPLICIT NONE

REAL, ALLOCATABLE :: global_lat_of_land_pts(:)
REAL, ALLOCATABLE :: global_lon_of_land_pts(:)

! Local scalar variables.

INTEGER :: l      !  loop counter (land point)
INTEGER :: i,j    !  work

!------------------------------------------------------------------------------
! Compute full land_pts grid lat/lon
!------------------------------------------------------------------------------

IF ( is_master_task() ) THEN
  ALLOCATE(global_lat_of_land_pts(global_land_pts))
  ALLOCATE(global_lon_of_land_pts(global_land_pts))
ELSE
  ALLOCATE(global_lat_of_land_pts(1))
  ALLOCATE(global_lon_of_land_pts(1))
END IF
  
global_lat_of_land_pts(:) = 0.0
global_lon_of_land_pts(:) = 0.0

CALL gather_land_field(latitude_of_land_pts, global_lat_of_land_pts)
CALL gather_land_field(longitude_of_land_pts, global_lon_of_land_pts)

!------------------------------------------------------------------------------
! Precalculate landpoints mapping to full 2D grid if river grid and land
! grids do not coincide
!------------------------------------------------------------------------------
IF ( is_master_task() ) THEN
  IF ( rivers_regrid .AND. rivers_reglatlon ) THEN

    DO l = 1,global_land_pts
      i = NINT( (global_lon_of_land_pts(l) - reg_lon1) / reg_dlon ) + 1
      j = NINT( (global_lat_of_land_pts(l) - reg_lat1) / reg_dlat ) + 1
      ir_land_grid(l) = (j-1) * nx_grid + i
    END DO
      
  END IF    
END IF
  
DEALLOCATE(global_lat_of_land_pts)
DEALLOCATE(global_lon_of_land_pts)

END SUBROUTINE rivers_remap_unmatch
!###############################################################################

!###############################################################################
! subroutine rivers_regrid_from_land
! Handles regridding of runoff from "main" to rivers grids.

SUBROUTINE rivers_regrid_from_land( land_pts, runoff_land, riv_pts,         &
                                    runoff_riv )
!------------------------------------------------------------------------------
! Description:
!   Regrids runoff from a source grid to a target grid (the rivers grid).
!   Both grids must be regular in latitude and longitude.

USE jules_rivers_mod, ONLY:                                              &
!  imported scalars with intent(in)
     nx_rivers,ny_rivers, nx_grid, ny_grid, rivers_index_rp, ir_land_grid
    
IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts  !  a vector length
INTEGER, INTENT(IN) :: riv_pts  !  a vector length

! Array arguments with intent(in)
    
REAL, INTENT(IN) :: runoff_land(land_pts)
!  runoff rate on model grid (kg m-2 s-1)
    
! Array arguments with intent(out)
REAL, INTENT(OUT) :: runoff_riv(riv_pts)
!  runoff rate on rivers grid (kg m-2 s-1)

INTEGER :: maxlin, l, ip, ix, iy
    
REAL :: runoff_grid(nx_grid,ny_grid)
REAL :: runoff_out(nx_rivers,ny_rivers)

! Convert from land_pts to full grid
runoff_grid(:,:) = 0.0
DO l = 1,land_pts
  CALL rivers_get_xy_pos( ir_land_grid(l), nx_grid, ny_grid, ix, iy )
  runoff_grid(ix,iy) = runoff_land(l)
END DO
    
! Call to lat/lon grid-based regridding routine
maxlin = ( nx_grid + nx_rivers ) * ( ny_grid + ny_rivers )
CALL rivers_route_regrid ( maxlin, runoff_grid, runoff_out )

! Convert from rivers grid to riv_pts
DO ip = 1,riv_pts
  CALL rivers_get_xy_pos(rivers_index_rp(ip),nx_rivers,ny_rivers,ix,iy)
  runoff_riv(ip) = runoff_out(ix,iy)
END DO
    
END SUBROUTINE rivers_regrid_from_land

!###############################################################################
! subroutine rivers_regrid_to_land
! Handles regridding of runoff from rivers to "main" grids.

SUBROUTINE rivers_regrid_to_land( riv_pts, runoff_riv, land_pts, runoff_land )
!------------------------------------------------------------------------------
! Description:
!   Regrids runoff from a source grid to a target grid (the rivers grid).
!   Both grids must be regular in latitude and longitude.

USE jules_rivers_mod, ONLY:                                              &
!  imported scalars with intent(in)
     nx_rivers,ny_rivers, nx_grid, ny_grid, rivers_index_rp, ir_land_grid
    
IMPLICIT NONE

INTEGER, INTENT(IN) :: land_pts  !  a vector length
INTEGER, INTENT(IN) :: riv_pts  !  a vector length

! Array arguments with intent(in)
REAL, INTENT(IN) :: runoff_riv(riv_pts)
!  runoff rate on rivers grid (kg m-2 s-1)

! Array arguments with intent(out)
REAL, INTENT(OUT) :: runoff_land(land_pts)
!  runoff rate on model grid (kg m-2 s-1)
    
INTEGER :: maxlin, l, ip, ix, iy
    
REAL :: runoff_grid(nx_grid,ny_grid)
REAL :: runoff_out(nx_rivers,ny_rivers)

! Convert from riv_pts to rivers grid
runoff_out(:,:) = 0.0
DO ip = 1,riv_pts
  CALL rivers_get_xy_pos(rivers_index_rp(ip),nx_rivers,ny_rivers,ix,iy)
  runoff_out(ix,iy) = runoff_riv(ip)
END DO
        
! Call to lat/lon grid-based regridding routine
maxlin = ( nx_grid + nx_rivers ) * ( ny_grid + ny_rivers )
CALL rivers_route_regrid_invert ( maxlin, runoff_out, runoff_grid )

! Convert from full grid to land_pts
DO l = 1,land_pts
  CALL rivers_get_xy_pos( ir_land_grid(l), nx_grid, ny_grid, ix, iy )
  runoff_land(l) = runoff_grid(ix,iy)
END DO
    
END SUBROUTINE rivers_regrid_to_land
  
!###############################################################################
! subroutine rivers_route_regrid
! Handles regridding of runoff from "main" to rivers grids.

SUBROUTINE rivers_route_regrid( maxlin, runoff_grid, runoff_out )
!------------------------------------------------------------------------------
! Description:
!   Regrids runoff from a source grid to a target grid (the rivers grid).
!   Both grids must be regular in latitude and longitude.
!
!------------------------------------------------------------------------------
! Modules used:

USE jules_rivers_mod, ONLY:                                                &
!  imported scalars with intent(in)
     nx_rivers,ny_rivers,rivers_dlat,rivers_dlon,rivers_lat1,rivers_lon1      &
     , nx_grid, ny_grid, reg_dlat, reg_dlon, reg_lat1, reg_lon1

USE areaver_mod, ONLY: pre_areaver, do_areaver

USE jules_print_mgr, ONLY:                                                 &
  jules_message,                                                            &
  jules_print

IMPLICIT NONE

! Scalar arguments with intent(in)

INTEGER, INTENT(IN) :: maxlin  !  a vector length

! Array arguments with intent(in)

REAL, INTENT(IN) :: runoff_grid(nx_grid,ny_grid)
                               !  runoff rate on model grid (kg m-2 s-1)

! Array arguments with intent(out)

REAL, INTENT(OUT) :: runoff_out(nx_rivers,ny_rivers)
                               !  runoff rate on rivers grid (kg m-2 s-1)

! Local scalar variables

INTEGER :: adjust       !  mode (option number) of area averaging
INTEGER :: icode        !  exit code from subroutines
INTEGER :: ix           !  loop counter
INTEGER :: iy           !  loop counter
INTEGER :: maxL         !  number of entries in output from pre_areaver

LOGICAL :: cyclic_srce  !  TRUE source (model) grid is cyclic in x
LOGICAL :: cyclic_targ  !  TRUE target (model) grid is cyclic in x
LOGICAL :: invert_srce  !  TRUE source (model) grid runs N to S
                        !  FALSE source grid runs S to N
LOGICAL :: spherical    !  TRUE  coordinates are lat/lon on a sphere
                        !  FALSE Cartesian axes.
LOGICAL :: want         !  Value of masks at locations for which data are
                        !  required

CHARACTER(LEN=80) :: cmessage  !  error message from subroutines

! Local array variables.

INTEGER :: count_targ(nx_rivers,ny_rivers)
                        !  number of model gridboxes that
                        !  contribute to each rivers gridbox
INTEGER :: base_targ(nx_rivers,ny_rivers)
                        !  base (starting) index for each rivers gridbox
                        !  (i.e. location of first element in lists)
INTEGER :: index_srce(maxLin)
                        !  list of source (model) gridboxes
                        !  that contribute to each rivers gridbox

REAL :: sourcelat(ny_grid+1)
                        !  latitudes of edges of source (model) gridboxes.
                        !  First value is the S edge of first gridbox,
                        !  all other values are the N edge of each gridbox.
REAL :: sourcelon(nx_grid+1)
                        !  longitudes of edges of source (model) gridboxes
                        !  First value is the W edge of first gridbox, all
                        !  other values are the E edge of each gridbox.
REAL :: targetlat(ny_rivers+1)
                        !  latitudes of edges of target (mrivers) gridboxes.
                        !  First value is the S edge of first gridbox, all
                        !  other values are the N edge of each gridbox.
REAL :: targetlon(nx_rivers+1)
                        !  longitudes of edges of target (rivers) gridboxes
                        !  First value is the W edge of first gridbox, all
                        !  other values are the E edge of each gridbox.

REAL :: adjust_targ(nx_rivers,ny_rivers)
                        !  adjustment factors (not used)
REAL :: weight(maxLin)
                        !   lists of weights for each source (model) gridbox

LOGICAL :: mask_srce(nx_grid,ny_grid)
LOGICAL :: mask_targ(nx_rivers,ny_rivers)

!------------------------------------------------------------------------------

! Set values

spherical = .TRUE.    !  Calculations are for lat/lon coordinates.
adjust    = 0         !  "normal" area averaging
maxL      = maxlIn

! Decide if grids run S-N or N-S in latitude

IF ( reg_dlat > 0.0) THEN
  invert_srce = .FALSE.                   !  model grid runs S to N
ELSE
  invert_srce = .TRUE.                    !  model grid runs N to S
END IF

! Decide if grids are cyclic in longitude.

IF ( REAL(nx_grid) * reg_dlon > 359.9 ) THEN
  cyclic_srce = .TRUE.
ELSE
  cyclic_srce = .FALSE.
END IF
IF ( REAL(nx_rivers) * rivers_dlon > 359.9 ) THEN
  cyclic_targ = .TRUE.
ELSE
  cyclic_targ = .FALSE.
END IF

!------------------------------------------------------------------------------
! Set coordinates of edges of model gridboxes
!------------------------------------------------------------------------------

DO ix = 1,nx_grid+1
  sourcelon(ix) = reg_lon1 + (REAL(ix-1) - 0.5) * reg_dlon
END DO
DO iy = 1,ny_grid+1
  sourcelat(iy) = reg_lat1 + (REAL(iy-1) - 0.5) * reg_dlat
END DO

!------------------------------------------------------------------------------
! Set coordinates of edges of rivers gridboxes
!------------------------------------------------------------------------------

DO ix = 1,nx_rivers+1
  targetlon(ix) = rivers_lon1 + (REAL(ix-1) - 0.5) * rivers_dlon
END DO
DO iy = 1,ny_rivers+1
  targetlat(iy) = rivers_lat1 + (REAL(iy-1) - 0.5) * rivers_dlat
END DO

!------------------------------------------------------------------------------
! Set masks to indicate that all points in both grids are to be used
!------------------------------------------------------------------------------

want           = .TRUE.
mask_srce(:,:) = want
mask_targ(:,:) = want

!------------------------------------------------------------------------------
! Call setup routing for averaging
!------------------------------------------------------------------------------

CALL pre_areaver( nx_grid, sourcelon, ny_grid, sourcelat                    &
                 ,cyclic_srce, nx_grid, want, mask_srce                     &
                 ,nx_rivers, targetlon, ny_rivers, targetlat                &
                 ,cyclic_targ, spherical, maxL, count_targ, base_targ       &
                 ,index_srce, weight, icode, cmessage )

!------------------------------------------------------------------------------
! Call averaging routine
!------------------------------------------------------------------------------

CALL do_areaver( nx_grid, ny_grid, nx_grid                                  &
                ,invert_srce, runoff_grid, nx_rivers, ny_rivers             &
                ,count_targ, base_targ, nx_rivers, want, mask_targ          &
                ,index_srce, weight, adjust, runoff_out                     &
                ,adjust_targ, icode, cmessage )

END SUBROUTINE rivers_route_regrid
!###############################################################################

!###############################################################################
! subroutine rivers_route_regrid_invert
! Handles regridding of runoff from "main" to rivers grids.

SUBROUTINE rivers_route_regrid_invert( maxlin, runoff_out, runoff_grid )
!------------------------------------------------------------------------------
! Description:
!   Regrids runoff from a source grid to a target grid (the rivers grid).
!   Both grids must be regular in latitude and longitude.
!   Inverse of rivers_route_regrid
!
!------------------------------------------------------------------------------
! Modules used:

USE jules_rivers_mod, ONLY:                                                &
!  imported scalars with intent(in)
     nx_rivers,ny_rivers,rivers_dlat,rivers_dlon,rivers_lat1,rivers_lon1      &
     , nx_grid, ny_grid, reg_dlat, reg_dlon, reg_lat1, reg_lon1

USE model_grid_mod, ONLY: latitude, longitude

USE areaver_mod, ONLY: pre_areaver, do_areaver

USE jules_print_mgr, ONLY:                                                 &
  jules_message,                                                            &
  jules_print

IMPLICIT NONE

! Scalar arguments with intent(in)

INTEGER, INTENT(IN) :: maxlin    !  a vector length

! Array arguments with intent(in)

REAL, INTENT(IN) :: runoff_out(nx_rivers,ny_rivers)
                                 !  runoff rate on rivers grid (kg m-2 s-1)

! Array arguments with intent(out)

REAL, INTENT(OUT) :: runoff_grid(nx_grid,ny_grid)
                                 !  runoff rate on model grid (kg m-2 s-1)

! Local scalar variables

INTEGER :: adjust        !  mode (option number) of area averaging
INTEGER :: icode         !  exit code from subroutines
INTEGER :: ix            !  loop counter
INTEGER :: iy            !  loop counter
INTEGER :: maxL          !  number of entries in output from pre_areaver

LOGICAL :: cyclic_srce   !  TRUE source (model) grid is cyclic in x
LOGICAL :: cyclic_targ   !  TRUE target (model) grid is cyclic in x
LOGICAL :: invert_srce   !  TRUE source (model) grid runs N to S
                         !  FALSE source grid runs S to N
LOGICAL :: spherical     !  TRUE  coordinates are lat/lon on a sphere
                         !  FALSE Cartesian axes.
LOGICAL :: want
                         !  Value of masks at locations for which data
                         !  are required

CHARACTER(LEN=80) :: cmessage  !  error message from subroutines

! Local array variables

INTEGER :: count_targ(nx_grid,ny_grid)
                         !  number of model gridboxes that contribute to
                         !  each rivers gridbox
INTEGER :: base_targ(nx_grid,ny_grid)
                         !  base (starting) index for each rivers gridbox
                         !  (i.e. location of first element in lists)
INTEGER :: index_srce(maxLin)
                         !  list of source (model) gridboxes that contribute
                         !  to each rivers gridbox

REAL :: sourcelat(ny_rivers+1)
                         !  latitudes of edges of target (model) gridboxes.
                         !  First value is the S edge of first gridbox, all
                         !  other values are the N edge of each gridbox.
REAL :: sourcelon(nx_rivers+1)
                         !  longitudes of edges of target (model) gridboxes
                         !  First value is the W edge of first gridbox, all
                         !  other values are the E edge of each gridbox.
REAL :: targetlat(nx_grid+1)
                         !  latitudes of edges of source (rivers) gridboxes.
                         !  First value is the S edge of first gridbox, all
                         !  other values are the N edge of each gridbox.
REAL :: targetlon(nx_grid+1)
                         !  longitudes of edges of source (rivers) gridboxes
                         !  First value is the W edge of first gridbox, all
                         !  other values are the E edge of each gridbox.

REAL :: adjust_targ(nx_grid,ny_grid)
                         !  adjustment factors (not used)
REAL :: weight(maxlin)
                         !  lists of weights for each source (model) gridbox

LOGICAL :: mask_srce(nx_rivers,ny_rivers)
LOGICAL :: mask_targ(nx_grid,ny_grid)

!------------------------------------------------------------------------------

! Set values
spherical   = .TRUE.    !  Calculations are for lat/lon coordinates.
adjust      = 0         !  "normal" area averaging
maxl        = maxlin
invert_srce = .FALSE.   !  rivers grid runs S to N

! Decide if grids are cyclic in longitude

IF ( REAL(nx_rivers) * rivers_dlon > 359.9 ) THEN
  cyclic_srce = .TRUE.
ELSE
  cyclic_srce = .FALSE.
END IF
IF ( REAL(nx_grid) * reg_dlon > 359.9 ) THEN
  cyclic_targ = .TRUE.
ELSE
  cyclic_targ = .FALSE.
END IF

!------------------------------------------------------------------------------
! Set coordinates of edges of rivers gridboxes
!------------------------------------------------------------------------------

DO ix = 1,nx_rivers+1
  sourcelon(ix) = rivers_lon1 + (REAL(ix-1) - 0.5) * rivers_dlon
END DO
DO iy = 1,ny_rivers+1
  sourcelat(iy) = rivers_lat1 + (REAL(iy-1) - 0.5) * rivers_dlat
END DO

!------------------------------------------------------------------------------
! Set coordinates of edges of model gridboxes
!------------------------------------------------------------------------------

DO ix = 1,nx_grid+1
  targetlon(ix) = reg_lon1 + (REAL(ix-1) - 0.5) * reg_dlon
END DO
DO iy = 1,ny_grid+1
  targetlat(iy) = reg_lat1 + (REAL(iy-1) - 0.5) * reg_dlat
END DO

!------------------------------------------------------------------------------
! Set masks to indicate that all points in both grids are to be used
!------------------------------------------------------------------------------

want           = .TRUE.
mask_srce(:,:) = want
mask_targ(:,:) = want

!------------------------------------------------------------------------------
! Call setup rivers for averaging.
!------------------------------------------------------------------------------

CALL pre_areaver( nx_rivers, sourcelon, ny_rivers, sourcelat, cyclic_srce   &
                 ,nx_rivers, want, mask_srce, nx_grid, targetlon, ny_grid   &
                 ,targetlat, cyclic_targ, spherical, maxL, count_targ       &
                 ,base_targ, index_srce, weight, icode, cmessage )

!------------------------------------------------------------------------------
! Call averaging routine.
!------------------------------------------------------------------------------

CALL do_areaver( nx_rivers, ny_rivers, nx_rivers, invert_srce, runoff_out   &
                ,nx_grid, ny_grid, count_targ, base_targ, nx_grid, want     &
                ,mask_targ, index_srce, weight, adjust,runoff_grid          &
                ,adjust_targ, icode, cmessage )

END SUBROUTINE rivers_route_regrid_invert
!###############################################################################

!###############################################################################
! subroutine get_rivers_len
!     Driver routine that calls procedures that calculates distance
!     between gridpoints.

SUBROUTINE get_rivers_len_rp( np_rivers, rivers_next, lat, lon, length )

USE jules_rivers_mod, ONLY:                                                &
!  imported scalars with intent(in)
     rivers_dlon   

USE jules_print_mgr, ONLY:                                                 &
  jules_message,                                                            &
  jules_print

IMPLICIT NONE

! Array arguments with intent(in)

INTEGER, INTENT(IN) ::  np_rivers,                                          &
                           ! size of river routing vector
                        rivers_next(np_rivers)
                          !  location of the next downstream point
                          
REAL, INTENT(IN) ::                                                         &
                    lat(np_rivers),                                         &
                          !  latitude of points on rivers grid (degrees)
                    lon(np_rivers)
                          !  longitude of points on rivers grid (degrees)

! Array arguments with intent(out)

REAL, INTENT(OUT) :: length(np_rivers)
                          !  distance between each gridpoint and downstream
                          !  gridpoint (m)

! Local scalar variables.

INTEGER ::  ip,jp         !  work
REAL ::  dx,dy            !  work

!------------------------------------------------------------------------------

! Only calculate length at rivers grid points with valid flow direction

DO ip = 1,np_rivers

  !   Get coords of this point and any point immediately downstream

  jp = rivers_next(ip)

  IF ( jp > 0 ) THEN

    !     There is an outflow direction, and the downstream point is on the grid

    length(ip) = givelength( lat(ip), lon(ip), lat(jp), lon(jp) )
      
  ELSE     
     
    !     Outflow to sea, or no outflow (pit or depression).
    !     This is relatively common, hence treatment separately from flow across
    !     grid edge. Use distance to an adjacent point at the same latitude

    length(ip) = givelength( lat(ip), lon(ip), lat(ip), lon(ip) + rivers_dlon )
      
  END IF

END DO ! np_rivers

END SUBROUTINE get_rivers_len_rp
!###############################################################################


!###############################################################################
! subroutine get_rivers_len
!     Driver routine that calls procedures that calculates distance
!     between gridpoints.

SUBROUTINE get_rivers_len( x, xnext, y, ynext, lat, lon, length )

USE jules_rivers_mod, ONLY:                                                &
!  imported scalars with intent(in)
     np_rivers,nx_rivers,ny_rivers,rivers_dlat,rivers_dlon                    &
!  imported arrays with intent(in)
    ,flow_dir_delta

USE jules_print_mgr, ONLY:                                                 &
  jules_message,                                                            &
  jules_print

IMPLICIT NONE

! Array arguments with intent(in)

INTEGER, INTENT(IN) ::                                                      &
                       x(np_rivers),                                        &
                          !  x index on the rivers grid
                       xnext(np_rivers),                                    &
                          !  x location of the next downstream point
                          !      <0 indicates next point is off grid
                          !      and gives the direction of the next point,
                          !      in terms of index in flow_dir_delta
                       y(np_rivers),                                        &
                          !  y index on the rivers grid
                       ynext(np_rivers)
                          !  y location of the next downstream point
                          !      <0 interpreted as for xnext.

REAL, INTENT(IN) ::                                                         &
                    lat(nx_rivers,ny_rivers),                               &
                          !  latitude of points on rivers grid (degrees)
                    lon(nx_rivers,ny_rivers)
                          !  longitude of points on rivers grid (degrees)

! Array arguments with intent(out)

REAL, INTENT(OUT) :: length(nx_rivers,ny_rivers)
                          !  distance between each gridpoint and downstream
                          !  gridpoint (m)

! Local scalar variables.

INTEGER ::  ip,ix,iy,jx,jy          !  work
REAL ::  dx,dy                      !  work

!------------------------------------------------------------------------------

! Only calculate length at rivers grid points with valid flow direction

DO ip = 1,np_rivers

  !   Get coords of this point and any point immediately downstream

  ix = x(ip)
  iy = y(ip)
  jx = xnext(ip)
  jy = ynext(ip)

  IF ( jx > 0 ) THEN

    !     There is an outflow direction, and the downstream point is on the grid

    length(ix,iy) = givelength( lat(ix,iy), lon(ix,iy),                     &
                                lat(jx,jy), lon(jx,jy) )

  ELSE IF ( jx == -9 ) THEN

    !       Outflow to sea, or no outflow (pit or depression).
    !       This is relatively common, hence treatment separately from flow across
    !       grid edge. Use distance to an adjacent point at same latitude.

    length(ix,iy) = givelength( lat(ix,iy), lon(ix,iy),                   &
                                lat(ix,iy), lon(ix,iy) + rivers_dlon )

  ELSE

    !       Outflow across the edge of the grid. We would still like to calculate
    !       a length, so that, as far as possible, a regional implementation gives
    !       the same results as a global implementation, even for the edge points.
    !       NB This works for outflow from grid, but if a regional run is missing
    !       an inflow from outside the grid, answers will generally be different
    !       in global and regional implementations.
    !       The lat/lon of the downstream point is not available in these cases,
    !       so we ASSUME a "regular" lat/lon grid.
    !              jx is -1 * index in flowDirSet.

    !       Get number of gridboxes in each direction to the downstream location

    dx = REAL( flow_dir_delta(ABS(jx),1) )
    dy = REAL( flow_dir_delta(ABS(jx),2) )

    length(ix,iy) = givelength( lat(ix,iy), lon(ix,iy),                   &
                                lat(ix,iy) + dy * rivers_dlat,                &
                                lon(ix,iy) + dx * rivers_dlon )

  END IF

END DO ! np_rivers

END SUBROUTINE get_rivers_len
!###############################################################################

!###############################################################################
! function rivers_earth_area
!
! Internal procedure in module earth_utils.
!     Calculates area of Earth surface between two lines of latitude and two of
!     longitude, assuming that the Earth is a spheriod.
!     Uses equation A6 of Oki and Sud, 1998, Earth Interactions, Vol.2, Paper 1.

FUNCTION rivers_earth_area( lat1d,lat2d,lon1d,lon2d ) RESULT( area )

IMPLICIT NONE

! Scalar function result

REAL ::  area   !  area (m2) (actually in units of earth_radius**2)

! Scalar arguments with intent(in)

REAL, INTENT(IN) :: lat1d  !  latitude of southern edge of strip (degrees)
REAL, INTENT(IN) :: lat2d  !  latitude of northern edge of strip (degrees)
REAL, INTENT(IN) :: lon1d  !  longitude of western edge of strip (degrees)
REAL, INTENT(IN) :: lon2d  !  longitude of eastern edge of strip (degrees)

! Local scalars

REAL ::  esinlat1  !  product [eccen * SIN(lat1)], with lat1 in radians
REAL ::  esinlat2  !  product [eccen * SIN(lat2)], with lat2 in radians
REAL ::  val1, val2, val3      ! work variables

!-------------------------------------------------------------------------------

! Calculate SIN(latitudes)
esinlat1 = eccen * SIN(lat1d * pi_over_180)
esinlat2 = eccen * SIN(lat2d * pi_over_180)

! Evaluate terms at each of lat1 and lat2
val1 = 0.5 * esinlat1 / ( 1.0 - (esinlat1 * esinlat1) )
val1 = val1 + 0.25 * LOG( ABS( (1.0 + esinlat1) / (1.0 - esinlat1) ) )

val2 = 0.5 * esinlat2 / ( 1.0 - (esinlat2 * esinlat2) )
val2 = val2 + 0.25 * LOG( ABS( (1.0 + esinlat2) / (1.0 - esinlat2) ) )

val3 = pi_over_180 * planet_eq_radius * planet_eq_radius *                  &
            (1.0 - eccensq) / eccen

area = ABS(lon2d - lon1d) * val3 * ( val2 - val1 )

END FUNCTION rivers_earth_area
!###############################################################################

!###############################################################################
! function givelength
!
! Internal procedure in module earth_utils.
!      Calculates distance on surface of Earth between two locations,
!      assuming Earth is a spheroid.
!      Based on function giveLen by Taikan Oki (26/August/1996).

FUNCTION givelength( lat1,lon1,lat2,lon2 ) RESULT ( length )

  !  Scalar function result
REAL ::  length   !  distance (m)

!  Scalars with intent (in)

REAL, INTENT(IN) ::  lat1   ! latitude of southern edge of strip (degrees)
REAL, INTENT(IN) ::  lat2   ! latitude of northern edge of strip (degrees)
REAL, INTENT(IN) ::  lon1   ! longitude of western edge of strip (degrees)
REAL, INTENT(IN) ::  lon2   ! longitude of eastern edge of strip (degrees)

!  Local scalars

REAL ::  dlat   !  difference in latitude (degrees)
REAL ::  dlon   !  difference in longitude (degrees)
REAL ::  dx     !  work
REAL ::  dy     !  work
REAL ::  lat    !  work: latitude (degrees)
REAL ::  radius !  equivalent radius of Earth at given latitude (m)

!-------------------------------------------------------------------------------

dlon = ABS( lon2 - lon1 )
IF ( dlon >= 180.0 ) THEN
  dlon = 360.0 - dlon
END IF
dlat = ABS( lat2 - lat1 )

IF ( dlon < EPSILON( dlon ) ) THEN

  !   Constant longitude.
  lat = ( lat1 + lat2 ) * 0.5
  length = givelatlength(lat) * dlat

ELSE IF ( dlat < EPSILON(dlat) ) THEN

  !   Constant latitude.
  length = givelonlength( lat1 ) * dlon

ELSE

  !   Both lat and lon change.
  !   Use equation A8 of Oki and Sud, 1998, Earth Interactions, Vol.2, Paper 1
  lat = ( lat1 + lat2 ) * 0.5
  radius = giveearthradius( lat )
  dx = givelonlength( lat ) * dlon / radius
  dy = givelatlength( lat ) * dlat / radius
  length = ACOS( COS(dx) * COS(dy) ) * radius

END IF

END FUNCTION givelength

!###############################################################################

!###############################################################################
! function givelatlength
!
! Internal procedure in module earth_utils.
!     Calculates the distance (km) along the surface of the Earth between two
!     points per 1 degree of latitude and at the same longitude.
!     Based on function givelat by Taikan Oki (23/April/1996).
!     See EqnA2 of Oki and Sud, 1998, Earth Interactions, Vol.2, Paper 1.

FUNCTION givelatlength( lat )  RESULT (latlen)

IMPLICIT NONE

! Scalar function result
REAL ::  latlen               !  distance between points (m)
                              !  [i.e. same units as planet_eq_radius]

! Scalars with intent (in)
REAL, INTENT(IN) ::  lat      !  effective (e.g. average) latitude (degrees)


! Local scalars
REAL :: esinlat  !  product [eccen * SIN(lat_r)]

esinlat = eccen * SIN ( lat * pi_over_180 )

latlen = pi_over_180 * planet_eq_radius * ( 1 - eccensq)                    &
         / (SQRT(1.0 - ( esinlat * esinlat )))**3

END FUNCTION givelatlength
!###############################################################################

!###############################################################################
! function givelonlength
!
! Internal procedure in module earth_utils.
!    Calculates the distance (km) along the surface of the Earth between two
!    points per degree of longitude and at the same latitude.
!    Based on function givelat by Taikan Oki (23/April/1996).
!    See EqnA2 of Oki and Sud, 1998, Earth Interactions, Vol.2, Paper 1.

FUNCTION givelonlength( lat )  RESULT (lonlen)

IMPLICIT NONE

! Scalar function result
REAL ::  lonlen                !  distance between points (m)
                               !  [i.e. same units as planet_eq_radius]

! Scalars with intent (in)
REAL, INTENT(IN) ::  lat       !  latitude (degrees)

! Local scalars
REAL ::  esinlat               !  product [eccen * SIN(lat_r)]

esinlat = eccen * SIN ( lat * pi_over_180 )

lonlen = pi_over_180 * planet_eq_radius * COS(lat * pi_over_180)            &
         / SQRT(1.0 - (esinlat * esinlat))

END FUNCTION givelonlength
!###############################################################################

!###############################################################################
! function giveearthradius
!
! Internal procedure in module earth_utils.
!    Calculates equivalent radius of Earth at a given latitude, assuming the
!    Earth is a spheroid.
!    From equations A10 and A11 in Appendix A of Oki and Sud, 1998, Earth
!    Interactions, Vol.2, Paper 1. Based on function giverade by Taikan Oki
!    (26/August/1996).

FUNCTION giveearthradius( lat ) RESULT( radius )

IMPLICIT NONE

! Scalar function result
REAL ::  radius            !  equivalent radius (m)
                           !  [i.e. same units as planet_eq_radius]

! Scalars with intent (in)
REAL, INTENT(IN) ::  lat   !  latitude (degrees)

! Local scalars
REAL ::  esinlat           !  product [eccen * SIN(lat_r)]
REAL ::  esinlat2          !  product [eccen * SIN(lat_r)]^2
REAL ::  rn                !  work

esinlat = eccen * SIN ( lat * pi_over_180 )
esinlat2 = esinlat * esinlat

rn = planet_eq_radius / SQRT(1.0 - esinlat2)

radius = rn * SQRT(1.0 - (2.0 * eccen * esinlat) + (esinlat2 * esinlat2))

END FUNCTION giveearthradius
!###############################################################################

END MODULE rivers_utils
!###############################################################################

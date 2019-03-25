










! *****************************COPYRIGHT*******************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT*******************************

MODULE jules_rivers_mod

!-----------------------------------------------------------------------------
! Description:
!   Contains river routing options and a namelist for setting them
!   This currently holds both "physics" and control variables
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE missing_data_mod, ONLY: imdi, rmdi

USE jules_riversparm, ONLY: cland, criver, cbland, cbriver, runoff_factor,  &
                            retl, retr, slfac, a_thresh, rivers_meander,    &
                            rivers_speed

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Module constants
!-----------------------------------------------------------------------------

!-----------------------------------------------------------------------------
! Scalar parameters.
INTEGER, PARAMETER ::                                                       &
   opt_len = 10             ! length used for many model options

CHARACTER(LEN=opt_len), PARAMETER ::                                        &
   rivers_rfm = 'rfm'
                            ! value of rivers_type indicating that the RFM
                            ! Kinematic Wave model is to be used.

CHARACTER(LEN=opt_len), PARAMETER ::                                        &
   rivers_trip = 'trip'
                            ! value of rivers_type indicating that the TRIP
                            ! linear model is to be used.
                            ! TRIP really only refers to the river network
                            ! data, but is used here to refer to a linear
                            ! model that has often been used with the data.

!-----------------------------------------------------------------------------
! Array parameters.

INTEGER,PARAMETER :: flow_dir_delta(0:10,2) = RESHAPE( (/                   &
!      missing  N    NE    E    SE    S    SW     W   NW   no_dir no_dir
!  x offsets:
         -9,    0,   1,    1,    1,   0,   -1,   -1,  -1,    0,   0,          &
!  y offsets:
         -9,    1,   1,    0,   -1,  -1,   -1,    0,   1,    0,   0           &
         /), (/ 11,2 /) )

!      Components of displacement (number of gridboxes) in x and
!      y directions to the immediately downstream gridbox. These
!      correspond to the directions in flow_dir_set.
!      The elements in the 2nd dimension are x and y respectively.
!             e.g. flow_dir_delta(1,1:2)=(/0,1/)
!             flow_dir_set(1) refers to a displacement to the N
!             flow_dir_delta(1,2)=1 means 1 gridbox in the N (y) direction
!             flow_dir_delta(1,1)=0 means no displacement in the W-E (x)
!                                 direction
!     The zeroth column (flow_dir_delta(0,:)) is not currently used, but is
!     included so as to make size(1) of flow_dir_delta and flow_dir_set equal
!     in an attempt for clarity.
!     The "no defined direction" must be the last column.

! Note on flow direction assumptions:
!       values of flow direction code that represent the
!       displacement given by the corresponding positions in flow_dir_delta.
!       The following are the meanings of each element of the array:
!        0: no data value (e.g. over sea)
!        1-8: N,NE,E,SE,S,SW,W,NW  i.e. clockwise from N
!             Although referred to via these compass directions, they are used
!             as "grid-relative" directions, i.e. it is assumed that columns
!             run S-N, and rows W-E, so "N" means "same column, one row up".
!             If the grid is actually rotated (so that columns do not run S-N
!             on the earth), the point that is "same column, one row up" in
!             fact does not lie immediately N.
!        9: undefined flow direction (i.e. no outflow, or outflow to sea)
!             For some encoding schemes, there is not a single value that
!             represents undefined flow (e.g. ARC combines values of all
!             directions with equal slope - so "undefined" flow direction
!             depends upon slope to neighbouring points). This is not a problem
!             here, since none of these schemes is currently encoded, BUT may
!             have to be accounted for in future - code could be added below
!             where currently we stop with "unexpected value".
!        Note that the values of flow_dir_set that correspond to "real" flow
!        directions (as opposed to missing data or no defined flow direction)
!        must be >0. Further, ALL values of flow_dir_set (incl sea and no flow)
!        must be >=0. In that case, values <0 can be used to indicate points
!        that have a flow direction that points off the edge of the grid (for
!        non-global applications).  These restrictions are necessary to make
!        the current algorithm work. In particular, a run with a smaller grid
!        (e.g. regional) should give the same routing pathways as a run with a
!        larger grid (at the points that are on both grids).

!-----------------------------------------------------------------------------
! Items set in namelist
!-----------------------------------------------------------------------------

LOGICAL ::                                                                  &
   l_rivers = .FALSE.                                                       &
                            ! Switch for runoff routing
   ,l_riv_overbank = .FALSE.
                            ! Switch for river overbank inundation

CHARACTER(LEN=opt_len) ::                                                   &
   rivers_type = 'rfm'
                            ! Choice of river routing model

INTEGER ::                                                                  &
   rivers_timestep = imdi
                            ! Timestep for runoff routing
                            ! (number of model timesteps)

!-----------------------------------------------------------------------------
! Rivers parameters
!-----------------------------------------------------------------------------

! Scalar variables (general)

INTEGER ::                                                                  &
   np_rivers                                                                &
                            ! number of points in the rivers grid at which
                            ! routing is calculated
   ,nx_rivers                                                               &
                            ! row length for rivers grid
   ,ny_rivers                                                               &
                            ! column length for rivers grid
   ,rivers_count                                                            &
                            ! counter of timesteps done in current routing
                            ! timestep. Generally equals rivers_step, but
                            ! is < rivers_step if part of a routing
                            ! timestep was missed (e.g. at start of run).
   ,rivers_step
                            ! counter of timesteps since routing last done
                            ! (actually a counter that triggers call to
                            ! rivers. May be > steps actually done -
                            ! see rivers_count).

!-----------------------------------------------------------------------------
! Definition of the river routing grid
!-----------------------------------------------------------------------------

REAL ::                                                                     &
   rivers_dlat  = rmdi                                                      &
                            ! size of gridbox of (regular) rivers grid
                            ! in latitude (degrees)
   ,rivers_dlon = rmdi                                                      &
                            ! size of gridbox of (regular) rivers grid
                            ! in longitude (degrees)
   ,rivers_lat1 = rmdi                                                      &
                            ! latitude of southernmost row of gridpoints
                            ! on a regular rivers grid (degrees)
   ,rivers_lon1 = rmdi                                                      &
                            !  longitude of westernmost (first) column of
                            ! gridpoints on a regular rivers grid (degrees)
   ,rivers_dx   = rmdi
                            ! size of gridbox of rivers grid in m (for
                            ! non-regular lat/lon grids)

INTEGER ::                                                                  &
   nx_grid      = imdi                                                      &
                            ! row length of full land model grid
                            ! (only needed for river routing)
   ,ny_grid     = imdi                                                      &
                            ! column length of model grid
                            ! (only needed for river routing)
   ,nseqmax     = imdi
                            ! maximum value of routing grid sequence

REAL ::                                                                     &
   reg_lon1     = rmdi                                                      &
                            ! longitude of westernnmost row of gridpoints
                            ! on a regular full model grid (degrees)
   ,reg_lat1    = rmdi                                                      &
                            ! latitude of southernnmost row of gridpoints
                            ! on a regular full model grid (degrees)
   ,reg_dlon    = rmdi                                                      &
                            ! size of gridbox of (regular) full model grid
                            ! in longitude (degrees)
   ,reg_dlat    = rmdi
                            ! size of gridbox of (regular) full model grid
                            ! in latitude (degrees)

LOGICAL ::                                                                  &
   rivers_reglatlon = .TRUE.                                                &
                            ! flag indicating if rivers grid is regular in
                            ! latitude and longitude See above for
                            ! definition of a regular grid.
   ,rivers_regrid   = .TRUE.                                                &
                            ! flag indicating if model and rivers grids
                            ! are identical
                            !     FALSE grids are identical
                            !     TRUE grids differ and regridding required
   ,rivers_first    = .TRUE.
                            ! TRUE indicates first river rivers timestep

!-----------------------------------------------------------------------------
! Array variables defined on land points required as input for river routing
!-----------------------------------------------------------------------------

REAL, ALLOCATABLE ::                                                        &
  tot_surf_runoff_gb(:),                                                    &
                           !  accumulated surface runoff (production)
                           !  between calls to rivers (kg m-2 s-1)
  tot_sub_runoff_gb(:),                                                     &
                           !  accumulatd sub-surface runoff (production)
                           !  rate between calls to rivers (kg m-2 s-1)
  acc_lake_evap_gb(:),                                                      &
                           !  accumulated lake evap over river routing
                           !  timestep (Kg/m2) - on land points
  rivers_sto_per_m2_on_landpts(:),                                          &
                           ! Water storage (kg m-2) on land points
  rivers_adj_on_landpts(:)
                           ! adjustment factor for water storage on landpts

INTEGER, ALLOCATABLE ::                                                     &
  il_river_grid(:),                                                         &
                           ! map of land point index on river routing grid
  ir_land_grid(:)      
                           ! map of river point index on land model grid

!-----------------------------------------------------------------------------
! Array variables defined on river routing grid used in routing calculations.
!-----------------------------------------------------------------------------

! Arrays defined on rivers points only

INTEGER, ALLOCATABLE ::                                                     &
   rivers_index_rp(:)                                                       &
                            ! Index of points where routing is calculated
   ,rivers_next_rp(:)                                                       
                            ! Index of the next downstream point.

REAL, ALLOCATABLE ::                                                        &
   rivers_seq_rp(:),                                                        &
                            ! River routing pathway sequence
   rivers_dir_rp(:),                                                        &
                            ! River routing direction index
   rivers_sto_rp(:),                                                        &
                            ! Water storage (kg)
   rivers_dra_rp(:),                                                        &
                            ! Catchment area draining to a grid cell
                            !    (no. of grid cells)
   rivers_lat_rp(:),                                                        &
                            ! River routing point latitude
   rivers_lon_rp(:)
                            ! River routing point longitude

! Arrays defined on full 2D rivers grid
!     [HL: note most of these can be rationalised to just routing points]

REAL, ALLOCATABLE ::                                                        &
   rivers_seq(:,:),                                                         &
                            ! River routing pathway sequence
   rivers_dir(:,:),                                                         &
                            ! River routing direction index
   rivers_sto(:,:),                                                         &
                            ! Water storage (kg)
   rivers_dra(:,:),                                                         &
                            ! Catchment area draining to a grid cell
                            !    (no. of grid cells)
   rivers_boxareas(:,:),                                                    &
                            ! Gridbox area of each river grid pixel (m2)
   rivers_lat2d(:,:),                                                       &
                            ! Full 2D latitude field
                            ! (enables non-regular lat-lon river grids)
   rivers_lon2d(:,:),                                                       &
                            ! Full 2D longitude field
                            ! (enables non-regular lat-lon river grids)
   rivers_xgrid(:),                                                         &
                            ! 1D x-dimension of rivers grid
   rivers_ygrid(:)
                            ! 1D y-dimension of rivers grid

! RFM-specific variables, currently defined on full river routing grid

REAL, ALLOCATABLE ::                                                        &
   rfm_flowobs1(:,:),                                                       &
                            ! Initial (observed) river flow (kg m-2 s-1)
   rfm_surfstore(:,:),                                                      &
                            ! Surface storage (m3)
   rfm_substore(:,:),                                                       &
                            ! Sub-surface storage (m3)
   rfm_flowin(:,:),                                                         &
                            ! Surface lateral inflow (m3)
   rfm_bflowin(:,:),                                                        &
                            ! Sub-surface lateral inflow (m3)
   rfm_rivflow(:,:),                                                        &
                            ! Surface river flow (m3 s-1)
   rfm_baseflow(:,:)
                            ! Sub-surface flow (m3 s-1)

! Defined on river points
REAL, ALLOCATABLE ::                                                        &
   rfm_flowobs1_rp(:),                                                      &
                            ! Initial (observed) river flow (kg m-2 s-1)
   rfm_surfstore_rp(:),                                                     &
                            ! Surface storage (m3)
   rfm_substore_rp(:),                                                      &
                            ! Sub-surface storage (m3)
   rfm_flowin_rp(:),                                                        &
                            ! Surface lateral inflow (m3)
   rfm_bflowin_rp(:),                                                       &
                            ! Sub-surface lateral inflow (m3)
   rfm_rivflow_rp(:),                                                       &
                            ! Surface river flow (m3 s-1)
   rfm_baseflow_rp(:)
                            ! Sub-surface flow (m3 s-1)

REAL, ALLOCATABLE ::                                                        &
   rivers_boxareas_rp(:),                                                   &
                            ! Gridbox area of each river grid pixel (m2)
   rfm_iarea_rp(:),                                                         &
                            ! Number of pixels draining to a pixel
   rfm_land_rp(:)
                            ! Flag to indicate river grid pixel type
                            !    0 = sea, 1 = river, 2 = land
                            ! (n.b. declared REAL not INTEGER)

!-----------------------------------------------------------------------------
! Single namelist definition for UM and standalone
!-----------------------------------------------------------------------------

NAMELIST  / jules_rivers/                                                   &
  l_rivers, rivers_type, rivers_timestep,                                   &
  cland, criver, cbland, cbriver, runoff_factor, retl, retr, slfac,         &
  a_thresh, rivers_meander, rivers_speed, l_riv_overbank

!-----------------------------------------------------------------------------

CONTAINS

SUBROUTINE check_jules_rivers()

USE ereport_mod, ONLY: ereport

USE jules_vegetation_mod, ONLY: l_irrig_limit

!-----------------------------------------------------------------------------
! Description:
!   Checks JULES_RIVERS namelist for consistency
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

IMPLICIT NONE

INTEGER :: errcode

IF ( .NOT. l_rivers ) THEN
  IF (l_irrig_limit) THEN
    errcode = 101
    CALL ereport("check_jules_rivers", errcode,                           &
                 'l_irrig_limit=T requires l_rivers=T ')
  END IF

  ! If rivers are not enabled, there is nothing further to check
  RETURN
END IF

! Check that a timestep was given
IF ( rivers_timestep <= 0 ) THEN
  errcode = 101
  CALL ereport("check_jules_rivers", errcode, 'rivers_timestep must be > 0')
END IF

! Check that parameter values are appropriate for the selected algorithm
! This also serves as a check that we have a recognised rivers type
SELECT CASE ( rivers_type )
CASE ( rivers_rfm )
  IF ( cland <= 0.0 .OR. criver <= 0.0 ) THEN
    errcode = -101
    CALL ereport("check_jules_rivers", errcode,                         &
                 "Surface wave speeds must be > 0")
  END IF
  IF ( cbland <= 0.0 .OR. cbriver <= 0.0) THEN
    errcode = -102
    CALL ereport("check_jules_rivers", errcode,                         &
                 "Sub surface wave speeds must be > 0")
  END IF
  IF ( runoff_factor <= 0.0 ) THEN
    errcode = -103
    CALL ereport("check_jules_rivers", errcode,                         &
                 "Runoff factor must be > 0")
  END IF

CASE ( rivers_trip )
  IF ( rivers_speed <= 0.0 .OR. rivers_meander <= 0.0 ) THEN
    errcode = -104
    CALL ereport("check_jules_rivers", errcode,                         &
                 "River speed and meander ratio must be > 0")
  END IF

CASE DEFAULT
  errcode = 101
  CALL ereport("check_jules_rivers", errcode,                           &
               'Unrecognised river routing algorithm')
END SELECT

IF ( l_irrig_limit .AND. ( rivers_type /= rivers_trip ) ) THEN
  errcode = 101
  CALL ereport("check_jules_rivers", errcode,                             &
               'l_irrig_limit=T requires rivers_type=rivers_trip')
END IF

END SUBROUTINE check_jules_rivers

END MODULE jules_rivers_mod

!-----------------------------------------------------------------------------

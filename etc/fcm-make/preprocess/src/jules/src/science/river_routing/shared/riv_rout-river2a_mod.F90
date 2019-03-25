










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
MODULE riv_rout_mod_2A


IMPLICIT NONE


CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='RIV_ROUT_MOD_2A'

CONTAINS



SUBROUTINE riv_rout_2a(                                                 &
   surf_runoff,sub_surf_runoff,icols,jrows,                             &
   boxareas,delta_phi,first,riv_timestep,                               &
! ancillary variables
         iarea, slope, flowobs1,inext,jnext,land,                             &
! prognostic variables
         substore,surfstore,flowin,bflowin,rivflow)


!
! Purpose:
!
! Perform the routing of surface and sub-surface runoff for Regional
!  model.
!
! Method:
! This subroutine routes surface and subsurface runoff
! for the RCM. The routing procedure is performed on the
! RCM grid, so no regridding is needed. The routing
! procedure is currently based on the Kinematic Wave
! routing model used at CEH Wallingford.
!
!-----------------------------------------------------------------

USE planet_constants_mod, ONLY: planet_radius

USE yomhook, ONLY: lhook, dr_hook
USE parkind1, ONLY: jprb, jpim
USE c_grid2grid_mod, ONLY: cland, criver, cbland, cbriver,              &
                           runoff_factor, retl, retr, slfac, a_thresh
IMPLICIT NONE


! IN Arguments

! External variables
!=====================

INTEGER ::                                                              &
 icols                                                                  &
            ! IN NO. OF COLUMNS IN ATMOSPHERE (icols)
,jrows      ! IN NO. OF ROWS IN ATMOSPHERE (TP GRID) : jrows
REAL ::                                                                 &
 surf_runoff(icols,jrows)                                               &
  ! IN TOTAL RATE OF surface RUNOFF (kg/m2/s = mm/s)
,sub_surf_runoff(icols,jrows)                                           &
  ! IN TOTAL RATE OF  sub-surf RUNOFF (kg/m2/s = mm/s)
,boxareas(icols,jrows)                                                  &
  ! IN gridbox area (m2)
,delta_phi                                                              &
  ! RCM gridsize (radians)
,riv_timestep   ! IN river timestep (secs)

REAL ::                                                                 &
 rivflow(icols,jrows)
! OUT river flow out from each gridbox (kg/m2/s = mm/s)

LOGICAL :: first  ! first call to river routing ?(T/F)
! ancillary variables

INTEGER ::                                                             &
 iarea(icols,jrows)                                                    &
                     !accumulated areas file
, inext(icols,jrows)                                                    &
                     !x-coordinate of downstream grid point
, jnext(icols,jrows)                                                    &
                     !y-coordinate of downstream grid point
, land(icols,jrows)   !land/river depends on value a_thresh ?

REAL ::                                                                &
 slope(icols,jrows)                                                    &
                         !slopes (not used yet)
 ,flowobs1(icols,jrows)  !optional initialisation for flows


! prognostic variables

REAL ::                                                                &
 substore(icols,jrows)                                                 &
                         !routing sub_surface store (mm)
,surfstore(icols,jrows)                                                &
                         !routing surface store (mm)
,flowin(icols,jrows)                                                   &
                         !surface lateral inflow (mm)
,bflowin(icols,jrows)    !sub-surface lateral inflow (mm)

! internal variables
!====================
INTEGER ::                                                             &
landtype                                                               &
                 !local for land type
,IN,jn                                                                  &
                 !local co-ords of downstream point
,i,j              !co-ordinate counters in do loops


REAL ::                                                                &
 landtheta,rivertheta                                                  &
    !parameters- surface wave speeds
, sublandtheta, subrivertheta                                           &
    !parameters- sub-surface wave speeds
!    &, retl,retr
!          !parameters - return flow for land and river
      , substore_n(icols,jrows)                                               &
           !internal array for surface store at next timestep(mm)
      , surfstore_n(icols,jrows)                                              &
           !internal array for subsurface store at next timestep(mm)
      , flowin_n(icols,jrows)                                                 &
           !internal, surface lateral inflow at next timestep(mm)
      , bflowin_n(icols,jrows)                                                &
           !internal, sub-surface lateral inflow at next timestep(mm)
      , returnflow                                                            &
           !internal variable for returnflow
!    &, slfac
!          !currently unused slope variables
      , real_area(icols,jrows)                                                &
           !internal, real value of accumulated area (iarea)
      , baseflow(icols,jrows)                                                 &
           !sub-surface flow - currently internal
      , dt                                                                    &
           !internal value of river routing model timestep
      , surf_roff(icols,jrows)                                                &
           !internal value of surf_runoff
      , sub_surf_roff(icols,jrows)                                            &
           !internal value of sub_surf_runoff
      , dx                                                                    &
           !RCM grid spacing in metres
      , area_correction(icols,jrows)
           !Lat/lon area dependency

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='RIV_ROUT_2A'



! include file to setup river routing model parameters

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,         &
                        zhook_handle)
dt = riv_timestep              ! model timestep
dx = planet_radius * delta_phi ! horizontal gridsize (m)

! Set up routing parameters

landtheta = cland * dt  / dx
rivertheta = criver * dt  / dx
sublandtheta = cbland * dt  / dx
subrivertheta = cbriver * dt  / dx

IF (landtheta >  1.0 .OR. rivertheta >  1 .OR.                       &
   sublandtheta >  1.0 .OR. subrivertheta >  1.0) THEN
  landtheta     = 0.0
  rivertheta    = 0.0
  sublandtheta  = 0.0
  subrivertheta = 0.0
END IF

! Initialise variables at first timestep
!-----------------------------------------

IF (first) THEN

  ! From the cumulative catchment areas dataset, determine which
  ! squares are land or river
  !------------------------------------------------------------------
  ! land(i,j) is set at the first entry, and the values must be
  ! remembered for subsequent timesteps
  !-------------------------------------------------------------------

  DO  j = 1, jrows
    DO i = 1, icols
      IF (iarea(i,j) <  0) THEN
        land(i,j) = 0          !Sea
      ELSE IF (iarea(i,j) >  a_thresh) THEN
        land(i,j) = 1          !river
      ELSE
        land(i,j) = 2          !land
      END IF
      real_area(i,j) = REAL(iarea(i,j)) + 1.0
      ! include current point, so add 1
    END DO
  END DO


  ! initialisation of variables

  DO  j = 1, jrows
    DO i = 1, icols

      ! this is where we will eventually initialise the routing stores
      ! using flow observations; here they are just set to zero....
      surfstore(i,j) = 0.0
      substore(i,j) = 0.0

      ! initialise surface and sub-surface inflows
      flowin(i,j) = 0.0
      bflowin(i,j) = 0.0
      rivflow(i,j) = 0.0

    END DO
  END DO

END IF     ! FIRST
!----------------------------------------------------------------


! For each timestep...
!-----------------------------

! convert runoff from mm/sec to mm (over the model timestep)
! assume dx in metres and dt in seconds

DO j = 1,jrows
  DO i = 1,icols

    area_correction(i,j) = boxareas(i,j) / 1000.0

    IF (surf_runoff(i,j) >= 0 .AND. sub_surf_runoff(i,j)              &
             >= 0.0) THEN
      surf_roff(i,j)     = runoff_factor * surf_runoff(i,j) * dt           &
                           *area_correction(i,j)
      sub_surf_roff(i,j) = runoff_factor * sub_surf_runoff(i,j) * dt       &
                           *area_correction(i,j)
    ELSE
      surf_roff(i,j) = 0.0       ! weed out dodgy data
      sub_surf_roff(i,j) = 0.0
    END IF
  END DO
END DO


! Initialise accumulated inflows and stores for the next timestep

DO  j = 1, jrows
  DO i = 1, icols
    flowin_n(i,j) = 0.0
    bflowin_n(i,j) = 0.0
    surfstore_n(i,j) = 0.0
    substore_n(i,j)= 0.0
  END DO
END DO

! Route runoff


DO j = 1,jrows
  DO i = 1,icols

    IN = inext(i,j)
    jn = jnext(i,j)
    !               sl = slope(i,j)       ! not used at the moment
    landtype = land(i,j)

    ! route runoff using simple kw model

    IF (landtype == 2) THEN  !land
      !-----------------------------------------------------------------
      ! land surface
      surfstore_n(i,j) = (1.0 - landtheta ) * surfstore(i,j) +   &
                         flowin(i,j) + surf_roff(i,j)
      !! HL: UM code bugfix - update inflow at next timestep based
      !!     on surfstore, not surfstore_n
      !!                   flowin_n(in,jn) =flowin_n(in,jn) +                   &
      !!                         landtheta*surfstore_n(i,j)
      !! HL: Revised code (consistent with UM vn8.2 apgw code branch and sjd JULES)
      flowin_n(IN,jn) = flowin_n(IN,jn) +                        &
                        landtheta * surfstore(i,j)

      ! land subsurface
      substore_n(i,j) = (1.0 - sublandtheta ) * substore(i,j)    &
                        + bflowin(i,j) + sub_surf_roff(i,j)
      !! HL: UM code bugfix - update inflow at next timestep based
      !!     on surfstore, not surfstore_n
      !!                   bflowin_n(in,jn) = bflowin_n(in,jn)                  &
      !!                      + sublandtheta*substore_n(i,j)
      bflowin_n(IN,jn) = bflowin_n(IN,jn)                        &
                        + sublandtheta * substore(i,j)

      ! return flow
      returnflow = MAX(ABS(substore_n(i,j) * retl),0.0)
      substore_n(i,j) = substore_n(i,j) - returnflow
      surfstore_n(i,j) = surfstore_n(i,j) +returnflow


    ELSE IF (landtype == 1) THEN  !river
      !-------------------------------------------------------------------

      ! river subsurface
      substore_n(i,j) = (1.0 - subrivertheta ) * substore(i,j) +  &
                         bflowin(i,j) + sub_surf_roff(i,j)
      !! HL: UM code bugfix
      !!                  bflowin_n(in,jn) = bflowin_n(in,jn)                   &
      !!                       + subrivertheta*substore_n(i,j)
      bflowin_n(IN,jn) = bflowin_n(IN,jn)                         &
                         + subrivertheta * substore(i,j)

      ! river surface
      surfstore_n(i,j) = (1.0 - rivertheta ) * surfstore(i,j) +   &
                         flowin(i,j) + surf_roff(i,j)
      !! HL: UM code bugfix
      !!                  flowin_n(in,jn) = flowin_n(in,jn)                     &
      !!                      + rivertheta*surfstore_n(i,j)
      flowin_n(IN,jn) = flowin_n(IN,jn)                           &
                        + rivertheta * surfstore(i,j)

      ! return flow
      returnflow = MAX(ABS(substore_n(i,j) * retr),0.0)
      substore_n(i,j) = substore_n(i,j) - returnflow
      surfstore_n(i,j) = surfstore_n(i,j) +returnflow

    END IF
    !------------------------------------------------------------------
  END DO
END DO   ! end of routing loop

! housekeeping for next timestep

DO  j = 1, jrows
  DO i = 1, icols

    ! keep inflows for next timestep

    flowin(i,j) = flowin_n(i,j)
    bflowin(i,j) = bflowin_n(i,j)

    ! calculate flow in rivers (mm/s = kg/m2/s)

    IF (land(i,j) == 1) THEN
      rivflow(i,j) = rivertheta / dt * surfstore(i,j)                 &
                            /area_correction(i,j)
      baseflow(i,j) = subrivertheta / dt * substore(i,j)              &
                            /area_correction(i,j)

      IN = inext(i,j)
      jn = jnext(i,j)

      ! calculate flow into the sea if the next point is sea
      !(assume it's equal to river flow in the adjacent land pt)

      IF (land(IN,jn) == 0) rivflow(IN,jn) = rivflow(i,j)

    END IF

    ! HL: Potential UM code fix - matching sjd JULES code comment (sjd: 20/09/05)
                  !calculate flow for land points (sjd: 20/09/05)
    IF (land(i,j) == 2) THEN
      rivflow(i,j) = landtheta / dt * surfstore(i,j)                    &
                        /area_correction(i,j)
      baseflow(i,j) = sublandtheta / dt * substore(i,j)                 &
                        /area_correction(i,j)

      IN = inext(i,j)
      jn = jnext(i,j)

      ! calculate flow into the sea if the next point is sea
      !(assume it's equal to river flow in the adjacent land pt)
      IF (land(IN,jn) == 0) rivflow(IN,jn) = rivflow(i,j)
    END IF
    !! HL: Potential UM code fix end

    ! keep routing stores for next timestep
    surfstore(i,j) = surfstore_n(i,j)
    substore(i,j) = substore_n(i,j)
  END DO
END DO

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,       &
                        zhook_handle)
RETURN
END SUBROUTINE riv_rout_2a

END MODULE riv_rout_mod_2A


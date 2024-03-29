










MODULE metstats_timestep_mod
CHARACTER(LEN=*), PARAMETER, PRIVATE :: ModuleName='METSTATS_TIMESTEP_MOD'

CONTAINS
SUBROUTINE metstats_timestep(msi, msd,                                        &
    !Things that really ought to come in via USE but won't work with the UM    &
    current_time, timestep_len,land_pts)

  ! < Module imports >
USE metstats_mod,       ONLY:  metstats_flag,                              &
                                metstats_inis,                              &
                                metstats_prog_struct,                       &
                                metstats_input_struct
USE dewpnt_mod,         ONLY: dewpnt
USE conversions_mod,    ONLY: hours_in_day => ihour_per_day,                &
                              mins_in_hour => imin_per_hour,                &
                              secs_in_day => isec_per_day,                  &
                              secs_in_hour => isec_per_hour,                &
                              secs_in_min => isec_per_min

USE qsat_mod, ONLY: qsat_wat_new => qsat_wat,                                 &
                    l_new_qsat_jules

USE parkind1,           ONLY: jprb, jpim
USE yomhook,            ONLY: lhook, dr_hook

!  Statements we'd like to use but can't because of the UM
!  USE model_time_mod,     ONLY : current_time, timestep_len

IMPLICIT NONE

!
! Description:
!   Called every model timestep, this subroutine updates the metstat variables
!
!   All arrays are land_pts
!   Pay attention to (:) notation
!   Some local arrays could be made ALLOCATABLE although more IFs
!   would be needed
!
! Method:
! We carry out a 3-step process:
!Step 1: Extract the required information from the raw met data is the msi
!        structure. Keep them in the _ext variables.
!
!Step 2: Calculate local time for each point so that we know which grid boxes
!        have reached the end of their sampling period (eg midnight or noon)
!
!Step 3: Update the metstats variables with the _ext variables as appropriate,
!        finish off calculations for gridpoints that have reached the end of
!        their sampling periods, and copy over to the %fin fields.
!
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!

! Arguments
REAL, INTENT(IN)    :: timestep_len
INTEGER, INTENT(IN) :: current_time, land_pts

TYPE (metstats_input_struct), INTENT(IN)    :: msi(land_pts)
TYPE (metstats_prog_struct),  INTENT(INOUT) :: msd(land_pts)

! Local variables

!Arrays for holding intermediate values extracted from the input raw data.
REAL    ::  svp_ext(land_pts),     & !svp = saturated vapour pressure
            temp_ext(land_pts),                                             &
            relhum_ext(land_pts),                                           &
            wind_ext(land_pts),                                             &
            precip_ext(land_pts),                                           &
            dewpt_ext(land_pts)

REAL     :: local_time(land_pts)

!Recording which points are at important local times
LOGICAL ::  local_00h(land_pts) ,                                           &
            local_12h(land_pts)

! Number of timesteps per day
REAL               :: n_daily_ave

INTEGER, PARAMETER :: noon         = 12 * mins_in_hour * secs_in_min,       &
                      midnight     = 0

INTEGER(KIND=jpim), PARAMETER :: zhook_in  = 0
INTEGER(KIND=jpim), PARAMETER :: zhook_out = 1
REAL(KIND=jprb)               :: zhook_handle

CHARACTER(LEN=*), PARAMETER :: RoutineName='METSTATS_TIMESTEP'

! End of header
IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_in,zhook_handle)

n_daily_ave = secs_in_day / timestep_len

!----------------------------------------------------------------------------
!Step 1: Extract the required information from the raw met data is the msi
!        structure. Keep them in the _ext variables.

!Temperature. Simple extraction.
IF (metstats_flag%temp_max_00h .OR.                                         &
    metstats_flag%temp_ave_00h .OR.                                         &
    metstats_flag%temp_pnt_12h) THEN
  temp_ext(:) = msi(:)%temp
END IF

!Precipitation. Result in mm, assuming that 1 kg/m2 = 1 mm
IF (metstats_flag%prec_tot_00h .OR. metstats_flag%prec_tot_12h) THEN
  precip_ext(:) = timestep_len * (msi(:)%ls_rain + msi(:)%con_rain +        &
                              msi(:)%ls_snow + msi(:)%con_snow)
END IF

!Relative humidity. Run through qsat_wat.
IF (metstats_flag%rhum_min_00h .OR. metstats_flag%rhum_pnt_12h) THEN

  IF (l_new_qsat_jules) THEN
    CALL qsat_wat_new(svp_ext(:), msi(:)%temp, msi(:)%press, land_pts)
  ELSE
    ! DEPENDS ON: qsat_wat
    CALL qsat_wat(svp_ext(:), msi(:)%temp, msi(:)%press, land_pts)
  END IF

  relhum_ext(:) = (msi(:)%spec_hum / svp_ext(:)) * 100.0
END IF

!Dewpoint. Result in K. Run through dewpnt.
IF (metstats_flag%dewp_ave_00h) THEN
  CALL dewpnt(msi(:)%spec_hum,msi(:)%press,msi(:)%temp,land_pts,dewpt_ext(:))
END IF

!Wind. Result in m/s. Use Pythagorus to back out speed.
IF (metstats_flag%wind_ave_00h .OR. metstats_flag%wind_pnt_12h) THEN
  wind_ext(:) = SQRT(msi(:)%wind_u**2.0 + msi(:)%wind_v**2.0)
END IF

!----------------------------------------------------------------------------
!Step 2: Calculate local time for each point so that we know which grid boxes
!        have reached the end of their sampling period (eg midnight or noon)

local_time(:) = current_time + msd(:)%lon_time_diff

!   Correct local time to get it in the interval 0 to secs_in_day-1
WHERE (local_time < 0)
  local_time = local_time + secs_in_day
END WHERE
WHERE (local_time >= secs_in_day)
  local_time = local_time - secs_in_day
END WHERE

!   Determine which points are just past the various sampling end points.
!   At present, these are midnight and noon local time.
local_00h(:) = .FALSE.
WHERE (local_time >= midnight .AND. local_time < (midnight + timestep_len))
  local_00h = .TRUE.
END WHERE

local_12h(:) = .FALSE.
WHERE (local_time >= noon     .AND. local_time < (noon + timestep_len))
  local_12h = .TRUE.
END WHERE

!----------------------------------------------------------------------------
!Step 3: Update the metstats variables with the _ext variables as appropriate,
!        finish off calculations for gridpoints that have reached the end of
!        their sampling periods, and copy over to the %fin fields.

IF (metstats_flag%temp_max_00h) THEN
  !End of period operation
  WHERE (local_00h)
    msd%temp_max_00h%fin = msd%temp_max_00h%run
    msd%temp_max_00h%run = metstats_inis%temp_max_00h
  END WHERE
  !Perform normal timestep operation
  msd(:)%temp_max_00h%run = MAX(msd(:)%temp_max_00h%run, temp_ext)
END IF

IF (metstats_flag%temp_ave_00h) THEN
  !End of period operation
  WHERE (local_00h)
    msd%temp_ave_00h%fin = msd%temp_ave_00h%run / n_daily_ave
    msd%temp_ave_00h%run = metstats_inis%temp_ave_00h
  END WHERE
  !Perform normal timestep operation
  msd(:)%temp_ave_00h%run = msd(:)%temp_ave_00h%run + temp_ext(:)
END IF

IF (metstats_flag%temp_pnt_12h) THEN
  WHERE (local_12h)
    msd%temp_pnt_12h%fin = temp_ext
    !No reset
  END WHERE
  !Perform normal timestep operation
  !None
END IF

IF (metstats_flag%prec_tot_00h) THEN
  WHERE (local_00h)
    msd%prec_tot_00h%fin = msd%prec_tot_00h%run
    msd%prec_tot_00h%run = metstats_inis%prec_tot_00h
  END WHERE
  !Perform normal timestep operation
  msd(:)%prec_tot_00h%run = msd(:)%prec_tot_00h%run + precip_ext(:)
END IF

IF (metstats_flag%prec_tot_12h) THEN
  WHERE (local_12h)
    msd%prec_tot_12h%fin = msd%prec_tot_12h%run
    msd%prec_tot_12h%run = metstats_inis%prec_tot_12h
  END WHERE
  !Perform normal timestep operation
  msd(:)%prec_tot_12h%run = msd(:)%prec_tot_12h%run + precip_ext(:)
END IF

IF (metstats_flag%rhum_min_00h) THEN
  WHERE (local_00h)
    msd%rhum_min_00h%fin = msd%rhum_min_00h%run
    msd%rhum_min_00h%run = metstats_inis%rhum_min_00h
  END WHERE
  !Perform normal timestep operation
  msd(:)%rhum_min_00h%run = MIN(msd(:)%rhum_min_00h%run, relhum_ext(:))
END IF

IF (metstats_flag%rhum_pnt_12h) THEN
  WHERE (local_12h)
    msd%rhum_pnt_12h%fin = relhum_ext
    !No reset
  END WHERE
  !Perform normal timestep operation
  !None
END IF

IF (metstats_flag%dewp_ave_00h) THEN
  WHERE (local_00h)
    msd%dewp_ave_00h%fin = msd%dewp_ave_00h%run / n_daily_ave
    msd%dewp_ave_00h%run = metstats_inis%dewp_ave_00h
  END WHERE
  !Perform normal timestep operation
  msd(:)%dewp_ave_00h%run = msd(:)%dewp_ave_00h%run + dewpt_ext(:)
END IF

IF (metstats_flag%wind_ave_00h) THEN
  WHERE (local_00h)
    msd%wind_ave_00h%fin = msd%wind_ave_00h%run / n_daily_ave
    msd%wind_ave_00h%run = metstats_inis%wind_ave_00h
  END WHERE
  !Perform normal timestep operation
  msd(:)%wind_ave_00h%run = msd(:)%wind_ave_00h%run + wind_ext(:)
END IF

IF (metstats_flag%wind_pnt_12h) THEN
  WHERE (local_12h)
    msd%wind_pnt_12h%fin = wind_ext
    !No reset
  END WHERE
  !Perform normal timestep operation
  !None
END IF

IF (lhook) CALL dr_hook(ModuleName//':'//RoutineName,zhook_out,zhook_handle)
RETURN
END SUBROUTINE metstats_timestep
END MODULE metstats_timestep_mod

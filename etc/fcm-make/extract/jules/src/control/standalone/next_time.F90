#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************


SUBROUTINE next_time()

USE string_utils_mod, ONLY: to_string

USE datetime_mod, ONLY: datetime,                                          &
! Also import the comparison operators on the datetime type
                           operator( == ), operator( /= ), operator( < ),     &
                           operator( > ), operator( <= ), operator( >= ),     &
                           datetime_advance, datetime_subtract,               &
                           datetime_to_string

USE time_varying_input_mod, ONLY: seek_all_to_current_datetime,            &
                                   advance_all

USE spinup_mod, ONLY: spinup_check

USE model_time_mod

USE dump_mod, ONLY: write_dump

USE output_mod, ONLY:                                                      &
!  imported scalars (IN)
     dump_period

USE trifctl, ONLY: asteps_since_triffid

USE logging_mod, ONLY: log_info, log_debug, log_warn, log_error, log_fatal

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Advances the model to the next timestep, performing all the necessary
!   checks and seeking of data files
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------
! Work variables
TYPE(datetime) :: next_tstep  ! Temporarily holds the value of the time at
                              ! timestep we are moving in to

LOGICAL :: seek_required  !   T - the input files require a seek
                          !   F - the input files just need to be advanced
                          ! A seek of the input files is only required
                          ! when moving from spinup into the main run
                          ! Ramping between spinup cycles is handled by
                          ! time_varying_input_mod

TYPE(datetime) :: dt  ! Placeholder for datetime in start/end of year
                      ! calculations


!-----------------------------------------------------------------------------


next_tstep = datetime_advance(current_time, timestep_len)
seek_required = .FALSE.  ! The default is to advance input files normally,
                         ! rather than seek to an out-of-sequence time

!-----------------------------------------------------------------------------
! The only conditions that can cause us to do something other than advance
! the time normally are:
!   - End of a spinup cycle
!   - End of main run
! So we check for these here
!-----------------------------------------------------------------------------
! Check if the current spinup cycle has ended
IF ( is_spinup .AND. ( next_tstep >= spinup_end ) ) THEN

  ! Possible things to happen from this point are:
  !   - Starting the next spinup cycle
  !   - Terminating after a failed spinup
  !   - Starting the main run

  IF ( spinup_check() ) THEN
    !-----------------------------------------------------------------------------
    ! All points are spun up, we can move on to the main run with no problems
    !-----------------------------------------------------------------------------
    CALL log_info("next_time",                                              &
                  "All points spun up after " //                            &
                  TRIM(to_string(spinup_cycle)) // " cycles")
    CALL log_info("next_time", "Starting main run")

    is_spinup = .FALSE.
    next_tstep = main_run_start

    ! A seek of the input files is required when starting the main run
    seek_required = .TRUE.

  ELSE IF ( spinup_cycle >= max_spinup_cycles ) THEN
    !-----------------------------------------------------------------------------
    ! We have reached the end of the last spinup cycle without all points being
    ! spun up
    !-----------------------------------------------------------------------------
    CALL log_warn("next_time",                                              &
                  "Model is not fully spun up after maximum number " //     &
                  "of spinup cycles")

    ! Regardless of whether we stop or not, we will no longer be in spinup
    is_spinup = .FALSE.
    next_tstep = main_run_start

    IF ( terminate_on_spinup_fail ) THEN
      ! If this flag is set, we want to end the run
      ! First, issue a warning that the model failed to spin up
      CALL log_error("next_time",                                           &
                     "Model failed to spin up - terminating run")
      ! We use end_of_run rather than a fatal error to abort the run as we want a
      ! final dump to be written, and output from the final spinup cycle to be written
      ! correctly
      end_of_run = .TRUE.
    ELSE
      ! If the flag is not set, we will continue with the main run with a warning
      CALL log_warn("next_time",                                            &
                    "Model failed to spin up - continuing with main run")
      ! A seek of the input files is required when starting the main run
      seek_required = .TRUE.
    END IF

  ELSE
    !-----------------------------------------------------------------------------
    ! Some points are still not spun up
    !-----------------------------------------------------------------------------
    CALL log_info("next_time",                                              &
                  "Model is not fully spun up after " //                    &
                  TRIM(to_string(spinup_cycle)) // " cycles")
    CALL log_info("next_time",                                              &
                  "Starting another cycle of spinup")

    next_tstep = spinup_start
    spinup_cycle = spinup_cycle + 1

    ! A seek of the input files is not required for starting a new cycle of spinup
    ! since advance_all handles ramping between spinup_end and spinup_start
    ! smoothly
  END IF
ELSE IF ( .NOT. is_spinup .AND. next_tstep >= main_run_end ) THEN
  !-----------------------------------------------------------------------------
  ! We are at the end of the main run
  !-----------------------------------------------------------------------------
  end_of_run = .TRUE.

  CALL log_info("jules", "Run completed successfully")

END IF

!-----------------------------------------------------------------------------
! Now we have corrected the next time in light of spinup and stopped the run
! if required, we can update the necessary variables
!-----------------------------------------------------------------------------
! Set the current model time and advance the timestep
current_time = next_tstep
timestep = timestep + 1
asteps_since_triffid = asteps_since_triffid + 1

! Set the variables that indicate if we are starting or ending a year

! If going back one timestep length causes the year to change, then we are in
! the first timestep of a year
dt = datetime_subtract(current_time, timestep_len)
start_of_year = dt%year /= current_time%year
! If advancing the current time causes the year to change, then it is the last
! timestep in a year
dt = datetime_advance(current_time, timestep_len)
end_of_year = dt%year /= current_time%year

!-----------------------------------------------------------------------------
! Update the input data as required and print timestep info
!
! If the run has ended, we don't need to do this
!-----------------------------------------------------------------------------
IF ( .NOT. end_of_run ) THEN
  ! Seek the input data if required, otherwise just advance it normally
  ! A seek of the input files is only required when moving from spinup into
  ! the main run
  ! Ramping between spinup cycles is handled in advance_all
  IF ( seek_required ) THEN
    CALL seek_all_to_current_datetime()
  ELSE
    CALL advance_all()
  END IF

  ! Periodically print time information
  ! If the run has ended, we don't want to print
  IF ( MOD(timestep, print_step) == 0 ) THEN
    CALL log_info("next_time",                                              &
                  "Timestep: " // TRIM(to_string(timestep)) // "; " //      &
                  "Started at: " // datetime_to_string(current_time))
    IF ( is_spinup )                                                        &
      CALL log_info("next_time",                                            &
                    "Spinup cycle: " // TRIM(to_string(spinup_cycle)))
  END IF
END IF

!-----------------------------------------------------------------------------
! Now that the timestep has rolled over, check if we need to write a dump
!
! We write a dump if:
!   * We are about to start a new spinup cycle
!   * We are about to start the main run
!   * We are about to start a new year of simulation - a dump is written
!       once every dump_period years (triggered by calendar year, not by
!       number of years simulated so far).
!   * We are at the end of the run
!-----------------------------------------------------------------------------
IF ( current_time == spinup_start .OR. current_time == main_run_start .OR.  &
     ( start_of_year .AND. MOD(current_time%year,dump_period) == 0 )  .OR.  &
     end_of_run )                                                           &
  CALL write_dump()

RETURN

END SUBROUTINE next_time
#endif

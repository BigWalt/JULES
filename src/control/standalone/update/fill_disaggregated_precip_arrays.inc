#if !defined(UM_JULES)

SUBROUTINE fill_disaggregated_precip_arrays()

  USE disaggregated_precip, ONLY : ls_rain_disagg, con_rain_disagg,           &
                                   ls_snow_disagg, con_snow_disagg

  USE forcing, ONLY :  ls_rain_ij, con_rain_ij, ls_snow_ij, con_snow_ij

  USE model_time_mod, ONLY : timestep_len, timesteps_in_day

  USE datetime_mod, ONLY : secs_in_day, hours_in_day, secs_in_hour

  USE theta_field_sizes, ONLY : t_i_length, t_j_length

  USE prognostics, ONLY : seed_rain

  IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!     Disaggregates a day mean precip into a precip event
!     Used only when l_daily_disagg = T
!
! Method:
!     Same as used in day_calc within IMOGEN
!
! Code Author: Karina Williams
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

  INTEGER, PARAMETER :: n_prec = 4
  CHARACTER(LEN=7) :: list_precip_str(n_prec)
  CHARACTER(LEN=7) :: precip_str

  INTEGER :: i, t_i, t_j
  REAL :: dur_hours
  REAL,PARAMETER :: small = 1.0e-6
  REAL :: timestep_len_hours
  REAL :: max_precip_rate
  INTEGER :: n_tally
  INTEGER :: istep

  REAL,ALLOCATABLE ::    prec_3d(:,:,:)
  REAL,ALLOCATABLE ::    prec_2d(:,:)
  INTEGER,ALLOCATABLE :: n_event_local(:)
  REAL,ALLOCATABLE ::    prec_1d(:)


!-----------------------------------------------------------------------------


  IF ( .NOT. l_daily_disagg) return

  timestep_len_hours = REAL(timestep_len) / REAL(secs_in_hour)

!-----------------------------------------------------------------------------
! Calculate the maximum precipitation rate. It is noted that 58 mm/day
! over 8 timesteps, and where all fell within a single 3 hour period
! caused numerical issues for MOSES. This corresponded to a rate of
! 464 mm/day during the 3-hour period. Hence, place a limit of 350
! mm/day.
!-----------------------------------------------------------------------------
  max_precip_rate = 350.0 / REAL(secs_in_day) ! in mm s-1 i.e. kg m-2 s-1

  list_precip_str = (/ 'lscrain','conrain','lscsnow','consnow' /)

  ALLOCATE(n_event_local(timesteps_in_day))
  ALLOCATE(prec_1d(timesteps_in_day))
  ALLOCATE(prec_2d(t_i_length, t_j_length))
  ALLOCATE(prec_3d(t_i_length, t_j_length, timesteps_in_day))

  DO i = 1,n_prec

    precip_str = list_precip_str(i)

    SELECT CASE( precip_str )
      CASE( 'lscrain' )
        dur_hours = dur_ls_rain / REAL(secs_in_hour)
        prec_2d(:,:) = ls_rain_ij(:,:)

      CASE( 'conrain' )
        dur_hours = dur_conv_rain / REAL(secs_in_hour)
        prec_2d(:,:) = con_rain_ij(:,:)

      CASE( 'lscsnow' )
        dur_hours = dur_ls_snow / REAL(secs_in_hour)
        prec_2d(:,:) = ls_snow_ij(:,:)

      CASE( 'consnow' )
        dur_hours = dur_conv_snow / REAL(secs_in_hour)
        prec_2d(:,:) = con_snow_ij(:,:)
    END SELECT

!-----------------------------------------------------------------------------
! Ensure that the durations are at least as long as a time period for
! the model to prevent solution "falling through gaps"
!-----------------------------------------------------------------------------

    IF ( dur_hours <= timestep_len_hours ) THEN
      dur_hours = timestep_len_hours + small
    END IF

    DO t_j = 1,t_j_length
      DO t_i = 1,t_i_length

        CALL fill_n_event_local(dur_hours, timestep_len_hours,                &
                                precip_disagg_method, n_event_local, n_tally)

! recall prec_2d and prec_3d are rates
        prec_3d(t_i,t_j,:) =  REAL(n_event_local(:)) *                        &
                   (REAL(timesteps_in_day) / REAL(n_tally)) * prec_2d(t_i,t_j)

        IF ( precip_disagg_method == 2 ) THEN
          ! Check that no convective rain periods
          ! exceed max_precip_rate, or if so,
          ! then redistribute.

          prec_1d(:) = prec_3d(t_i,t_j,:)

          CALL redis(                                                         &
            timesteps_in_day, timesteps_in_day, max_precip_rate, prec_1d,     &
            n_event_local, n_tally                                            &
          )

          prec_3d(t_i,t_j,:) =  prec_1d(:)

        END IF

      END DO
    END DO

    SELECT CASE( precip_str )
      CASE( 'lscrain' )
        ls_rain_disagg(:,:,:)  = prec_3d(:,:,:)

      CASE( 'conrain' )
        con_rain_disagg(:,:,:) = prec_3d(:,:,:)

      CASE( 'lscsnow' )
        ls_snow_disagg(:,:,:)  = prec_3d(:,:,:)

      CASE( 'consnow' )
        con_snow_disagg(:,:,:) = prec_3d(:,:,:)
    END SELECT
  END DO

  IF ( precip_disagg_method > 1 ) THEN
   ! Update the stored seed in case a dump is written this timestep
    CALL RANDOM_SEED( GET = seed_rain )
  END IF

  DEALLOCATE(prec_3d)
  DEALLOCATE(prec_2d)
  DEALLOCATE(prec_1d)
  DEALLOCATE(n_event_local)

END SUBROUTINE fill_disaggregated_precip_arrays


!###############################################################################
!###############################################################################


SUBROUTINE fill_n_event_local(dur_hours, timestep_len_hours,                  &
                              precip_disagg_method, n_event_local, n_tally)

  USE model_time_mod, ONLY : timestep_len, timesteps_in_day
  USE datetime_mod, ONLY : hours_in_day

  IMPLICIT NONE

! Arguments
  REAL, INTENT(IN) :: dur_hours
  REAL, INTENT(IN) :: timestep_len_hours
  INTEGER, INTENT(IN) :: precip_disagg_method

  INTEGER, INTENT(OUT) :: n_event_local(:)
  INTEGER, INTENT(OUT) :: n_tally

! Work variables
  REAL :: init_hour_prec
  REAL :: end_hour_prec
  REAL :: hourevent
  REAL :: random_num_sd

  INTEGER, ALLOCATABLE :: loc(:)
  INTEGER :: istep
  INTEGER :: j


!-----------------------------------------------------------------------------

! Initialise arrays
  n_event_local(:)  = 0

  SELECT CASE( precip_disagg_method )
    CASE(1)
      n_event_local(:)  = 1
      n_tally = SUM(n_event_local(:))

    CASE(2, 3)
      CALL random_number(random_num_sd)

      init_hour_prec = random_num_sd * ( REAL(hours_in_day) - dur_hours )
      end_hour_prec = init_hour_prec + dur_hours

      DO istep=1,timesteps_in_day
        hourevent = (REAL(istep)-0.5) * timestep_len_hours

        IF ( hourevent >= init_hour_prec .AND. hourevent < end_hour_prec) THEN
          n_event_local(istep) = 1
        END IF
      END DO

      n_tally = SUM(n_event_local(:))

    CASE(4)
      n_tally = NINT( dur_hours/REAL(hours_in_day) * timesteps_in_day )

      ALLOCATE(loc(n_tally))

      DO istep = 1, n_tally
          loc(istep) = istep
      END DO

      DO istep = n_tally + 1, timesteps_in_day
          CALL random_number(random_num_sd) ! 0 <= random_num_sd < 1

          j = INT (random_num_sd * REAL(istep) ) + 1 ! 1 <= integer random number <= istep

          IF ( j <= n_tally ) THEN
              loc(j) = istep
          END IF
      END DO

      DO istep = 1, n_tally
        n_event_local(loc(istep)) = 1
      END DO

      DEALLOCATE(loc)

  END SELECT

END SUBROUTINE fill_n_event_local

#endif


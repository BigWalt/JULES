#if !defined(UM_JULES)
!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology. All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237] 
!******************************COPYRIGHT**************************************
SUBROUTINE imogen_check(                                                      &
  c_emissions,include_co2,include_non_co2,land_feed_co2,                      &
  ocean_feed,anlg,anom                                                        &
)

USE logging_mod, ONLY: log_fatal

USE jules_print_mgr, ONLY:                                                    &
  jules_message,                                                              &
  jules_print
IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This code is designed to make a quick cross-check to ensure that
!   the flags in IMOGEN are set properly for the currently allowed configu
!   The currently allowed configurations should fit with the available doc
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
! Written by: C. Huntingford (4th March 2004)
! 
! Code Description:
!   Language: Fortran 90.
!   
!-----------------------------------------------------------------------------

LOGICAL ::                                                                    &
  c_emissions,                                                                &
         !IN If true, means CO2 concentration is calcula
  include_co2,                                                                &
         !IN Are adjustments to CO2 values allowed?
  include_non_co2,                                                            &
         !IN Are adjustments to non-CO2 values allowed?
  land_feed_co2,                                                              &
         !IN Are land CO2 feedbacks allowed on atmospheric C
  ocean_feed,                                                                 &
         !IN Are ocean feedbacks allowed on atmospheric
  check_flag,                                                                 &
         !IN Confirms that configuration is OK
  anlg,                                                                       &
         !IN True if the analogue model is used
  anom   !IN True if the anomalies are used



check_flag = .FALSE.

! Now make checks on structures which will hopefully all be available at
! To make this code easy to read, simple "if"
! statements comments are used for each instance and case printed.

! Spin-up
IF ( .NOT. anom) THEN
  WRITE(jules_message,*) 'Run is a spin-up'
  CALL jules_print('imogen_check',jules_message)
  WRITE(jules_message,*) 'Are flags set OK for any future transient run?'
  CALL jules_print('imogen_check',jules_message)
  check_flag = .TRUE.
END IF

! User prescribed anomalies
IF (anom .AND. ( .NOT. anlg)) THEN
  WRITE(jules_message,*) 'Run is reading in user anomalies'
  CALL jules_print('imogen_check',jules_message)
  check_flag = .TRUE.
END IF

! Currently coded analogue model possibilities
IF (anom .AND. anlg) THEN
  IF (( .NOT. include_co2) .AND. include_non_co2) THEN
    WRITE(jules_message,*) 'Run is for prescribed non-CO2 gases only'
    CALL jules_print('imogen_check',jules_message)
    check_flag = .TRUE.
  END IF

  IF (include_co2) THEN
    IF ( .NOT. c_emissions) THEN
      IF (include_non_co2) THEN
        WRITE(jules_message,*) 'Run is for prescribed CO2 and non-CO2 gases'
        CALL jules_print('imogen_check',jules_message)
        check_flag = .TRUE.
      ELSE
        WRITE(jules_message,*) 'Run is for prescribed CO2 only'
        CALL jules_print('imogen_check',jules_message)
        check_flag = .TRUE.
      END IF
    END IF

    IF (c_emissions) THEN
      IF ( .NOT. land_feed_co2 .AND. .NOT. ocean_feed) THEN
        IF ( .NOT. include_non_co2) THEN
          WRITE(jules_message,*) 'Run is for CO2 emissions only'
          CALL jules_print('imogen_check',jules_message)
          WRITE(jules_message,*) 'There are NO land CO2 or ocean feedbacks'
          CALL jules_print('imogen_check',jules_message)
          check_flag = .TRUE.
        ELSE
          WRITE(jules_message,*) 'Run is for CO2 emissions along with ' //    &
                  'prescribed values of non-CO2 gases'
          CALL jules_print('imogen_check',jules_message)
          WRITE(jules_message,*) 'There are NO land/ocean feedbacks'
          CALL jules_print('imogen_check',jules_message)
          check_flag = .TRUE.
        END IF
      END IF

      IF (land_feed_co2 .AND. ( .NOT. ocean_feed)) THEN
        IF ( .NOT. include_non_co2) THEN
          WRITE(jules_message,*) 'Run is for CO2 emissions only'
          CALL jules_print('imogen_check',jules_message)
          WRITE(jules_message,*) 'There are land CO2 but no ocean feedbacks'
          CALL jules_print('imogen_check',jules_message)
          check_flag = .TRUE.
        ELSE
          WRITE(jules_message,*) 'Run is for CO2 emissions along with ' //    &
                  'prescribed values of non-CO2 gases'
          CALL jules_print('imogen_check',jules_message)
          WRITE(jules_message,*) 'There are land but no ocean feedbacks'
          CALL jules_print('imogen_check',jules_message)
          check_flag = .TRUE.
        END IF
      END IF

      IF ( .NOT. land_feed_co2 .AND. ocean_feed) THEN
        IF ( .NOT. include_non_co2) THEN
          WRITE(jules_message,*) 'Run is for CO2 emissions only'
          CALL jules_print('imogen_check',jules_message)
          WRITE(jules_message,*) 'There are ocean but no land CO2 feedbacks'
          CALL jules_print('imogen_check',jules_message)
          check_flag = .TRUE.
        ELSE
          WRITE(jules_message,*) 'Run is for CO2 emissions along with ' //    &
                  'prescribed values of non-CO2 gases'
          CALL jules_print('imogen_check',jules_message)
          WRITE(jules_message,*) 'There are ocean but no land feedbacks'
          CALL jules_print('imogen_check',jules_message)
          check_flag = .TRUE.
        END IF
      END IF

      IF (land_feed_co2 .AND. ocean_feed) THEN
        IF ( .NOT. include_non_co2) THEN
          WRITE(jules_message,*) 'Run is for CO2 emissions only'
          CALL jules_print('imogen_check',jules_message)
          WRITE(jules_message,*) 'There are ocean and land CO2 feedbacks'
          CALL jules_print('imogen_check',jules_message)
          check_flag = .TRUE.
        ELSE
          WRITE(jules_message,*) 'Run is for CO2 emissions along with ' //    &
                  'prescribed values of non-CO2 gases'
          CALL jules_print('imogen_check',jules_message)
          WRITE(jules_message,*) 'There are ocean and land feedbacks'
          CALL jules_print('imogen_check',jules_message)
          check_flag = .TRUE.
        END IF
      END IF
      ! End of check over different combinations with prescribed carbon
      ! emissions.
    END IF
    ! End of check if CO2 is included.
  END IF
  ! End of check if analogue model used.
END IF

IF ( .NOT. check_flag)                                                        &
  CALL log_fatal("IMOGEN_CHECK",                                              &
                 'Combination not yet allowed')

RETURN

END SUBROUTINE imogen_check
#endif

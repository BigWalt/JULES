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

SUBROUTINE imogen_confirmed_run(                                              &
  c_emissions, include_co2, include_non_co2, land_feed_co2, ocean_feed,       &
  l_impacts, wgen, anlg, anom)

USE logging_mod, ONLY: log_fatal

USE jules_print_mgr, ONLY:                                                    &
  jules_message,                                                              &
  jules_print
IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   This is a far more rigourous check than IMOGEN_CHECK. In time these checks
!   will be merged with IMOGEN_CHECK and this routine will be retired.
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Written by: C. Huntingford (5th October 2004)
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
  check_flag_extra,                                                           &
              !WORK Confirms that configuration is OK
  l_impacts,                                                                  &
              !IN If true an impacts model used
  wgen,                                                                       &
              !IN Weather generator
  anlg,                                                                       &
              !IN True if the analogue model is used
  anom        !IN True if the anomalies are used



check_flag_extra = .FALSE.

! Transient run with carbon cycle
!      IF (C_EMISSIONS.AND.INCLUDE_CO2.AND.INCLUDE_NON_CO2.AND.
IF (c_emissions .AND. include_co2 .AND. land_feed_co2   .AND.                 &
   ocean_feed  .AND. l_impacts   .AND. ( .NOT. wgen) .AND.                    &
   anlg        .AND. anom) THEN
  WRITE(jules_message,*) ' '
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*)                                                      &
    '***************************************************'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'This is a full analogue model simulation'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Carbon cycle driven by emissions'
  CALL jules_print('imogen_confirmed_run',jules_message)

  !        WRITE(*,*) 'Non-co2 forcings are prescribed'
  WRITE(jules_message,*)                                                      &
    'Land co2 and ocean feedbacks on the carbon cycle included'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*)                                                      &
    'Terrestrial carbon content calculated with user DGVM'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Weather generator is switched off'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*)                                                      &
    '***************************************************'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) ' '
  CALL jules_print('imogen_confirmed_run',jules_message)

  check_flag_extra = .TRUE.
END IF

! Spin-up to the above
!      IF (C_EMISSIONS.AND.INCLUDE_CO2.AND.INCLUDE_NON_CO2.AND.
IF (c_emissions .AND. include_co2 .AND. land_feed_co2   .AND.                 &
   ocean_feed  .AND. l_impacts   .AND. ( .NOT. wgen) .AND.                    &
   anlg        .AND. ( .NOT. anom)) THEN
  WRITE(jules_message,*) ' '
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*)                                                      &
    '***************************************************'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'SPIN UP for full analogue model simulation below:'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Carbon cycle driven by emissions'
  CALL jules_print('imogen_confirmed_run',jules_message)

  !        WRITE(*,*) 'Non-co2 forcings are prescribed'
  WRITE(jules_message,*)                                                      &
    'Land CO2 and ocean feedbacks on the carbon cycle included'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*)                                                      &
    'Terrestrial carbon content calculated with user DGVM'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Weather generator is switched off'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*)                                                      &
    '***************************************************'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) ' '
  CALL jules_print('imogen_confirmed_run',jules_message)

  check_flag_extra = .TRUE.
END IF

! "Hydrology 20th century simulations"
IF (( .NOT. c_emissions)     .AND. include_co2      .AND.                     &
   ( .NOT. include_non_co2) .AND. ( .NOT. land_feed_co2) .AND.                &
   ( .NOT. ocean_feed)      .AND. l_impacts        .AND.                      &
   ( .NOT. wgen)            .AND. ( .NOT. anlg)      .AND.                    &
   ( .NOT. anom)) THEN
  WRITE(jules_message,*) ' '
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*)                                                      &
    '***************************************************'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Hydrology of 20th Century simulation'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Allows user to provide a file of CO2 concs.'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Terrestrial response calculated with user SVAT'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Weather generator is switched off'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*)                                                      &
    '***************************************************'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) ' '
  CALL jules_print('imogen_confirmed_run',jules_message)

  check_flag_extra = .TRUE.
END IF

IF (( .NOT. c_emissions)     .AND. ( .NOT. include_co2) .AND.                 &
   ( .NOT. include_non_co2) .AND. ( .NOT. land_feed_co2)   .AND.              &
   ( .NOT. ocean_feed)      .AND. l_impacts          .AND.                    &
   ( .NOT. wgen)            .AND. ( .NOT. anlg)        .AND.                  &
   ( .NOT. anom)) THEN
  WRITE(jules_message,*) ' '
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*)                                                      &
    '***************************************************'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'SPIN TO or CO2 FIXED for:'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Hydrology of 20th Century simulation'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Allows user to provide a file of CO2 concs.'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Terrestrial response calculated with user SVAT'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Weather generator is switched off'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*)                                                      &
    '***************************************************'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) ' '
  CALL jules_print('imogen_confirmed_run',jules_message)

  check_flag_extra = .TRUE.
END IF

! Analogue model simulations with CO2 prescribed
IF (( .NOT. c_emissions) .AND. (include_co2)    .AND.                         &
   (include_non_co2)  .AND. ( .NOT. land_feed_co2) .AND.                      &
   ( .NOT. ocean_feed)  .AND. l_impacts        .AND.                          &
   ( .NOT. wgen)        .AND. anlg             .AND.                          &
   anom) THEN
  WRITE(jules_message,*) ' '
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*)                                                      &
    '***************************************************'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Transient analogue model simulation'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Allows user to provide a file of CO2 concs.'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Terrestrial response calculated with user SVAT'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Weather generator is switched off'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*)                                                      &
    '***************************************************'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) ' '
  CALL jules_print('imogen_confirmed_run',jules_message)

  check_flag_extra = .TRUE.
END IF

IF (( .NOT. c_emissions)     .AND. ( .NOT. include_co2) .AND.                 &
   ( .NOT. include_non_co2) .AND. ( .NOT. land_feed_co2)   .AND.              &
   ( .NOT. ocean_feed)      .AND. l_impacts          .AND.                    &
   ( .NOT. wgen)            .AND. anlg               .AND.                    &
   ( .NOT. anom)) THEN
  WRITE(jules_message,*) ' '
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*)                                                      &
    '***************************************************'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'SPIN-UP solution to:'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Transient analogue model simulation'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Allows user to provide a file of CO2 concs.'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Terrestrial response calculated with user SVAT'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) 'Weather generator is switched off'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*)                                                      &
    '***************************************************'
  CALL jules_print('imogen_confirmed_run',jules_message)

  WRITE(jules_message,*) ' '
  CALL jules_print('imogen_confirmed_run',jules_message)

  check_flag_extra = .TRUE.
END IF

IF ( .NOT. anom) THEN
  check_flag_extra = .TRUE.
END IF

IF ( .NOT. check_flag_extra)                                                  &
  CALL log_fatal("IMOGEN_CONFIRMED_RUN",                                      &
                 'This configuration of the AM will be ' //                   &
                 'available at some point, but is not ' //                    &
                 'yet checked')

RETURN

END SUBROUTINE imogen_confirmed_run
#endif

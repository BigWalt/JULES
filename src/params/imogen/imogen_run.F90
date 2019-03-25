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

MODULE imogen_run

USE io_constants, ONLY: max_file_name_len

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Description:
!   Default parameters and variables required for the general imogen setup
!     Values can be set in the imogen.nml
!
! Code Owner: Please refer to ModuleLeaders.txt
!             This file belongs in IMOGEN
!
! Code Description:
!   Language: Fortran 90.
!   
!-----------------------------------------------------------------------------

REAL ::                                                                       &
  co2_init_ppmv = 286.085
              ! Initial CO2 concentration (ppmv)

CHARACTER(LEN=max_file_name_len) ::                                           &
  file_points_order = 'data/imogen/points_order.dat',                         &
              ! File containing the mapping of points in the
              ! IMOGEN grid to the JULES grid
  file_scen_emits = 'data/imogen/emits_HADCM3.dat',                           &
              ! If used, file containing CO2 emissions in G
  file_non_co2_vals='',                                                       &
              ! If used, file containing non-CO2 values
  file_scen_co2_ppmv = 'data/imogen/co2_vals.dat'
              ! If used, file containing CO2 values

LOGICAL ::                                                                    &
  anlg = .TRUE.,                                                              &
              ! If true, then use the GCM analogue model
  anom = .TRUE.,                                                              &
              ! If true, then use the GCM analogue model
  c_emissions = .TRUE.,                                                       &
              ! If true, means CO2 concentration is calcula
  include_co2 = .TRUE.,                                                       &
              ! Are adjustments to CO2 values allowed?
  include_non_co2 = .TRUE.,                                                   &
              ! Are adjustments to non-CO2 values allowed?
  land_feed_co2 = .FALSE.,                                                    &
              ! Are land CO2 feedbacks allowed on atmospheric C
  ocean_feed = .FALSE.,                                                       &
              ! Are ocean feedbacks allowed on atmospheric
  wgen = .FALSE.
              ! Is the weather generator switched on.

INTEGER ::                                                                    &
  nyr_emiss = 241,                                                            &
              ! Number of years of emission data in file.
  initial_co2_year = 1860
              ! Year of intialisation CO2 value: required to get
              ! ocean feedback correct on restart
LOGICAL :: initialise_from_dump = .FALSE.
              ! T - initialise variables from a dump file
              ! F - let IMOGEN handle initialisation
CHARACTER(LEN=max_file_name_len) :: dump_file
              ! The dump file to initialise from if required

NAMELIST  / imogen_run_list/ co2_init_ppmv,file_scen_emits,                   &
                           file_scen_co2_ppmv,nyr_emiss,                      &
                           c_emissions,include_co2,                           &
                           include_non_co2,land_feed_co2,                     &
                           ocean_feed,wgen,anom,anlg,                         &
                           file_non_co2_vals,                                 &
                           file_points_order,                                 &
                           initialise_from_dump, dump_file,                   &
                           initial_co2_year



END MODULE imogen_run
#endif

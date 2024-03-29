










!******************************COPYRIGHT**************************************
! (c) Centre for Ecology and Hydrology.
! All rights reserved.
!
! This routine has been licensed to the other JULES partners for use and
! distribution under the JULES collaboration agreement, subject to the terms
! and conditions set out therein.
!
! [Met Office Ref SC0237] 
!******************************COPYRIGHT**************************************
MODULE init_soil_biogeochem_mod

IMPLICIT NONE

CONTAINS

SUBROUTINE init_soil_biogeochem(nml_dir)
!-----------------------------------------------------------------------------
! Description:
!   Reads in soil biogeochemistry namelist items and checks them for
!   consistency.
!
! Code Owner: Please refer to ModuleLeaders.txt
! This file belongs in TECHNICAL
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!-----------------------------------------------------------------------------

USE io_constants, ONLY: namelist_unit

USE string_utils_mod, ONLY: to_string

USE jules_soil_biogeochem_mod, ONLY:                                       &
 ! imported scalar parameters
   soil_model_ecosse, soil_model_1pool, soil_model_rothc,                   &
 ! imported procedures
   check_jules_soil_biogeochem,                                             &
 ! imported scalar variables
   l_q10, soil_bgc_model,                                                   &
 ! imported namelists
   jules_soil_biogeochem

USE logging_mod, ONLY: log_info, log_error, log_fatal
  
USE errormessagelength_mod, ONLY: errormessagelength

IMPLICIT NONE

!-----------------------------------------------------------------------------
! Arguments
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                         ! namelists

!-----------------------------------------------------------------------------
! Local scalar parameters.
!-----------------------------------------------------------------------------
CHARACTER(LEN=*), PARAMETER ::                                              &
     RoutineName = 'init_soil_biogeochem'   ! Name of this routine.
!-----------------------------------------------------------------------------
! Local variables.
!-----------------------------------------------------------------------------
INTEGER :: error  ! Error indicator
CHARACTER(LEN=errormessagelength) :: iomessage

!-----------------------------------------------------------------------------
! Open the namelist file.
!-----------------------------------------------------------------------------
CALL log_info(RoutineName, "Reading JULES_SOIL_BIOGEOCHEM namelist...")

OPEN(namelist_unit,                                                         &
     FILE=(TRIM(nml_dir) // '/' // 'jules_soil_biogeochem.nml'),            &
     STATUS='old', POSITION='rewind', ACTION='read', IOSTAT = error,          &
     IOMSG = iomessage)

IF ( error /= 0 ) THEN
  CALL log_fatal( RoutineName,                                              &
                  "Error opening namelist file "                    //      &
                  "jules_soil_biogeochem.nml "                      //      &
                  "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //      &
                  TRIM(iomessage) // ")" )
END IF

!-----------------------------------------------------------------------------
! Read the namelist.
!-----------------------------------------------------------------------------
READ(namelist_unit, NML = jules_soil_biogeochem, IOSTAT = error,                &
     IOMSG = iomessage)
IF ( error /= 0 ) THEN
  CALL log_fatal( RoutineName,                                              &
                  "Error reading namelist JULES_SOIL_BIOGEOCHEM " //        &
                  "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //      &
                  TRIM(iomessage) // ")" )
END IF

!-----------------------------------------------------------------------------
! Close namelist file.
!-----------------------------------------------------------------------------
CLOSE(namelist_unit, IOSTAT = error, IOMSG = iomessage)
IF ( error /= 0 ) THEN
  CALL log_fatal( RoutineName,                                              &
                  "Error closing namelist file "                    //      &
                  "jules_soil_biogeochem.nml "                      //      &
                  "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //      &
                  TRIM(iomessage) // ")" )
END IF

!-----------------------------------------------------------------------------
! Check that settings are reasonable.
!-----------------------------------------------------------------------------
CALL check_jules_soil_biogeochem()

!-----------------------------------------------------------------------------
! Print some human friendly summary information about the selected options.
!-----------------------------------------------------------------------------
IF ( soil_bgc_model == soil_model_1pool ) THEN
  CALL log_info( RoutineName,                                               &
                 "Soil C is modelled using a single pool model with " //    &
                 "fixed C." )
ELSE IF ( soil_bgc_model == soil_model_rothc ) THEN
  CALL log_info( RoutineName, "Soil C is modelled using RothC." )
ELSE IF ( soil_bgc_model == soil_model_ecosse ) THEN
  CALL log_info( RoutineName, "Soil C is modelled using ECOSSE." )
ELSE
  CALL log_info( RoutineName, "Soil C is modelled using model " //          &
                 to_string(soil_bgc_model) )
END IF

SELECT CASE ( soil_bgc_model )
CASE ( soil_model_1pool, soil_model_rothc )
  IF ( l_q10 ) THEN
    CALL log_info( RoutineName,                                           &
                   "Q10 equation will be used for soil respiration" )
  ELSE
    CALL log_info( RoutineName,                                           &
                   "RothC equations will be used for soil respiration" )
  END IF
END SELECT

RETURN
END SUBROUTINE init_soil_biogeochem

END MODULE init_soil_biogeochem_mod

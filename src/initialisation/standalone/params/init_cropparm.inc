#if !defined(UM_JULES)
! *****************************COPYRIGHT**************************************
! (C) Crown copyright Met Office. All rights reserved.
! For further details please refer to the file COPYRIGHT.txt
! which you should have received as part of this distribution.
! *****************************COPYRIGHT**************************************

SUBROUTINE init_cropparm(nml_dir)

  USE missing_data_mod, ONLY : rmdi

  USE io_constants, ONLY : namelist_unit

  USE string_utils_mod, ONLY : to_string

  USE jules_surface_types_mod, ONLY : ncpft, nnpft
  
  USE cropparm
  
  USE cropparm_io
  
  USE jules_vegetation_mod, ONLY : l_crop
  
  USE errormessagelength_mod, ONLY: errormessagelength

  IMPLICIT NONE
  
!-----------------------------------------------------------------------------
! Description:
!   Initialises the crop PFT parameters
!
! Code Owner: Please refer to ModuleLeaders.txt
!
! Code Description:
!   Language: Fortran 90.
!   This code is written to JULES coding standards v1.
!
!-----------------------------------------------------------------------------
! Arguments
  CHARACTER(LEN=*), INTENT(IN) :: nml_dir  ! The directory containing the
                                           ! namelists

! Work variables
  INTEGER :: error  ! Error indicator
  CHARACTER(LEN=errormessagelength) :: iomessage

  INTEGER :: i ! WORK Loop counter

!----------------------------------------------------------------------------

! Nothing to do if crop model is not selected
  IF ( .NOT. l_crop ) RETURN

!-----------------------------------------------------------------------------
! Initialise namelist values before reading them
!-----------------------------------------------------------------------------

  T_BSE_io(:)     = rmdi
  T_OPT_io(:)     = rmdi
  T_MAX_io(:)     = rmdi
  TT_EMR_io(:)    = rmdi
  
  CRIT_PP_io(:)   = rmdi 
  PP_SENS_io(:)   = rmdi
  RT_DIR_io(:)    = rmdi
  ALPHA1_io(:)    = rmdi
  
  ALPHA2_io(:)    = rmdi
  ALPHA3_io(:)    = rmdi
  BETA1_io(:)     = rmdi
  BETA2_io(:)     = rmdi
  BETA3_io(:)     = rmdi
  
  gamma_io(:)     = rmdi
  DELTA_io(:)     = rmdi
  REMOB_io(:)     = rmdi
  CFRAC_S_io(:)   = rmdi
  CFRAC_R_io(:)   = rmdi
  
  CFRAC_L_io(:)   = rmdi
  ALLO1_io(:)     = rmdi
  ALLO2_io(:)     = rmdi
  
  mu_io(:)             = rmdi
  nu_io(:)             = rmdi
  yield_frac_io(:)     = rmdi
  initial_carbon_io(:) = rmdi
  initial_c_dvi_io(:)  = rmdi
  sen_dvi_io(:)        = rmdi
  t_mort_io(:)         = rmdi
  
!-----------------------------------------------------------------------------
! Read namelist
!-----------------------------------------------------------------------------
  CALL log_info("init_cropparm", "Reading JULES_CROPPARM namelist...")

! Open the crop pft parameters namelist file
  OPEN(namelist_unit, FILE=(TRIM(nml_dir) // '/' // 'crop_params.nml'),       &
                 STATUS='old', POSITION='rewind', ACTION='read', IOSTAT=error,&
                 IOMSG=iomessage)
  IF ( error /= 0 )                                                           &
    CALL log_fatal("init_cropparm",                                           &
                   "Error opening namelist file crop_params.nml " //          &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                   TRIM(iomessage) // ")")

  READ(namelist_unit, nml=jules_cropparm, IOSTAT=error, IOMSG=iomessage)
  IF ( error /= 0 )                                                           &
    CALL log_fatal("init_cropparm",                                           &
                   "Error reading namelist JULES_CROPPARM " //                &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                   TRIM(iomessage) // ")")

! Close the namelist file
  CLOSE(namelist_unit, IOSTAT=error, IOMSG=iomessage)
  IF ( error /= 0 )                                                           &
    CALL log_fatal("init_cropparm",                                           &
                   "Error closing namelist file crop_params.nml " //          &
                   "(IOSTAT=" // TRIM(to_string(error)) // " IOMSG=" //       &
                   TRIM(iomessage) // ")")


!-----------------------------------------------------------------------------
! Process the namelist values
!-----------------------------------------------------------------------------
! Copy values from fixed length arrays used in namelist into allocated arrays  

  T_BSE(:)   = T_BSE_io(1:ncpft)
  T_OPT(:)   = T_OPT_io(1:ncpft)
  T_MAX(:)   = T_MAX_io(1:ncpft)
  TT_EMR(:)  = TT_EMR_io(1:ncpft)
  CRIT_PP(:) = CRIT_PP_io(1:ncpft)
  PP_SENS(:) = PP_SENS_io(1:ncpft)
  RT_DIR(:)  = RT_DIR_io(1:ncpft)
  ALPHA1(:)  = ALPHA1_io(1:ncpft)
  ALPHA2(:)  = ALPHA2_io(1:ncpft)
  ALPHA3(:)  = ALPHA3_io(1:ncpft)
  BETA1(:)   = BETA1_io(1:ncpft)
  BETA2(:)   = BETA2_io(1:ncpft)
  BETA3(:)   = BETA3_io(1:ncpft)
  r_gamma(:) = gamma_io(1:ncpft)
  DELTA(:)   = DELTA_io(1:ncpft)
  REMOB(:)   = REMOB_io(1:ncpft)
  CFRAC_S(:) = CFRAC_S_io(1:ncpft)
  CFRAC_R(:) = CFRAC_R_io(1:ncpft)
  CFRAC_L(:) = CFRAC_L_io(1:ncpft)
  ALLO1(:)   = ALLO1_io(1:ncpft)
  ALLO2(:)   = ALLO2_io(1:ncpft)
  mu(:)             = mu_io(1:ncpft)
  nu(:)             = nu_io(1:ncpft)
  yield_frac(:)     = yield_frac_io(1:ncpft)  
  initial_carbon(:) = initial_carbon_io(1:ncpft)
  initial_c_dvi(:)  = initial_c_dvi_io(1:ncpft)
  sen_dvi(:)        = sen_dvi_io(1:ncpft)
  t_mort(:)         = t_mort_io(1:ncpft)

!-----------------------------------------------------------------------------
! Check that all required variables were present in the namelist.
! The namelist variables were initialised to rmdi.
!-----------------------------------------------------------------------------  
  error = 0  

  IF ( ANY( ABS( T_BSE(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for T_BSE")
  END IF
  IF ( ANY( ABS( T_OPT(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for T_OPT")
  END IF
  IF ( ANY( ABS( T_MAX(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for T_MAX")
  END IF
  IF ( ANY( ABS( TT_EMR(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for TT_EMR")
  END IF  
  
  
  IF ( ANY( ABS( CRIT_PP(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for CRIT_PP")
  END IF
  IF ( ANY( ABS( PP_SENS(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for PP_SENS")
  END IF
  IF ( ANY( ABS( RT_DIR(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for RT_DIR")
  END IF  
  IF ( ANY( ABS( ALPHA1(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for ALPHA1")
  END IF  
  

  IF ( ANY( ABS( ALPHA2(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for ALPHA2")
  END IF
  IF ( ANY( ABS( ALPHA3(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for ALPHA3")
  END IF
  IF ( ANY( ABS( BETA1(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for BETA1")
  END IF
  IF ( ANY( ABS( BETA2(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for BETA2")
  END IF  
  IF ( ANY( ABS( BETA3(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for BETA3")
  END IF
  

  IF ( ANY( ABS( r_gamma(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for gamma")
  END IF
  IF ( ANY( ABS( DELTA(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for DELTA")
  END IF
  IF ( ANY( ABS( REMOB(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for REMOB")
  END IF
  IF ( ANY( ABS( CFRAC_S(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for CFRAC_S")
  END IF  
  IF ( ANY( ABS( CFRAC_R(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for CFRAC_R")
  END IF  


  IF ( ANY( ABS( CFRAC_L(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for CFRAC_L")
  END IF
  IF ( ANY( ABS( ALLO1(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for ALLO1")
  END IF
  IF ( ANY( ABS( ALLO2(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for ALLO2")
  END IF
  

  IF ( ANY( ABS( mu(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for mu")
  END IF
  IF ( ANY( ABS( nu(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for nu")
  END IF

  IF ( ANY( ABS( yield_frac(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for yield_frac")
  END IF
  IF ( ANY( ABS( initial_carbon(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for initial_carbon")
  END IF
  IF ( ANY( ABS( initial_c_dvi(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for initial_c_dvi")
  END IF
  IF ( ANY( ABS( sen_dvi(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for sen_dvi")
  END IF
  IF ( ANY( ABS( t_mort(:) - rmdi ) < EPSILON(1.0) ) ) THEN
    error = 1
    CALL log_error("init_cropparm", "No value for t_mort")
  END IF


  IF ( error /= 0 ) THEN
    CALL log_fatal("init_cropparm",                                           &
                   "Variable(s) missing from namelist - see earlier " //      &
                   "error message(s)")
  END IF
                   
  RETURN
  
END SUBROUTINE init_cropparm
#endif

